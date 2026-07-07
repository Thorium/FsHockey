/// THE FS HOCKEY LEAGUE — Program Entry Point
/// WinForms window, 30 FPS game loop, keyboard input, menu/league state
module HockeyDemo.Program

open System
open System.Drawing
open System.Windows.Forms
open HockeyDemo.Physics
open HockeyDemo.Game
open HockeyDemo.Renderer

// ─── Application Mode ─────────────────────────────────────────────────

[<Struct>]
type AppMode =
    | Menu
    | Playing
    | LeagueMatchup
    | LeaguePlaying
    | LeagueStandings
    | LeagueFinalStandings

// ─── Application State ────────────────────────────────────────────────

type AppState =
    { GameState: GameState
      mutable Mode: AppMode
      mutable SelectedTeam1: int
      mutable SelectedTeam2: int
      mutable ActiveColumn: int
      mutable FastHuman: bool
      mutable HardMode: bool
      mutable FivePlayerMode: bool
      mutable GamepadEnabled: bool
      mutable League: LeagueState option }

let createAppState () =
    { GameState = createGameState ()
      Mode = Menu
      SelectedTeam1 = 0
      SelectedTeam2 = 1
      ActiveColumn = 0
      FastHuman = true
      HardMode = false
      FivePlayerMode = false
      GamepadEnabled = true
      League = None }

// ─── Helpers ──────────────────────────────────────────────────────────

/// Configure entity speeds/power based on team selection.
/// Acceleration is role-based: goalie = 5, forward = 2.
let setTeamSpeeds (app: AppState) =
    let gs = app.GameState
    let ppt = gs.PlayersPerTeam

    let applyTeam teamIdx startEnt isFast isCpu =
        let srcIdx = if isFast then humanFastTeamIdx else teamIdx
        let speeds = teamMaxSpeed.[srcIdx]
        let powers = teamShotPower.[srcIdx]
        let mult = if isCpu && app.HardMode then HardModeSpeedMult else 1.0

        for i in 0 .. ppt - 1 do
            let ent = gs.Entities.[startEnt + i]

            let statIdx =
                if gs.FivePlayerMode then
                    match i with
                    | 0 -> 0 // goalie
                    | i when i <= 2 -> min i 2 // center/forward
                    | _ -> 2 // wings and extra forward use forward stats
                else
                    min i 2

            ent.MaxSpeed <- speeds.[statIdx] * mult
            ent.ShotPower <- powers.[statIdx] * mult

            ent.Accel <-
                (if i = 0 && gs.FivePlayerMode then
                     GoalieAccel
                 else
                     ForwardAccel)
                * mult

            // Goalie is always capped at GoalieMaxSpeed regardless of team stats
            if i = 0 && gs.FivePlayerMode then
                ent.MaxSpeed <- min ent.MaxSpeed GoalieMaxSpeed

    let t1Human = (gs.Team1Idx = 0)
    let t2Human = (gs.Team2Idx = 0)
    applyTeam gs.Team1Idx 0 (app.FastHuman && t1Human) (not t1Human)
    applyTeam gs.Team2Idx gs.Team2Start (app.FastHuman && t2Human) (not t2Human)

    gs.ShotSpeed <-
        if app.HardMode then HardShotReleaseSpeed else ShotReleaseSpeed

/// Start a league match for the current round
let startLeagueMatch (app: AppState) =
    match app.League with
    | None -> ()
    | Some league ->
        let gs = app.GameState
        let t1, t2 = currentMatchup league
        gs.Team1Idx <- t1
        gs.Team2Idx <- t2
        gs.Team1Human <- (t1 = league.HumanTeam)
        gs.Team2Human <- false
        gs.NumPeriods <- LeaguePeriods
        setPlayerMode gs app.FivePlayerMode
        setTeamSpeeds app
        initMatch gs
        app.Mode <- LeaguePlaying

/// Start an exhibition match
let startExhibitionMatch (app: AppState) =
    let gs = app.GameState
    gs.Team1Idx <- app.SelectedTeam1
    gs.Team2Idx <- app.SelectedTeam2
    gs.Team1Human <- (gs.Team1Idx = 0)
    gs.Team2Human <- (gs.Team2Idx = 0)
    gs.NumPeriods <- ExhibitionPeriods
    setPlayerMode gs app.FivePlayerMode
    setTeamSpeeds app
    initMatch gs
    app.Mode <- Playing

/// Is the match over? (not playing, clock expired)
let inline matchOver (gs: GameState) =
    not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Key Mapping Helpers ──────────────────────────────────────────────
// Keyboard state is kept as Input snapshots separate from GameState.Input1/2
// so gamepad input can be merged in per tick without fighting the key events.

/// Apply player 1 directional + fire keys (Arrow keys + RShift/Enter)
let private mapPlayer1Keys (input: Input) key down =
    match key with
    | Keys.Left -> { input with Left = down }
    | Keys.Right -> { input with Right = down }
    | Keys.Up -> { input with Up = down }
    | Keys.Down -> { input with Down = down }
    | Keys.ShiftKey
    | Keys.Enter -> { input with Fire = down }
    | _ -> input

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (input: Input) key down =
    match key with
    | Keys.A -> { input with Left = down }
    | Keys.D -> { input with Right = down }
    | Keys.W -> { input with Up = down }
    | Keys.S -> { input with Down = down }
    | Keys.Space
    | Keys.Tab -> { input with Fire = down }
    | _ -> input

// ─── Gamepad input (XInput) ───────────────────────────────────────────
// Left stick / d-pad to skate, A / B / right trigger to shoot.
// Pad 1 drives player 1, pad 2 drives player 2, merged with the keyboard.

module private XInput =
    open System.Runtime.InteropServices

    [<StructLayout(LayoutKind.Sequential)>]
    type Gamepad =
        struct
            val mutable wButtons: uint16
            val mutable bLeftTrigger: byte
            val mutable bRightTrigger: byte
            val mutable sThumbLX: int16
            val mutable sThumbLY: int16
            val mutable sThumbRX: int16
            val mutable sThumbRY: int16
        end

    [<StructLayout(LayoutKind.Sequential)>]
    type State =
        struct
            val mutable dwPacketNumber: uint32
            val mutable Gamepad: Gamepad
        end

    [<DllImport("xinput1_4.dll")>]
    extern int XInputGetState(uint32 dwUserIndex, State& pState)

/// False once XInput turns out to be unavailable on this machine.
let mutable private xinputAvailable = true

/// ~0.35 of the full int16 stick range (matches the browser build's deadzone)
let private GamepadDeadzone = 11469

/// Read pad `idx` as an Input snapshot (Input.none when not connected).
let private gamepadInput (idx: int) : Input =
    if not xinputAvailable then
        Input.none
    else
        let mutable state = XInput.State()

        let connected =
            try
                XInput.XInputGetState(uint32 idx, &state) = 0
            with :? DllNotFoundException | :? EntryPointNotFoundException ->
                xinputAvailable <- false
                false

        if connected then
            let gp = state.Gamepad
            let buttons = int gp.wButtons
            let lx = int gp.sThumbLX
            let ly = int gp.sThumbLY // XInput Y axis: positive = up

            { Left = lx < -GamepadDeadzone || buttons &&& 0x0004 <> 0
              Right = lx > GamepadDeadzone || buttons &&& 0x0008 <> 0
              Up = ly > GamepadDeadzone || buttons &&& 0x0001 <> 0
              Down = ly < -GamepadDeadzone || buttons &&& 0x0002 <> 0
              Fire = buttons &&& 0x1000 <> 0 || buttons &&& 0x2000 <> 0 || gp.bRightTrigger > 30uy }
        else
            Input.none

/// Combine keyboard and gamepad snapshots (either source counts).
let private mergeInput (a: Input) (b: Input) : Input =
    { Left = a.Left || b.Left
      Right = a.Right || b.Right
      Up = a.Up || b.Up
      Down = a.Down || b.Down
      Fire = a.Fire || b.Fire }

// ─── Main Form ────────────────────────────────────────────────────────

type HockeyForm() as this =
    inherit Form()

    let app = createAppState ()
    let gs = app.GameState

    // Keyboard-only input snapshots; merged with gamepads each tick
    let mutable kbInput1 = Input.none
    let mutable kbInput2 = Input.none

    let panel =
        { new Panel() with
            override _.OnPaintBackground _ = () }

    let timer = new Timer(Interval = 1000 / GameFps)

    do
        this.Text <-
            "The FS Hockey League \u2014 By Tuomas Hietanen 2026"

        this.ClientSize <- Size(960, 620)
        this.StartPosition <- FormStartPosition.CenterScreen
        this.KeyPreview <- true
        this.DoubleBuffered <- true

        panel.Dock <- DockStyle.Fill
        panel.BackColor <- Color.Black
        this.Controls.Add panel

        // Enable double buffering via reflection
        let setStyle flag value =
            let flags =
                Reflection.BindingFlags.Instance
                ||| Reflection.BindingFlags.InvokeMethod
                ||| Reflection.BindingFlags.NonPublic

            panel.GetType().InvokeMember("SetStyle", flags, null, panel, [| box flag; box value |])
            |> ignore

        setStyle ControlStyles.DoubleBuffer true
        setStyle ControlStyles.AllPaintingInWmPaint true
        setStyle ControlStyles.UserPaint true

        panel.Paint.Add(fun e -> this.OnRender e.Graphics)
        timer.Tick.Add(fun _ -> this.OnTick())
        this.KeyDown.Add(fun e -> this.OnKey(e, true))
        this.KeyUp.Add(fun e -> this.OnKey(e, false))
        timer.Start()

    member _.OnTick() =
        match app.Mode with
        | Playing
        | LeaguePlaying ->
            gs.Input1 <- kbInput1
            gs.Input2 <- kbInput2

            if app.GamepadEnabled then
                gs.Input1 <- mergeInput gs.Input1 (gamepadInput 0)

                if app.Mode = Playing then
                    gs.Input2 <- mergeInput gs.Input2 (gamepadInput 1)

            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)
        | _ -> ()

        panel.Invalidate()

    member _.OnRender(target: Graphics) =
        let w = panel.ClientSize.Width
        let h = panel.ClientSize.Height

        if w > 0 && h > 0 then
            // The panel is double-buffered (ControlStyles set in the ctor), so
            // draw straight into it — no manual per-frame backbuffer Bitmap.
            let g = target
            g.SmoothingMode <- Drawing2D.SmoothingMode.AntiAlias
            g.TextRenderingHint <- Text.TextRenderingHint.ClearTypeGridFit

            let fw = float32 w
            let fh = float32 h

            match app.Mode with
            | Menu ->
                drawMenu
                    g
                    fw
                    fh
                    app.SelectedTeam1
                    app.SelectedTeam2
                    app.ActiveColumn
                    app.FastHuman
                    app.HardMode
                    app.FivePlayerMode
                    app.GamepadEnabled

            | Playing -> renderFrame g gs w h false

            | LeagueMatchup ->
                match app.League with
                | Some league ->
                    let t1, t2 = currentMatchup league

                    drawLeagueMatchup
                        g
                        fw
                        fh
                        (league.CurrentRound + 1)
                        league.Schedule.Length
                        teamNames.[t1]
                        teamNames.[t2]
                | None -> ()

            | LeaguePlaying -> renderFrame g gs w h true

            | LeagueStandings ->
                app.League
                |> Option.iter (fun league ->
                    drawLeagueStandings g fw fh (getSortedStandings league) false league.HumanTeam)

            | LeagueFinalStandings ->
                app.League
                |> Option.iter (fun league ->
                    drawLeagueStandings g fw fh (getSortedStandings league) true league.HumanTeam)

    member _.OnKey(e: KeyEventArgs, down) =
        if not down then
            kbInput1 <- mapPlayer1Keys kbInput1 e.KeyCode false
            kbInput2 <- mapPlayer2Keys kbInput2 e.KeyCode false

        match app.Mode with
        | Menu when down -> this.HandleMenuKey e.KeyCode
        | Playing -> this.HandleGameKey(e.KeyCode, down)
        | LeagueMatchup when down -> this.HandleLeagueMatchupKey e.KeyCode
        | LeaguePlaying -> this.HandleLeagueGameKey(e.KeyCode, down)
        | LeagueStandings when down -> this.HandleLeagueStandingsKey e.KeyCode
        | LeagueFinalStandings when down -> this.HandleLeagueFinalKey e.KeyCode
        | _ -> ()

        e.Handled <- true

    // ─── Menu ─────────────────────────────────────────────────────

    member _.HandleMenuKey key =
        match key with
        | Keys.Tab -> app.ActiveColumn <- 1 - app.ActiveColumn

        | Keys.Up
        | Keys.Down ->
            let delta = if key = Keys.Up then -1 else 1

            if app.ActiveColumn = 0 then
                app.SelectedTeam1 <- (app.SelectedTeam1 + delta + NumTeams) % NumTeams
            else
                app.SelectedTeam2 <- (app.SelectedTeam2 + delta + NumTeams) % NumTeams

        | Keys.Enter
        | Keys.Return -> startExhibitionMatch app

        | Keys.L ->
            app.League <- Some(createLeagueState app.SelectedTeam1)
            app.Mode <- LeagueMatchup

        | Keys.F -> app.FastHuman <- not app.FastHuman
        | Keys.H -> app.HardMode <- not app.HardMode
        | Keys.D5 -> app.FivePlayerMode <- not app.FivePlayerMode
        | Keys.G -> app.GamepadEnabled <- not app.GamepadEnabled

        | Keys.Escape -> Application.Exit()

        | _ -> ()

    // ─── Exhibition Game ──────────────────────────────────────────

    member _.HandleGameKey(key, down) =
        kbInput1 <- mapPlayer1Keys kbInput1 key down
        kbInput2 <- mapPlayer2Keys kbInput2 key down

        if down then
            match key with
            | Keys.Escape -> app.Mode <- Menu
            | Keys.Space when matchOver gs -> app.Mode <- Menu
            | _ -> ()

    // ─── League: Pre-match ────────────────────────────────────────

    member _.HandleLeagueMatchupKey key =
        match key with
        | Keys.Space -> startLeagueMatch app
        | Keys.Escape ->
            app.League <- None
            app.Mode <- Menu
        | _ -> ()

    // ─── League: In-game ──────────────────────────────────────────

    member _.HandleLeagueGameKey(key, down) =
        kbInput1 <- mapPlayer1Keys kbInput1 key down

        if down then
            match key with
            | Keys.Space when matchOver gs ->
                match app.League with
                | Some league ->
                    recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
                    simulateCpuRound league league.CurrentRound
                    let finished = advanceRound league
                    app.Mode <- if finished then LeagueFinalStandings else LeagueStandings
                | None -> app.Mode <- Menu
            | Keys.Escape ->
                app.League <- None
                app.Mode <- Menu
            | _ -> ()

    // ─── League: Standings ────────────────────────────────────────

    member _.HandleLeagueStandingsKey key =
        match key with
        | Keys.Space -> app.Mode <- LeagueMatchup
        | Keys.Escape ->
            app.League <- None
            app.Mode <- Menu
        | _ -> ()

    // ─── League: Final Standings ──────────────────────────────────

    member _.HandleLeagueFinalKey key =
        match key with
        | Keys.Space
        | Keys.Escape ->
            app.League <- None
            app.Mode <- Menu
        | _ -> ()

    override this.Dispose disposing =
        if disposing then
            timer.Stop()
            timer.Dispose()

        base.Dispose disposing

// ─── Entry Point ──────────────────────────────────────────────────────

[<STAThread; EntryPoint>]
let main _ =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false
    use form = new HockeyForm()
    Application.Run form
    0
