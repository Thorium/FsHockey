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

/// Start a league match for the current round
let startLeagueMatch (app: AppState) =
    match app.League with
    | None -> ()
    | Some league ->
        let gs = app.GameState
        let t1, t2 = currentMatchup league
        gs.Team1Idx <- t1
        gs.Team2Idx <- t2
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
    gs.NumPeriods <- ExhibitionPeriods
    setPlayerMode gs app.FivePlayerMode
    setTeamSpeeds app
    initMatch gs
    app.Mode <- Playing

/// Is the match over? (not playing, clock expired)
let inline matchOver (gs: GameState) =
    not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Key Mapping Helpers ──────────────────────────────────────────────

/// Apply player 1 directional + fire keys (Arrow keys + RShift/Enter)
let private mapPlayer1Keys (gs: GameState) key down =
    match key with
    | Keys.Left -> gs.KeyLeft1 <- down
    | Keys.Right -> gs.KeyRight1 <- down
    | Keys.Up -> gs.KeyUp1 <- down
    | Keys.Down -> gs.KeyDown1 <- down
    | Keys.RShiftKey
    | Keys.Enter -> gs.KeyFire1 <- down
    | _ -> ()

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (gs: GameState) key down =
    match key with
    | Keys.A -> gs.KeyLeft2 <- down
    | Keys.D -> gs.KeyRight2 <- down
    | Keys.W -> gs.KeyUp2 <- down
    | Keys.S -> gs.KeyDown2 <- down
    | Keys.Space
    | Keys.Tab -> gs.KeyFire2 <- down
    | _ -> ()

// ─── Main Form ────────────────────────────────────────────────────────

type HockeyForm() as this =
    inherit Form()

    let app = createAppState ()
    let gs = app.GameState

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
            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)
        | _ -> ()

        panel.Invalidate()

    member _.OnRender(target: Graphics) =
        let w = panel.ClientSize.Width
        let h = panel.ClientSize.Height

        if w <= 0 || h <= 0 then
            ()
        else

            use backBuffer = new Bitmap(w, h)
            use g = Graphics.FromImage backBuffer
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

            | Playing ->
                renderFrame
                    g
                    gs
                    w
                    h
                    false
                    app.SelectedTeam1
                    app.SelectedTeam2
                    app.ActiveColumn
                    false
                    app.FastHuman
                    app.HardMode
                    app.FivePlayerMode

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

            | LeaguePlaying ->
                renderFrame
                    g
                    gs
                    w
                    h
                    false
                    app.SelectedTeam1
                    app.SelectedTeam2
                    app.ActiveColumn
                    true
                    app.FastHuman
                    app.HardMode
                    app.FivePlayerMode

            | LeagueStandings ->
                app.League
                |> Option.iter (fun league ->
                    drawLeagueStandings g fw fh (getSortedStandings league) false league.HumanTeam)

            | LeagueFinalStandings ->
                app.League
                |> Option.iter (fun league ->
                    drawLeagueStandings g fw fh (getSortedStandings league) true league.HumanTeam)

            target.DrawImageUnscaled(backBuffer, 0, 0)

    member _.OnKey(e: KeyEventArgs, down) =
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

        | Keys.Escape -> Application.Exit()

        | _ -> ()

    // ─── Exhibition Game ──────────────────────────────────────────

    member _.HandleGameKey(key, down) =
        mapPlayer1Keys gs key down
        mapPlayer2Keys gs key down

        if down then
            match key with
            | Keys.Escape -> app.Mode <- Menu
            | Keys.Space when matchOver gs -> initMatch gs
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
        mapPlayer1Keys gs key down

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
