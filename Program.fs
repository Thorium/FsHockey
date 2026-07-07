/// THE FS HOCKEY LEAGUE — Program Entry Point (MonoGame)
/// MonoGame window, 30 FPS game loop, keyboard input, menu/league state
module HockeyDemo.Program

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open HockeyDemo.Physics
open HockeyDemo.Game
open HockeyDemo.Drawing
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
      mutable Paused: bool
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
      Paused = false
      League = None }

// ─── Helpers ──────────────────────────────────────────────────────────

/// Configure entity speeds/power based on team selection.
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
        app.Paused <- false
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
    app.Paused <- false
    app.Mode <- Playing

/// Is the match over?
let inline matchOver (gs: GameState) =
    not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Key Mapping Helpers ──────────────────────────────────────────────

/// Apply player 1 directional + fire keys (Arrow keys + RShift/Enter)
let private mapPlayer1Keys (gs: GameState) (ks: KeyboardState) =
    gs.Input1 <-
        { Left = ks.IsKeyDown(Keys.Left)
          Right = ks.IsKeyDown(Keys.Right)
          Up = ks.IsKeyDown(Keys.Up)
          Down = ks.IsKeyDown(Keys.Down)
          Fire = ks.IsKeyDown(Keys.RightShift) || ks.IsKeyDown(Keys.Enter) }

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (gs: GameState) (ks: KeyboardState) =
    gs.Input2 <-
        { Left = ks.IsKeyDown(Keys.A)
          Right = ks.IsKeyDown(Keys.D)
          Up = ks.IsKeyDown(Keys.W)
          Down = ks.IsKeyDown(Keys.S)
          Fire = ks.IsKeyDown(Keys.Space) || ks.IsKeyDown(Keys.Tab) }

// ─── Gamepad input ────────────────────────────────────────────────────
// Left stick / d-pad to skate, A / B / right trigger to shoot.
// Pad 1 drives player 1, pad 2 drives player 2, merged with the keyboard.

let private GamepadDeadzone = 0.35f

/// Read a pad as an Input snapshot (Input.none when not connected).
let private gamepadInput (playerIndex: PlayerIndex) : Input =
    let pad = GamePad.GetState(playerIndex)

    if pad.IsConnected then
        let stick = pad.ThumbSticks.Left // Y axis: positive = up

        { Left = stick.X < -GamepadDeadzone || pad.DPad.Left = ButtonState.Pressed
          Right = stick.X > GamepadDeadzone || pad.DPad.Right = ButtonState.Pressed
          Up = stick.Y > GamepadDeadzone || pad.DPad.Up = ButtonState.Pressed
          Down = stick.Y < -GamepadDeadzone || pad.DPad.Down = ButtonState.Pressed
          Fire =
            pad.Buttons.A = ButtonState.Pressed
            || pad.Buttons.B = ButtonState.Pressed
            || pad.Triggers.Right > 0.12f }
    else
        Input.none

/// Combine keyboard and gamepad snapshots (either source counts).
let private mergeInput (a: Input) (b: Input) : Input =
    { Left = a.Left || b.Left
      Right = a.Right || b.Right
      Up = a.Up || b.Up
      Down = a.Down || b.Down
      Fire = a.Fire || b.Fire }

// ─── Main Game (MonoGame) ─────────────────────────────────────────────

type HockeyGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch: SpriteBatch = null
    let app = createAppState ()
    let gs = app.GameState
    let mutable prevKeyState = KeyboardState()

    do
        this.Window.Title <-
            "The FS Hockey League \u2014 By Tuomas Hietanen 2026"

        graphics.PreferredBackBufferWidth <- 960
        graphics.PreferredBackBufferHeight <- 620
        this.IsMouseVisible <- true
        this.IsFixedTimeStep <- true
        this.TargetElapsedTime <- System.TimeSpan.FromSeconds(1.0 / float GameFps)

        // Let the back buffer track the window size so the renderer can use
        // all of it (the renderer does its own aspect-correct scaling).
        this.Window.AllowUserResizing <- true

        this.Window.ClientSizeChanged.Add(fun _ ->
            let b = this.Window.ClientBounds

            if
                not graphics.IsFullScreen
                && b.Width > 0
                && b.Height > 0
                && (graphics.PreferredBackBufferWidth <> b.Width
                    || graphics.PreferredBackBufferHeight <> b.Height)
            then
                graphics.PreferredBackBufferWidth <- b.Width
                graphics.PreferredBackBufferHeight <- b.Height
                graphics.ApplyChanges())

    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        initTextures graphics.GraphicsDevice
        initFonts graphics.GraphicsDevice

    override _.UnloadContent() =
        disposeTextures ()
        disposeFonts ()

    /// Check if a key was just pressed this frame (not held)
    member private _.IsKeyPressed(key: Keys, current: KeyboardState) =
        current.IsKeyDown(key) && prevKeyState.IsKeyUp(key)

    override this.Update(gameTime) =
        let ks = Keyboard.GetState()

        // F11 toggles fullscreen (works in any mode). Fullscreen uses the
        // desktop resolution (borderless) so the game renders at native size
        // instead of stretching a 960x620 back buffer.
        if this.IsKeyPressed(Keys.F11, ks) then
            if graphics.IsFullScreen then
                graphics.IsFullScreen <- false
                graphics.PreferredBackBufferWidth <- 960
                graphics.PreferredBackBufferHeight <- 620
            else
                let dm = GraphicsAdapter.DefaultAdapter.CurrentDisplayMode
                graphics.HardwareModeSwitch <- false
                graphics.PreferredBackBufferWidth <- dm.Width
                graphics.PreferredBackBufferHeight <- dm.Height
                graphics.IsFullScreen <- true

            graphics.ApplyChanges()

        match app.Mode with
        | Menu ->
            if this.IsKeyPressed(Keys.Tab, ks) then
                app.ActiveColumn <- 1 - app.ActiveColumn

            if this.IsKeyPressed(Keys.Up, ks) || this.IsKeyPressed(Keys.Down, ks) then
                let delta = if ks.IsKeyDown(Keys.Up) then -1 else 1

                if app.ActiveColumn = 0 then
                    app.SelectedTeam1 <- (app.SelectedTeam1 + delta + NumTeams) % NumTeams
                else
                    app.SelectedTeam2 <- (app.SelectedTeam2 + delta + NumTeams) % NumTeams

            if this.IsKeyPressed(Keys.Enter, ks) then
                startExhibitionMatch app

            if this.IsKeyPressed(Keys.L, ks) then
                app.League <- Some(createLeagueState app.SelectedTeam1)
                app.Mode <- LeagueMatchup

            if this.IsKeyPressed(Keys.F, ks) then
                app.FastHuman <- not app.FastHuman

            if this.IsKeyPressed(Keys.H, ks) then
                app.HardMode <- not app.HardMode

            if this.IsKeyPressed(Keys.D5, ks) then
                app.FivePlayerMode <- not app.FivePlayerMode

            if this.IsKeyPressed(Keys.G, ks) then
                app.GamepadEnabled <- not app.GamepadEnabled

            if this.IsKeyPressed(Keys.Escape, ks) then
                this.Exit()

        | Playing ->
            if this.IsKeyPressed(Keys.P, ks) then
                app.Paused <- not app.Paused

            if not app.Paused then
                mapPlayer1Keys gs ks
                mapPlayer2Keys gs ks

                if app.GamepadEnabled then
                    gs.Input1 <- mergeInput gs.Input1 (gamepadInput PlayerIndex.One)
                    gs.Input2 <- mergeInput gs.Input2 (gamepadInput PlayerIndex.Two)

                for _ in 1..PhysicsTicksPerFrame do
                    gameTick gs

                gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.Mode <- Menu
                app.Paused <- false

            if this.IsKeyPressed(Keys.Space, ks) && matchOver gs then
                app.Mode <- Menu

        | LeagueMatchup ->
            if this.IsKeyPressed(Keys.Space, ks) then
                startLeagueMatch app

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.League <- None
                app.Mode <- Menu

        | LeaguePlaying ->
            if this.IsKeyPressed(Keys.P, ks) then
                app.Paused <- not app.Paused

            if not app.Paused then
                mapPlayer1Keys gs ks

                if app.GamepadEnabled then
                    gs.Input1 <- mergeInput gs.Input1 (gamepadInput PlayerIndex.One)

                for _ in 1..PhysicsTicksPerFrame do
                    gameTick gs

                gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)

            if this.IsKeyPressed(Keys.Space, ks) && matchOver gs then
                match app.League with
                | Some league ->
                    recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
                    simulateCpuRound league league.CurrentRound
                    let finished = advanceRound league
                    app.Mode <- if finished then LeagueFinalStandings else LeagueStandings
                | None -> app.Mode <- Menu

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.League <- None
                app.Mode <- Menu

        | LeagueStandings ->
            if this.IsKeyPressed(Keys.Space, ks) then
                app.Mode <- LeagueMatchup

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.League <- None
                app.Mode <- Menu

        | LeagueFinalStandings ->
            if this.IsKeyPressed(Keys.Space, ks) || this.IsKeyPressed(Keys.Escape, ks) then
                app.League <- None
                app.Mode <- Menu

        prevKeyState <- ks
        base.Update(gameTime)

    override _.Draw(gameTime) =
        graphics.GraphicsDevice.Clear(Color.Black)

        let w = graphics.GraphicsDevice.Viewport.Width
        let h = graphics.GraphicsDevice.Viewport.Height
        let fw = float32 w
        let fh = float32 h

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

        match app.Mode with
        | Menu ->
            drawMenu
                spriteBatch
                fw
                fh
                app.SelectedTeam1
                app.SelectedTeam2
                app.ActiveColumn
                app.FastHuman
                app.HardMode
                app.FivePlayerMode
                app.GamepadEnabled

        | Playing ->
            renderFrame spriteBatch gs w h false

            if app.Paused then
                drawPauseOverlay spriteBatch fw fh

        | LeagueMatchup ->
            match app.League with
            | Some league ->
                let t1, t2 = currentMatchup league

                drawLeagueMatchup
                    spriteBatch
                    fw
                    fh
                    (league.CurrentRound + 1)
                    league.Schedule.Length
                    teamNames.[t1]
                    teamNames.[t2]
            | None -> ()

        | LeaguePlaying ->
            renderFrame spriteBatch gs w h true

            if app.Paused then
                drawPauseOverlay spriteBatch fw fh

        | LeagueStandings ->
            app.League
            |> Option.iter (fun league ->
                drawLeagueStandings spriteBatch fw fh (getSortedStandings league) false league.HumanTeam)

        | LeagueFinalStandings ->
            app.League
            |> Option.iter (fun league ->
                drawLeagueStandings spriteBatch fw fh (getSortedStandings league) true league.HumanTeam)

        spriteBatch.End()
        base.Draw(gameTime)

// ─── Entry Point ──────────────────────────────────────────────────────

[<EntryPoint>]
let main _ =
    use game = new HockeyGame()
    game.Run()
    0
