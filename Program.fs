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
                    | 0 -> 0
                    | i when i <= 2 -> min i 2
                    | _ -> 2
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

/// Is the match over?
let inline matchOver (gs: GameState) =
    not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Key Mapping Helpers ──────────────────────────────────────────────

/// Apply player 1 directional + fire keys (Arrow keys + RShift/Enter)
let private mapPlayer1Keys (gs: GameState) (ks: KeyboardState) =
    gs.KeyLeft1 <- ks.IsKeyDown(Keys.Left)
    gs.KeyRight1 <- ks.IsKeyDown(Keys.Right)
    gs.KeyUp1 <- ks.IsKeyDown(Keys.Up)
    gs.KeyDown1 <- ks.IsKeyDown(Keys.Down)
    gs.KeyFire1 <- ks.IsKeyDown(Keys.RightShift) || ks.IsKeyDown(Keys.Enter)

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (gs: GameState) (ks: KeyboardState) =
    gs.KeyLeft2 <- ks.IsKeyDown(Keys.A)
    gs.KeyRight2 <- ks.IsKeyDown(Keys.D)
    gs.KeyUp2 <- ks.IsKeyDown(Keys.W)
    gs.KeyDown2 <- ks.IsKeyDown(Keys.S)
    gs.KeyFire2 <- ks.IsKeyDown(Keys.Space) || ks.IsKeyDown(Keys.Tab)

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

            if this.IsKeyPressed(Keys.Escape, ks) then
                this.Exit()

        | Playing ->
            mapPlayer1Keys gs ks
            mapPlayer2Keys gs ks

            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.Mode <- Menu

            if this.IsKeyPressed(Keys.Space, ks) && matchOver gs then
                initMatch gs

        | LeagueMatchup ->
            if this.IsKeyPressed(Keys.Space, ks) then
                startLeagueMatch app

            if this.IsKeyPressed(Keys.Escape, ks) then
                app.League <- None
                app.Mode <- Menu

        | LeaguePlaying ->
            mapPlayer1Keys gs ks

            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)

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

        | Playing ->
            renderFrame
                spriteBatch
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
                    spriteBatch
                    fw
                    fh
                    (league.CurrentRound + 1)
                    league.Schedule.Length
                    teamNames.[t1]
                    teamNames.[t2]
            | None -> ()

        | LeaguePlaying ->
            renderFrame
                spriteBatch
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
