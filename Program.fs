/// THE FS HOCKEY LEAGUE — Browser entry point (Fable)
/// Sets up the canvas, runs the 30 FPS fixed-step game loop on
/// requestAnimationFrame, maps keyboard input, and drives the
/// menu / exhibition / league state machine (ported from the MonoGame build).
///
/// Controls:
///   Player 1: Arrow keys + RShift/Enter to shoot
///   Player 2: WASD + Space/Tab to shoot
///   Menu: UP/DOWN select team, TAB switch column, ENTER start, L league
///   F = fast human, H = hard mode, 5 = 3v3/6v6, F11 = fullscreen
module HockeyDemo.Program

open Fable.Core
open HockeyDemo.Physics
open HockeyDemo.Game
open HockeyDemo.Renderer

// ─── DOM / JS interop (raw Emit, binding-version independent) ───────────
[<Emit("document.getElementById($0)")>]
let private getEl (id: string) : obj = jsNative
[<Emit("$0.getContext('2d')")>]
let private get2dCtx (canvas: obj) : obj = jsNative
[<Emit("$0.width")>]
let private canvasW (c: obj) : float = jsNative
[<Emit("$0.height")>]
let private canvasH (c: obj) : float = jsNative
[<Emit("document.addEventListener($0, $1)")>]
let private onDocument (event: string) (handler: obj -> unit) : unit = jsNative
[<Emit("window.requestAnimationFrame($0)")>]
let private requestFrame (cb: float -> unit) : unit = jsNative
[<Emit("$0.code")>]
let private evCode (e: obj) : string = jsNative
[<Emit("$0.preventDefault()")>]
let private preventDefault (e: obj) : unit = jsNative
/// Toggle browser fullscreen on the document element.
[<Emit("(function(){ if (document.fullscreenElement) { document.exitFullscreen(); } else { document.documentElement.requestFullscreen(); } })()")>]
let private toggleFullscreen () : unit = jsNative

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

// ─── Helpers (ported from the MonoGame Program) ────────────────────────

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

// ─── Input state ───────────────────────────────────────────────────────

let private keys = System.Collections.Generic.HashSet<string>()
let private prevKeys = System.Collections.Generic.HashSet<string>()

let private has (code: string) = keys.Contains code
/// True only on the frame the key transitions from up to down.
let private pressed (code: string) = keys.Contains code && not (prevKeys.Contains code)

/// Apply player 1 directional + fire keys (Arrow keys + RShift/Enter)
let private mapPlayer1Keys (gs: GameState) =
    gs.KeyLeft1 <- has "ArrowLeft"
    gs.KeyRight1 <- has "ArrowRight"
    gs.KeyUp1 <- has "ArrowUp"
    gs.KeyDown1 <- has "ArrowDown"
    gs.KeyFire1 <- has "ShiftRight" || has "Enter"

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (gs: GameState) =
    gs.KeyLeft2 <- has "KeyA"
    gs.KeyRight2 <- has "KeyD"
    gs.KeyUp2 <- has "KeyW"
    gs.KeyDown2 <- has "KeyS"
    gs.KeyFire2 <- has "Space" || has "Tab"

// ─── State ───────────────────────────────────────────────────────────────

let private canvas = getEl "screen"
let private ctx = get2dCtx canvas
let private viewW = canvasW canvas
let private viewH = canvasH canvas

let private app = createAppState ()
let private gs = app.GameState

// ─── Fixed-step update (mirrors MonoGame Update at 30 FPS) ──────────────

let private update () =
    // F11 toggles fullscreen (works in any mode)
    if pressed "F11" then toggleFullscreen ()

    match app.Mode with
    | Menu ->
        if pressed "Tab" then
            app.ActiveColumn <- 1 - app.ActiveColumn

        if pressed "ArrowUp" || pressed "ArrowDown" then
            let delta = if has "ArrowUp" then -1 else 1

            if app.ActiveColumn = 0 then
                app.SelectedTeam1 <- (app.SelectedTeam1 + delta + NumTeams) % NumTeams
            else
                app.SelectedTeam2 <- (app.SelectedTeam2 + delta + NumTeams) % NumTeams

        if pressed "Enter" then startExhibitionMatch app

        if pressed "KeyL" then
            app.League <- Some(createLeagueState app.SelectedTeam1)
            app.Mode <- LeagueMatchup

        if pressed "KeyF" then app.FastHuman <- not app.FastHuman
        if pressed "KeyH" then app.HardMode <- not app.HardMode
        if pressed "Digit5" then app.FivePlayerMode <- not app.FivePlayerMode

    | Playing ->
        mapPlayer1Keys gs
        mapPlayer2Keys gs

        for _ in 1..PhysicsTicksPerFrame do
            gameTick gs

        gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)

        if pressed "Escape" then app.Mode <- Menu

        if pressed "Space" && matchOver gs then initMatch gs

    | LeagueMatchup ->
        if pressed "Space" then startLeagueMatch app

        if pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    | LeaguePlaying ->
        mapPlayer1Keys gs

        for _ in 1..PhysicsTicksPerFrame do
            gameTick gs

        gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)

        if pressed "Space" && matchOver gs then
            match app.League with
            | Some league ->
                recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
                simulateCpuRound league league.CurrentRound
                let finished = advanceRound league
                app.Mode <- if finished then LeagueFinalStandings else LeagueStandings
            | None -> app.Mode <- Menu

        if pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    | LeagueStandings ->
        if pressed "Space" then app.Mode <- LeagueMatchup

        if pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    | LeagueFinalStandings ->
        if pressed "Space" || pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    // Snapshot key state for next-frame edge detection
    prevKeys.Clear()
    for k in keys do
        prevKeys.Add k |> ignore

// ─── Render (mirrors MonoGame Draw) ─────────────────────────────────────

let private render () =
    let fw = viewW
    let fh = viewH

    match app.Mode with
    | Menu ->
        drawMenu ctx fw fh app.SelectedTeam1 app.SelectedTeam2 app.ActiveColumn app.FastHuman app.HardMode app.FivePlayerMode

    | Playing -> renderFrame ctx gs viewW viewH false

    | LeagueMatchup ->
        match app.League with
        | Some league ->
            let t1, t2 = currentMatchup league

            drawLeagueMatchup
                ctx
                fw
                fh
                (league.CurrentRound + 1)
                league.Schedule.Length
                teamNames.[t1]
                teamNames.[t2]
        | None -> ()

    | LeaguePlaying -> renderFrame ctx gs viewW viewH true

    | LeagueStandings ->
        app.League
        |> Option.iter (fun league ->
            drawLeagueStandings ctx fw fh (getSortedStandings league) false league.HumanTeam)

    | LeagueFinalStandings ->
        app.League
        |> Option.iter (fun league ->
            drawLeagueStandings ctx fw fh (getSortedStandings league) true league.HumanTeam)

// ─── Input handlers ──────────────────────────────────────────────────────

/// Keys we consume for the game; prevent their default browser behavior.
let private gameKeys =
    System.Collections.Generic.HashSet<string>(
        [| "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown"
           "ShiftRight"; "Enter"; "KeyA"; "KeyD"; "KeyW"; "KeyS"
           "Space"; "Tab"; "KeyL"; "KeyF"; "KeyH"; "Digit5"; "F11" |])

let private onKeyDown (e: obj) =
    let code = evCode e
    keys.Add code |> ignore
    if gameKeys.Contains code then preventDefault e

let private onKeyUp (e: obj) =
    keys.Remove(evCode e) |> ignore

// ─── Game loop (fixed 30 FPS step, rendered every animation frame) ──────

let private frameMs = 1000.0 / float GameFps
let mutable private lastTime = 0.0
let mutable private acc = 0.0

let rec private loop (ts: float) =
    let dt = if lastTime = 0.0 then frameMs else ts - lastTime
    lastTime <- ts
    acc <- acc + min dt 200.0   // clamp to avoid spiral-of-death after a stall

    while acc >= frameMs do
        update ()
        acc <- acc - frameMs

    render ()
    requestFrame loop

// ─── Bootstrap ──────────────────────────────────────────────────────────
onDocument "keydown" onKeyDown
onDocument "keyup" onKeyUp
requestFrame loop
