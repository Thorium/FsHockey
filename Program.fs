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
[<Emit("$0.getBoundingClientRect().width")>]
let private clientW (c: obj) : float = jsNative
[<Emit("$0.getBoundingClientRect().height")>]
let private clientH (c: obj) : float = jsNative
[<Emit("$0.width = $1")>]
let private setCanvasW (c: obj) (w: float) : unit = jsNative
[<Emit("$0.height = $1")>]
let private setCanvasH (c: obj) (h: float) : unit = jsNative
[<Emit("window.devicePixelRatio || 1")>]
let private getDpr () : float = jsNative
[<Emit("$0.setTransform($1, 0, 0, $1, 0, 0)")>]
let private setCtxScale (ctx: obj) (s: float) : unit = jsNative
[<Emit("document.addEventListener($0, $1)")>]
let private onDocument (event: string) (handler: obj -> unit) : unit = jsNative
[<Emit("window.addEventListener($0, $1)")>]
let private onWindow (event: string) (handler: obj -> unit) : unit = jsNative
/// Observe layout-size changes of an element (fires on initial layout too,
/// unlike the window resize event).
[<Emit("new ResizeObserver($1).observe($0)")>]
let private onElementResize (el: obj) (handler: unit -> unit) : unit = jsNative
[<Emit("window.requestAnimationFrame($0)")>]
let private requestFrame (cb: float -> unit) : unit = jsNative
[<Emit("$0.code")>]
let private evCode (e: obj) : string = jsNative
[<Emit("$0.preventDefault()")>]
let private preventDefault (e: obj) : unit = jsNative
/// Toggle browser fullscreen on the document element.
[<Emit("(function(){ if (document.fullscreenElement) { document.exitFullscreen(); } else { document.documentElement.requestFullscreen(); } })()")>]
let private toggleFullscreen () : unit = jsNative

// Gamepad API. getGamepads() entries may be null (disconnected slots), so the
// axis/button reads are null-safe.
[<Emit("(navigator.getGamepads ? navigator.getGamepads() : [])")>]
let private getGamepads () : obj array = jsNative
[<Emit("($0 && $0.axes && $0.axes.length > $1) ? $0.axes[$1] : 0")>]
let private gpAxis (gp: obj) (i: int) : float = jsNative
[<Emit("($0 && $0.buttons && $0.buttons.length > $1) ? $0.buttons[$1].pressed : false")>]
let private gpButton (gp: obj) (i: int) : bool = jsNative

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
    gs.Input1 <-
        { Left = has "ArrowLeft"
          Right = has "ArrowRight"
          Up = has "ArrowUp"
          Down = has "ArrowDown"
          Fire = has "ShiftRight" || has "Enter" }

/// Apply player 2 directional + fire keys (WASD + Space/Tab)
let private mapPlayer2Keys (gs: GameState) =
    gs.Input2 <-
        { Left = has "KeyA"
          Right = has "KeyD"
          Up = has "KeyW"
          Down = has "KeyS"
          Fire = has "Space" || has "Tab" }

// ─── Gamepad input (standard mapping) ────────────────────────────────────
// Left stick / d-pad to skate, A / B / right trigger to shoot.

let private GamepadDeadzone = 0.35

/// Read pad `idx` as an Input snapshot (Input.none when not connected).
let private gamepadInput (idx: int) : Input =
    let gps = getGamepads ()

    if idx < gps.Length then
        let gp = gps.[idx]
        let ax = gpAxis gp 0
        let ay = gpAxis gp 1

        { Left = ax < -GamepadDeadzone || gpButton gp 14
          Right = ax > GamepadDeadzone || gpButton gp 15
          Up = ay < -GamepadDeadzone || gpButton gp 12
          Down = ay > GamepadDeadzone || gpButton gp 13
          Fire = gpButton gp 0 || gpButton gp 1 || gpButton gp 7 }
    else
        Input.none

/// Combine keyboard and gamepad snapshots (either source counts).
let private mergeInput (a: Input) (b: Input) : Input =
    { Left = a.Left || b.Left
      Right = a.Right || b.Right
      Up = a.Up || b.Up
      Down = a.Down || b.Down
      Fire = a.Fire || b.Fire }

// ─── State ───────────────────────────────────────────────────────────────

let private canvas = getEl "screen"
let private ctx = get2dCtx canvas

/// Canvas size in CSS pixels. The backing store is scaled by devicePixelRatio
/// for crisp rendering on HiDPI screens, with a matching context transform so
/// all drawing code keeps working in CSS pixels.
let mutable private viewW = 0.0
let mutable private viewH = 0.0

/// Size the canvas backing store from its displayed (CSS) size.
/// Called at startup and whenever the window is resized.
let private resizeCanvas () =
    let cw = clientW canvas
    let ch = clientH canvas

    if cw > 0.0 && ch > 0.0 then
        let dpr = getDpr ()
        viewW <- cw
        viewH <- ch
        setCanvasW canvas (floor (cw * dpr))
        setCanvasH canvas (floor (ch * dpr))
        setCtxScale ctx dpr

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
        if pressed "KeyG" then app.GamepadEnabled <- not app.GamepadEnabled

    | Playing ->
        mapPlayer1Keys gs
        mapPlayer2Keys gs

        if app.GamepadEnabled then
            gs.Input1 <- mergeInput gs.Input1 (gamepadInput 0)
            gs.Input2 <- mergeInput gs.Input2 (gamepadInput 1)

        for _ in 1..PhysicsTicksPerFrame do
            gameTick gs

        gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2)

        if pressed "Escape" then app.Mode <- Menu

        if pressed "Space" && matchOver gs then app.Mode <- Menu

    | LeagueMatchup ->
        if pressed "Space" then startLeagueMatch app

        if pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    | LeaguePlaying ->
        mapPlayer1Keys gs

        if app.GamepadEnabled then
            gs.Input1 <- mergeInput gs.Input1 (gamepadInput 0)

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
        drawMenu ctx fw fh app.SelectedTeam1 app.SelectedTeam2 app.ActiveColumn app.FastHuman app.HardMode app.FivePlayerMode app.GamepadEnabled

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
           "Space"; "Tab"; "KeyL"; "KeyF"; "KeyH"; "KeyG"; "Digit5"; "F11" |])

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

/// Forces a redraw outside the fixed step (initial frame, canvas resize —
/// resizing the backing store wipes the canvas).
let mutable private needsRender = true

let rec private loop (ts: float) =
    let dt = if lastTime = 0.0 then frameMs else ts - lastTime
    lastTime <- ts
    acc <- acc + min dt 200.0   // clamp to avoid spiral-of-death after a stall

    while acc >= frameMs do
        update ()
        needsRender <- true
        acc <- acc - frameMs

    // The game state only changes on 30 FPS fixed-step updates; skip redrawing
    // identical frames on high-refresh displays (rAF can fire at 120+ Hz).
    if needsRender then
        needsRender <- false
        render ()

    requestFrame loop

// ─── Bootstrap ──────────────────────────────────────────────────────────
onDocument "keydown" onKeyDown
onDocument "keyup" onKeyUp
// ResizeObserver catches every displayed-size change of the canvas (window
// resize, fullscreen, initial layout), including ones that don't fire the
// window resize event.
onElementResize canvas (fun () ->
    resizeCanvas ()
    needsRender <- true)
resizeCanvas ()
requestFrame loop
