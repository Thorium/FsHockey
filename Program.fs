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
///   Touch devices: tap menu, floating stick + SHOOT per human team
module HockeyDemo.Program

open Fable.Core
open HockeyDemo.Physics
open HockeyDemo.Game
open HockeyDemo.Renderer
open System.Collections.Generic

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

// ─── Input state ───────────────────────────────────────────────────────

let private keys = HashSet<string>()
let private prevKeys = HashSet<string>()

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

[<Literal>]
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

/// Register the offline service worker (public/sw.js) for the installable app.
/// Production builds only: Vite replaces import.meta.env.PROD (and BASE_URL,
/// the GitHub Pages sub-path), and a worker caching dev-server modules would
/// serve stale code during development.
[<Emit("(import.meta.env.PROD && 'serviceWorker' in navigator) ? navigator.serviceWorker.register(import.meta.env.BASE_URL + 'sw.js').catch(function(){}) : void 0")>]
let private registerServiceWorker () : unit = jsNative
[<Emit("window.matchMedia && window.matchMedia('(pointer: coarse)').matches")>]
let private coarsePointer () : bool = jsNative
[<Emit("new URLSearchParams(window.location.search).get($0)")>]
let private queryParam (name: string) : string = jsNative

// ─── State ───────────────────────────────────────────────────────────────

let private canvas = getEl "screen"
let private ctx = get2dCtx canvas

/// Touch layout: a tap menu, a floating skate stick + SHOOT button per human
/// team, CONTINUE on result screens. On for coarse-pointer (phone/tablet)
/// devices; `?touch=1` / `?touch=0` forces it either way.
let private touchMode =
    match queryParam "touch" with
    | "1" -> true
    | "0" -> false
    | _ -> coarsePointer ()

/// Touch layout: the stick and SHOOT button sit in the HUD's bottom corners,
/// so the team names and scores move this far (CSS px) in from the edges.
let private hudInset = if touchMode then 120.0 else 0.0

[<Emit("document.body.style.setProperty($0, $1)")>]
let private setCssVar (name: string) (value: string) : unit = jsNative

/// Portrait orientation, when the touch layout covers the game with the
/// "turn your device sideways" message.
[<Emit("window.matchMedia('(orientation: portrait)').matches")>]
let private isPortrait () : bool = jsNative

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
        // The touch controls centre themselves in the HUD strip
        if touchMode then setCssVar "--hud-h" $"{hudHeight cw ch}px"

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
        if pressed "KeyP" then app.Paused <- not app.Paused

        if not app.Paused then
            mapPlayer1Keys gs
            mapPlayer2Keys gs

            if app.GamepadEnabled then
                gs.Input1 <- mergeInput gs.Input1 (gamepadInput 0)
                gs.Input2 <- mergeInput gs.Input2 (gamepadInput 1)

            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)

        if pressed "Escape" then
            app.Paused <- false
            app.Mode <- Menu

        if pressed "Space" && matchOver gs then app.Mode <- Menu

    | LeagueMatchup ->
        if pressed "Space" then startLeagueMatch app

        if pressed "Escape" then
            app.League <- None
            app.Mode <- Menu

    | LeaguePlaying ->
        if pressed "KeyP" then app.Paused <- not app.Paused

        if not app.Paused then
            mapPlayer1Keys gs

            if app.GamepadEnabled then
                gs.Input1 <- mergeInput gs.Input1 (gamepadInput 0)

            for _ in 1..PhysicsTicksPerFrame do
                gameTick gs

            gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)

        if pressed "Space" && matchOver gs then
            match app.League with
            | Some league ->
                recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
                simulateCpuRound league league.CurrentRound
                let finished = advanceRound league
                app.Mode <- if finished then LeagueFinalStandings else LeagueStandings
            | None -> app.Mode <- Menu

        if pressed "Escape" then
            app.Paused <- false
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
    | Menu -> drawMenuBackdrop ctx fw fh   // the menu itself is HTML (index.html)

    | Playing ->
        renderFrame ctx gs viewW viewH false hudInset app.FastHuman app.HardMode
        if app.Paused then drawPauseOverlay ctx viewW viewH

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
                t1
                t2
                app.FastHuman
                app.HardMode
                touchMode
        | None -> ()

    | LeaguePlaying ->
        renderFrame ctx gs viewW viewH true hudInset app.FastHuman app.HardMode
        if app.Paused then drawPauseOverlay ctx viewW viewH

    | LeagueStandings ->
        app.League
        |> Option.iter (fun league ->
            drawLeagueStandings ctx fw fh (getSortedStandings league) false league.HumanTeam touchMode)

    | LeagueFinalStandings ->
        app.League
        |> Option.iter (fun league ->
            drawLeagueStandings ctx fw fh (getSortedStandings league) true league.HumanTeam touchMode)

// ─── Input handlers ──────────────────────────────────────────────────────

/// Keys we consume for the game; prevent their default browser behavior.
let private gameKeys =
    HashSet<string>(
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

// ─── Touch controls ─────────────────────────────────────────────────────────
// The touch layout drives the game through the same key set as the keyboard.
// A stick holds the arrow (or WASD) keys for its direction, SHOOT holds the
// fire key (so a long press is still a hard shot), and taps on PAUSE / MENU /
// CONTINUE pulse a key for exactly one update — `pressed` is edge-triggered,
// so a pulse reads as one key press. Menu choices change the app state
// directly.

[<Emit("document.body")>]
let private body () : obj = jsNative
[<Emit("$0.classList.toggle($1, $2)")>]
let private setClass (el: obj) (cls: string) (on: bool) : unit = jsNative
[<Emit("$0.textContent = $1")>]
let private setText (el: obj) (s: string) : unit = jsNative
[<Emit("($0 && $0.closest) ? $0.closest($1) : null")>]
let private closest (el: obj) (selector: string) : obj = jsNative
[<Emit("$0.getAttribute($1) || ''")>]
let private attr (el: obj) (name: string) : string = jsNative
[<Emit("$0.setAttribute($1, $2)")>]
let private setAttr (el: obj) (name: string) (value: string) : unit = jsNative
[<Emit("$0.style.borderColor = $1")>]
let private setBorderColor (el: obj) (css: string) : unit = jsNative
[<Emit("$0.style.color = $1")>]
let private setTextColor (el: obj) (css: string) : unit = jsNative
[<Emit("$0.querySelector($1)")>]
let private query (el: obj) (selector: string) : obj = jsNative
[<Emit("$0.target")>]
let private evTarget (e: obj) : obj = jsNative
[<Emit("document.elementFromPoint($0, $1)")>]
let private elementAt (x: float) (y: float) : obj = jsNative
[<Emit("$0 === $1")>]
let private sameEl (a: obj) (b: obj) : bool = jsNative
[<Emit("$0.pointerId")>]
let private pointerId (e: obj) : int = jsNative
[<Emit("$0.clientX")>]
let private clientX (e: obj) : float = jsNative
[<Emit("$0.clientY")>]
let private clientY (e: obj) : float = jsNative
[<Emit("($0.style.left = $1, $0.style.top = $2)")>]
let private setPos (el: obj) (left: string) (top: string) : unit = jsNative
[<Emit("$0.style.transform = $1")>]
let private setTransform (el: obj) (t: string) : unit = jsNative
[<Emit("document.activeElement && document.activeElement.blur && document.activeElement.blur()")>]
let private blurActive () : unit = jsNative
/// Fullscreen + landscape lock where the browser allows it (Android, iPad —
/// older iPads only have the webkit-prefixed call); a no-op on iPhone, whose
/// Safari has no fullscreen for pages. Must run inside a tap handler.
/// Never throws: older browsers return no promise from requestFullscreen.
[<Emit("(function(){ try { var d = document.documentElement; var req = d.requestFullscreen || d.webkitRequestFullscreen; if (!(document.fullscreenElement || document.webkitFullscreenElement) && req) { Promise.resolve(req.call(d)).then(function(){ if (screen.orientation && screen.orientation.lock) return screen.orientation.lock('landscape'); }).catch(function(){}); } } catch (e) {} })()")>]
let private enterFullscreenLandscape () : unit = jsNative
/// An iPhone running the game in a browser tab rather than from the Home
/// Screen: the browser bars stay, and only Add to Home Screen gives the game
/// the full screen.
[<Emit("/iPhone|iPod/.test(navigator.userAgent) && !navigator.standalone && !(window.matchMedia && window.matchMedia('(display-mode: standalone), (display-mode: fullscreen)').matches)")>]
let private iPhoneBrowserTab () : bool = jsNative
[<Emit("$0.prompt()")>]
let private showInstallPrompt (prompt: obj) : unit = jsNative

/// The browser's install prompt (Android / desktop Chrome, Edge), captured on
/// `beforeinstallprompt` and shown by the rotate screen's Install as app button.
let mutable private installPrompt: obj = null

/// Show the install prompt (from a tap); a prompt can be shown only once.
let private promptInstall () =
    if not (isNull installPrompt) then
        showInstallPrompt installPrompt
        installPrompt <- null
        setClass (body ()) "can-install" false

/// Keys tapped since the last update; released again right after it.
let private pulses = List<string>()

let private pulseKey (code: string) =
    keys.Add code |> ignore
    pulses.Add code

let private releasePulses () =
    for k in pulses do
        keys.Remove k |> ignore
    pulses.Clear()

// SHOOT buttons: finger → data-key button it is holding.
let private touchHeld = Dictionary<int, obj>()

/// Release `el`'s key and highlight unless another finger still holds it.
let private releaseButton (el: obj) =
    if not (touchHeld.Values |> Seq.exists (sameEl el)) then
        keys.Remove(attr el "data-key") |> ignore
        setClass el "down" false

/// Point finger `id` at button `el` (null = none), swapping held keys.
let private holdButton (id: int) (el: obj) =
    let prev = match touchHeld.TryGetValue id with | true, p -> p | _ -> null
    if not (sameEl prev el) then
        if not (isNull prev) then
            touchHeld.Remove id |> ignore
            releaseButton prev
        if not (isNull el) then
            touchHeld[id] <- el
            setClass el "down" true
            keys.Add(attr el "data-key") |> ignore

/// Stick travel (CSS px) before it counts as a direction.
[<Literal>]
let private StickDeadzone = 16.0
/// How far the knob is drawn from the stick centre at most.
[<Literal>]
let private StickRadius = 44.0

/// A floating stick: touching its zone puts the base under the finger, and the
/// drag direction (8 sectors) holds the matching direction keys.
type private Stick =
    { Zone: obj
      Base: obj
      Knob: obj
      mutable Pointer: int
      mutable OriginX: float
      mutable OriginY: float
      mutable Held: string list
      /// Left, right, up, down key codes this stick holds
      mutable Keys: string * string * string * string }

let private makeStick (n: int) =
    let stickBase = getEl $"stick-base-{n}"
    { Zone = getEl $"stick-zone-{n}"; Base = stickBase; Knob = query stickBase ".stick-knob"
      Pointer = -1; OriginX = 0.0; OriginY = 0.0; Held = []
      Keys = ("ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown") }

let private sticks = [| makeStick 1; makeStick 2 |]
let private shootButtons = [| getEl "tc-shoot-1"; getEl "tc-shoot-2" |]

let private setStickKeys (s: Stick) (wanted: string list) =
    for k in s.Held do
        if not (List.contains k wanted) then keys.Remove k |> ignore
    for k in wanted do
        if not (List.contains k s.Held) then keys.Add k |> ignore
    s.Held <- wanted

let private moveStick (s: Stick) (x: float) (y: float) =
    let dx, dy = x - s.OriginX, y - s.OriginY
    let len = sqrt (dx * dx + dy * dy)
    let k = if len > StickRadius then StickRadius / len else 1.0
    setTransform s.Knob $"translate({dx * k}px, {dy * k}px)"
    let left, right, up, down = s.Keys
    // 8 sectors of 45°, sector 0 = right, counting clockwise (screen y is down)
    let wanted =
        if len < StickDeadzone then []
        else
            match (int (round (atan2 dy dx / (System.Math.PI / 4.0))) + 8) % 8 with
            | 0 -> [ right ]
            | 1 -> [ right; down ]
            | 2 -> [ down ]
            | 3 -> [ left; down ]
            | 4 -> [ left ]
            | 5 -> [ left; up ]
            | 6 -> [ up ]
            | _ -> [ right; up ]
    setStickKeys s wanted

let private releaseStick (s: Stick) =
    s.Pointer <- -1
    setStickKeys s []
    setClass s.Base "active" false
    setPos s.Base "" ""
    setTransform s.Knob ""

let private releaseAllTouches () =
    for el in Seq.toArray touchHeld.Values do
        setClass el "down" false
        keys.Remove(attr el "data-key") |> ignore
    touchHeld.Clear()
    for s in sticks do releaseStick s

/// Bind the control sets to the human teams of the match about to be played:
/// set 1 drives the first human team, set 2 (two-human matches) the second.
/// Team 1 plays on the arrows + RShift, team 2 on WASD + Space; each set is
/// rimmed in its team's colour.
let private bindControls () =
    releaseAllTouches ()
    // One `if` per line: on one line, `; if` would nest inside the first `then`
    let humans =
        [ if gs.Team1Human then 1
          if gs.Team2Human then 2 ]
    humans
    |> List.iteri (fun set team ->
        let s = sticks[set]
        s.Keys <-
            if team = 1 then ("ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown")
            else ("KeyA", "KeyD", "KeyW", "KeyS")
        setAttr shootButtons[set] "data-key" (if team = 1 then "ShiftRight" else "Space")
        let rim = if team = 1 then teamColor false app.FastHuman app.HardMode gs.Team1Idx else teamColor true app.FastHuman app.HardMode gs.Team2Idx
        for el in [ s.Base; shootButtons[set] ] do
            setBorderColor el rim.Css)

let private onPointerDown (e: obj) =
    let target = evTarget e
    let button = closest target "[data-key]"
    if not (isNull button) then
        preventDefault e
        holdButton (pointerId e) button
    else
        let zone = closest target ".stick-zone"
        match sticks |> Array.tryFind (fun s -> s.Pointer < 0 && sameEl s.Zone zone) with
        | Some s ->
            preventDefault e
            s.Pointer <- pointerId e
            s.OriginX <- clientX e
            s.OriginY <- clientY e
            setClass s.Base "active" true
            setPos s.Base $"{s.OriginX}px" $"{s.OriginY}px"
            moveStick s s.OriginX s.OriginY
        | None -> ()

let private onPointerMove (e: obj) =
    let id = pointerId e
    match sticks |> Array.tryFind (fun s -> s.Pointer = id) with
    | Some s -> moveStick s (clientX e) (clientY e)
    | None ->
        if touchHeld.ContainsKey id then
            holdButton id (closest (elementAt (clientX e) (clientY e)) "[data-key]")

let private onPointerUp (e: obj) =
    let id = pointerId e
    match sticks |> Array.tryFind (fun s -> s.Pointer = id) with
    | Some s -> releaseStick s
    | None -> holdButton id null

let private menuTeam1 = getEl "tm-team1"
let private menuTeam2 = getEl "tm-team2"
let private menuPlayers = getEl "tm-players"
let private menuFast = getEl "tm-fast"
let private menuHard = getEl "tm-hard"
let mutable private shownPlay = false

// ─── Desktop menu ───────────────────────────────────────────────────────────
// The tap menu's layout for the mouse (index.html #desk-menu): both team lists
// in full, built here from teamNames, plus the toggles and start buttons. The
// keyboard keeps driving the same app state; syncMenuUi mirrors it.

[<Emit("$0.innerHTML = $1")>]
let private setHtml (el: obj) (html: string) : unit = jsNative
[<Emit("Array.from($0.querySelectorAll($1))")>]
let private queryAllIn (el: obj) (selector: string) : obj[] = jsNative

/// Team buttons of one column (1 = left, 2 = right), indexed like teamNames.
let private buildTeamList (col: int) =
    let list = getEl $"dm-list{col}"
    teamNames
    |> Array.mapi (fun i name ->
        $"<button class=\"dm-team\" data-action=\"pick\" data-col=\"{col}\" data-team=\"{i}\" type=\"button\">"
        + $"<span>{name}</span><span class=\"dm-speed\"></span></button>")
    |> String.concat ""
    |> setHtml list
    queryAllIn list ".dm-team"

let private deskTeamRows = [| buildTeamList 1; buildTeamList 2 |]
let private deskCols = [| getEl "dm-col1"; getEl "dm-col2" |]
let private deskHeads = [| getEl "dm-head1"; getEl "dm-head2" |]
let private deskPlayers = getEl "dm-players"
let private deskFast = getEl "dm-fast"
let private deskHard = getEl "dm-hard"
let private deskGamepad = getEl "dm-gamepad"

/// Selected team per column, active column, live speeds and toggle states.
let private syncDeskMenu () =
    for c in 0 .. 1 do
        let selected = if c = 0 then app.SelectedTeam1 else app.SelectedTeam2
        setClass deskCols[c] "active" (app.ActiveColumn = c)
        setTextColor deskHeads[c] (teamColor (c = 1) app.FastHuman app.HardMode selected).Css
        deskTeamRows[c]
        |> Array.iteri (fun i row ->
            setClass row "selected" (i = selected)
            setText (query row ".dm-speed") $"speed {displayedTeamSpeed app.FastHuman app.HardMode i}")
    setText (query deskPlayers "span") (if app.FivePlayerMode then "6 v 6" else "3 v 3")
    setClass deskPlayers "on" true
    setClass deskFast "on" app.FastHuman
    setClass deskHard "on" app.HardMode
    setClass deskGamepad "on" app.GamepadEnabled

let inline private isMatchMode mode = (mode = Playing || mode = LeaguePlaying)

/// Mirror the app state into body classes (which pick the visible menu and
/// controls) and the menu labels. Called after every update and tap.
let private syncMenuUi () =
    let play = isMatchMode app.Mode
    if play <> shownPlay then
        shownPlay <- play
        if touchMode then (if play then bindControls () else releaseAllTouches ())
    let b = body ()
    setClass b "mode-menu" (app.Mode = Menu)
    setClass b "mode-play" play
    setClass b "paused" (play && app.Paused)
    setClass b "two-humans" (play && gs.Team1Human && gs.Team2Human)
    setClass b "no-humans" (play && not gs.Team1Human && not gs.Team2Human)
    setClass b "show-continue"
        (match app.Mode with
         | Playing | LeaguePlaying -> matchOver gs
         | LeagueMatchup | LeagueStandings | LeagueFinalStandings -> true
         | Menu -> false)
    if app.Mode = Menu && not touchMode then syncDeskMenu ()
    if app.Mode = Menu && touchMode then
        setText menuTeam1 teamNames[app.SelectedTeam1]
        setText menuTeam2 teamNames[app.SelectedTeam2]
        // Each name in the jersey colour that team will wear
        setTextColor menuTeam1 (teamColor false app.FastHuman app.HardMode app.SelectedTeam1).Css
        setTextColor menuTeam2 (teamColor true app.FastHuman app.HardMode app.SelectedTeam2).Css
        setText menuPlayers (if app.FivePlayerMode then "6 v 6" else "3 v 3")
        setClass menuPlayers "on" true
        setClass menuFast "on" app.FastHuman
        setClass menuHard "on" app.HardMode

let private stepTeam (current: int) (delta: int) = (current + delta + NumTeams) % NumTeams

/// A tap on a data-action button.
let private onTap (e: obj) =
    let button = closest (evTarget e) "[data-action]"
    if not (isNull button) then
        match attr button "data-action" with
        | "team1-prev" -> app.SelectedTeam1 <- stepTeam app.SelectedTeam1 -1
        | "team1-next" -> app.SelectedTeam1 <- stepTeam app.SelectedTeam1 1
        | "team2-prev" -> app.SelectedTeam2 <- stepTeam app.SelectedTeam2 -1
        | "team2-next" -> app.SelectedTeam2 <- stepTeam app.SelectedTeam2 1
        | "players" -> app.FivePlayerMode <- not app.FivePlayerMode
        | "fast" -> app.FastHuman <- not app.FastHuman
        | "hard" -> app.HardMode <- not app.HardMode
        | "gamepad" -> app.GamepadEnabled <- not app.GamepadEnabled
        // Desktop team list: pick that team and make its column the active one
        | "pick" when app.Mode = Menu ->
            let col = int (attr button "data-col") - 1
            let team = int (attr button "data-team")
            app.ActiveColumn <- col
            if col = 0 then app.SelectedTeam1 <- team else app.SelectedTeam2 <- team
        | "exhibition" when app.Mode = Menu ->
            startExhibitionMatch app
            if touchMode then enterFullscreenLandscape ()
        | "league" when app.Mode = Menu ->
            app.League <- Some(createLeagueState app.SelectedTeam1)
            app.Mode <- LeagueMatchup
            if touchMode then enterFullscreenLandscape ()
        | "pause" -> pulseKey "KeyP"
        | "menu" -> pulseKey "Escape"
        | "continue" -> pulseKey "Space"
        | "install" -> promptInstall ()
        | _ -> ()
        syncMenuUi ()
        needsRender <- true
        blurActive ()   // a focused button would also "click" on a keyboard Space/Enter

let rec private loop (ts: float) =
    let dt = if lastTime = 0.0 then frameMs else ts - lastTime
    lastTime <- ts
    acc <- acc + min dt 200.0   // clamp to avoid spiral-of-death after a stall

    // Touch layout in portrait: the rotate message hides the rink, so pause a
    // running match; it stays paused after rotating back until PAUSE is tapped
    if touchMode && isPortrait () && isMatchMode app.Mode && not app.Paused && not (matchOver gs) then
        app.Paused <- true
        releaseAllTouches ()
        needsRender <- true

    while acc >= frameMs do
        update ()
        releasePulses ()   // tapped keys count for exactly one update
        needsRender <- true
        acc <- acc - frameMs

    // The game state only changes on 30 FPS fixed-step updates; skip redrawing
    // identical frames on high-refresh displays (rAF can fire at 120+ Hz).
    if needsRender then
        needsRender <- false
        render ()
        syncMenuUi ()

    requestFrame loop

// ─── Bootstrap ──────────────────────────────────────────────────────────
onDocument "keydown" onKeyDown
onDocument "keyup" onKeyUp
// Menu buttons (tap menu and desktop menu)
onDocument "click" onTap
if touchMode then
    setClass (body ()) "touch" true
    setClass (body ()) "iphone-tab" (iPhoneBrowserTab ())
    // Install as app: browsers with an install API hand over a prompt to show
    // from a tap; it goes away once the app is installed
    onWindow "beforeinstallprompt" (fun e ->
        preventDefault e
        installPrompt <- e
        setClass (body ()) "can-install" true)
    onWindow "appinstalled" (fun _ ->
        installPrompt <- null
        setClass (body ()) "can-install" false)
    onDocument "pointerdown" onPointerDown
    onDocument "pointermove" onPointerMove
    onDocument "pointerup" onPointerUp
    onDocument "pointercancel" onPointerUp
    onDocument "contextmenu" preventDefault   // long-press menus
    // A backgrounded tab never delivers pointerup: drop every held key
    onWindow "blur" (fun _ -> releaseAllTouches (); keys.Clear())
    syncMenuUi ()
// ResizeObserver catches every displayed-size change of the canvas (window
// resize, fullscreen, initial layout), including ones that don't fire the
// window resize event.
onElementResize canvas (fun () ->
    resizeCanvas ()
    needsRender <- true)
resizeCanvas ()
registerServiceWorker ()
requestFrame loop
