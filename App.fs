/// THE FS HOCKEY LEAGUE — Elmish Core (Mibo)
/// Backend-free model/update: semantic input actions, 60 Hz fixed-step
/// simulation, menu/league flow. References only Mibo.Core, so the whole
/// game loop runs headlessly (see test.fsx) as well as under the MonoGame
/// host in Program.fs.
module HockeyDemo.App

open Mibo.Elmish
open Mibo.Input
open HockeyDemo.Physics
open HockeyDemo.Game

// ─── Input Actions ────────────────────────────────────────────────────
// Semantic actions instead of raw keys: Mibo's InputMap resolves keyboard
// bindings to these, and the update loop reads Held/Started sets. The same
// key may drive different actions depending on the app mode (e.g. Space is
// both P2Fire and Continue).

[<Struct>]
type HockeyAction =
    | P1Left
    | P1Right
    | P1Up
    | P1Down
    | P1Fire
    | P2Left
    | P2Right
    | P2Up
    | P2Down
    | P2Fire
    | MenuUp
    | MenuDown
    | SwitchColumn
    | Select
    | StartLeague
    | ToggleFastHuman
    | ToggleHardMode
    | TogglePlayerCount
    | ToggleGamepad
    | TogglePause
    | ToggleFullscreen
    | Back
    | Continue

let inputMap =
    InputMap.empty
    // Player 1: Arrow keys + RShift/Enter
    |> InputMap.key P1Left KeyCode.Left
    |> InputMap.key P1Right KeyCode.Right
    |> InputMap.key P1Up KeyCode.Up
    |> InputMap.key P1Down KeyCode.Down
    |> InputMap.key P1Fire KeyCode.RightShift
    |> InputMap.key P1Fire KeyCode.Enter
    // Player 2: WASD + Space/Tab
    |> InputMap.key P2Left KeyCode.A
    |> InputMap.key P2Right KeyCode.D
    |> InputMap.key P2Up KeyCode.W
    |> InputMap.key P2Down KeyCode.S
    |> InputMap.key P2Fire KeyCode.Space
    |> InputMap.key P2Fire KeyCode.Tab
    // Menu / app control
    |> InputMap.key MenuUp KeyCode.Up
    |> InputMap.key MenuDown KeyCode.Down
    |> InputMap.key SwitchColumn KeyCode.Tab
    |> InputMap.key Select KeyCode.Enter
    |> InputMap.key StartLeague KeyCode.L
    |> InputMap.key ToggleFastHuman KeyCode.F
    |> InputMap.key ToggleHardMode KeyCode.H
    |> InputMap.key TogglePlayerCount KeyCode.D5
    |> InputMap.key ToggleGamepad KeyCode.G
    |> InputMap.key TogglePause KeyCode.P
    |> InputMap.key ToggleFullscreen KeyCode.F11
    |> InputMap.key Back KeyCode.Escape
    |> InputMap.key Continue KeyCode.Space

// ─── Gamepad State ────────────────────────────────────────────────────
// Mibo's Gamepad subscription delivers deltas (pressed/released + analog),
// so the model accumulates a held-button set and the latest analog values.

type PadState =
    { Analog: GamepadAnalog
      Held: Set<GamepadButtonCode> }

module PadState =
    let empty =
        { Analog =
            { LeftThumbstick = System.Numerics.Vector2.Zero
              RightThumbstick = System.Numerics.Vector2.Zero
              LeftTrigger = 0.0f
              RightTrigger = 0.0f }
          Held = Set.empty }

    let apply (delta: GamepadDelta) (p: PadState) =
        let held = (p.Held, delta.Buttons.Pressed) ||> Array.fold (fun s b -> Set.add b s)
        let held = (held, delta.Buttons.Released) ||> Array.fold (fun s b -> Set.remove b s)
        { Analog = delta.Analog; Held = held }

let private GamepadDeadzone = 0.35f

/// Read a pad state as an Input snapshot (left stick / d-pad to skate,
/// A / B / right trigger to shoot). Mibo's MonoGame backend reports the
/// stick Y axis screen-oriented: positive = down.
let private padInput (p: PadState) : Input =
    let stick = p.Analog.LeftThumbstick

    { Left = stick.X < -GamepadDeadzone || p.Held.Contains GamepadButtonCode.DPadLeft
      Right = stick.X > GamepadDeadzone || p.Held.Contains GamepadButtonCode.DPadRight
      Up = stick.Y < -GamepadDeadzone || p.Held.Contains GamepadButtonCode.DPadUp
      Down = stick.Y > GamepadDeadzone || p.Held.Contains GamepadButtonCode.DPadDown
      Fire =
        p.Held.Contains GamepadButtonCode.FaceDown
        || p.Held.Contains GamepadButtonCode.FaceRight
        || p.Analog.RightTrigger > 0.12f }

/// Combine keyboard and gamepad snapshots (either source counts).
let private mergeInput (a: Input) (b: Input) : Input =
    { Left = a.Left || b.Left
      Right = a.Right || b.Right
      Up = a.Up || b.Up
      Down = a.Down || b.Down
      Fire = a.Fire || b.Fire }

// ─── Application Mode ─────────────────────────────────────────────────

[<Struct>]
type AppMode =
    | Menu
    | Playing
    | LeagueMatchup
    | LeaguePlaying
    | LeagueStandings
    | LeagueFinalStandings

// ─── Model / Msg ──────────────────────────────────────────────────────
// App flow (mode, menu selections, options) is immutable MVU state; the
// per-tick simulation lives in GameState, which is mutated in place by
// gameTick — the Elmish loop owns it, the fixed-step message drives it.

type Model =
    { Gs: GameState
      Mode: AppMode
      SelectedTeam1: int
      SelectedTeam2: int
      ActiveColumn: int
      FastHuman: bool
      HardMode: bool
      FivePlayerMode: bool
      GamepadEnabled: bool
      Paused: bool
      League: LeagueState option
      Input: ActionState<HockeyAction>
      Pad1: PadState
      Pad2: PadState }

[<Struct>]
type Msg =
    | PhysicsStep of dt: float32
    | InputChanged of inputs: ActionState<HockeyAction>
    | PadChanged of delta: GamepadDelta

/// Backend hook installed by the entry point (no-op when headless): F11
/// fullscreen needs the GraphicsDeviceManager, which Mibo.Core cannot see.
let mutable toggleFullscreenHook: unit -> unit = ignore

// ─── Match Setup Helpers ──────────────────────────────────────────────

/// Configure entity speeds/power based on team selection.
let private setTeamSpeeds (gs: GameState) fastHuman hardMode =
    let ppt = gs.PlayersPerTeam

    let applyTeam teamIdx startEnt isFast isCpu =
        let srcIdx = if isFast then humanFastTeamIdx else teamIdx
        let speeds = teamMaxSpeed.[srcIdx]
        let powers = teamShotPower.[srcIdx]
        let mult = if isCpu && hardMode then HardModeSpeedMult else 1.0

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
    applyTeam gs.Team1Idx 0 (fastHuman && t1Human) (not t1Human)
    applyTeam gs.Team2Idx gs.Team2Start (fastHuman && t2Human) (not t2Human)

    gs.ShotSpeed <- if hardMode then HardShotReleaseSpeed else ShotReleaseSpeed

/// Start an exhibition match
let private startExhibitionMatch (m: Model) =
    let gs = m.Gs
    gs.Team1Idx <- m.SelectedTeam1
    gs.Team2Idx <- m.SelectedTeam2
    gs.Team1Human <- (gs.Team1Idx = 0)
    gs.Team2Human <- (gs.Team2Idx = 0)
    gs.NumPeriods <- ExhibitionPeriods
    setPlayerMode gs m.FivePlayerMode
    setTeamSpeeds gs m.FastHuman m.HardMode
    initMatch gs
    { m with Mode = Playing; Paused = false }

/// Start a league match for the current round
let private startLeagueMatch (m: Model) =
    match m.League with
    | None -> m
    | Some league ->
        let gs = m.Gs
        let t1, t2 = currentMatchup league
        gs.Team1Idx <- t1
        gs.Team2Idx <- t2
        gs.Team1Human <- (t1 = league.HumanTeam)
        gs.Team2Human <- false
        gs.NumPeriods <- LeaguePeriods
        setPlayerMode gs m.FivePlayerMode
        setTeamSpeeds gs m.FastHuman m.HardMode
        initMatch gs
        { m with Mode = LeaguePlaying; Paused = false }

/// Is the match over?
let inline matchOver (gs: GameState) =
    not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Init ─────────────────────────────────────────────────────────────

let init (_ctx: GameContext) : struct (Model * Cmd<Msg>) =
    { Gs = createGameState ()
      Mode = Menu
      SelectedTeam1 = 0
      SelectedTeam2 = 1
      ActiveColumn = 0
      FastHuman = true
      HardMode = false
      FivePlayerMode = false
      GamepadEnabled = true
      Paused = false
      League = None
      Input = ActionState.empty
      Pad1 = PadState.empty
      Pad2 = PadState.empty },
    Cmd.none

// ─── Update: per-mode input handling ──────────────────────────────────
// Edge-triggered actions are read from the Started set of a fresh
// InputChanged delta only (the stored ActionState is used purely for
// Held-based movement during physics steps).

let private handleMenu (started: Set<HockeyAction>) (m: Model) : struct (Model * Cmd<Msg>) =
    let m =
        if started.Contains SwitchColumn then
            { m with ActiveColumn = 1 - m.ActiveColumn }
        else
            m

    let m =
        let delta =
            (if started.Contains MenuUp then -1 else 0)
            + (if started.Contains MenuDown then 1 else 0)

        if delta = 0 then m
        elif m.ActiveColumn = 0 then
            { m with SelectedTeam1 = (m.SelectedTeam1 + delta + NumTeams) % NumTeams }
        else
            { m with SelectedTeam2 = (m.SelectedTeam2 + delta + NumTeams) % NumTeams }

    let m =
        if started.Contains ToggleFastHuman then { m with FastHuman = not m.FastHuman } else m

    let m =
        if started.Contains ToggleHardMode then { m with HardMode = not m.HardMode } else m

    let m =
        if started.Contains TogglePlayerCount then { m with FivePlayerMode = not m.FivePlayerMode } else m

    let m =
        if started.Contains ToggleGamepad then
            // Reset accumulated pad state so nothing sticks while disabled
            { m with GamepadEnabled = not m.GamepadEnabled; Pad1 = PadState.empty; Pad2 = PadState.empty }
        else
            m

    if started.Contains Select then startExhibitionMatch m, Cmd.none
    elif started.Contains StartLeague then
        { m with League = Some(createLeagueState m.SelectedTeam1); Mode = LeagueMatchup }, Cmd.none
    elif started.Contains Back then m, Cmd.signalExit
    else m, Cmd.none

let private handlePlaying (started: Set<HockeyAction>) (m: Model) : struct (Model * Cmd<Msg>) =
    let m =
        if started.Contains TogglePause then { m with Paused = not m.Paused } else m

    if started.Contains Back then { m with Mode = Menu; Paused = false }, Cmd.none
    elif started.Contains Continue && matchOver m.Gs then { m with Mode = Menu }, Cmd.none
    else m, Cmd.none

let private handleLeaguePlaying (started: Set<HockeyAction>) (m: Model) : struct (Model * Cmd<Msg>) =
    let m =
        if started.Contains TogglePause then { m with Paused = not m.Paused } else m

    if started.Contains Continue && matchOver m.Gs then
        match m.League with
        | Some league ->
            let gs = m.Gs
            recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
            simulateCpuRound league league.CurrentRound
            let finished = advanceRound league

            { m with Mode = (if finished then LeagueFinalStandings else LeagueStandings) }, Cmd.none
        | None -> { m with Mode = Menu }, Cmd.none
    elif started.Contains Back then
        { m with League = None; Mode = Menu }, Cmd.none
    else
        m, Cmd.none

let private handleInput (input: ActionState<HockeyAction>) (m: Model) : struct (Model * Cmd<Msg>) =
    let started = input.Started
    let m = { m with Input = input }

    if started.Contains ToggleFullscreen then
        toggleFullscreenHook ()

    match m.Mode with
    | Menu -> handleMenu started m
    | Playing -> handlePlaying started m
    | LeaguePlaying -> handleLeaguePlaying started m
    | LeagueMatchup ->
        if started.Contains Continue then startLeagueMatch m, Cmd.none
        elif started.Contains Back then { m with League = None; Mode = Menu }, Cmd.none
        else m, Cmd.none
    | LeagueStandings ->
        if started.Contains Continue then { m with Mode = LeagueMatchup }, Cmd.none
        elif started.Contains Back then { m with League = None; Mode = Menu }, Cmd.none
        else m, Cmd.none
    | LeagueFinalStandings ->
        if started.Contains Continue || started.Contains Back then
            { m with League = None; Mode = Menu }, Cmd.none
        else
            m, Cmd.none

// ─── Update: fixed-step simulation ────────────────────────────────────
// Mibo's fixed timestep dispatches PhysicsStep at a constant 60 Hz — the
// same effective rate as the original 30 FPS x 2 physics ticks per frame.

let private keysInput (held: Set<HockeyAction>) l r u d f : Input =
    { Left = held.Contains l
      Right = held.Contains r
      Up = held.Contains u
      Down = held.Contains d
      Fire = held.Contains f }

let private physicsStep (m: Model) : Model =
    match m.Mode with
    | Playing
    | LeaguePlaying when not m.Paused ->
        let gs = m.Gs
        let held = m.Input.Held
        let k1 = keysInput held P1Left P1Right P1Up P1Down P1Fire
        let k2 = keysInput held P2Left P2Right P2Up P2Down P2Fire

        gs.Input1 <- if m.GamepadEnabled then mergeInput k1 (padInput m.Pad1) else k1
        gs.Input2 <- if m.GamepadEnabled then mergeInput k2 (padInput m.Pad2) else k2

        gameTick gs

        // Puck animation ran once per rendered frame at 30 FPS; at the 60 Hz
        // step rate that is every second tick.
        if int gs.GameTick % 2 = 0 then
            gs.PuckAnimFrame <- (gs.PuckAnimFrame + 1) % (PuckAnimFrames * 2)

        m
    | _ -> m

let update (msg: Msg) (m: Model) : struct (Model * Cmd<Msg>) =
    match msg with
    | PhysicsStep _ -> physicsStep m, Cmd.none
    | InputChanged input -> handleInput input m
    | PadChanged delta ->
        match delta.PlayerIndex with
        | 0 -> { m with Pad1 = PadState.apply delta m.Pad1 }, Cmd.none
        | 1 -> { m with Pad2 = PadState.apply delta m.Pad2 }, Cmd.none
        | _ -> m, Cmd.none

/// Fixed-step configuration shared by the MonoGame host and headless tests.
let fixedStepConfig: FixedStepConfig<Msg> =
    { StepSeconds = 1.0f / 60.0f
      MaxStepsPerFrame = 5
      MaxFrameSeconds = ValueSome 0.25f
      Map = PhysicsStep }
