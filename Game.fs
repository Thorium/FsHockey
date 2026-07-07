/// THE FS HOCKEY LEAGUE — Game Logic
/// Entity update, AI, human input, collision, puck physics, scoring.
/// Taking influence from Solar Hockey by Galifir Developments (Harm Hanemaayer & John Remyn, 1990-1992)
module HockeyDemo.Game

open System
open HockeyDemo.Physics

// ─── Types ─────────────────────────────────────────────────────────────

type Entity =
    { mutable X: float<px>
      mutable Y: float<px>
      mutable VelX: float<subpx / tick>
      mutable VelY: float<subpx / tick>
      mutable DirX: float
      mutable DirY: float
      mutable MaxSpeed: float<subpx / tick>
      mutable Accel: float<subpx / tick>
      mutable ShotPower: float<subpx / tick> }

[<Struct>]
type PuckState =
    | Free
    | HeldBy of entityIdx: int

/// Which team scored (for goal-flash display)
[<Struct>]
type GoalScoredBy =
    | NoGoal
    | Team1Scored
    | Team2Scored

/// A skate mark left on the ice during a tight turn
type TrailMark =
    { mutable X: float<px>
      mutable Y: float<px>
      mutable Life: int<tick> }

/// Player role for 5-player mode AI dispatch
[<Struct>]
type PlayerRole =
    | Goalie
    | Forward
    | Wing

/// One player's directional + fire input for the current tick.
[<Struct>]
type Input =
    { Left: bool; Right: bool; Up: bool; Down: bool; Fire: bool }

module Input =
    let none = { Left = false; Right = false; Up = false; Down = false; Fire = false }

// ─── League / Tournament Types ─────────────────────────────────────────

type TeamStats =
    { mutable Wins: int
      mutable Losses: int
      mutable Draws: int
      mutable Points: int
      mutable GoalsFor: int
      mutable GoalsAgainst: int }

type LeagueState =
    { Stats: TeamStats array
      /// Full round-robin: Schedule.[round] = array of (team1, team2) matchups
      Schedule: (int * int) array array
      Rng: Random
      mutable CurrentRound: int
      mutable Finished: bool
      HumanTeam: int }

type GameState =
    { Entities: Entity array
      Rng: Random
      mutable Team1Score: int
      mutable Team2Score: int
      mutable ClockSeconds: int<sec>
      mutable ClockTick: int<tick>
      mutable PuckState: PuckState
      mutable PossessionTimer: int<tick>
      // Shot re-capture cooldown: after a player releases the puck, that
      // player (and only that player) cannot re-capture it for a short
      // window, so you cannot pass to yourself.
      mutable LastReleaser: int
      mutable RecaptureBlockTicks: int<tick>
      mutable StalemateCounter: int<tick>
      mutable PrevPuckState: PuckState
      mutable ActivePlayer1: int
      mutable ActivePlayer2: int
      mutable PuckAnimFrame: int
      mutable PuckFrictionCounter: int
      mutable GameTick: int<tick>
      mutable Playing: bool
      mutable GoalFlashTimer: int<tick>
      mutable GoalScoredBy: GoalScoredBy
      mutable Team1Idx: int
      mutable Team2Idx: int
      mutable Team1Human: bool
      mutable Team2Human: bool
      // Puck release speed for the current match (hard mode shoots harder);
      // set by the UI at match start alongside the team stats.
      mutable ShotSpeed: float<subpx / tick>
      mutable PeriodLength: int<sec>
      mutable CurrentPeriod: int
      mutable NumPeriods: int
      // Dynamic player layout
      mutable PlayersPerTeam: int
      mutable FivePlayerMode: bool
      // Keyboard state (one input snapshot per player)
      mutable Input1: Input
      mutable Input2: Input
      // Shoot charge
      mutable FireHoldTicks1: int<tick>
      mutable FireHoldTicks2: int<tick>
      // Stick animation timer per entity
      mutable StickAnimTimers: int array
      // AI wander: per-player random target offset, re-rolled periodically
      WanderX: float<px> array
      WanderY: float<px> array
      mutable WanderTimer: int<tick>
      // Ice trail: skate marks from tight turns
      TrailMarks: TrailMark array
      mutable TrailMarkCount: int
      mutable TrailMarkHead: int       // circular buffer write index
      PrevDirX: float array            // previous direction per entity
      PrevDirY: float array }

    // Layout derived from the team size. Computed members rather than stored
    // fields so they cannot drift out of sync with PlayersPerTeam.
    member s.NumPlayers = s.PlayersPerTeam * 2
    member s.Team2Start = s.PlayersPerTeam
    member s.PuckIdx = s.PlayersPerTeam * 2
    member s.NumEntities = s.PlayersPerTeam * 2 + 1

// ─── Helpers ───────────────────────────────────────────────────────────

let private zeroVel = 0.0<subpx / tick>

/// Determine the role of a local player index in the current mode
let playerRole (fivePlayer: bool) (localIdx: int) =
    match fivePlayer, localIdx with
    | false, _      -> Forward
    | true, 0       -> Goalie
    | true, (3 | 4) -> Wing
    | true, _       -> Forward          // indices 1, 2, 5 = forwards

/// Is the entity on team 1?
let inline isOnTeam1 (gs: GameState) idx = idx < gs.Team2Start

/// Does the given team own the puck?
let teamOwnsPuck (gs: GameState) isTeam1 =
    match gs.PuckState with
    | HeldBy owner ->
        if isTeam1 then
            isOnTeam1 gs owner
        else
            not (isOnTeam1 gs owner)
    | Free -> false

/// Does the opponent team own the puck?
let opponentOwnsPuck (gs: GameState) isTeam1 = teamOwnsPuck gs (not isTeam1)

/// Sign-based velocity from a direction component
let private dirToVel (dir: float) (power: float<subpx / tick>) =
    if dir > 0.0 then power
    elif dir < 0.0 then -power
    else zeroVel

// ─── Factory ───────────────────────────────────────────────────────────

let createEntity maxSpd accel shotPwr : Entity =
    { X = 0.0<px>
      Y = 0.0<px>
      VelX = zeroVel
      VelY = zeroVel
      DirX = 0.0
      DirY = 0.0
      MaxSpeed = maxSpd
      Accel = accel
      ShotPower = shotPwr }

let createGameState () : GameState =
    let ppt = PlayersPerTeam3

    let ents =
        Array.init MaxEntities (fun i ->
            if i < MaxPlayersPerTeam * 2 then
                createEntity zeroVel ForwardAccel zeroVel
            else
                createEntity PuckMaxSpeed zeroVel zeroVel)

    { Entities = ents
      Rng = Random()
      Team1Score = 0
      Team2Score = 0
      ClockSeconds = 0<sec>
      ClockTick = 0<tick>
      PuckState = Free
      PossessionTimer = 0<tick>
      LastReleaser = -1
      RecaptureBlockTicks = 0<tick>
      StalemateCounter = 0<tick>
      PrevPuckState = Free
      ActivePlayer1 = 0
      ActivePlayer2 = ppt
      PuckAnimFrame = PuckAnimFrames
      PuckFrictionCounter = PuckAnimFrames
      GameTick = 0<tick>
      Playing = false
      GoalFlashTimer = 0<tick>
      GoalScoredBy = NoGoal
      Team1Idx = 0
      Team2Idx = 1
      Team1Human = true
      Team2Human = false
      ShotSpeed = ShotReleaseSpeed
      PeriodLength = PeriodMinutes * 60 * 1<sec>
      CurrentPeriod = 0
      NumPeriods = ExhibitionPeriods
      PlayersPerTeam = ppt
      FivePlayerMode = false
      Input1 = Input.none
      Input2 = Input.none
      FireHoldTicks1 = 0<tick>
      FireHoldTicks2 = 0<tick>
      StickAnimTimers = Array.zeroCreate MaxEntities
      WanderX = Array.zeroCreate MaxEntities
      WanderY = Array.zeroCreate MaxEntities
      WanderTimer = 0<tick>
      TrailMarks = Array.init MaxTrailMarks (fun _ -> { X = 0.0<px>; Y = 0.0<px>; Life = 0<tick> })
      TrailMarkCount = 0
      TrailMarkHead = 0
      PrevDirX = Array.zeroCreate MaxEntities
      PrevDirY = Array.zeroCreate MaxEntities }

// ─── Set Player Mode ──────────────────────────────────────────────────

let setPlayerMode (gs: GameState) fivePlayer =
    let ppt = if fivePlayer then PlayersPerTeam5 else PlayersPerTeam3
    gs.PlayersPerTeam <- ppt
    gs.FivePlayerMode <- fivePlayer
    // Entity indices change between modes — drop any stale re-capture block
    gs.LastReleaser <- -1
    gs.RecaptureBlockTicks <- 0<tick>
    // Ensure puck entity has correct stats
    let puck = gs.Entities.[gs.PuckIdx]
    puck.MaxSpeed <- PuckMaxSpeed
    puck.Accel <- zeroVel
    puck.ShotPower <- zeroVel

// ─── Position Reset ────────────────────────────────────────────────────

let private resetTeamPositions (gs: GameState) startIdx (homeX: float<px> array) (homeY: float<px> array) dirX =
    for i in 0 .. gs.PlayersPerTeam - 1 do
        let e = gs.Entities.[startIdx + i]
        e.X <- homeX.[i]
        e.Y <- homeY.[i]
        e.VelX <- zeroVel
        e.VelY <- zeroVel
        e.DirX <- dirX
        e.DirY <- 0.0

let resetPositions (gs: GameState) =
    if gs.FivePlayerMode then
        resetTeamPositions gs 0 team1HomeX5 team1HomeY5 1.0
        resetTeamPositions gs gs.Team2Start team2HomeX5 team2HomeY5 -1.0
    else
        resetTeamPositions gs 0 team1HomeX team1HomeY 1.0
        resetTeamPositions gs gs.Team2Start team2HomeX team2HomeY -1.0

    let puck = gs.Entities.[gs.PuckIdx]
    // Small random jitter so faceoff races aren't decided the same way
    // every time (with exact spawn + symmetric positions it's always a tie,
    // and ties break by entity index — i.e. always toward team 1)
    puck.X <- CenterX + (gs.Rng.NextDouble() * 2.0 - 1.0) * 6.0<px>
    puck.Y <- CenterY + (gs.Rng.NextDouble() * 2.0 - 1.0) * 6.0<px>
    puck.VelX <- zeroVel
    puck.VelY <- zeroVel
    gs.PuckState <- Free
    gs.PossessionTimer <- 0<tick>
    gs.LastReleaser <- -1
    gs.RecaptureBlockTicks <- 0<tick>
    gs.StalemateCounter <- 0<tick>
    gs.PrevPuckState <- Free
    gs.PuckAnimFrame <- PuckAnimFrames
    gs.PuckFrictionCounter <- PuckAnimFrames

// ─── Init Match ────────────────────────────────────────────────────────

let initMatch (gs: GameState) =
    gs.Team1Score <- 0
    gs.Team2Score <- 0
    gs.ClockSeconds <- 0<sec>
    gs.ClockTick <- 0<tick>
    gs.GameTick <- 0<tick>
    gs.GoalFlashTimer <- 0<tick>
    gs.CurrentPeriod <- 0
    gs.Playing <- true
    gs.Input1 <- Input.none
    gs.Input2 <- Input.none
    gs.FireHoldTicks1 <- 0<tick>
    gs.FireHoldTicks2 <- 0<tick>
    let skipGoalie = if gs.FivePlayerMode then 1 else 0
    gs.ActivePlayer1 <- skipGoalie
    gs.ActivePlayer2 <- gs.Team2Start + skipGoalie
    resetPositions gs

// ─── Find Nearest Player to Puck ───────────────────────────────────────

let findNearestToPuck (gs: GameState) startIdx endIdx =
    let puck = gs.Entities.[gs.PuckIdx]
    let mutable bestDist = Double.MaxValue
    let mutable bestIdx = startIdx

    for i in startIdx..endIdx do
        let e = gs.Entities.[i]
        let dx = float (e.X - puck.X)
        let dy = float (e.Y - puck.Y)
        let d = dx * dx + dy * dy

        if d < bestDist then
            bestDist <- d
            bestIdx <- i

    bestIdx

// ─── Release Puck (kick/shoot) ─────────────────────────────────────────

/// Ticks a player is blocked from re-capturing the puck after releasing it.
/// 18 ticks = 0.3 s at the ~60 Hz physics rate (30 FPS x 2 physics ticks
/// per frame). Prevents shooting and immediately picking the puck back up.
let RecaptureCooldownTicks = 18<tick>

/// powerFrac: 0.0..1.0 — fraction of the match's ShotSpeed (pass vs full shot)
let releasePuck (gs: GameState) entityIdx (powerFrac: float) =
    let ent = gs.Entities.[entityIdx]
    let puck = gs.Entities.[gs.PuckIdx]
    let power = gs.ShotSpeed * powerFrac
    ent.VelX <- zeroVel
    ent.VelY <- zeroVel
    puck.VelX <- dirToVel ent.DirX power
    puck.VelY <- dirToVel ent.DirY power
    gs.PuckState <- Free
    gs.PuckAnimFrame <- PuckAnimFrames
    gs.PuckFrictionCounter <- PuckAnimFrames
    // Block the releaser (and only them) from re-capturing for a short
    // window, starting the very tick the puck leaves the stick.
    gs.LastReleaser <- entityIdx
    gs.RecaptureBlockTicks <- RecaptureCooldownTicks
    gs.StickAnimTimers.[entityIdx] <- 10

// ─── Apply Friction ────────────────────────────────────────────────────
// Constant +/-1 per tick toward zero (NOT multiplicative)

let applyFriction (ent: Entity) =
    let decel v =
        if v > zeroVel then max zeroVel (v - FrictionRate)
        elif v < zeroVel then min zeroVel (v + FrictionRate)
        else v

    ent.VelX <- decel ent.VelX
    ent.VelY <- decel ent.VelY

// ─── Clamp Velocity ────────────────────────────────────────────────────

let clampVel (ent: Entity) =
    ent.VelX <- clamp -ent.MaxSpeed ent.MaxSpeed ent.VelX
    ent.VelY <- clamp -ent.MaxSpeed ent.MaxSpeed ent.VelY

// ─── Wall Bounce + Goal Check ──────────────────────────────────────────
// Returns true if a goal was scored

let checkWallsAndGoals (gs: GameState) idx =
    let ent = gs.Entities.[idx]
    let isPuck = (idx = gs.PuckIdx)
    let mutable scored = false

    let inGoalY () = ent.Y >= GoalTop && ent.Y <= GoalBottom

    // Left wall / left goal
    if ent.VelX < zeroVel && ent.X <= FieldLeft then
        if isPuck && inGoalY () then
            gs.Team2Score <- gs.Team2Score + 1
            gs.GoalFlashTimer <- 90<tick>
            gs.GoalScoredBy <- Team2Scored
            scored <- true
        else
            ent.X <- FieldLeft
            ent.VelX <- abs ent.VelX
    elif ent.X <= FieldLeft && not isPuck then
        ent.X <- FieldLeft

        if ent.VelX < zeroVel then
            ent.VelX <- abs ent.VelX

    // Right wall / right goal
    if ent.VelX > zeroVel && ent.X >= FieldRight then
        if isPuck && inGoalY () then
            gs.Team1Score <- gs.Team1Score + 1
            gs.GoalFlashTimer <- 90<tick>
            gs.GoalScoredBy <- Team1Scored
            scored <- true
        else
            ent.X <- FieldRight
            ent.VelX <- -(abs ent.VelX)
    elif ent.X >= FieldRight && not isPuck then
        ent.X <- FieldRight

        if ent.VelX > zeroVel then
            ent.VelX <- -(abs ent.VelX)

    // Top/bottom walls
    if ent.VelY < zeroVel && ent.Y <= FieldTop then
        ent.Y <- FieldTop
        ent.VelY <- abs ent.VelY

    if ent.VelY > zeroVel && ent.Y >= FieldBottom then
        ent.Y <- FieldBottom
        ent.VelY <- -(abs ent.VelY)

    // Clamp safety
    if not isPuck || not scored then
        ent.X <- clamp FieldLeft FieldRight ent.X

    ent.Y <- clamp FieldTop FieldBottom ent.Y

    scored

// ─── Human Input ───────────────────────────────────────────────────────

let applyHumanInput (gs: GameState) idx (input: Input) (fireHoldTicks: int<tick> byref) =
    let ent = gs.Entities.[idx]
    let mutable dx = 0.0
    let mutable dy = 0.0

    if input.Left then
        ent.VelX <- ent.VelX - ent.Accel
        dx <- -1.0

    if input.Right then
        ent.VelX <- ent.VelX + ent.Accel
        dx <- 1.0

    if input.Up then
        ent.VelY <- ent.VelY - ent.Accel
        dy <- -1.0

    if input.Down then
        ent.VelY <- ent.VelY + ent.Accel
        dy <- 1.0

    clampVel ent

    if dx <> 0.0 || dy <> 0.0 then
        ent.DirX <- dx
        ent.DirY <- dy

    // Charge mechanic: hold fire key for harder shot, release to fire
    match gs.PuckState with
    | HeldBy owner when owner = idx ->
        if input.Fire then
            fireHoldTicks <- fireHoldTicks + 1<tick>
        elif fireHoldTicks > 0<tick> then
            let t = float (int fireHoldTicks) / float (int ChargeTicksForFull)
            let chargeFrac = PassPowerFraction + (1.0 - PassPowerFraction) * (min 1.0 t)
            fireHoldTicks <- 0<tick>
            releasePuck gs idx chargeFrac
    | _ -> fireHoldTicks <- 0<tick>

// ─── AI: Move toward target ────────────────────────────────────────────

let aiMoveToward (ent: Entity) (targetX: float<px>) (targetY: float<px>) =
    if ent.X > targetX then
        ent.VelX <- ent.VelX - ent.Accel
    elif ent.X < targetX then
        ent.VelX <- ent.VelX + ent.Accel

    if ent.Y > targetY then
        ent.VelY <- ent.VelY - ent.Accel
    elif ent.Y < targetY then
        ent.VelY <- ent.VelY + ent.Accel

    clampVel ent
    let dx = float (targetX - ent.X)
    let dy = float (targetY - ent.Y)

    if abs dx > 2.0 || abs dy > 2.0 then
        ent.DirX <- float (sign dx)
        ent.DirY <- float (sign dy)

// ─── AI: Active Player Logic ───────────────────────────────────────────

/// Nearest opposing skater to entity `idx` (goalie excluded in 5-player
/// mode: it never leaves the crease, so it doesn't apply pressure).
/// Returns (opponent index, distance in px as float).
let private nearestOpponent (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let skipGoalie = if gs.FivePlayerMode then 1 else 0
    let oppStart = if isTeam1 then gs.Team2Start else 0
    let mutable bestDistSq = Double.MaxValue
    let mutable bestIdx = oppStart + skipGoalie

    for i in oppStart + skipGoalie .. oppStart + gs.PlayersPerTeam - 1 do
        let o = gs.Entities.[i]
        let dx = float (o.X - ent.X)
        let dy = float (o.Y - ent.Y)
        let d = dx * dx + dy * dy

        if d < bestDistSq then
            bestDistSq <- d
            bestIdx <- i

    bestIdx, sqrt bestDistSq

/// Pick a teammate worth passing to: within pass range, unmarked, and not
/// far behind the carrier; prefer the most forward-positioned candidate.
let private tryFindPassMate (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let goalDir = if isTeam1 then 1.0 else -1.0
    let skipGoalie = if gs.FivePlayerMode then 1 else 0
    let startEnt = if isTeam1 then 0 else gs.Team2Start
    let mutable best = -1
    let mutable bestForward = -20.0 // allow a slight drop pass, nothing deeper

    for i in skipGoalie .. gs.PlayersPerTeam - 1 do
        let ei = startEnt + i

        if ei <> idx then
            let mate = gs.Entities.[ei]
            let dx = float (mate.X - ent.X)
            let dy = float (mate.Y - ent.Y)
            let dist = sqrt (dx * dx + dy * dy)

            if dist >= float AiPassMinDist && dist <= float AiPassMaxDist then
                let _, oppDist = nearestOpponent gs ei isTeam1

                if oppDist > float AiMateOpenDist then
                    let forward = dx * goalDir

                    if forward > bestForward then
                        bestForward <- forward
                        best <- ei

    if best >= 0 then Some best else None

/// Aim at a teammate (8-way, since the puck leaves along DirX/DirY) and pass.
let private aiPassTo (gs: GameState) idx mateIdx =
    let ent = gs.Entities.[idx]
    let mate = gs.Entities.[mateIdx]
    let dx = float (mate.X - ent.X)
    let dy = float (mate.Y - ent.Y)
    let adx = abs dx
    let ady = abs dy
    // Closest of the 8 directions: pure axis when within ~22.5° of it
    if adx > 2.414 * ady then
        ent.DirX <- float (sign dx)
        ent.DirY <- 0.0
    elif ady > 2.414 * adx then
        ent.DirX <- 0.0
        ent.DirY <- float (sign dy)
    else
        ent.DirX <- float (sign dx)
        ent.DirY <- float (sign dy)

    releasePuck gs idx AiPassPowerFraction

let aiActivePlayer (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let puck = gs.Entities.[gs.PuckIdx]
    let goalDir = if isTeam1 then 1.0 else -1.0

    match gs.PuckState with
    | Free -> aiMoveToward ent puck.X puck.Y

    | HeldBy owner when owner = idx ->
        let oppIdx, oppDist = nearestOpponent gs idx isTeam1
        let opp = gs.Entities.[oppIdx]
        // Blocking = close AND on the goal side of the carrier (roughly
        // between the carrier and the target goal)
        let goalSide = float (opp.X - ent.X) * goalDir > -6.0
        let blocked = oppDist < float AiBlockDist && goalSide
        let pressured = oppDist < float AiPressureDist

        let inShootZone =
            if isTeam1 then
                ent.X > FieldRight - AiShootZoneX
            else
                ent.X < FieldLeft + AiShootZoneX

        let alignedWithGoal = ent.Y >= GoalTop && ent.Y <= GoalBottom

        // Fresh possession: for the first couple of seconds the carrier
        // rushes toward the opponent goal instead of passing or backing off
        let rushing = gs.PossessionTimer > PossessionTimer - AiInitialRushTicks

        if inShootZone && alignedWithGoal && not blocked then
            // Clear look at the goal: shoot — but not with perfect aim; a
            // fair share of shots go off diagonally and miss from range
            ent.DirX <- goalDir

            ent.DirY <-
                let r = gs.Rng.Next(100)
                if r < 20 then 1.0
                elif r < 40 then -1.0
                else 0.0

            releasePuck gs idx 1.0
        elif rushing then
            let targetX =
                if isTeam1 then FieldRight - AiCarryTargetMargin
                else FieldLeft + AiCarryTargetMargin

            let lane =
                if blocked || pressured then
                    // swerve around the defender while still advancing
                    if opp.Y > ent.Y then ent.Y - 28.0<px> else ent.Y + 28.0<px>
                else
                    ent.Y + gs.WanderY.[idx] * 1.5

            let targetY =
                if inShootZone then
                    clamp (GoalTop + 8.0<px>) (GoalBottom - 8.0<px>) lane
                else
                    clamp (FieldTop + 12.0<px>) (FieldBottom - 12.0<px>) lane

            aiMoveToward ent targetX targetY
        elif pressured then
            // Opponent right on us: pass if someone is open, otherwise
            // skate a bit backwards and sideways to find a better spot
            match tryFindPassMate gs idx isTeam1 with
            | Some mateIdx -> aiPassTo gs idx mateIdx
            | None ->
                let backX = ent.X - goalDir * 20.0<px>
                let sideY =
                    if opp.Y > ent.Y then ent.Y - 24.0<px> else ent.Y + 24.0<px>

                aiMoveToward
                    ent
                    (clamp FieldLeft FieldRight backX)
                    (clamp FieldTop FieldBottom sideY)
        elif gs.PossessionTimer <= AiForcedShotTimer then
            // Held long enough — get a shot away before the possession
            // timer force-releases the puck in a random direction
            let rndY = float (gs.Rng.Next(int AiRandomShot * 2 + 1)) - AiRandomShot
            ent.DirX <- goalDir

            ent.DirY <-
                if rndY > 3.0 then 1.0
                elif rndY < -3.0 then -1.0
                else 0.0

            releasePuck gs idx 1.0
        elif blocked then
            // Blocker ahead but not on us yet: pass if a mate is open,
            // otherwise dodge laterally around the blocker, keeping the puck
            match tryFindPassMate gs idx isTeam1 with
            | Some mateIdx -> aiPassTo gs idx mateIdx
            | None ->
                let sideY =
                    if opp.Y > ent.Y then ent.Y - 28.0<px> else ent.Y + 28.0<px>

                aiMoveToward
                    ent
                    (clamp FieldLeft FieldRight (ent.X + goalDir * 8.0<px>))
                    (clamp FieldTop FieldBottom sideY)
        else
            // Open ice: carry toward the opponent end, weaving a random
            // route via the wander offset; funnel toward the goal mouth
            // once inside the shooting zone
            let targetX =
                if isTeam1 then FieldRight - AiCarryTargetMargin
                else FieldLeft + AiCarryTargetMargin

            let lane = ent.Y + gs.WanderY.[idx] * 1.5

            let targetY =
                if inShootZone then
                    clamp (GoalTop + 8.0<px>) (GoalBottom - 8.0<px>) lane
                else
                    clamp (FieldTop + 12.0<px>) (FieldBottom - 12.0<px>) lane

            aiMoveToward ent targetX targetY

    | HeldBy _ ->
        if opponentOwnsPuck gs isTeam1 then
            aiMoveToward ent puck.X puck.Y
        else
            let supportX =
                (if isTeam1 then puck.X - 30.0<px> else puck.X + 30.0<px>)
                + gs.WanderX.[idx]

            let supportY =
                clamp FieldTop FieldBottom (puck.Y + gs.WanderY.[idx])

            aiMoveToward ent (clamp FieldLeft FieldRight supportX) supportY

// ─── AI: Defender Logic ────────────────────────────────────────────────

let aiDefender (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let localIdx = if isTeam1 then idx else idx - gs.Team2Start
    let hasPuck = teamOwnsPuck gs isTeam1

    if not gs.FivePlayerMode && opponentOwnsPuck gs isTeam1 then
        // 3v3 has no goalie: non-active players collapse in front of their
        // own goal (staggered depths) to block the shooting lane
        let puck = gs.Entities.[gs.PuckIdx]

        // Stand off the crease like defensemen — challenge the shooter,
        // don't stand in the net
        let guardX =
            if isTeam1 then
                FieldLeft + 24.0<px> + float localIdx * 12.0<px>
            else
                FieldRight - 24.0<px> - float localIdx * 12.0<px>

        // Track the puck's Y exactly — this is net-minding duty, wander
        // here means goals against
        let guardY = clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>) puck.Y

        aiMoveToward ent guardX guardY
    else

    let homeX, homeY =
        if gs.FivePlayerMode then
            let hx =
                if hasPuck then
                    (if isTeam1 then team1HomeX5Attack else team2HomeX5Attack).[localIdx]
                else
                    (if isTeam1 then team1HomeX5 else team2HomeX5).[localIdx]

            let hy = (if isTeam1 then team1HomeY5 else team2HomeY5).[localIdx]
            hx, hy
        else
            let hx =
                if hasPuck then
                    (if isTeam1 then team1HomeXAttack else team2HomeXAttack).[localIdx]
                else
                    (if isTeam1 then team1HomeX else team2HomeX).[localIdx]

            let hy = (if isTeam1 then team1HomeY else team2HomeY).[localIdx]
            hx, hy

    // Wander offset so players don't park on exactly the same spot every time
    let targetX = clamp FieldLeft FieldRight (homeX + gs.WanderX.[idx])
    let targetY = clamp FieldTop FieldBottom (homeY + gs.WanderY.[idx])
    aiMoveToward ent targetX targetY

// ─── AI: Goalie Logic (5-player mode, index 0 per team) ──────────────
// Goalie patrols a square zone in front of the goal (not just a line).
// When holding the puck, immediately passes forward to nearest teammate.

let private goalieAutoPass (gs: GameState) goalieIdx isTeam1 =
    let goalie = gs.Entities.[goalieIdx]
    let ppt = gs.PlayersPerTeam
    let startEnt = if isTeam1 then 0 else gs.Team2Start

    // Find nearest non-goalie teammate to pass to
    let mutable bestDist = Double.MaxValue
    let mutable bestIdx = -1
    for i in 1 .. ppt - 1 do       // skip index 0 (goalie itself)
        let ei = startEnt + i
        let mate = gs.Entities.[ei]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let d = dx * dx + dy * dy
        if d < bestDist then
            bestDist <- d
            bestIdx <- ei

    if bestIdx >= 0 then
        let mate = gs.Entities.[bestIdx]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let len = sqrt (dx * dx + dy * dy)
        if len > 1.0 then
            goalie.DirX <- float (sign dx)
            goalie.DirY <- float (sign dy)
        else
            goalie.DirX <- if isTeam1 then 1.0 else -1.0
            goalie.DirY <- 0.0
        releasePuck gs goalieIdx PassPowerFraction

let aiGoalie (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let puck = gs.Entities.[gs.PuckIdx]

    // Auto-pass when holding puck (pass immediately, no delay)
    match gs.PuckState with
    | HeldBy owner when owner = idx ->
        goalieAutoPass gs idx isTeam1
    | _ -> ()

    // Movement: square zone in front of goal (like real crease)
    // Goalie tracks puck Y, but allowed forward shift depends on game situation:
    //   - Opponent has puck: stay very close to goal line (minimal forward shift)
    //   - Puck free: moderate forward shift
    //   - Team has puck: can come out a bit more
    let baseX = if isTeam1 then GoaliePatrolXLeft else GoaliePatrolXRight
    let forwardShift =
        if opponentOwnsPuck gs isTeam1 then 6.0<px>     // stay deep
        elif teamOwnsPuck gs isTeam1 then 14.0<px>       // come out a bit
        else 10.0<px>                                     // moderate
    let goalieMinX, goalieMaxX =
        if isTeam1 then baseX, baseX + forwardShift
        else baseX - forwardShift, baseX

    // Move toward puck but clamped within the crease square
    let targetX = clamp goalieMinX goalieMaxX puck.X
    let targetY = clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>) puck.Y
    aiMoveToward ent targetX targetY

// ─── AI: Wing Logic (5-player mode, indices 3-4 per team) ────────────

let aiWing (gs: GameState) idx isTeam1 =
    let ent = gs.Entities.[idx]
    let puck = gs.Entities.[gs.PuckIdx]
    let localIdx = if isTeam1 then idx else idx - gs.Team2Start

    let wx = gs.WanderX.[idx]
    let wy = gs.WanderY.[idx]

    if teamOwnsPuck gs isTeam1 then
        let targetX =
            if isTeam1 then
                clamp (FieldLeft + 40.0<px>) (FieldRight - 20.0<px>) (puck.X + 40.0<px> + wx)
            else
                clamp (FieldLeft + 20.0<px>) (FieldRight - 40.0<px>) (puck.X - 40.0<px> + wx)

        let baseY = (if isTeam1 then team1HomeY5 else team2HomeY5).[localIdx]
        let targetY = clamp FieldTop FieldBottom (baseY + wy)
        aiMoveToward ent targetX targetY
    elif opponentOwnsPuck gs isTeam1 then
        let retreatX =
            if isTeam1 then
                clamp FieldLeft (CenterX - 20.0<px>) (puck.X - 50.0<px> + wx)
            else
                clamp (CenterX + 20.0<px>) FieldRight (puck.X + 50.0<px> + wx)

        let targetY = clamp (GoalTop - 10.0<px>) (GoalBottom + 10.0<px>) (puck.Y + wy)
        aiMoveToward ent retreatX targetY
    else
        let homeX = (if isTeam1 then team1HomeX5 else team2HomeX5).[localIdx]
        let homeY = (if isTeam1 then team1HomeY5 else team2HomeY5).[localIdx]
        let targetX = clamp FieldLeft FieldRight ((homeX + puck.X) / 2.0 + wx)
        let targetY = clamp FieldTop FieldBottom ((homeY + puck.Y) / 2.0 + wy)
        aiMoveToward ent targetX targetY

// ─── Move Puck When Possessed ──────────────────────────────────────────

let movePuckPossessed (gs: GameState) =
    match gs.PuckState with
    | HeldBy owner ->
        let ent = gs.Entities.[owner]
        let puck = gs.Entities.[gs.PuckIdx]
        puck.X <- ent.X + ent.DirX * 8.0<px>
        puck.Y <- ent.Y + ent.DirY * 8.0<px>
        puck.VelX <- zeroVel
        puck.VelY <- zeroVel
    | Free -> ()

// ─── Puck Pickup Collision ─────────────────────────────────────────────

let checkPuckPickup (gs: GameState) =
    match gs.PuckState with
    | HeldBy _ -> ()
    | Free ->
        let puck = gs.Entities.[gs.PuckIdx]
        // Alternate which team's players are checked first, so that when two
        // opponents reach the puck on the same tick the tie doesn't always
        // break toward team 1.
        let offset = if int gs.GameTick % 2 = 0 then 0 else gs.Team2Start

        let rec tryPickup n =
            if n < gs.NumPlayers then
                let i = (n + offset) % gs.NumPlayers
                let ent = gs.Entities.[i]
                // The player who just released the puck cannot re-capture it
                // during the cooldown; teammates and opponents still can.
                let blocked = i = gs.LastReleaser && gs.RecaptureBlockTicks > 0<tick>

                if not blocked
                   && abs (ent.X - puck.X) < CollisionDist
                   && abs (ent.Y - puck.Y) < CollisionDist then
                    gs.PuckState <- HeldBy i
                    gs.PossessionTimer <- PossessionTimer
                    puck.VelX <- zeroVel
                    puck.VelY <- zeroVel
                else
                    tryPickup (n + 1)

        tryPickup 0

// ─── Stalemate Detection ──────────────────────────────────────────────

let checkStalemate (gs: GameState) =
    match gs.PrevPuckState, gs.PuckState with
    | Free, HeldBy _ -> gs.StalemateCounter <- 0<tick>
    | _, Free -> gs.StalemateCounter <- gs.StalemateCounter + 1<tick>
    | HeldBy a, HeldBy b when a <> b -> gs.StalemateCounter <- 0<tick>
    | _ -> gs.StalemateCounter <- gs.StalemateCounter + 1<tick>

    gs.PrevPuckState <- gs.PuckState
    gs.StalemateCounter >= StalemateFaceoff

// ─── Game Clock ────────────────────────────────────────────────────────

let updateClock (gs: GameState) =
    gs.ClockTick <- gs.ClockTick + 1<tick>

    if gs.ClockTick >= ClockTicksPerSec * 1<sec> then
        gs.ClockTick <- 0<tick>
        gs.ClockSeconds <- gs.ClockSeconds + 1<sec>

// ─── Process One Team ──────────────────────────────────────────────────

let private processTeam
    (gs: GameState)
    isTeam1
    isHuman
    activeIdx
    (input: Input)
    (holdTicks: int<tick> byref)
    =
    let ppt = gs.PlayersPerTeam
    let t2s = gs.Team2Start
    let startEnt = if isTeam1 then 0 else t2s

    for i in 0 .. ppt - 1 do
        let ei = startEnt + i
        let role = playerRole gs.FivePlayerMode i

        match role with
        | Goalie -> aiGoalie gs ei isTeam1
        | _ when ei = activeIdx ->
            if isHuman then
                applyHumanInput gs ei input &holdTicks
            else
                aiActivePlayer gs ei isTeam1
        | Wing -> aiWing gs ei isTeam1
        | _ -> aiDefender gs ei isTeam1

// ─── Main Game Tick ────────────────────────────────────────────────────

let gameTick (gs: GameState) =
    if not gs.Playing then
        ()
    else

        gs.GameTick <- gs.GameTick + 1<tick>

        // Decrement stick animation timers
        for i in 0 .. gs.NumEntities - 1 do
            let t = gs.StickAnimTimers.[i]

            if t > 0 then
                gs.StickAnimTimers.[i] <- t - 1

        // Goal flash countdown
        if gs.GoalFlashTimer > 0<tick> then
            gs.GoalFlashTimer <- gs.GoalFlashTimer - 1<tick>

            if gs.GoalFlashTimer = 0<tick> then
                resetPositions gs
        else

            let ppt = gs.PlayersPerTeam
            let t2s = gs.Team2Start

            // Re-capture cooldown countdown. Decremented before the teams are
            // processed, so a release during this tick keeps the full window:
            // the releaser is blocked on the release tick plus the 17 ticks
            // after it, and may re-capture 18 ticks (0.3 s) after the release.
            if gs.RecaptureBlockTicks > 0<tick> then
                gs.RecaptureBlockTicks <- gs.RecaptureBlockTicks - 1<tick>

            // Re-roll each player's AI wander offset periodically
            gs.WanderTimer <- gs.WanderTimer - 1<tick>

            if gs.WanderTimer <= 0<tick> then
                gs.WanderTimer <- AiWanderIntervalTicks

                for i in 0 .. gs.NumPlayers - 1 do
                    gs.WanderX.[i] <- (gs.Rng.NextDouble() * 2.0 - 1.0) * AiWanderRange
                    gs.WanderY.[i] <- (gs.Rng.NextDouble() * 2.0 - 1.0) * AiWanderRange

            // Active player: the holder while a skater has the puck,
            // otherwise nearest to puck (skip goalie in 5-player mode).
            // Human-controlled teams get hysteresis: the marker only jumps
            // to a teammate clearly closer to the puck, so the player being
            // steered isn't handed over to the AI on every micro-difference.
            let skipGoalie = if gs.FivePlayerMode then 1 else 0

            let activeFor startIdx currentActive isHuman =
                match gs.PuckState with
                | HeldBy owner when owner >= startIdx + skipGoalie && owner < startIdx + ppt -> owner
                | _ ->
                    let nearest = findNearestToPuck gs (startIdx + skipGoalie) (startIdx + ppt - 1)

                    if not isHuman
                       || currentActive < startIdx + skipGoalie
                       || currentActive >= startIdx + ppt then
                        nearest
                    else
                        let puck = gs.Entities.[gs.PuckIdx]

                        let distToPuck i =
                            let e = gs.Entities.[i]
                            let dx = float (e.X - puck.X)
                            let dy = float (e.Y - puck.Y)
                            sqrt (dx * dx + dy * dy)

                        if distToPuck nearest < distToPuck currentActive - float AiActiveSwitchMargin then
                            nearest
                        else
                            currentActive

            gs.ActivePlayer1 <- activeFor 0 gs.ActivePlayer1 gs.Team1Human
            gs.ActivePlayer2 <- activeFor t2s gs.ActivePlayer2 gs.Team2Human

            // Process both teams
            processTeam
                gs
                true
                gs.Team1Human
                gs.ActivePlayer1
                gs.Input1
                &gs.FireHoldTicks1

            processTeam
                gs
                false
                gs.Team2Human
                gs.ActivePlayer2
                gs.Input2
                &gs.FireHoldTicks2

            // Possession timer — auto-shoot when it expires
            match gs.PuckState with
            | HeldBy owner ->
                gs.PossessionTimer <- gs.PossessionTimer - 1<tick>

                if gs.PossessionTimer <= 0<tick> then
                    let ent = gs.Entities.[owner]
                    let vx = ent.VelX
                    let vy = ent.VelY
                    releasePuck gs owner 1.0
                    ent.VelX <- -vx
                    ent.VelY <- -vy
            | Free -> ()

            // Puck friction: only every 8th tick (when PuckFrictionCounter resets)
            let mutable applyPuckFric = false

            match gs.PuckState with
            | HeldBy _ -> movePuckPossessed gs
            | Free ->
                gs.PuckFrictionCounter <- gs.PuckFrictionCounter - 1

                if gs.PuckFrictionCounter <= 0 then
                    gs.PuckFrictionCounter <- PuckAnimFrames
                    applyPuckFric <- true

            // Friction: every tick for players, every 8th tick for the free puck
            for i in 0 .. gs.NumEntities - 1 do
                if i = gs.PuckIdx then
                    if applyPuckFric then
                        applyFriction gs.Entities.[i]
                else
                    applyFriction gs.Entities.[i]

            // Teammate separation: push same-team players apart when too close (6v6 only)
            if gs.FivePlayerMode then
                let sepDist = float TeammateSeparationDist
                let sepDistSq = sepDist * sepDist
                let sepForce = TeammateSeparationForce

                let pushApart startIdx count =
                    for i in startIdx .. startIdx + count - 2 do
                        for j in i + 1 .. startIdx + count - 1 do
                            let ei = gs.Entities.[i]
                            let ej = gs.Entities.[j]
                            let dx = float (ei.X - ej.X)
                            let dy = float (ei.Y - ej.Y)
                            let distSq = dx * dx + dy * dy
                            if distSq < sepDistSq && distSq > 0.01 then
                                let dist = sqrt distSq
                                let nx = dx / dist
                                let ny = dy / dist
                                ei.VelX <- ei.VelX + nx * sepForce
                                ei.VelY <- ei.VelY + ny * sepForce
                                ej.VelX <- ej.VelX - nx * sepForce
                                ej.VelY <- ej.VelY - ny * sepForce

                pushApart 0 ppt
                pushApart t2s ppt

            // When a player is (near-)stationary, face toward the puck
            let puck = gs.Entities.[gs.PuckIdx]
            for i in 0 .. gs.NumEntities - 1 do
                if i <> gs.PuckIdx then
                    let ent = gs.Entities.[i]
                    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
                    if speedSq < 4.0 then  // effectively stopped
                        let dx = float (puck.X - ent.X)
                        let dy = float (puck.Y - ent.Y)
                        if abs dx > 2.0 || abs dy > 2.0 then
                            ent.DirX <- float (sign dx)
                            ent.DirY <- float (sign dy)

            // Ice trail: detect tight turns and leave skate marks
            // A tight turn = direction changed significantly while moving fast
            for i in 0 .. gs.NumPlayers - 1 do
                let ent = gs.Entities.[i]
                let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
                let prevDx = gs.PrevDirX.[i]
                let prevDy = gs.PrevDirY.[i]
                // Dot product of previous and current direction: < 0 means >90 degree turn
                let dot = ent.DirX * prevDx + ent.DirY * prevDy
                if speedSq > 100.0 && dot < 0.1 && (prevDx <> 0.0 || prevDy <> 0.0) then
                    let markIdx = gs.TrailMarkHead
                    let mark = gs.TrailMarks.[markIdx]
                    mark.X <- ent.X
                    mark.Y <- ent.Y
                    mark.Life <- TrailMarkLifetime
                    gs.TrailMarkHead <- (markIdx + 1) % MaxTrailMarks
                    if gs.TrailMarkCount < MaxTrailMarks then
                        gs.TrailMarkCount <- gs.TrailMarkCount + 1
                // Update previous direction
                gs.PrevDirX.[i] <- ent.DirX
                gs.PrevDirY.[i] <- ent.DirY

            // Decay trail marks
            for i in 0 .. gs.TrailMarkCount - 1 do
                let mark = gs.TrailMarks.[i]
                if mark.Life > 0<tick> then
                    mark.Life <- mark.Life - 1<tick>

            // Move entities and check walls/goals
            let mutable goalScored = false

            for i in 0 .. gs.NumEntities - 1 do
                let ent = gs.Entities.[i]
                ent.X <- ent.X + ent.VelX * 1.0<tick> / SubPixelUnit
                ent.Y <- ent.Y + ent.VelY * 1.0<tick> / SubPixelUnit

                if checkWallsAndGoals gs i then
                    goalScored <- true

            if not goalScored then
                checkPuckPickup gs

                if checkStalemate gs then
                    resetPositions gs

            // Clock
            updateClock gs

            // Period end check (deferred while a goal flash is showing)
            if gs.ClockSeconds >= gs.PeriodLength && gs.GoalFlashTimer <= 0<tick> then
                gs.CurrentPeriod <- gs.CurrentPeriod + 1

                if gs.CurrentPeriod >= gs.NumPeriods then
                    gs.Playing <- false
                else
                    gs.ClockSeconds <- 0<sec>
                    gs.ClockTick <- 0<tick>
                    resetPositions gs

// ─── League Mode ───────────────────────────────────────────────────────

/// Generate full round-robin schedule for N teams (N must be even).
/// Returns an array of rounds; each round is an array of (team1, team2) matchups.
/// Uses the standard circle method: fix team 0, rotate the rest.
let generateSchedule (numTeams: int) =
    let n = numTeams
    // Standard circle method: team index 0 is the fixed pivot, the rest rotate.
    let rounds = Array.init (n - 1) (fun _ -> Array.zeroCreate<int * int> (n / 2))

    for r in 0 .. n - 2 do
        // Build current arrangement: pivot (index 0) + rotated list
        let arrangement = Array.zeroCreate n
        arrangement.[0] <- 0

        for i in 0 .. n - 2 do
            arrangement.[i + 1] <- (i + r) % (n - 1) + 1

        // Pair first with last, second with second-to-last, etc.
        for m in 0 .. (n / 2) - 1 do
            rounds.[r].[m] <- (arrangement.[m], arrangement.[n - 1 - m])

    rounds

let createTeamStats () =
    { Wins = 0
      Losses = 0
      Draws = 0
      Points = 0
      GoalsFor = 0
      GoalsAgainst = 0 }

let createLeagueState humanTeam =
    let rng = Random()
    let schedule = generateSchedule NumTeams

    // Shuffle the round order (Fisher-Yates) so the human faces opponents
    // in a random order each league. Every round is still a full round —
    // each team plays exactly once per round.
    for i in schedule.Length - 1 .. -1 .. 1 do
        let j = rng.Next(i + 1)
        let tmp = schedule.[i]
        schedule.[i] <- schedule.[j]
        schedule.[j] <- tmp

    { Stats = Array.init NumTeams (fun _ -> createTeamStats ())
      Schedule = schedule
      Rng = rng
      CurrentRound = 0
      Finished = false
      HumanTeam = humanTeam }

/// Record match result for both teams
let recordMatchResult (league: LeagueState) team1Idx team2Idx team1Goals team2Goals =
    let s1 = league.Stats.[team1Idx]
    let s2 = league.Stats.[team2Idx]
    s1.GoalsFor <- s1.GoalsFor + team1Goals
    s1.GoalsAgainst <- s1.GoalsAgainst + team2Goals
    s2.GoalsFor <- s2.GoalsFor + team2Goals
    s2.GoalsAgainst <- s2.GoalsAgainst + team1Goals

    if team1Goals > team2Goals then
        s1.Wins <- s1.Wins + 1
        s1.Points <- s1.Points + 2
        s2.Losses <- s2.Losses + 1
    elif team2Goals > team1Goals then
        s2.Wins <- s2.Wins + 1
        s2.Points <- s2.Points + 2
        s1.Losses <- s1.Losses + 1
    else
        s1.Draws <- s1.Draws + 1
        s1.Points <- s1.Points + 1
        s2.Draws <- s2.Draws + 1
        s2.Points <- s2.Points + 1

/// Simulate a single CPU-vs-CPU match.
/// Each team's expected goals = baseGoals * strength; actual goals are Poisson-sampled.
/// Scores clamped to 0..10.
let simulateCpuGoals (rng: Random) (strength: float) =
    // Expected goals: ranges from ~1.5 (weakest) to ~5.0 (strongest)
    let lambda = 1.5 + strength * 3.5
    // Poisson sampling via Knuth's method
    let mutable k = 0
    let mutable p = 1.0
    let l = exp (-lambda)

    while p > l do
        k <- k + 1
        p <- p * rng.NextDouble()

    min 10 (max 0 (k - 1))

/// Simulate all CPU-vs-CPU matches for the given round and record results.
let simulateCpuRound (league: LeagueState) (roundIdx: int) =
    let round = league.Schedule.[roundIdx]

    for t1, t2 in round do
        // Skip the matchup involving the human team (already played live)
        if t1 <> league.HumanTeam && t2 <> league.HumanTeam then
            let goals1 = simulateCpuGoals league.Rng teamStrength.[t1]
            let goals2 = simulateCpuGoals league.Rng teamStrength.[t2]
            recordMatchResult league t1 t2 goals1 goals2

/// Sort standings by points descending, goal difference as tiebreak
let getSortedStandings (league: LeagueState) =
    league.Stats
    |> Array.indexed
    |> Array.sortByDescending (fun (_, s) -> s.Points, s.GoalsFor - s.GoalsAgainst)

/// Advance to next round; returns true if league is complete
let advanceRound (league: LeagueState) =
    league.CurrentRound <- league.CurrentRound + 1

    if league.CurrentRound >= league.Schedule.Length then
        league.Finished <- true

    league.Finished

/// Get the human team's matchup for the current round (human always returned as t1)
let currentMatchup (league: LeagueState) =
    let round = league.Schedule.[league.CurrentRound]
    match round |> Array.tryFind (fun (t1, t2) -> t1 = league.HumanTeam || t2 = league.HumanTeam) with
    | Some(a, b) -> if a = league.HumanTeam then (a, b) else (b, a)
    // Unreachable with an even team count (every team plays each round); fall back
    // to the first matchup rather than throwing if the schedule is ever malformed.
    | None -> round.[0]
