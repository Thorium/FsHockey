/// Quick smoke test for HockeyDemo game logic
/// Run with: dotnet fsi test.fsx  (from HockeyDemo directory)

// ── Inline types and constants ──

[<Measure>]
type px

[<Measure>]
type subpx

[<Measure>]
type tick

[<Measure>]
type sec

let NumEntities = 7
let NumPlayers = 6
let BallIdx = 6
let SubPixelUnit = 32.0<subpx / px>
let FrictionRate = 1.0<subpx / tick>
let BallMaxSpeed = 16.0<subpx / tick>
let BallAnimFrames = 8
let CollisionDist = 14.0<px>
let PossessionTimerVal = 200<tick>
let ForwardAccel = 2.0<subpx / tick>

let inline clampF lo hi v = max lo (min hi v)

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
type BallState =
    | Free
    | HeldBy of entityIdx: int

type MiniGS =
    { Entities: Entity array
      mutable BallState: BallState
      mutable PossessionTimer: int<tick>
      mutable BallAnimFrame: int }

let mkEnt x y spd accel shot dx dy : Entity =
    { X = x
      Y = y
      VelX = 0.0<subpx / tick>
      VelY = 0.0<subpx / tick>
      DirX = dx
      DirY = dy
      MaxSpeed = spd
      Accel = accel
      ShotPower = shot }

let farEnt () =
    mkEnt 999.0<px> 999.0<px> 0.0<subpx / tick> 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

/// Make a 7-entity gamestate: provide entities 0-5 + ball
let mkGS (ents: Entity list) (ball: Entity) (bs: BallState) =
    let padded = ents @ List.init (6 - ents.Length) (fun _ -> farEnt ())
    let all = Array.ofList (padded @ [ ball ])

    { Entities = all
      BallState = bs
      PossessionTimer = PossessionTimerVal
      BallAnimFrame = BallAnimFrames }

// ── Functions under test (copied from Game.fs) ──

let releaseBall (gs: MiniGS) entityIdx =
    let ent = gs.Entities[entityIdx]
    let ball = gs.Entities[BallIdx]
    ent.VelX <- 0.0<subpx / tick>
    ent.VelY <- 0.0<subpx / tick>

    ball.VelX <-
        (if ent.DirX > 0.0 then ent.ShotPower
         elif ent.DirX < 0.0 then -ent.ShotPower
         else 0.0<subpx / tick>)

    ball.VelY <-
        (if ent.DirY > 0.0 then ent.ShotPower
         elif ent.DirY < 0.0 then -ent.ShotPower
         else 0.0<subpx / tick>)

    gs.BallState <- Free
    gs.BallAnimFrame <- BallAnimFrames

let checkBallPickup (gs: MiniGS) =
    match gs.BallState with
    | Free ->
        let ball = gs.Entities[BallIdx]

        for i in 0 .. NumPlayers - 1 do
            match gs.BallState with
            | Free ->
                let ent = gs.Entities[i]

                if
                    abs (float (ent.X - ball.X)) < float CollisionDist
                    && abs (float (ent.Y - ball.Y)) < float CollisionDist
                then
                    gs.BallState <- HeldBy i
                    gs.PossessionTimer <- PossessionTimerVal
            | _ -> ()
    | _ -> ()

let clampVel (ent: Entity) =
    ent.VelX <- clampF -ent.MaxSpeed ent.MaxSpeed ent.VelX
    ent.VelY <- clampF -ent.MaxSpeed ent.MaxSpeed ent.VelY

let applyHumanInput (gs: MiniGS) idx left right up down fire =
    let ent = gs.Entities[idx]
    let mutable dx = 0.0
    let mutable dy = 0.0

    if left then
        ent.VelX <- ent.VelX - ent.Accel
        dx <- -1.0

    if right then
        ent.VelX <- ent.VelX + ent.Accel
        dx <- 1.0

    if up then
        ent.VelY <- ent.VelY - ent.Accel
        dy <- -1.0

    if down then
        ent.VelY <- ent.VelY + ent.Accel
        dy <- 1.0

    clampVel ent

    if dx <> 0.0 || dy <> 0.0 then
        ent.DirX <- dx
        ent.DirY <- dy

    match gs.BallState with
    | HeldBy owner when owner = idx && fire -> releaseBall gs idx
    | _ -> ()

// ── Test harness ──

let mutable passed = 0
let mutable failed = 0

let check (name: string) cond =
    if cond then
        passed <- passed + 1
        printfn "  PASS: %s" name
    else
        failed <- failed + 1
        printfn "  FAIL: %s" name

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 1: releaseBall — shoot right ──"

let t1p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t1b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t1gs = mkGS [ t1p ] t1b (HeldBy 0)
releaseBall t1gs 0
check "ball VelX = +38" (t1b.VelX = 38.0<subpx / tick>)
check "ball VelY = 0" (t1b.VelY = 0.0<subpx / tick>)
check "kicker VelX = 0" (t1p.VelX = 0.0<subpx / tick>)
check "state is Free" (t1gs.BallState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 2: releaseBall — shoot left ──"

let t2p =
    mkEnt 200.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 48.0<subpx / tick> (-1.0) 0.0

let t2b =
    mkEnt 200.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t2gs = mkGS [ farEnt (); farEnt (); farEnt (); t2p ] t2b (HeldBy 3)
releaseBall t2gs 3
check "ball VelX = -48" (t2b.VelX = -48.0<subpx / tick>)
check "ball VelY = 0" (t2b.VelY = 0.0<subpx / tick>)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 3: releaseBall — diagonal ──"

let t3p =
    mkEnt 150.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 48.0<subpx / tick> 1.0 (-1.0)

let t3b =
    mkEnt 150.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t3gs = mkGS [ t3p ] t3b (HeldBy 0)
releaseBall t3gs 0
check "ball VelX = +48" (t3b.VelX = 48.0<subpx / tick>)
check "ball VelY = -48" (t3b.VelY = -48.0<subpx / tick>)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 4: releaseBall — direction (0,0) → zero velocity ──"

let t4p =
    mkEnt 150.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 48.0<subpx / tick> 0.0 0.0

let t4b =
    mkEnt 150.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t4gs = mkGS [ t4p ] t4b (HeldBy 0)
releaseBall t4gs 0
check "ball VelX = 0" (t4b.VelX = 0.0<subpx / tick>)
check "ball VelY = 0" (t4b.VelY = 0.0<subpx / tick>)
printfn "  NOTE: Dir(0,0) produces zero-velocity shot — ball drops in place!"

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 5: applyHumanInput — fire shoots when holding ball ──"

let t5p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t5b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t5gs = mkGS [ t5p ] t5b (HeldBy 0)
applyHumanInput t5gs 0 false true false false true
check "ball is Free" (t5gs.BallState = Free)
check "ball VelX = +38" (t5b.VelX = 38.0<subpx / tick>)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 6: fire but different player holds ball ──"

let t6p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t6b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t6gs = mkGS [ t6p ] t6b (HeldBy 1)
applyHumanInput t6gs 0 false false false false true
check "ball still HeldBy 1" (t6gs.BallState = HeldBy 1)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 7: checkBallPickup — free ball near player ──"

let t7p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t7b =
    mkEnt 105.0<px> 82.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t7gs = mkGS [ t7p ] t7b Free
checkBallPickup t7gs
check "ball picked up by player 0" (t7gs.BallState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 8: checkBallPickup — held ball cannot be stolen ──"

let t8p0 =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t8p1 =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> (-1.0) 0.0

let t8b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t8gs = mkGS [ t8p0; t8p1 ] t8b (HeldBy 0)
checkBallPickup t8gs
check "still HeldBy 0 (no steal)" (t8gs.BallState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 9: shoot-then-pickup cycle ──"

let t9p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t9b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t9gs = mkGS [ t9p ] t9b (HeldBy 0)
releaseBall t9gs 0
check "after shoot: Free" (t9gs.BallState = Free)
t9b.X <- 200.0<px>
checkBallPickup t9gs
check "ball far: still Free" (t9gs.BallState = Free)
t9b.X <- 103.0<px>
t9b.Y <- 82.0<px>
checkBallPickup t9gs
check "ball near: picked up" (t9gs.BallState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 10: fire without pressing direction uses initial Dir ──"

let t10p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t10b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t10gs = mkGS [ t10p ] t10b (HeldBy 0)
applyHumanInput t10gs 0 false false false false true // fire only, no direction keys
check "shoots right (initial DirX=1)" (t10b.VelX = 38.0<subpx / tick>)
check "no Y component" (t10b.VelY = 0.0<subpx / tick>)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 11: direction updated same tick as fire ──"

let t11p =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t11b =
    mkEnt 100.0<px> 80.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t11gs = mkGS [ t11p ] t11b (HeldBy 0)
applyHumanInput t11gs 0 true false true false true // left + up + fire
check "shoots left" (t11b.VelX = -38.0<subpx / tick>)
check "shoots up" (t11b.VelY = -38.0<subpx / tick>)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 12: active player != holder — can't shoot ──"

let t12p0 =
    mkEnt 100.0<px> 80.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t12p2 =
    mkEnt 50.0<px> 50.0<px> 32.0<subpx / tick> ForwardAccel 38.0<subpx / tick> 1.0 0.0

let t12b =
    mkEnt 50.0<px> 50.0<px> BallMaxSpeed 0.0<subpx / tick> 0.0<subpx / tick> 0.0 0.0

let t12gs = mkGS [ t12p0; farEnt (); t12p2 ] t12b (HeldBy 2)
applyHumanInput t12gs 0 false false false false true
check "ball still HeldBy 2" (t12gs.BallState = HeldBy 2)
printfn "  NOTE: Human controls nearest-to-ball, but teammate may hold the ball!"

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 13: ball travel distance with friction ──"
let fieldWidth = 286.0

let travelTest (power: int) =
    let mutable pos = 0.0
    let mutable vel = float power
    let mutable ticks = 0

    while pos < fieldWidth && vel > 0.0 do
        pos <- pos + vel / 32.0
        vel <- vel - 1.0
        ticks <- ticks + 1

    printfn "  power=%2d: %.0f px in %d ticks (%.1fs @60Hz)" power pos ticks (float ticks / 60.0)

travelTest 16
travelTest 32
travelTest 38
travelTest 48
travelTest 64

// ══════════════════════════════════════════════════════════════════════
printfn ""
printfn "════════════════════════════════════════════════════════════════"
printfn "  Results: %d passed, %d failed" passed failed

if failed > 0 then
    printfn "  *** SOME TESTS FAILED ***"
else
    printfn "  All tests passed!"

printfn "════════════════════════════════════════════════════════════════"
