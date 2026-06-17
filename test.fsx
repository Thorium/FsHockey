/// Smoke test for HockeyDemo game logic.
/// Runs against the REAL Game.fs / Physics.fs (no MonoGame dependency needed),
/// so it exercises shipped code rather than a copy.
/// Run with: dotnet fsi test.fsx  (from the HockeyDemo directory)

#load "Physics.fs"
#load "Game.fs"

open HockeyDemo.Physics
open HockeyDemo.Game

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

let approx (a: float) (b: float) = abs (a - b) < 1e-6

/// Fresh 3v3 game state with player 0 holding the ball, facing (dirX, dirY).
let mkHolding dirX dirY (shotPower: float<subpx / tick>) =
    let gs = createGameState ()
    let p = gs.Entities.[0]
    p.X <- 100.0<px>
    p.Y <- 80.0<px>
    p.DirX <- dirX
    p.DirY <- dirY
    p.ShotPower <- shotPower
    gs.BallState <- HeldBy 0
    gs

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 1: releaseBall — full-power shot right ──"

let t1gs = mkHolding 1.0 0.0 38.0<subpx / tick>
releaseBall t1gs 0 1.0
let t1ball = t1gs.Entities.[t1gs.BallIdx]
check "ball VelX = +38" (approx (float t1ball.VelX) 38.0)
check "ball VelY = 0" (approx (float t1ball.VelY) 0.0)
check "kicker VelX = 0" (approx (float t1gs.Entities.[0].VelX) 0.0)
check "state is Free" (t1gs.BallState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 2: releaseBall — diagonal shot ──"

let t2gs = mkHolding 1.0 -1.0 48.0<subpx / tick>
releaseBall t2gs 0 1.0
let t2ball = t2gs.Entities.[t2gs.BallIdx]
check "ball VelX = +48" (approx (float t2ball.VelX) 48.0)
check "ball VelY = -48" (approx (float t2ball.VelY) -48.0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 3: releaseBall — pass fraction is weaker than a shot ──"

let t3gs = mkHolding 1.0 0.0 38.0<subpx / tick>
releaseBall t3gs 0 PassPowerFraction
let t3ball = t3gs.Entities.[t3gs.BallIdx]
check "pass VelX = 38 * PassPowerFraction" (approx (float t3ball.VelX) (38.0 * PassPowerFraction))

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 4: charge mechanic — full hold shoots at full power ──"

let t4gs = mkHolding 1.0 0.0 38.0<subpx / tick>
let mutable t4hold = 0<tick>

for _ in 1 .. int ChargeTicksForFull do
    applyHumanInput t4gs 0 false false false false true &t4hold

check "held the full charge duration" (t4hold = ChargeTicksForFull)
// Release (fire key up) fires the shot
applyHumanInput t4gs 0 false false false false false &t4hold
let t4ball = t4gs.Entities.[t4gs.BallIdx]
check "full charge shoots at full power" (approx (float t4ball.VelX) 38.0)
check "hold counter reset after release" (t4hold = 0<tick>)
check "ball is Free after shot" (t4gs.BallState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 5: charge mechanic — quick tap is a weak pass ──"

let t5gs = mkHolding 1.0 0.0 38.0<subpx / tick>
let mutable t5hold = 0<tick>
applyHumanInput t5gs 0 false false false false true &t5hold // 1 tick hold
applyHumanInput t5gs 0 false false false false false &t5hold // release
let t5ball = t5gs.Entities.[t5gs.BallIdx]

let t5expected =
    38.0
    * (PassPowerFraction + (1.0 - PassPowerFraction) * (1.0 / float (int ChargeTicksForFull)))

check "quick tap = weak shot" (approx (float t5ball.VelX) t5expected)
check "quick tap weaker than full power" (float t5ball.VelX < 38.0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 6: fire while a teammate holds the ball does nothing ──"

let t6gs = createGameState ()
t6gs.BallState <- HeldBy 1
let mutable t6hold = 0<tick>
applyHumanInput t6gs 0 false false false false true &t6hold
check "ball still HeldBy 1" (t6gs.BallState = HeldBy 1)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 7: checkBallPickup — free ball within CollisionDist ──"

let t7gs = createGameState ()
t7gs.Entities.[0].X <- 100.0<px>
t7gs.Entities.[0].Y <- 80.0<px>
let t7ball = t7gs.Entities.[t7gs.BallIdx]
t7ball.X <- 105.0<px> // dx = 5 < 8
t7ball.Y <- 82.0<px> // dy = 2 < 8
t7gs.BallState <- Free
checkBallPickup t7gs
check "ball picked up by player 0" (t7gs.BallState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 8: checkBallPickup — just out of reach stays free ──"

let t8gs = createGameState ()

for i in 0 .. t8gs.NumPlayers - 1 do
    t8gs.Entities.[i].X <- 0.0<px>
    t8gs.Entities.[i].Y <- 0.0<px>

t8gs.Entities.[0].X <- 100.0<px>
t8gs.Entities.[0].Y <- 80.0<px>
let t8ball = t8gs.Entities.[t8gs.BallIdx]
t8ball.X <- 110.0<px> // dx = 10, not < 8
t8ball.Y <- 80.0<px>
t8gs.BallState <- Free
checkBallPickup t8gs
check "no pickup just out of reach" (t8gs.BallState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 9: checkBallPickup — held ball cannot be stolen ──"

let t9gs = createGameState ()
t9gs.Entities.[1].X <- 100.0<px>
t9gs.Entities.[1].Y <- 80.0<px>
let t9ball = t9gs.Entities.[t9gs.BallIdx]
t9ball.X <- 100.0<px>
t9ball.Y <- 80.0<px>
t9gs.BallState <- HeldBy 0
checkBallPickup t9gs
check "still HeldBy 0 (no steal)" (t9gs.BallState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 10: findNearestToBall picks the closest skater ──"

let t10gs = createGameState ()

for i in 0 .. t10gs.NumPlayers - 1 do
    t10gs.Entities.[i].X <- float (200 + i) * 1.0<px>
    t10gs.Entities.[i].Y <- 80.0<px>

t10gs.Entities.[2].X <- 100.0<px> // closest to centred ball
let t10ball = t10gs.Entities.[t10gs.BallIdx]
t10ball.X <- 100.0<px>
t10ball.Y <- 80.0<px>
check "nearest team-1 player is index 2" (findNearestToBall t10gs 0 (t10gs.PlayersPerTeam - 1) = 2)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 11: generateSchedule produces a valid round-robin ──"

let sched = generateSchedule NumTeams
check "N-1 rounds" (sched.Length = NumTeams - 1)
check "N/2 matches per round" (sched |> Array.forall (fun r -> r.Length = NumTeams / 2))

let eachTeamOncePerRound =
    sched
    |> Array.forall (fun round ->
        let seen = round |> Array.collect (fun (a, b) -> [| a; b |]) |> Array.sort
        seen = [| 0 .. NumTeams - 1 |])

check "each team plays exactly once per round" eachTeamOncePerRound

let allPairs =
    sched
    |> Array.collect id
    |> Array.map (fun (a, b) -> if a < b then (a, b) else (b, a))

let expectedPairs = NumTeams * (NumTeams - 1) / 2
check $"{expectedPairs} total matchups" (allPairs.Length = expectedPairs)
check "every pairing is unique" ((Array.distinct allPairs).Length = expectedPairs)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 12: simulateCpuGoals stays within 0..10 ──"

let rng = System.Random(12345)

let goalsInRange =
    [ for _ in 1..1000 -> simulateCpuGoals rng 0.95 ]
    |> List.forall (fun g -> g >= 0 && g <= 10)

check "CPU goals clamped to 0..10" goalsInRange

// ══════════════════════════════════════════════════════════════════════
printfn ""
printfn "════════════════════════════════════════════════════════════════"
printfn "  Results: %d passed, %d failed" passed failed

if failed > 0 then
    printfn "  *** SOME TESTS FAILED ***"
else
    printfn "  All tests passed!"

printfn "════════════════════════════════════════════════════════════════"

exit failed
