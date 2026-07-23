/// Smoke test for HockeyDemo.
/// Part 1 runs against the REAL Game.fs / Physics.fs (pure game logic).
/// Part 2 boots the REAL Elmish app (App.fs) in Mibo's headless runtime and
/// plays through menu -> exhibition -> league -> quit with simulated input,
/// so the full update loop is exercised without a window.
/// Run with: dotnet fsi test.fsx  (from the HockeyDemo directory)

#r "nuget: Mibo.Core, 3.1.1"
#load "Physics.fs"
#load "Game.fs"
#load "App.fs"

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

/// Fresh 3v3 game state with player 0 holding the puck, facing (dirX, dirY).
/// Puck release speed comes from the match-level ShotSpeed, not player stats.
let mkHolding dirX dirY (shotSpeed: float<subpx / tick>) =
    let gs = createGameState ()
    let p = gs.Entities.[0]
    p.X <- 100.0<px>
    p.Y <- 80.0<px>
    p.DirX <- dirX
    p.DirY <- dirY
    gs.ShotSpeed <- shotSpeed
    gs.PuckState <- HeldBy 0
    gs

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 1: releasePuck — full-power shot right ──"

let t1gs = mkHolding 1.0 0.0 38.0<subpx / tick>
releasePuck t1gs 0 1.0
let t1puck = t1gs.Entities.[t1gs.PuckIdx]
check "puck VelX = +38" (approx (float t1puck.VelX) 38.0)
check "puck VelY = 0" (approx (float t1puck.VelY) 0.0)
check "kicker VelX = 0" (approx (float t1gs.Entities.[0].VelX) 0.0)
check "state is Free" (t1gs.PuckState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 2: releasePuck — diagonal shot ──"

let t2gs = mkHolding 1.0 -1.0 48.0<subpx / tick>
releasePuck t2gs 0 1.0
let t2puck = t2gs.Entities.[t2gs.PuckIdx]
check "puck VelX = +48" (approx (float t2puck.VelX) 48.0)
check "puck VelY = -48" (approx (float t2puck.VelY) -48.0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 3: releasePuck — pass fraction is weaker than a shot ──"

let t3gs = mkHolding 1.0 0.0 38.0<subpx / tick>
releasePuck t3gs 0 PassPowerFraction
let t3puck = t3gs.Entities.[t3gs.PuckIdx]
check "pass VelX = 38 * PassPowerFraction" (approx (float t3puck.VelX) (38.0 * PassPowerFraction))

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 4: charge mechanic — full hold shoots at full power ──"

let fireHeld = { Input.none with Fire = true }

let t4gs = mkHolding 1.0 0.0 38.0<subpx / tick>
let mutable t4hold = 0<tick>

for _ in 1 .. int ChargeTicksForFull do
    applyHumanInput t4gs 0 fireHeld &t4hold

check "held the full charge duration" (t4hold = ChargeTicksForFull)
// Release (fire key up) fires the shot
applyHumanInput t4gs 0 Input.none &t4hold
let t4puck = t4gs.Entities.[t4gs.PuckIdx]
check "full charge shoots at full power" (approx (float t4puck.VelX) 38.0)
check "hold counter reset after release" (t4hold = 0<tick>)
check "puck is Free after shot" (t4gs.PuckState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 5: charge mechanic — quick tap is a weak pass ──"

let t5gs = mkHolding 1.0 0.0 38.0<subpx / tick>
let mutable t5hold = 0<tick>
applyHumanInput t5gs 0 fireHeld &t5hold // 1 tick hold
applyHumanInput t5gs 0 Input.none &t5hold // release
let t5puck = t5gs.Entities.[t5gs.PuckIdx]

let t5expected =
    38.0
    * (PassPowerFraction + (1.0 - PassPowerFraction) * (1.0 / float (int ChargeTicksForFull)))

check "quick tap = weak shot" (approx (float t5puck.VelX) t5expected)
check "quick tap weaker than full power" (float t5puck.VelX < 38.0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 6: fire while a teammate holds the puck does nothing ──"

let t6gs = createGameState ()
t6gs.PuckState <- HeldBy 1
let mutable t6hold = 0<tick>
applyHumanInput t6gs 0 fireHeld &t6hold
check "puck still HeldBy 1" (t6gs.PuckState = HeldBy 1)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 7: checkPuckPickup — free puck within CollisionDist ──"

let t7gs = createGameState ()
t7gs.Entities.[0].X <- 100.0<px>
t7gs.Entities.[0].Y <- 80.0<px>
let t7puck = t7gs.Entities.[t7gs.PuckIdx]
t7puck.X <- 105.0<px> // dx = 5 < 8
t7puck.Y <- 82.0<px> // dy = 2 < 8
t7gs.PuckState <- Free
checkPuckPickup t7gs
check "puck picked up by player 0" (t7gs.PuckState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 8: checkPuckPickup — just out of reach stays free ──"

let t8gs = createGameState ()

for i in 0 .. t8gs.NumPlayers - 1 do
    t8gs.Entities.[i].X <- 0.0<px>
    t8gs.Entities.[i].Y <- 0.0<px>

t8gs.Entities.[0].X <- 100.0<px>
t8gs.Entities.[0].Y <- 80.0<px>
let t8puck = t8gs.Entities.[t8gs.PuckIdx]
t8puck.X <- 110.0<px> // dx = 10, not < 8
t8puck.Y <- 80.0<px>
t8gs.PuckState <- Free
checkPuckPickup t8gs
check "no pickup just out of reach" (t8gs.PuckState = Free)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 9: checkPuckPickup — held puck cannot be stolen ──"

let t9gs = createGameState ()
t9gs.Entities.[1].X <- 100.0<px>
t9gs.Entities.[1].Y <- 80.0<px>
let t9puck = t9gs.Entities.[t9gs.PuckIdx]
t9puck.X <- 100.0<px>
t9puck.Y <- 80.0<px>
t9gs.PuckState <- HeldBy 0
checkPuckPickup t9gs
check "still HeldBy 0 (no steal)" (t9gs.PuckState = HeldBy 0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 10: findNearestToPuck picks the closest skater ──"

let t10gs = createGameState ()

for i in 0 .. t10gs.NumPlayers - 1 do
    t10gs.Entities.[i].X <- float (200 + i) * 1.0<px>
    t10gs.Entities.[i].Y <- 80.0<px>

t10gs.Entities.[2].X <- 100.0<px> // closest to centred puck
let t10puck = t10gs.Entities.[t10gs.PuckIdx]
t10puck.X <- 100.0<px>
t10puck.Y <- 80.0<px>
check "nearest team-1 player is index 2" (findNearestToPuck t10gs 0 (t10gs.PlayersPerTeam - 1) = 2)

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
// Part 2 — the whole Elmish app under Mibo's headless runtime
// ══════════════════════════════════════════════════════════════════════

open System
open Mibo.Elmish
open Mibo.Input
open HockeyDemo.App

printfn "── Test 13: headless app — menu to exhibition match ──"

let program =
    HeadlessProgram.mkHeadless init update
    |> HeadlessProgram.withFixedStep fixedStepConfig

let runner = new HeadlessRunner<Model, Msg>(program)
let frame = TimeSpan.FromSeconds(1.0 / 60.0)

/// Simulate a key press: a delta with the action started+held, one frame,
/// then a release delta (mirrors what InputMapper dispatches).
let pressOn (r: HeadlessRunner<Model, Msg>) (a: HockeyAction) =
    r.Dispatch(InputChanged { ActionState.empty with Started = Set.singleton a; Held = Set.singleton a })
    r.Step(frame)
    r.Dispatch(InputChanged { ActionState.empty with Released = Set.singleton a })
    r.Step(frame)

/// Hold a set of actions (movement keys): Held only, no edge actions.
let holdOn (r: HeadlessRunner<Model, Msg>) (actions: HockeyAction list) =
    r.Dispatch(InputChanged { ActionState.empty with Held = Set.ofList actions })

let releaseOn (r: HeadlessRunner<Model, Msg>) =
    r.Dispatch(InputChanged ActionState.empty)

let press = pressOn runner

check "boots into the menu" (runner.Model.Mode = Menu)

// Pick Phobos (CPU) as team 1 so the match is CPU vs CPU
press MenuDown
check "menu selection moved to team 1" (runner.Model.SelectedTeam1 = 1)

press Select
check "Enter starts an exhibition match" (runner.Model.Mode = Playing)
check "match is running" runner.Model.Gs.Playing

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 14: headless app — pause holds the clock ──"

runner.StepN(300, frame) // 5 s: get past the PERIOD 1 banner hold
let clockBefore = runner.Model.Gs.ClockSeconds
check "clock is running" (clockBefore > 0<sec>)

press TogglePause
check "P pauses" runner.Model.Paused
runner.StepN(120, frame)
check "clock frozen while paused" (runner.Model.Gs.ClockSeconds = clockBefore)
press TogglePause
check "P resumes" (not runner.Model.Paused)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 15: headless app — match plays to the final whistle ──"

runner.StepN(3600, frame) // 60 virtual seconds; a 1-period match ends well within
let gs = runner.Model.Gs
check "match over" (matchOver gs)
check "clock reached the period length" (gs.ClockSeconds >= gs.PeriodLength)
check "entities stayed on the ice" (
    seq { 0 .. gs.NumEntities - 1 }
    |> Seq.forall (fun i ->
        let e = gs.Entities.[i]
        e.X >= FieldLeft && e.X <= FieldRight && e.Y >= FieldTop && e.Y <= FieldBottom))

press Continue
check "Space returns to the menu" (runner.Model.Mode = Menu)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 16: headless app — league round records results ──"

press StartLeague
check "L opens the league matchup screen" (runner.Model.Mode = LeagueMatchup)
check "league created" runner.Model.League.IsSome

press Continue
check "Space starts the league match" (runner.Model.Mode = LeaguePlaying)
check "league match uses 3 periods" (runner.Model.Gs.NumPeriods = LeaguePeriods)

runner.StepN(9000, frame) // 150 virtual seconds; 3 x 30 s periods + banners
check "league match over" (matchOver runner.Model.Gs)

press Continue
check "results screen after the match" (runner.Model.Mode = LeagueStandings)

match runner.Model.League with
| Some league ->
    let games = league.Stats |> Array.sumBy (fun s -> s.Wins + s.Losses + s.Draws)
    check "every team has one game recorded" (games = NumTeams)
    check "round advanced" (league.CurrentRound = 1)
| None -> check "league still exists" false

press Back
check "Esc leaves the league" (runner.Model.Mode = Menu && runner.Model.League.IsNone)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 17: headless app — Esc in the menu quits ──"

press Back
check "quit signaled" runner.ShouldQuit

(runner :> IDisposable).Dispose()

// ══════════════════════════════════════════════════════════════════════
// Part 3 — end-user gameplay scenarios (fresh runner)
// ══════════════════════════════════════════════════════════════════════

printfn "── Test 18: headless app — human keyboard control moves the skater ──"

let r2 = new HeadlessRunner<Model, Msg>(program)
let press2 = pressOn r2

check "boots into the menu (fresh runner)" (r2.Model.Mode = Menu)
check "team 1 defaults to HUMAN PLAYER" (r2.Model.SelectedTeam1 = 0)

press2 Select // HUMAN vs Phobos
check "exhibition started" (r2.Model.Mode = Playing)
check "team 1 is human-controlled" r2.Model.Gs.Team1Human

r2.StepN(180, frame) // past the PERIOD 1 banner, let active-player settle

let g18 = r2.Model.Gs
let active18 = g18.ActivePlayer1
let startX18 = float g18.Entities.[active18].X
let startY18 = float g18.Entities.[active18].Y
holdOn r2 [ P1Right; P1Down ]
r2.StepN(40, frame)
releaseOn r2
r2.Step(frame)
check "held keys skate the active player right" (float g18.Entities.[active18].X > startX18 + 1.0)
check "held keys skate the active player down" (float g18.Entities.[active18].Y > startY18 + 1.0)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 19: headless app — a shot into the net scores end-to-end ──"

let g19 = r2.Model.Gs
// Park all skaters far from the goal so nobody intercepts the staged shot
for i in 0 .. g19.NumPlayers - 1 do
    let e = g19.Entities.[i]
    e.X <- 40.0<px>
    e.Y <- 20.0<px>
    e.VelX <- 0.0<subpx / tick>
    e.VelY <- 0.0<subpx / tick>

g19.PuckState <- Free
g19.LastReleaser <- -1
g19.RecaptureBlockTicks <- 0<tick>
let puck19 = g19.Entities.[g19.PuckIdx]
puck19.X <- FieldRight - 6.0<px>
puck19.Y <- CenterY // inside the goal mouth band
puck19.VelX <- 64.0<subpx / tick>
puck19.VelY <- 0.0<subpx / tick>

let score19 = g19.Team1Score
r2.StepN(30, frame)
check "goal counted for team 1" (g19.Team1Score = score19 + 1)
check "goal attributed to team 1" (g19.GoalScoredBy = Team1Scored)
check "goal flash showing" (g19.GoalFlashTimer > 0<tick>)

r2.StepN(120, frame) // flash runs out (90 ticks) and positions reset
// By now a CPU skater may already have legitimately picked the puck up at
// the faceoff, so assert the reset happened and play carried on — not that
// the puck is still untouched.
check "faceoff reset after the goal" (abs (float puck19.X - float CenterX) < 10.0)
check "play resumed after the goal" (g19.GoalFlashTimer = 0<tick> && g19.Playing)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 20: headless app — gamepad stick and d-pad drive the player ──"

let padDelta stickX stickY (pressed: GamepadButtonCode[]) (released: GamepadButtonCode[]) : GamepadDelta =
    { PlayerIndex = 0
      Buttons = { Pressed = pressed; Released = released }
      Analog =
        { LeftThumbstick = System.Numerics.Vector2(stickX, stickY)
          RightThumbstick = System.Numerics.Vector2.Zero
          LeftTrigger = 0.0f
          RightTrigger = 0.0f } }

check "gamepads enabled by default" r2.Model.GamepadEnabled

let g20 = r2.Model.Gs
let active20 = g20.ActivePlayer1
let startX20 = float g20.Entities.[active20].X
r2.Dispatch(PadChanged(padDelta 1.0f 0.0f [||] [||])) // stick hard right
r2.StepN(40, frame)
check "stick right skates the player right" (float g20.Entities.[active20].X > startX20 + 1.0)

r2.Dispatch(PadChanged(padDelta 0.0f 0.0f [| GamepadButtonCode.DPadUp |] [||]))
let active20b = g20.ActivePlayer1
let startY20 = float g20.Entities.[active20b].Y
r2.StepN(40, frame)
check "d-pad up skates the player up" (float g20.Entities.[active20b].Y < startY20 - 1.0)

// release everything
r2.Dispatch(PadChanged(padDelta 0.0f 0.0f [||] [| GamepadButtonCode.DPadUp |]))
r2.Step(frame)

press2 Back
check "back to the menu" (r2.Model.Mode = Menu)

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 21: headless app — two-player match, P2 keys drive team 2 ──"

press2 SwitchColumn // to the team-2 column
press2 MenuUp // Phobos -> HUMAN PLAYER
check "team 2 set to HUMAN PLAYER" (r2.Model.SelectedTeam2 = 0)

press2 Select
check "two-player match started" (r2.Model.Mode = Playing)
check "team 2 is human-controlled" r2.Model.Gs.Team2Human

r2.StepN(180, frame)
let g21 = r2.Model.Gs
let active21 = g21.ActivePlayer2
let startX21 = float g21.Entities.[active21].X
holdOn r2 [ P2Left ]
r2.StepN(40, frame)
releaseOn r2
r2.Step(frame)
check "P2 held key skates team-2 player left" (float g21.Entities.[active21].X < startX21 - 1.0)

press2 Back

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 22: headless app — 6v6 + hard mode options shape the match ──"

press2 MenuDown // team 2 back to a CPU team (HUMAN -> Phobos)
press2 TogglePlayerCount
press2 ToggleHardMode
check "6v6 selected" r2.Model.FivePlayerMode
check "hard mode selected" r2.Model.HardMode

press2 Select
let g22 = r2.Model.Gs
check "6v6 match: 6 players per team" (g22.PlayersPerTeam = 6)
check "6v6 match: 13 entities on the ice" (g22.NumEntities = 13)
check "hard mode shot speed" (g22.ShotSpeed = HardShotReleaseSpeed)

// CPU team 2 (Phobos) forward: 32 subpx/tick base * 1.3 hard-mode multiplier
let cpuFwd = g22.Entities.[g22.Team2Start + 1]
check "hard mode multiplies CPU speed" (abs (float cpuFwd.MaxSpeed - 32.0 * HardModeSpeedMult) < 1e-6)
// Goalie is speed-capped regardless of team stats
check "goalie speed capped" (float g22.Entities.[g22.Team2Start].MaxSpeed <= float GoalieMaxSpeed)

r2.StepN(300, frame) // goalie AI + teammate separation paths run
check "6v6 sim stays on the ice" (
    seq { 0 .. g22.NumEntities - 1 }
    |> Seq.forall (fun i ->
        let e = g22.Entities.[i]
        e.X >= FieldLeft && e.X <= FieldRight && e.Y >= FieldTop && e.Y <= FieldBottom))

press2 Back
press2 TogglePlayerCount // restore 3v3
press2 ToggleHardMode // restore normal mode

// ══════════════════════════════════════════════════════════════════════
printfn "── Test 23: headless app — full league season to the champion screen ──"

press2 StartLeague
check "league started" (r2.Model.Mode = LeagueMatchup)

let mutable rounds23 = 0

while r2.Model.Mode <> LeagueFinalStandings && rounds23 < 12 do
    press2 Continue // start the round's match
    r2.StepN(9000, frame) // 150 virtual seconds: 3 periods + banners + flashes
    press2 Continue // record result, simulate CPU round, advance

    if r2.Model.Mode = LeagueStandings then
        press2 Continue // on to the next round's matchup

    rounds23 <- rounds23 + 1

check "season ran all 9 rounds" (rounds23 = 9)
check "final standings reached" (r2.Model.Mode = LeagueFinalStandings)

match r2.Model.League with
| Some league ->
    check "league finished" league.Finished
    let games = league.Stats |> Array.sumBy (fun s -> s.Wins + s.Losses + s.Draws)
    check "45 matches = 90 team-games recorded" (games = 90)
    let points = league.Stats |> Array.sumBy (fun s -> s.Points)
    check "2 points awarded per match" (points = 90)
    let winnerIdx, winnerStats = (getSortedStandings league).[0]
    check "champion has points on the board" (winnerStats.Points > 0 && winnerIdx >= 0)
| None -> check "league still exists at season end" false

press2 Continue
check "season over returns to the menu" (r2.Model.Mode = Menu && r2.Model.League.IsNone)

(r2 :> IDisposable).Dispose()

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
