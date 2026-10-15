/// THE FS HOCKEY LEAGUE — Renderer (Canvas 2D / Fable)
/// Canvas drawing: ice rink, players, puck, HUD, goal flash, game-over.
/// Ported from the MonoGame SpriteBatch renderer; same layout/coordinates,
/// drawn through the browser's 2D context.
module HockeyDemo.Renderer

open Fable.Core
open HockeyDemo.Physics
open HockeyDemo.Drawing
open HockeyDemo.Game

// ─── Canvas interop for text + transforms ──────────────────────────────

[<Emit("$0.font = $1")>]
let private setFontJs (ctx: obj) (f: string) : unit = jsNative
[<Emit("$0.textBaseline = $1")>]
let private setBaseline (ctx: obj) (b: string) : unit = jsNative
[<Emit("$0.fillStyle = $1")>]
let private setFill (ctx: obj) (c: string) : unit = jsNative
[<Emit("$0.fillText($1, $2, $3)")>]
let private fillTextJs (ctx: obj) (s: string) (x: float) (y: float) : unit = jsNative
[<Emit("$0.measureText($1).width")>]
let private measureWidthJs (ctx: obj) (s: string) : float = jsNative
[<Emit("$0.save()")>]
let private saveCtx (ctx: obj) : unit = jsNative
[<Emit("$0.restore()")>]
let private restoreCtx (ctx: obj) : unit = jsNative
[<Emit("$0.translate($1, $2)")>]
let private translateCtx (ctx: obj) (x: float) (y: float) : unit = jsNative
[<Emit("$0.rotate($1)")>]
let private rotateCtx (ctx: obj) (a: float) : unit = jsNative

// ─── Scale / Layout ────────────────────────────────────────────────────

[<Literal>]
let OrigW = 320.0

[<Literal>]
let OrigH = 200.0

[<Literal>]
let HudHeight = 48.0

// ─── Colors (CGA-inspired) ────────────────────────────────────────────

let iceColor = Color(200, 220, 240)
let boardColor = Color(60, 80, 120)
let lineColor = Color(180, 40, 40)
let blueLineColor = Color(40, 80, 180)
let team1Light = Color(255, 120, 120)
let team2Light = Color(120, 160, 255)

// ─── Team colours by side and speed class ────────────────────────────
// The left team (team 1) wears red / orange / red-purple and the right team
// (team 2) blue / yellow / green, fastest to slowest: the jersey tells how
// quick a team is, and the two sides never share a colour. The purple is
// tinted towards red to stay clear of the right side's blue; the green is
// dark enough not to hide the active-player marker.

[<Struct>]
type SpeedClass =
    | Fast
    | Medium
    | Slow

/// Fast, medium and slow jersey colours of the left team.
let private team1Colors = struct (Color(220, 60, 60), Color(225, 105, 10), Color(190, 80, 160))
/// Fast, medium and slow jersey colours of the right team.
let private team2Colors = struct (Color(60, 100, 220), Color(255, 205, 0), Color(30, 130, 60))

// Speed classes come from the CPU teams' skating speeds as the menu shows them
// under the current settings: the fastest CPU value is Fast, the slowest Slow,
// anything between Medium. Hard mode speeds every CPU team up alike, so CPU
// classes never change — but the human team is placed against those speeds,
// so in hard mode the fast human drops to Medium.

/// Slowest and fastest CPU team speed, without and with hard mode.
let private cpuSpeedRanges =
    [| false; true |]
    |> Array.map (fun hardMode ->
        let speeds = Array.init (NumTeams - 1) (fun i -> displayedTeamSpeed false hardMode (i + 1))
        struct (Array.min speeds, Array.max speeds))

/// Speed class of a team under the fast-human and hard-mode settings.
let speedClass (fastHuman: bool) (hardMode: bool) (teamIdx: int) =
    let struct (slowest, fastest) = cpuSpeedRanges[if hardMode then 1 else 0]
    let v = displayedTeamSpeed fastHuman hardMode teamIdx
    if v >= fastest then Fast
    elif v <= slowest then Slow
    else Medium

/// Jersey colour of a team playing on the left (team 1) or right (team 2).
let teamColor (rightSide: bool) (fastHuman: bool) (hardMode: bool) (teamIdx: int) =
    let struct (fast, medium, slow) = if rightSide then team2Colors else team1Colors
    match speedClass fastHuman hardMode teamIdx with
    | Fast -> fast
    | Medium -> medium
    | Slow -> slow
let puckColor = Color(20, 20, 20)
let puckHighlight = Color(60, 60, 60)
let hudBg = Color(20, 20, 40)
let hudText = Color(220, 220, 220)
let goalFlashColor = Color(255, 255, 80)
/// green caret over the controlled player
let activeMarkerColor = Color(90, 255, 120)
let stickBrown = Color(139, 90, 43)
let stickTape = Color(240, 240, 240)
let helmetBlack = Color(30, 30, 30)
let helmetGold = Color(200, 180, 40)
let trouserColor = Color(30, 30, 30)
let skateColor = Color(80, 80, 80)
let goaliePadColor = Color(230, 220, 200)
let goalieMaskColor = Color(220, 220, 220)
let skinColor = Color(230, 195, 160)
let gloveColor = Color(60, 60, 60)
let sockColor = Color(200, 200, 210)
let bgColor = Color(30, 30, 50)
let grayColor = Color(160, 160, 160)
/// Dimmed "(speed N)" hint after a team name (menu, league matchup).
let speedHintColor = Color(105, 105, 125)
let jerseyStripeColor = Color(255, 255, 255, 80)
let cageColor = Color(100, 100, 100)
let padOutlineColor = Color(160, 150, 130)

/// Trail-mark colors precomputed per remaining-life tick, so the trail loop
/// (up to MaxTrailMarks per frame) allocates nothing.
let private trailColors =
    Array.init (int TrailMarkLifetime + 1) (fun life ->
        let a = int (float life / float (int TrailMarkLifetime) * 180.0) + 40
        Color(255, 255, 255, min 220 a))

// ─── Font / Text Helpers ────────────────────────────────────────────────
// Canvas always has a monospace font available, so (unlike the FontStashSharp
// build) there is no "font missing" path. Sizes use the same numeric values as
// the original GDI+ point sizes.

let private fontStr (size: float) = $"{size}px 'Roboto Mono', 'Consolas', monospace"

/// Measure the rendered width of a string at a given size.
let private measureWidth (ctx: obj) (size: float) (text: string) =
    setFontJs ctx (fontStr size)
    measureWidthJs ctx text

/// Draw text at a top-left position.
let private drawText (ctx: obj) (size: float) (text: string) (x: float) (y: float) (color: Color) =
    setFontJs ctx (fontStr size)
    setBaseline ctx "top"
    setFill ctx color.Css
    fillTextJs ctx text x y

/// Draw a string centered horizontally at the given Y position.
let private drawCentered (ctx: obj) (size: float) (width: float) (y: float) (text: string) (color: Color) =
    let w = measureWidth ctx size text
    drawText ctx size text ((width - w) / 2.0) y color

/// Create font sizes at a given scale factor (matches the GDI+ point sizes).
/// How much larger the touch layout draws overlay text (game over): phone
/// screens are small, and the text is read at arm's length.
[<Literal>]
let private TouchTextScale = 1.6

/// Height (CSS px) the touch layout's CONTINUE button takes at the bottom of
/// the screen (styles.css .tc-continue: 24 px from the edge, 60 px tall, plus a
/// gap); result screens keep their text above it.
[<Literal>]
let private TouchContinueReserve = 96.0

/// Height (CSS px) of the touch layout's top button row (PAUSE / MENU in
/// styles.css .tc-top: 10 px from the edge, 36 px tall, plus a gap); tables
/// start below it.
[<Literal>]
let private TouchTopBarReserve = 52.0

let private mkFonts (scale: float) =
    let big = max 8.0 (9.0 * scale)
    let med = max 7.0 (6.0 * scale)
    let small = max 6.0 (5.0 * scale)
    struct (big, med, small)

// ─── Drawing Helpers ──────────────────────────────────────────────────

/// Scale game X-coordinate to screen
let inline gameX (sx: float) (x: float<px>) = stripPx x * sx

/// Scale game Y-coordinate to screen
let inline gameY (sy: float) (y: float<px>) = stripPx y * sy

// ─── Draw Rink ────────────────────────────────────────────────────────

let drawRink (ctx: obj) sx sy (leftGoalColor: Color) (rightGoalColor: Color) =
    let rinkW = OrigW * sx
    let rinkH = gameY sy FieldBottom + 4.0 * sy
    let fl, fr = gameX sx FieldLeft, gameX sx FieldRight
    let ft, fb = gameY sy FieldTop, gameY sy FieldBottom
    let gt, gb = gameY sy GoalTop, gameY sy GoalBottom
    let gd = stripPx GoalDepth * sx
    let cx = gameX sx CenterX
    let cy = gameY sy CenterY

    // Ice surface
    fillRect ctx 0.0 0.0 rinkW rinkH iceColor

    // Board outline
    drawRect ctx fl ft (fr - fl) (fb - ft) 3.0 boardColor

    // Goal nets
    let drawGoalNet x (color: Color) =
        let netColor = Color(color.R, color.G, color.B, 60)
        fillRect ctx x gt gd (gb - gt) netColor
        drawRect ctx x gt gd (gb - gt) 2.0 color

    drawGoalNet (fl - gd) leftGoalColor
    drawGoalNet fr rightGoalColor

    // Center line + circle
    drawLine ctx cx ft cx fb 1.5 lineColor
    let circR = 20.0 * sx
    drawEllipse ctx cx cy circR circR 1.5 lineColor iceColor

    // Center dot
    fillEllipse ctx cx cy 3.0 3.0 lineColor

    // Blue lines (1/3 and 2/3 of field width)
    let fieldW = stripPx FieldRight - stripPx FieldLeft
    let bl1 = gameX sx (FieldLeft + fieldW / 3.0 * 1.0<px>)
    let bl2 = gameX sx (FieldLeft + fieldW / 3.0 * 2.0<px>)
    drawLine ctx bl1 ft bl1 fb 2.0 blueLineColor
    drawLine ctx bl2 ft bl2 fb 2.0 blueLineColor

    // Goal lines (red dashed)
    let glx = gameX sx GoalLeftX
    let grx = gameX sx GoalRightX
    drawDashedLine ctx glx ft glx fb 1.0 4.0 lineColor
    drawDashedLine ctx grx ft grx fb 1.0 4.0 lineColor

// ─── Draw Retro Hockey Player ──────────────────────────────────────────

let drawRetroPlayer (ctx: obj) sx sy (ent: Entity) (jerseyColor: Color) (helmetColor: Color) isActive (stickAnim: int) isGoalie (gameTick: int) =
    let screenX = gameX sx ent.X
    let screenY = gameY sy ent.Y
    let u = 0.85 * sx
    let uy = 0.85 * sy

    // ─── Rotation: face direction of DirX/DirY (matches original behavior) ───
    let angleRad =
        if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
            System.Math.Atan2(ent.DirX, -ent.DirY)
        else
            0.0

    // Rotate the whole sprite around the entity's screen position.
    saveCtx ctx
    translateCtx ctx screenX screenY
    rotateCtx ctx angleRad
    translateCtx ctx -screenX -screenY

    // Draw body at origin-relative coordinates (px, py = screenX, screenY)
    let px = screenX
    let py = screenY

    // Skating leg animation — slow oscillation to look like skating, not running
    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
    let legOffset =
        if speedSq > 16.0 then
            sin (float gameTick * 0.08) * 1.2 * uy * 0.3
        else
            0.0

    // ─── Helmet (head)
    fillRect ctx (px - 1.5 * u) (py - 5.5 * uy) (3.0 * u) (2.0 * uy) helmetColor

    // Face area (skin visible below helmet)
    fillRect ctx (px - 1.0 * u) (py - 3.5 * uy) (2.0 * u) (1.0 * uy) skinColor

    // Goalie face mask
    if isGoalie then
        let maskX = px + 0.3 * u
        fillRect ctx maskX (py - 4.5 * uy) (1.2 * u) (1.5 * uy) goalieMaskColor
        let cagePen = cageColor
        let cageW = max 0.5 (0.3 * u)
        let cx0 = maskX + 0.3 * u
        let cx1 = maskX + 0.9 * u
        drawLine ctx cx0 (py - 4.5 * uy) cx0 (py - 3.0 * uy) cageW cagePen
        drawLine ctx cx1 (py - 4.5 * uy) cx1 (py - 3.0 * uy) cageW cagePen

    // ─── Jersey (body)
    // Shoulders
    fillRect ctx (px - 3.5 * u) (py - 2.5 * uy) (7.0 * u) (1.5 * uy) jerseyColor
    // Torso
    fillRect ctx (px - 3.0 * u) (py - 1.0 * uy) (6.0 * u) (2.5 * uy) jerseyColor
    // Jersey stripe
    fillRect ctx (px - 3.0 * u) (py - 0.5 * uy) (6.0 * u) (0.6 * uy) jerseyStripeColor

    // ─── Arms / Gloves
    // Left arm
    fillRect ctx (px - 4.0 * u) (py - 2.0 * uy) (1.2 * u) (2.0 * uy) jerseyColor
    fillRect ctx (px - 4.0 * u) (py + 0.0 * uy) (1.2 * u) (0.8 * uy) gloveColor
    // Right arm
    fillRect ctx (px + 2.8 * u) (py - 2.0 * uy) (1.2 * u) (2.0 * uy) jerseyColor
    fillRect ctx (px + 2.8 * u) (py + 0.0 * uy) (1.2 * u) (0.8 * uy) gloveColor

    // ─── Trousers / Goalie pads
    if isGoalie then
        // Hips
        fillRect ctx (px - 3.5 * u) (py + 1.5 * uy) (7.0 * u) (1.2 * uy) goaliePadColor
        // Leg pads
        fillRect ctx (px - 3.5 * u) (py + 2.7 * uy) (3.0 * u) (2.0 * uy) goaliePadColor
        fillRect ctx (px + 0.5 * u) (py + 2.7 * uy) (3.0 * u) (2.0 * uy) goaliePadColor
        // Pad outlines
        let outlineColor = padOutlineColor
        let outlineW = max 1.0 (0.4 * u)
        drawRect ctx (px - 3.5 * u) (py + 2.7 * uy) (3.0 * u) (2.0 * uy) outlineW outlineColor
        drawRect ctx (px + 0.5 * u) (py + 2.7 * uy) (3.0 * u) (2.0 * uy) outlineW outlineColor
    else
        // Hips
        fillRect ctx (px - 3.0 * u) (py + 1.5 * uy) (6.0 * u) (1.2 * uy) trouserColor
        // Left leg (animated)
        fillRect ctx (px - 2.5 * u) (py + 2.7 * uy + legOffset) (2.2 * u) (1.0 * uy) trouserColor
        // Right leg (animated opposite)
        fillRect ctx (px + 0.3 * u) (py + 2.7 * uy - legOffset) (2.2 * u) (1.0 * uy) trouserColor
        // Socks
        fillRect ctx (px - 2.2 * u) (py + 3.5 * uy + legOffset) (1.8 * u) (0.5 * uy) sockColor
        fillRect ctx (px + 0.4 * u) (py + 3.5 * uy - legOffset) (1.8 * u) (0.5 * uy) sockColor

    // ─── Skate blades
    let skateW = max 1.0 (0.5 * u)

    if isGoalie then
        let skateY = py + 4.8 * uy
        drawLine ctx (px - 2.0 * u) skateY (px - 0.5 * u) skateY skateW skateColor
        drawLine ctx (px + 0.5 * u) skateY (px + 2.0 * u) skateY skateW skateColor
    else
        let skateYL = py + 4.0 * uy + legOffset
        let skateYR = py + 4.0 * uy - legOffset
        drawLine ctx (px - 2.0 * u) skateYL (px - 0.3 * u) skateYL skateW skateColor
        drawLine ctx (px + 0.3 * u) skateYR (px + 2.0 * u) skateYR skateW skateColor

    // ─── Stick
    let shaftWidth = max 1.5 (1.2 * u)
    let wobble =
        if stickAnim > 0 then
            sin (float stickAnim * 1.5) * 2.5 * u
        else
            0.0

    // Stick always points forward (faceDir=1) since the whole sprite rotates
    let faceDir = 1.0

    let stickLen = 7.0 * u
    let startX = px + faceDir * 2.0 * u
    let startY = py - 0.5 * uy
    let endX = startX + faceDir * 2.5 * u
    let endY = startY - stickLen + wobble

    drawLine ctx startX startY endX endY shaftWidth stickBrown

    // Tape on handle
    let tapeFrac = 0.18
    let tapeEndX = startX + (endX - startX) * tapeFrac
    let tapeEndY = startY + (endY - startY) * tapeFrac
    drawLine ctx startX startY tapeEndX tapeEndY (shaftWidth + 0.5) stickTape

    // Blade
    let bladeLen = 2.5 * u
    let bladeW = max 2.0 (1.4 * u)
    let bladeEndX = endX + faceDir * bladeLen
    let bladeEndY = endY - 0.8 * uy
    drawLine ctx endX endY bladeEndX bladeEndY bladeW stickBrown

    // End the rotated transform, back to screen space
    restoreCtx ctx

    // ─── Active player marker (drawn in screen space, NOT rotated)
    if isActive then
        let my = screenY - 6.5 * uy
        let ms = 2.0 * sx
        let markerW = max 1.0 (1.2 * sx)
        drawLine ctx (screenX - ms) (my - ms) screenX my markerW activeMarkerColor
        drawLine ctx screenX my (screenX + ms) (my - ms) markerW activeMarkerColor

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (ctx: obj) sx sy (puck: Entity) (animFrame: int) =
    let px = gameX sx puck.X
    let py = gameY sy puck.Y
    let r = 2.5 * sx

    fillEllipse ctx px py r r puckColor

    // Spinning highlight: orbits the puck center once per animation cycle
    let phase = float animFrame / float (PuckAnimFrames * 2) * (2.0 * System.Math.PI)
    let hr = r * 0.4
    let orbit = r * 0.35
    let hx = px + cos phase * orbit
    let hy = py - 0.5 + sin phase * orbit
    fillEllipse ctx hx hy hr hr puckHighlight

// ─── Draw HUD ─────────────────────────────────────────────────────────

/// `edgeInset` keeps the team names and scores that far in from the left and
/// right edges (the touch layout puts its stick and SHOOT button there).
let drawHud (ctx: obj) (gs: GameState) sx sy rinkBottom width edgeInset fastHuman hardMode =
    let hudY = rinkBottom + 2.0
    let hudH = HudHeight * sy

    fillRect ctx 0.0 hudY width hudH hudBg
    let team1Color = teamColor false fastHuman hardMode gs.Team1Idx
    let team2Color = teamColor true fastHuman hardMode gs.Team2Idx
    drawLine ctx 0.0 hudY width hudY 2.0 boardColor

    let fontSize = max 12.0 (12.0 * min sx sy)
    let smallSize = fontSize * 0.75

    // Team 1 name + score (left)
    drawText ctx smallSize teamNames.[gs.Team1Idx] (edgeInset + 10.0 * sx) (hudY + 4.0) team1Color
    drawText ctx fontSize $"{gs.Team1Score}" (edgeInset + 10.0 * sx) (hudY + 4.0 + fontSize * 1.1) team1Color

    // Team 2 name + score (right)
    let t2Name = teamNames.[gs.Team2Idx]
    let t2W = measureWidth ctx smallSize t2Name
    drawText ctx smallSize t2Name (width - t2W - edgeInset - 10.0 * sx) (hudY + 4.0) team2Color
    let s2Str = $"{gs.Team2Score}"
    let s2W = measureWidth ctx fontSize s2Str
    drawText ctx fontSize s2Str (width - s2W - edgeInset - 10.0 * sx) (hudY + 4.0 + fontSize * 1.1) team2Color

    // Clock (center) — counts DOWN to the period end. Pad the seconds
    // manually — Fable does not support ":D2" in interpolated strings.
    let secs = max 0 (int gs.PeriodLength - int gs.ClockSeconds)
    let secPart = secs % 60
    let secStr = if secPart < 10 then $"0{secPart}" else $"{secPart}"
    let clockStr = $"{secs / 60}:{secStr}"
    drawCentered ctx fontSize width (hudY + 4.0) clockStr hudText

    // Period info ("FINAL RESULT" once the match is over)
    let periodStr =
        if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then "FINAL RESULT"
        elif gs.NumPeriods > 1 then $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
        else ""

    if periodStr <> "" then
        drawCentered ctx smallSize width (hudY + 4.0 + fontSize * 1.1) periodStr hudText

// ─── Pause Overlay ────────────────────────────────────────────────────

let drawPauseOverlay (ctx: obj) (width: float) (height: float) =
    fillRect ctx 0.0 0.0 width height (Color(0, 0, 0, 140))
    let scale = min (width / OrigW) (height / OrigH)
    drawCentered ctx (max 10.0 (14.0 * scale)) width (height * 0.42) "PAUSED" goalFlashColor
    drawCentered ctx (max 7.0 (6.0 * scale)) width (height * 0.42 + 40.0 * scale) "Press P to continue" grayColor

// ─── Period Start Banner ──────────────────────────────────────────────

let drawPeriodFlash (ctx: obj) (gs: GameState) (width: float) (height: float) =
    if gs.PeriodFlashTimer > 0<tick> && gs.GoalFlashTimer <= 0<tick> then
        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0 (20.0 * scale)
        let banner = $"PERIOD {gs.CurrentPeriod + 1}"
        let w = measureWidth ctx fontSize banner
        let tx = (width - w) / 2.0
        let ty = height / 2.0 - 20.0
        drawText ctx fontSize banner (tx + 2.0) (ty + 2.0) (Color(0, 0, 0, 180))
        drawText ctx fontSize banner tx ty goalFlashColor

// ─── Goal Flash Overlay ───────────────────────────────────────────────

let drawGoalFlash (ctx: obj) (gs: GameState) width height =
    if gs.GoalFlashTimer > 0<tick> then
        let alpha = 60 * int gs.GoalFlashTimer / 90
        let overlayColor = Color(goalFlashColor.R, goalFlashColor.G, goalFlashColor.B, alpha)
        fillRect ctx 0.0 0.0 width height overlayColor

        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0 (20.0 * scale)

        let scorerName =
            match gs.GoalScoredBy with
            | Team1Scored -> teamNames.[gs.Team1Idx]
            | Team2Scored -> teamNames.[gs.Team2Idx]
            | NoGoal -> ""

        let goalStr = $"GOAL! {scorerName}"
        let strW = measureWidth ctx fontSize goalStr
        let tx = (width - strW) / 2.0
        let ty = (height - fontSize) / 2.0 - 20.0

        drawText ctx fontSize goalStr (tx + 2.0) (ty + 2.0) (Color(0, 0, 0, 180))
        drawText ctx fontSize goalStr tx ty goalFlashColor

        let scoreStr = $"{gs.Team1Score} - {gs.Team2Score}"
        drawCentered ctx (fontSize * 0.7) width (ty + fontSize + 4.0) scoreStr Color.White

// ─── Game Over Screen ─────────────────────────────────────────────────

/// `touch`: the touch layout, whose small phone screens get the text larger
/// and its CONTINUE button instead of the "Press SPACE" line.
let drawGameOver (ctx: obj) (gs: GameState) width height leagueMode touch =
    fillRect ctx 0.0 0.0 width height (Color(0, 0, 0, 160))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts (if touch then scale * TouchTextScale else scale)
    // Shrink a line that would not fit the width (long team names, big text)
    let fitted size (text: string) = min size (size * width * 0.92 / measureWidth ctx size text)

    drawCentered ctx bigSize width (height * 0.25) "GAME OVER" goalFlashColor

    let scoreStr =
        $"{teamNames.[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames.[gs.Team2Idx]}"

    drawCentered ctx (fitted medSize scoreStr) width (height * 0.40) scoreStr Color.White

    let winner =
        match sign (compare gs.Team1Score gs.Team2Score) with
        | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
        | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
        | _ -> "IT'S A TIE!"

    drawCentered ctx (fitted medSize winner) width (height * 0.52) winner goalFlashColor

    let instrStr =
        if leagueMode then
            "Press SPACE for standings"
        else
            "Press SPACE for main menu"

    if not touch then
        drawCentered ctx smallSize width (height * 0.72) instrStr (Color(180, 180, 180))

// ─── League Matchup Screen ────────────────────────────────────────────

/// `touch`: the touch layout, whose text is larger and whose CONTINUE button
/// replaces the "Press SPACE" line.
let drawLeagueMatchup (ctx: obj) width height roundNum totalRounds (team1Idx: int) (team2Idx: int) fastHuman hardMode touch =
    fillRect ctx 0.0 0.0 width height (Color(10, 10, 30))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts (if touch then scale * TouchTextScale else scale)

    drawCentered ctx bigSize width (height * 0.12) "LEAGUE MODE" goalFlashColor
    drawCentered ctx medSize width (height * 0.28) $"ROUND {roundNum} of {totalRounds}" Color.White
    // Team name in its colour, then its skating speed dimmed as in the menu,
    // centred together; the smaller hint sits on the name's baseline
    let drawTeam (y: float) (teamIdx: int) (color: Color) =
        let name = teamNames.[teamIdx]
        let hint = $"  (speed {displayedTeamSpeed fastHuman hardMode teamIdx})"
        let nameW = measureWidth ctx medSize name
        let x = (width - nameW - measureWidth ctx smallSize hint) / 2.0
        drawText ctx medSize name x y color
        drawText ctx smallSize hint (x + nameW) (y + (medSize - smallSize) * 0.85) speedHintColor

    drawTeam (height * 0.42) team1Idx (teamColor false fastHuman hardMode team1Idx)
    drawCentered ctx smallSize width (height * 0.52) "vs" grayColor
    drawTeam (height * 0.60) team2Idx (teamColor true fastHuman hardMode team2Idx)
    if not touch then
        drawCentered ctx smallSize width (height * 0.80) "Press SPACE to start match" grayColor

// ─── League Standings Screen ──────────────────────────────────────────

/// `touch`: the touch layout — text as large as fits above its CONTINUE
/// button (up to TouchTextScale), the winner line right under the table, and
/// no "Press SPACE" line.
let drawLeagueStandings (ctx: obj) width height (standings: (int * TeamStats) array) isFinal humanTeam touch =
    fillRect ctx 0.0 0.0 width height (Color(10, 10, 30))

    let baseScale = min (width / OrigW) (height / OrigH)
    // Layout at content scale `s`. The desktop keeps its original spacing; the
    // touch layout puts the title at the top edge and the headers right under
    // it, packs the rows a little tighter and draws the winner line at the
    // title's size under the table, so the text can grow on a phone.
    let bigAt (s: float) = let struct (big, _, _) = mkFonts s in big
    let titleYAt (_: float) = if touch then 8.0 else height * 0.04
    let headerYAt (s: float) =
        if touch then max (titleYAt s + bigAt s * 1.3) TouchTopBarReserve else height * 0.14
    let rowHAt (s: float) = let struct (_, _, small) = mkFonts s in small * (if touch then 1.5 else 1.7)
    let winnerSizeAt (s: float) = if touch then bigAt s else max 14.0 (14.0 * s)
    // Bottom of the table, plus the winner line under it in the touch layout
    let contentBottom (s: float) =
        let struct (_, med, _) = mkFonts s
        let tableBottom = headerYAt s + med * 1.4 + 4.0 + float standings.Length * rowHAt s
        if isFinal then tableBottom + winnerSizeAt s * 1.4 else tableBottom
    let scale =
        if not touch then baseScale
        else
            // Largest step up to TouchTextScale whose content clears CONTINUE
            [ TouchTextScale .. -0.05 .. 1.0 ]
            |> List.tryFind (fun k -> contentBottom (baseScale * k) <= height - TouchContinueReserve)
            |> Option.defaultValue 1.0
            |> (*) baseScale
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    let title = if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS"
    drawCentered ctx bigSize width (titleYAt scale) title goalFlashColor

    // Column headers
    let tableX = width * 0.04
    let headerY = headerYAt scale
    let rowH = rowHAt scale
    let nameW = width * 0.38

    let colPositions =
        [| nameW
           nameW + width * 0.07
           nameW + width * 0.14
           nameW + width * 0.21
           nameW + width * 0.30
           nameW + width * 0.40
           nameW + width * 0.50 |]

    [| "TEAM"; "W"; "L"; "D"; "PTS"; "GF"; "GA" |]
    |> Array.iteri (fun i h ->
        let hx = if i = 0 then tableX else tableX + colPositions.[i - 1]
        drawText ctx medSize h hx headerY grayColor)

    // Separator
    let sepY = headerY + medSize * 1.4
    drawLine ctx tableX sepY (width - tableX) sepY 1.0 (Color(60, 80, 120))

    // Rows
    let dataY = sepY + 4.0

    standings
    |> Array.iteri (fun rank (teamIdx, stats) ->
        let ry = dataY + float rank * rowH
        let isHuman = (teamIdx = humanTeam)

        if isHuman then
            fillRect ctx (tableX - 2.0) (ry - 1.0) (width - tableX * 2.0 + 4.0) rowH (Color(30, 50, 80))

        let textColor = if isHuman then goalFlashColor else Color.White
        drawText ctx smallSize $"{rank + 1}." tableX ry grayColor
        drawText ctx smallSize teamNames.[teamIdx] (tableX + smallSize * 2.5) ry textColor

        [| $"{stats.Wins}"
           $"{stats.Losses}"
           $"{stats.Draws}"
           $"{stats.Points}"
           $"{stats.GoalsFor}"
           $"{stats.GoalsAgainst}" |]
        |> Array.iteri (fun i v ->
            drawText ctx smallSize v (tableX + colPositions.[i]) ry Color.White))

    // Winner announcement
    if isFinal && standings.Length > 0 then
        let winnerIdx, winnerStats = standings.[0]

        let winnerStr =
            $"{teamNames.[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"

        // Touch: right under the table, clear of the CONTINUE button
        let winnerY =
            if touch then dataY + float standings.Length * rowH + winnerSizeAt scale * 0.4
            else height * 0.88
        drawCentered ctx (winnerSizeAt scale) width winnerY winnerStr goalFlashColor

    let instrStr =
        if isFinal then
            "Press SPACE to return to menu"
        else
            "Press SPACE to continue"

    if not touch then
        drawCentered ctx smallSize width (height * 0.94) instrStr grayColor

// ─── Menu Screen ──────────────────────────────────────────────────────

/// Plain menu background: the menu itself is an HTML overlay (index.html).
let drawMenuBackdrop (ctx: obj) width height =
    fillRect ctx 0.0 0.0 width height (Color(10, 10, 30))

// ─── Main Render ──────────────────────────────────────────────────────

// The 320x200 layout is a CGA-style design with non-square pixels: on a
// 4:3 monitor its pixel aspect ratio is 1.2. Preserve the rink's shape by
// using one uniform content scale `s` (sx = 1.2 * s, sy = s), sized so the
// rink plus the HUD fill as much of the canvas as possible.
let private pixelAspect = 1.2
let private rinkGameH = stripPx FieldBottom + 4.0

let private contentScale (w: float) (h: float) =
    min (w / (OrigW * pixelAspect)) ((h - 2.0) / (rinkGameH + HudHeight))

/// Height of the HUD strip at the bottom of a `width` x `height` canvas.
let hudHeight (width: float) (height: float) = HudHeight * contentScale width height

/// `hudInset` is passed to drawHud: how far the team names and scores stay in
/// from the side edges. `fastHuman` and `hardMode` set the speed class colours.
let renderFrame (ctx: obj) (gs: GameState) (width: float) (height: float) leagueMode hudInset fastHuman hardMode =
    let w = width
    let h = height

    let par = pixelAspect
    let s = contentScale w h
    let sx = s * par
    let sy = s

    fillRect ctx 0.0 0.0 w h bgColor

    // HUD is anchored to the bottom edge; the rink is centered in the space
    // above it (horizontally, and vertically on very wide canvases).
    let hudH = HudHeight * sy
    let offX = (w - OrigW * sx) / 2.0
    let offY = max 0.0 ((h - hudH - 2.0 - rinkGameH * sy) / 2.0)

    saveCtx ctx
    translateCtx ctx offX offY

    let team1Color = teamColor false fastHuman hardMode gs.Team1Idx
    let team2Color = teamColor true fastHuman hardMode gs.Team2Idx
    drawRink ctx sx sy team1Color team2Color

    // Ice trail marks (drawn on ice, under players and puck)
    for i in 0 .. gs.TrailMarkCount - 1 do
        let mark = gs.TrailMarks.[i]

        if mark.Life > 0<tick> then
            let trailColor = trailColors.[int mark.Life]
            let mx = gameX sx mark.X
            let my = gameY sy mark.Y
            let r = 1.2 * sx
            fillEllipse ctx mx my r r trailColor

    let ppt = gs.PlayersPerTeam
    let t2s = gs.Team2Start

    // Helmet colors: human team = gold, CPU = black
    let t1Helmet = if gs.Team1Idx = 0 then helmetGold else helmetBlack
    let t2Helmet = if gs.Team2Idx = 0 then helmetGold else helmetBlack

    // Puck drawn UNDER players so skaters appear on top of it
    drawPuck ctx sx sy gs.Entities.[gs.PuckIdx] gs.PuckAnimFrame

    // Team 1 players
    for i in 0 .. ppt - 1 do
        let isGoalie = gs.FivePlayerMode && i = 0

        drawRetroPlayer
            ctx
            sx
            sy
            gs.Entities.[i]
            team1Color
            t1Helmet
            (i = gs.ActivePlayer1)
            gs.StickAnimTimers.[i]
            isGoalie
            (int gs.GameTick)

    // Team 2 players
    for i in 0 .. ppt - 1 do
        let ei = t2s + i
        let isGoalie = gs.FivePlayerMode && i = 0

        drawRetroPlayer
            ctx
            sx
            sy
            gs.Entities.[ei]
            team2Color
            t2Helmet
            (ei = gs.ActivePlayer2)
            gs.StickAnimTimers.[ei]
            isGoalie
            (int gs.GameTick)

    restoreCtx ctx

    // HUD (drawn in canvas space, spanning the full width at the bottom edge)
    let rinkBottom = h - hudH - 2.0
    drawHud ctx gs sx sy rinkBottom w hudInset fastHuman hardMode

    // Overlays
    drawGoalFlash ctx gs w h
    drawPeriodFlash ctx gs w h

    if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
        // Only the touch layout insets the HUD texts, so this means "touch"
        drawGameOver ctx gs w h leagueMode (hudInset > 0.0)
