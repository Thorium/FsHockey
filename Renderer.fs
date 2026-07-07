/// THE FS HOCKEY LEAGUE — Renderer
/// GDI+ drawing: ice rink, players, puck, HUD, goal flash, game-over
module HockeyDemo.Renderer

open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Drawing2D
open HockeyDemo.Physics
open HockeyDemo.Game

// ─── Scale / Layout ────────────────────────────────────────────────────

[<Literal>]
let OrigW = 320.0f

[<Literal>]
let OrigH = 200.0f

[<Literal>]
let HudHeight = 48.0f

// ─── Colors (CGA-inspired) ────────────────────────────────────────────

let iceColor = Color.FromArgb(200, 220, 240)
let boardColor = Color.FromArgb(60, 80, 120)
let lineColor = Color.FromArgb(180, 40, 40)
let blueLineColor = Color.FromArgb(40, 80, 180)
let team1Color = Color.FromArgb(220, 60, 60)
let team1Light = Color.FromArgb(255, 120, 120)
let team2Color = Color.FromArgb(60, 100, 220)
let team2Light = Color.FromArgb(120, 160, 255)
let puckColor = Color.FromArgb(20, 20, 20)
let puckHighlight = Color.FromArgb(60, 60, 60)
let hudBg = Color.FromArgb(20, 20, 40)
let hudText = Color.FromArgb(220, 220, 220)
let goalFlashColor = Color.FromArgb(255, 255, 80)
let activeMarker = Color.White
let stickBrown = Color.FromArgb(139, 90, 43)
let stickTape = Color.FromArgb(240, 240, 240)
let helmetBlack = Color.FromArgb(30, 30, 30)
let helmetGold = Color.FromArgb(200, 180, 40)
let trouserColor = Color.FromArgb(30, 30, 30)
let skateColor = Color.FromArgb(80, 80, 80)
let goaliePadColor = Color.FromArgb(230, 220, 200)
let goalieMaskColor = Color.FromArgb(220, 220, 220)

// ─── Cached GDI+ objects ──────────────────────────────────────────────
// Brushes, pens and fonts used to be allocated (and disposed) per draw call —
// hundreds of GDI handles per frame at 30 FPS. Cache them instead; the number
// of distinct colors/sizes is small and bounded, and entries live for the
// process lifetime. The returned objects are shared: never dispose them.

let private brushCache = Dictionary<Color, Brush>()

/// Cached solid brush for a color.
let private solidBrush (c: Color) : Brush =
    match brushCache.TryGetValue c with
    | true, b -> b
    | _ ->
        let b = new SolidBrush(c) :> Brush
        brushCache.[c] <- b
        b

let private penCache = Dictionary<Color, Pen>()

/// Cached pen for a color. Width (and dash style) are per-use state set on
/// every fetch, so fetch a pen only right before drawing with it.
let private penFor (c: Color) (width: float32) : Pen =
    let p =
        match penCache.TryGetValue c with
        | true, p -> p
        | _ ->
            let p = new Pen(c, width)
            penCache.[c] <- p
            p

    p.Width <- width
    p.DashStyle <- DashStyle.Solid
    p

let private fontCache = Dictionary<struct (float32 * FontStyle), Font>()

/// Cached Consolas font. Sizes vary continuously while the window is being
/// resized, so the cache is emptied if it ever grows past a sane bound.
let private fontFor (size: float32) (style: FontStyle) : Font =
    match fontCache.TryGetValue(struct (size, style)) with
    | true, f -> f
    | _ ->
        if fontCache.Count > 64 then
            for f in fontCache.Values do
                f.Dispose()

            fontCache.Clear()

        let f = new Font("Consolas", size, style)
        fontCache.[struct (size, style)] <- f
        f

// ─── Drawing Helpers ──────────────────────────────────────────────────

/// Scale game X-coordinate to screen
let inline gameX (sx: float32) (x: float<px>) = float32 (stripPx x) * sx

/// Scale game Y-coordinate to screen
let inline gameY (sy: float32) (y: float<px>) = float32 (stripPx y) * sy

/// Draw a string centered horizontally at the given Y position
let private drawCentered (g: Graphics) (font: Font) (brush: Brush) width y (text: string) =
    let sz = g.MeasureString(text, font)
    g.DrawString(text, font, brush, (width - sz.Width) / 2.0f, y)

/// Create fonts at a given scale factor
let private mkFonts (scale: float32) =
    let big = max 8.0f (9.0f * scale)
    let med = max 7.0f (6.0f * scale)
    let small = max 6.0f (5.0f * scale)
    struct (big, med, small)

// ─── Draw Rink ────────────────────────────────────────────────────────

let drawRink (g: Graphics) sx sy leftGoalColor rightGoalColor =
    let rinkW = OrigW * sx
    let rinkH = gameY sy FieldBottom + 4.0f * sy
    let fl, fr = gameX sx FieldLeft, gameX sx FieldRight
    let ft, fb = gameY sy FieldTop, gameY sy FieldBottom
    let gt, gb = gameY sy GoalTop, gameY sy GoalBottom
    let gd = float32 (stripPx GoalDepth) * sx
    let cx = gameX sx CenterX
    let cy = gameY sy CenterY

    // Ice surface
    g.FillRectangle(solidBrush iceColor, 0.0f, 0.0f, rinkW, rinkH)

    // Board outline
    g.DrawRectangle(penFor boardColor 3.0f, fl, ft, fr - fl, fb - ft)

    // Goal nets
    let drawGoalNet x color =
        g.FillRectangle(solidBrush (Color.FromArgb(60, color)), x, gt, gd, gb - gt)
        g.DrawRectangle(penFor color 2.0f, x, gt, gd, gb - gt)

    drawGoalNet (fl - gd) leftGoalColor
    drawGoalNet fr rightGoalColor

    // Center line + circle
    let centerPen = penFor lineColor 1.5f
    g.DrawLine(centerPen, cx, ft, cx, fb)
    let circR = 20.0f * sx
    g.DrawEllipse(centerPen, cx - circR, cy - circR, circR * 2.0f, circR * 2.0f)

    // Center dot
    g.FillEllipse(solidBrush lineColor, cx - 3.0f, cy - 3.0f, 6.0f, 6.0f)

    // Blue lines (1/3 and 2/3 of field width)
    let bluePen = penFor blueLineColor 2.0f
    let fieldW = stripPx FieldRight - stripPx FieldLeft
    let bl1 = gameX sx (FieldLeft + fieldW / 3.0 * 1.0<px>)
    let bl2 = gameX sx (FieldLeft + fieldW / 3.0 * 2.0<px>)
    g.DrawLine(bluePen, bl1, ft, bl1, fb)
    g.DrawLine(bluePen, bl2, ft, bl2, fb)

    // Goal lines (red dashed)
    let goalLinePen = penFor lineColor 1.0f
    goalLinePen.DashStyle <- DashStyle.Dash
    let glx = gameX sx GoalLeftX
    let grx = gameX sx GoalRightX
    g.DrawLine(goalLinePen, glx, ft, glx, fb)
    g.DrawLine(goalLinePen, grx, ft, grx, fb)

// ─── Draw Retro Hockey Player ──────────────────────────────────────────
// Wayne Gretzky Hockey 2 inspired pixel art — scaled rectangles.
// isGoalie: distinct goalie appearance (wider pads, face mask, leg pads)

let skinColor = Color.FromArgb(230, 195, 160)
let gloveColor = Color.FromArgb(60, 60, 60)
let sockColor = Color.FromArgb(200, 200, 210)

let drawRetroPlayer (g: Graphics) sx sy (ent: Entity) jerseyColor helmetColor isActive (stickAnim: int) isGoalie (gameTick: int) =
    let px = gameX sx ent.X
    let py = gameY sy ent.Y
    // Smaller unit size — more compact figures
    let u = 0.85f * sx
    let uy = 0.85f * sy

    // ─── Rotation: face direction of DirX/DirY ───
    let angleDeg =
        if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
            float32 (System.Math.Atan2(float ent.DirX, -(float ent.DirY))) * (180.0f / float32 System.Math.PI)
        else
            0.0f

    use savedTransform = g.Transform
    g.TranslateTransform(px, py)
    g.RotateTransform(angleDeg)
    let px = 0.0f
    let py = 0.0f

    let faceDir = 1.0f

    // Skating leg animation: oscillate based on speed
    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
    let legOffset =
        if speedSq > 16.0 then
            sin (float32 gameTick * 0.5f) * 1.2f * uy * 0.3f
        else
            0.0f

    // ─── Helmet (head) ─────────────
    g.FillRectangle(solidBrush helmetColor, px - 1.5f * u, py - 5.5f * uy, 3.0f * u, 2.0f * uy)

    // Face area (skin visible below helmet)
    g.FillRectangle(solidBrush skinColor, px - 1.0f * u, py - 3.5f * uy, 2.0f * u, 1.0f * uy)

    // ─── Goalie: face mask (cage) ──
    if isGoalie then
        let maskX = px + 0.3f * u
        g.FillRectangle(solidBrush goalieMaskColor, maskX, py - 4.5f * uy, 1.2f * u, 1.5f * uy)
        let cagePen = penFor (Color.FromArgb(100, 100, 100)) (max 0.5f (0.3f * u))
        let cx0 = maskX + 0.3f * u
        let cx1 = maskX + 0.9f * u
        g.DrawLine(cagePen, cx0, py - 4.5f * uy, cx0, py - 3.0f * uy)
        g.DrawLine(cagePen, cx1, py - 4.5f * uy, cx1, py - 3.0f * uy)

    // ─── Jersey (body) ─────────────
    let jerseyBrush = solidBrush jerseyColor
    // Shoulders
    g.FillRectangle(jerseyBrush, px - 3.5f * u, py - 2.5f * uy, 7.0f * u, 1.5f * uy)
    // Torso
    g.FillRectangle(jerseyBrush, px - 3.0f * u, py - 1.0f * uy, 6.0f * u, 2.5f * uy)
    // Jersey number stripe (white stripe across chest)
    g.FillRectangle(solidBrush (Color.FromArgb(80, 255, 255, 255)), px - 3.0f * u, py - 0.5f * uy, 6.0f * u, 0.6f * uy)

    // ─── Arms / Gloves ─────────────
    let gloveBrush = solidBrush gloveColor
    // Left arm (extends slightly out from shoulder)
    g.FillRectangle(jerseyBrush, px - 4.0f * u, py - 2.0f * uy, 1.2f * u, 2.0f * uy)
    g.FillRectangle(gloveBrush, px - 4.0f * u, py + 0.0f * uy, 1.2f * u, 0.8f * uy)
    // Right arm
    g.FillRectangle(jerseyBrush, px + 2.8f * u, py - 2.0f * uy, 1.2f * u, 2.0f * uy)
    g.FillRectangle(gloveBrush, px + 2.8f * u, py + 0.0f * uy, 1.2f * u, 0.8f * uy)

    // ─── Trousers / Goalie pads ────
    if isGoalie then
        let padBrush = solidBrush goaliePadColor
        // Hips
        g.FillRectangle(padBrush, px - 3.5f * u, py + 1.5f * uy, 7.0f * u, 1.2f * uy)
        // Leg pads
        g.FillRectangle(padBrush, px - 3.5f * u, py + 2.7f * uy, 3.0f * u, 2.0f * uy)
        g.FillRectangle(padBrush, px + 0.5f * u, py + 2.7f * uy, 3.0f * u, 2.0f * uy)
        // Pad outlines
        let padPen = penFor (Color.FromArgb(160, 150, 130)) (max 1.0f (0.4f * u))
        g.DrawRectangle(padPen, px - 3.5f * u, py + 2.7f * uy, 3.0f * u, 2.0f * uy)
        g.DrawRectangle(padPen, px + 0.5f * u, py + 2.7f * uy, 3.0f * u, 2.0f * uy)
    else
        let trouserBrush = solidBrush trouserColor
        // Hips
        g.FillRectangle(trouserBrush, px - 3.0f * u, py + 1.5f * uy, 6.0f * u, 1.2f * uy)
        // Left leg (animated)
        g.FillRectangle(trouserBrush, px - 2.5f * u, py + 2.7f * uy + legOffset, 2.2f * u, 1.0f * uy)
        // Right leg (animated opposite)
        g.FillRectangle(trouserBrush, px + 0.3f * u, py + 2.7f * uy - legOffset, 2.2f * u, 1.0f * uy)
        // Socks (between trousers and skates)
        let sockBrush = solidBrush sockColor
        g.FillRectangle(sockBrush, px - 2.2f * u, py + 3.5f * uy + legOffset, 1.8f * u, 0.5f * uy)
        g.FillRectangle(sockBrush, px + 0.4f * u, py + 3.5f * uy - legOffset, 1.8f * u, 0.5f * uy)

    // ─── Skate blades ──────────────
    let skatePen = penFor skateColor (max 1.0f (0.5f * u))
    if isGoalie then
        let skateY = py + 4.8f * uy
        g.DrawLine(skatePen, px - 2.0f * u, skateY, px - 0.5f * u, skateY)
        g.DrawLine(skatePen, px + 0.5f * u, skateY, px + 2.0f * u, skateY)
    else
        let skateYL = py + 4.0f * uy + legOffset
        let skateYR = py + 4.0f * uy - legOffset
        g.DrawLine(skatePen, px - 2.0f * u, skateYL, px - 0.3f * u, skateYL)
        g.DrawLine(skatePen, px + 0.3f * u, skateYR, px + 2.0f * u, skateYR)

    // ─── Stick ─────────────────────
    let shaftWidth = max 1.5f (1.2f * u)
    let shaftPen = penFor stickBrown shaftWidth

    let wobble =
        if stickAnim > 0 then
            sin (float32 stickAnim * 1.5f) * 2.5f * u
        else
            0.0f

    let stickLen = 7.0f * u
    let startX = px + faceDir * 2.0f * u
    let startY = py - 0.5f * uy
    let endX = startX + faceDir * 2.5f * u
    let endY = startY - stickLen + wobble

    g.DrawLine(shaftPen, startX, startY, endX, endY)

    // Tape on handle
    let tapeFrac = 0.18f
    let tapeEndX = startX + (endX - startX) * tapeFrac
    let tapeEndY = startY + (endY - startY) * tapeFrac
    g.DrawLine(penFor stickTape (shaftWidth + 0.5f), startX, startY, tapeEndX, tapeEndY)

    // Blade (re-fetching the stickBrown pen adjusts the shared pen's width;
    // the shaft has already been drawn by now)
    let bladeLen = 2.5f * u
    let bladeW = max 2.0f (1.4f * u)
    let bladePen = penFor stickBrown bladeW
    let bladeEndX = endX + faceDir * bladeLen
    let bladeEndY = endY - 0.8f * uy
    g.DrawLine(bladePen, endX, endY, bladeEndX, bladeEndY)

    // Restore transform (savedTransform is disposed at function scope exit)
    g.Transform <- savedTransform

    // ─── Active player marker: small downward-pointing arrow (no circle) ──────
    if isActive then
        let px0 = gameX sx ent.X
        let py0 = gameY sy ent.Y
        let my = py0 - 6.5f * uy   // above the head
        let ms = 2.0f * sx
        let markerPen = penFor activeMarker (max 1.0f (1.2f * sx))
        // Small downward arrow / chevron
        g.DrawLine(markerPen, px0 - ms, my - ms, px0, my)
        g.DrawLine(markerPen, px0, my, px0 + ms, my - ms)

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (g: Graphics) sx sy (puck: Entity) (animFrame: int) =
    let px = gameX sx puck.X
    let py = gameY sy puck.Y
    let r = 2.5f * sx

    g.FillEllipse(solidBrush puckColor, px - r, py - r, r * 2.0f, r * 2.0f)

    // Spinning highlight: orbits the puck center once per animation cycle
    let phase = float32 animFrame / float32 (PuckAnimFrames * 2) * (2.0f * float32 System.Math.PI)
    let hlBrush = solidBrush puckHighlight
    let hr = r * 0.4f
    let orbit = r * 0.35f
    let hx = px + cos phase * orbit
    let hy = py - 0.5f + sin phase * orbit
    g.FillEllipse(hlBrush, hx - hr, hy - hr, hr * 2.0f, hr * 2.0f)

    g.DrawEllipse(penFor Color.Black 1.0f, px - r, py - r, r * 2.0f, r * 2.0f)

// ─── Draw HUD ─────────────────────────────────────────────────────────

let drawHud (g: Graphics) (gs: GameState) sx sy rinkBottom width =
    let hudY = rinkBottom + 2.0f
    let hudH = HudHeight * sy

    g.FillRectangle(solidBrush hudBg, 0.0f, hudY, width, hudH)
    g.DrawLine(penFor boardColor 2.0f, 0.0f, hudY, width, hudY)

    let fontSize = max 6.0f (5.0f * min sx sy)
    let font = fontFor fontSize FontStyle.Bold
    let smallFont = fontFor (fontSize * 0.75f) FontStyle.Regular
    let textBrush = solidBrush hudText
    let t1Brush = solidBrush team1Color
    let t2Brush = solidBrush team2Color

    // Team 1 name + score (left)
    g.DrawString(teamNames.[gs.Team1Idx], smallFont, t1Brush, 10.0f * sx, hudY + 4.0f)
    g.DrawString($"{gs.Team1Score}", font, t1Brush, 10.0f * sx, hudY + 4.0f + fontSize * 1.1f)

    // Team 2 name + score (right)
    let t2Name = teamNames.[gs.Team2Idx]
    let t2Size = g.MeasureString(t2Name, smallFont)
    g.DrawString(t2Name, smallFont, t2Brush, width - t2Size.Width - 10.0f * sx, hudY + 4.0f)
    let s2Str = $"{gs.Team2Score}"
    let s2Size = g.MeasureString(s2Str, font)
    g.DrawString(s2Str, font, t2Brush, width - s2Size.Width - 10.0f * sx, hudY + 4.0f + fontSize * 1.1f)

    // Clock (center)
    let secs = int gs.ClockSeconds
    let clockStr = $"{secs / 60}:{secs % 60:D2}"
    drawCentered g font textBrush width (hudY + 4.0f) clockStr

    // Period info
    if gs.NumPeriods > 1 then
        let periodStr = $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
        drawCentered g smallFont textBrush width (hudY + 4.0f + fontSize * 1.1f) periodStr

// ─── Goal Flash Overlay ───────────────────────────────────────────────

let drawGoalFlash (g: Graphics) (gs: GameState) width height =
    if gs.GoalFlashTimer > 0<tick> then
        let alpha = 60 * int gs.GoalFlashTimer / 90
        g.FillRectangle(solidBrush (Color.FromArgb(alpha, goalFlashColor)), 0.0f, 0.0f, width, height)

        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 8.0f (10.0f * scale)
        let font = fontFor fontSize FontStyle.Bold

        let scorerName =
            match gs.GoalScoredBy with
            | Team1Scored -> teamNames.[gs.Team1Idx]
            | Team2Scored -> teamNames.[gs.Team2Idx]
            | NoGoal -> ""

        let goalStr = $"GOAL! {scorerName}"
        let strSize = g.MeasureString(goalStr, font)
        let tx = (width - strSize.Width) / 2.0f
        let ty = (height - strSize.Height) / 2.0f - 20.0f

        g.DrawString(goalStr, font, solidBrush (Color.FromArgb(180, Color.Black)), tx + 2.0f, ty + 2.0f)
        g.DrawString(goalStr, font, solidBrush goalFlashColor, tx, ty)

        let scoreStr = $"{gs.Team1Score} - {gs.Team2Score}"
        let scoreFont = fontFor (fontSize * 0.7f) FontStyle.Bold
        drawCentered g scoreFont (solidBrush Color.White) width (ty + strSize.Height + 4.0f) scoreStr

// ─── Game Over Screen ─────────────────────────────────────────────────

let drawGameOver (g: Graphics) (gs: GameState) width height leagueMode =
    g.FillRectangle(solidBrush (Color.FromArgb(160, Color.Black)), 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    let bigFont = fontFor bigSize FontStyle.Bold
    let medFont = fontFor medSize FontStyle.Bold
    let smallFont = fontFor smallSize FontStyle.Regular
    let whiteBrush = solidBrush Color.White
    let yellowBrush = solidBrush goalFlashColor
    let grayBrush = solidBrush (Color.FromArgb(180, 180, 180))

    drawCentered g bigFont yellowBrush width (height * 0.25f) "GAME OVER"

    let scoreStr =
        $"{teamNames.[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames.[gs.Team2Idx]}"

    drawCentered g medFont whiteBrush width (height * 0.40f) scoreStr

    let winner =
        match sign (compare gs.Team1Score gs.Team2Score) with
        | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
        | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
        | _ -> "IT'S A TIE!"

    drawCentered g medFont yellowBrush width (height * 0.52f) winner

    let instrStr =
        if leagueMode then
            "Press SPACE for standings"
        else
            "Press SPACE for main menu"

    drawCentered g smallFont grayBrush width (height * 0.72f) instrStr

// ─── League Matchup Screen ────────────────────────────────────────────

let drawLeagueMatchup (g: Graphics) width height roundNum totalRounds (team1Name: string) (team2Name: string) =
    g.FillRectangle(solidBrush (Color.FromArgb(10, 10, 30)), 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    let bigFont = fontFor bigSize FontStyle.Bold
    let medFont = fontFor medSize FontStyle.Bold
    let smallFont = fontFor smallSize FontStyle.Regular
    let yellowBrush = solidBrush goalFlashColor
    let whiteBrush = solidBrush Color.White
    let grayBrush = solidBrush (Color.FromArgb(160, 160, 160))
    let t1Brush = solidBrush team1Color
    let t2Brush = solidBrush team2Color

    drawCentered g bigFont yellowBrush width (height * 0.12f) "LEAGUE MODE"
    drawCentered g medFont whiteBrush width (height * 0.28f) $"ROUND {roundNum} of {totalRounds}"
    drawCentered g medFont t1Brush width (height * 0.42f) team1Name
    drawCentered g smallFont grayBrush width (height * 0.52f) "vs"
    drawCentered g medFont t2Brush width (height * 0.60f) team2Name
    drawCentered g smallFont grayBrush width (height * 0.80f) "Press SPACE to start match"

// ─── League Standings Screen ──────────────────────────────────────────

let drawLeagueStandings (g: Graphics) width height (standings: (int * TeamStats) array) isFinal humanTeam =
    g.FillRectangle(solidBrush (Color.FromArgb(10, 10, 30)), 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    let bigFont = fontFor bigSize FontStyle.Bold
    let medFont = fontFor medSize FontStyle.Bold
    let smallFont = fontFor smallSize FontStyle.Regular
    let yellowBrush = solidBrush goalFlashColor
    let whiteBrush = solidBrush Color.White
    let grayBrush = solidBrush (Color.FromArgb(160, 160, 160))

    let title = if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS"
    drawCentered g bigFont yellowBrush width (height * 0.04f) title

    // Column headers
    let tableX = width * 0.04f
    let headerY = height * 0.14f
    let rowH = smallSize * 1.7f
    let nameW = width * 0.38f

    let colPositions =
        [| nameW
           nameW + width * 0.07f
           nameW + width * 0.14f
           nameW + width * 0.21f
           nameW + width * 0.30f
           nameW + width * 0.40f
           nameW + width * 0.50f |]

    [| "TEAM"; "W"; "L"; "D"; "PTS"; "GF"; "GA" |]
    |> Array.iteri (fun i h ->
        let hx = if i = 0 then tableX else tableX + colPositions.[i - 1]
        g.DrawString(h, medFont, grayBrush, hx, headerY))

    // Separator
    let sepY = headerY + medSize * 1.4f
    g.DrawLine(penFor (Color.FromArgb(60, 80, 120)) 1.0f, tableX, sepY, width - tableX, sepY)

    // Rows
    let dataY = sepY + 4.0f

    standings
    |> Array.iteri (fun rank (teamIdx, stats) ->
        let ry = dataY + float32 rank * rowH
        let isHuman = (teamIdx = humanTeam)

        if isHuman then
            g.FillRectangle(solidBrush (Color.FromArgb(30, 50, 80)), tableX - 2.0f, ry - 1.0f, width - tableX * 2.0f + 4.0f, rowH)

        let textBr: Brush = if isHuman then yellowBrush else whiteBrush
        g.DrawString($"{rank + 1}.", smallFont, grayBrush, tableX, ry)
        g.DrawString(teamNames.[teamIdx], smallFont, textBr, tableX + smallSize * 2.5f, ry)

        [| $"{stats.Wins}"
           $"{stats.Losses}"
           $"{stats.Draws}"
           $"{stats.Points}"
           $"{stats.GoalsFor}"
           $"{stats.GoalsAgainst}" |]
        |> Array.iteri (fun i v -> g.DrawString(v, smallFont, whiteBrush, tableX + colPositions.[i], ry)))

    // Winner announcement
    if isFinal && standings.Length > 0 then
        let winnerIdx, winnerStats = standings.[0]

        let winnerStr =
            $"{teamNames.[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"

        let winFont = fontFor (max 7.0f (7.0f * scale)) FontStyle.Bold
        drawCentered g winFont yellowBrush width (height * 0.88f) winnerStr

    let instrStr =
        if isFinal then
            "Press SPACE to return to menu"
        else
            "Press SPACE to continue"

    drawCentered g smallFont grayBrush width (height * 0.94f) instrStr

// ─── Menu Screen ──────────────────────────────────────────────────────

let drawMenu (g: Graphics) width height selectedTeam1 selectedTeam2 activeColumn fastHuman hardMode fivePlayer gamepadOn =
    g.FillRectangle(solidBrush (Color.FromArgb(10, 10, 30)), 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    let bigFont = fontFor bigSize FontStyle.Bold
    let medFont = fontFor medSize FontStyle.Regular
    let smallFont = fontFor smallSize FontStyle.Regular
    let whiteBrush = solidBrush Color.White
    let yellowBrush = solidBrush goalFlashColor
    let grayBrush = solidBrush (Color.FromArgb(140, 140, 140))
    let dimBrush = solidBrush (Color.FromArgb(105, 105, 125))

    // Title + subtitle
    drawCentered g bigFont yellowBrush width (height * 0.06f) "THE FS HOCKEY LEAGUE"

    let sub =
        "Tuomas Hietanen, 2026"

    drawCentered g smallFont grayBrush width (height * 0.06f + bigSize * 1.4f) sub

    // Two-column team selection
    let colW = width * 0.42f
    let col1X = width * 0.04f
    let col2X = width * 0.54f
    let listY = height * 0.22f

    let drawColumn colX (headerText: string) (headerColor: Color) selectedIdx isActive =
        let headerFont = fontFor medSize FontStyle.Bold
        g.DrawString(headerText, headerFont, solidBrush headerColor, colX, listY)

        if isActive then
            let boxH = medSize * 1.3f + float32 NumTeams * smallSize * 1.6f + 6.0f
            g.DrawRectangle(penFor headerColor 1.5f, colX - 3.0f, listY - 2.0f, colW, boxH)

        for i in 0 .. NumTeams - 1 do
            let ty = listY + medSize * 1.5f + float32 i * smallSize * 1.6f
            let isSelected = (i = selectedIdx)

            if isSelected then
                g.FillRectangle(solidBrush (Color.FromArgb(40, 60, 100)), colX, ty - 1.0f, colW - 6.0f, smallSize * 1.4f)

            let brush = if isSelected then yellowBrush else whiteBrush
            let prefix = if isSelected then "> " else "  "
            let label = $"{prefix}{teamNames.[i]}"
            g.DrawString(label, smallFont, brush, colX + 4.0f, ty)

            // Team skating speed, dimmed — a hint of how hard a CPU opponent
            // is, reflecting the current fast-human/hard-mode settings
            let labelW = g.MeasureString(label, smallFont).Width
            let spd = displayedTeamSpeed fastHuman hardMode i
            g.DrawString($"(speed {spd})", smallFont, dimBrush, colX + labelW, ty)

    drawColumn col1X "TEAM 1 (LEFT)" team1Color selectedTeam1 (activeColumn = 0)
    drawColumn col2X "TEAM 2 (RIGHT)" team2Color selectedTeam2 (activeColumn = 1)

    // Instructions
    let fastStr = if fastHuman then "ON" else "OFF"
    let hardStr = if hardMode then "ON" else "OFF"
    let fiveStr = if fivePlayer then "6v6" else "3v3"
    let padStr = if gamepadOn then "ON" else "OFF"

    let instrLines =
        [| "UP/DOWN = Select Team  |  TAB = Switch Column"
           "ENTER = Start Game  |  L = Play League  |  ESC = Quit"
           $"F = Fast Human [{fastStr}]  |  H = Hard Mode [{hardStr}]  |  5 = Players [{fiveStr}]"
           $"G = Gamepad [{padStr}]"
           "Hold shoot key longer for harder shot, quick tap for a pass"
           "Player 1: Arrow Keys + Shift/Enter, or Gamepad 1"
           "Player 2: WASD + Space/Tab, or Gamepad 2"
           "(Set team to HUMAN PLAYER for keyboard control)" |]

    let baseY = height * 0.7f

    instrLines
    |> Array.iteri (fun i line -> drawCentered g smallFont grayBrush width (baseY + float32 i * smallSize * 1.3f) line)

// ─── Main Render ──────────────────────────────────────────────────────

let renderFrame (g: Graphics) (gs: GameState) width height leagueMode =
    g.SmoothingMode <- SmoothingMode.AntiAlias
    g.TextRenderingHint <- Text.TextRenderingHint.ClearTypeGridFit

    let w = float32 width
    let h = float32 height

    // The 320x200 layout is a CGA-style design with non-square pixels: on a
    // 4:3 monitor its pixel aspect ratio is 1.2. Preserve the rink's shape by
    // using one uniform content scale `s` (sx = 1.2 * s, sy = s), sized so the
    // rink plus the HUD fill as much of the window as possible.
    let rinkGameH = float32 (stripPx FieldBottom) + 4.0f
    let contentGameH = rinkGameH + HudHeight
    let par = 1.2f
    let s = min (w / (OrigW * par)) ((h - 2.0f) / contentGameH)
    let sx = s * par
    let sy = s

    g.FillRectangle(solidBrush (Color.FromArgb(30, 30, 50)), 0.0f, 0.0f, w, h)

    // HUD is anchored to the bottom edge; the rink is centered in the space
    // above it (horizontally, and vertically on very wide windows).
    let hudH = HudHeight * sy
    let offX = (w - OrigW * sx) / 2.0f
    let offY = max 0.0f ((h - hudH - 2.0f - rinkGameH * sy) / 2.0f)

    use savedTransform = g.Transform
    g.TranslateTransform(offX, offY)

    drawRink g sx sy team1Color team2Color

    // Ice trail marks (drawn on ice, under players and puck)
    for i in 0 .. gs.TrailMarkCount - 1 do
        let mark = gs.TrailMarks.[i]
        if mark.Life > 0<tick> then
            let alpha = int (float (int mark.Life) / float (int TrailMarkLifetime) * 180.0) + 40
            let alpha = min 220 alpha
            let trailBrush = solidBrush (Color.FromArgb(alpha, 255, 255, 255))
            let mx = gameX sx mark.X
            let my = gameY sy mark.Y
            let r = 1.2f * sx
            g.FillEllipse(trailBrush, mx - r, my - r, r * 2.0f, r * 2.0f)

    let ppt = gs.PlayersPerTeam
    let t2s = gs.Team2Start

    // Helmet colors: human team = gold, CPU = black
    let t1Helmet = if gs.Team1Idx = 0 then helmetGold else helmetBlack
    let t2Helmet = if gs.Team2Idx = 0 then helmetGold else helmetBlack

    // Puck drawn UNDER players so skaters appear on top of it
    drawPuck g sx sy gs.Entities.[gs.PuckIdx] gs.PuckAnimFrame

    // Team 1 players
    for i in 0 .. ppt - 1 do
        let isGoalie = gs.FivePlayerMode && i = 0

        drawRetroPlayer
            g
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
            g
            sx
            sy
            gs.Entities.[ei]
            team2Color
            t2Helmet
            (ei = gs.ActivePlayer2)
            gs.StickAnimTimers.[ei]
            isGoalie
            (int gs.GameTick)

    // Back to window space for the HUD and overlays
    g.Transform <- savedTransform

    // HUD (spans the full width, anchored to the bottom edge)
    let rinkBottom = h - hudH - 2.0f
    drawHud g gs sx sy rinkBottom w

    // Overlays
    drawGoalFlash g gs w h

    if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
        drawGameOver g gs w h leagueMode
