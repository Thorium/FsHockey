/// THE FS HOCKEY LEAGUE — Renderer
/// GDI+ drawing: ice rink, players, puck, HUD, goal flash, game-over
module HockeyDemo.Renderer

open System
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
    use iceBrush = new SolidBrush(iceColor)
    g.FillRectangle(iceBrush, 0.0f, 0.0f, rinkW, rinkH)

    // Board outline
    use boardPen = new Pen(boardColor, 3.0f)
    g.DrawRectangle(boardPen, fl, ft, fr - fl, fb - ft)

    // Goal nets
    let drawGoalNet x color =
        use brush = new SolidBrush(Color.FromArgb(60, color))
        use pen = new Pen(color, 2.0f)
        g.FillRectangle(brush, x, gt, gd, gb - gt)
        g.DrawRectangle(pen, x, gt, gd, gb - gt)

    drawGoalNet (fl - gd) leftGoalColor
    drawGoalNet fr rightGoalColor

    // Center line + circle
    use centerPen = new Pen(lineColor, 1.5f)
    g.DrawLine(centerPen, cx, ft, cx, fb)
    let circR = 20.0f * sx
    g.DrawEllipse(centerPen, cx - circR, cy - circR, circR * 2.0f, circR * 2.0f)

    // Center dot
    use dotBrush = new SolidBrush(lineColor)
    g.FillEllipse(dotBrush, cx - 3.0f, cy - 3.0f, 6.0f, 6.0f)

    // Blue lines (1/3 and 2/3 of field width)
    use bluePen = new Pen(blueLineColor, 2.0f)
    let fieldW = stripPx FieldRight - stripPx FieldLeft
    let bl1 = gameX sx (FieldLeft + fieldW / 3.0 * 1.0<px>)
    let bl2 = gameX sx (FieldLeft + fieldW / 3.0 * 2.0<px>)
    g.DrawLine(bluePen, bl1, ft, bl1, fb)
    g.DrawLine(bluePen, bl2, ft, bl2, fb)

    // Goal lines (red dashed)
    use goalLinePen = new Pen(lineColor, 1.0f, DashStyle = DashStyle.Dash)
    let glx = gameX sx GoalLeftX
    let grx = gameX sx GoalRightX
    g.DrawLine(goalLinePen, glx, ft, glx, fb)
    g.DrawLine(goalLinePen, grx, ft, grx, fb)

// ─── Draw Retro Hockey Player ──────────────────────────────────────────
// Wayne Gretzky Hockey 2 inspired pixel art — scaled rectangles.
// isGoalie: distinct goalie appearance (wider pads, face mask, leg pads)

let drawRetroPlayer (g: Graphics) sx sy (ent: Entity) jerseyColor helmetColor isActive (stickAnim: int) isGoalie =
    let px = gameX sx ent.X
    let py = gameY sy ent.Y
    // Unit size — compact figures so the rink doesn't feel crowded
    let u = 1.1f * sx
    let uy = 1.1f * sy

    // ─── Rotation: face direction of DirX/DirY ───
    // Sprite is drawn "facing up" (head at -Y). Rotate to match direction.
    let angleDeg =
        if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
            float32 (System.Math.Atan2(float ent.DirX, -(float ent.DirY))) * (180.0f / float32 System.Math.PI)
        else
            0.0f

    let savedTransform = g.Transform.Clone()
    g.TranslateTransform(px, py)
    g.RotateTransform(angleDeg)
    // All drawing is now relative to (0,0) = player center
    let px = 0.0f
    let py = 0.0f

    let faceDir = 1.0f  // stick always extends to the "right" side in local space

    // ─── Helmet (head) ─────────────
    use helmetBrush = new SolidBrush(helmetColor)
    g.FillRectangle(helmetBrush, px - 1.5f * u, py - 5.5f * uy, 3.0f * u, 2.5f * uy)

    // ─── Goalie: face mask (cage) ──
    if isGoalie then
        use maskBrush = new SolidBrush(goalieMaskColor)
        // Cage area on the face side
        let maskX = px + 0.5f * u
        g.FillRectangle(maskBrush, maskX, py - 5.0f * uy, 1.0f * u, 1.5f * uy)
        // Cage bars
        use cagePen = new Pen(Color.FromArgb(100, 100, 100), max 0.5f (0.3f * u))
        let cx0 = maskX + 0.2f * u
        let cx1 = maskX + 0.8f * u
        g.DrawLine(cagePen, cx0, py - 5.0f * uy, cx0, py - 3.5f * uy)
        g.DrawLine(cagePen, cx1, py - 5.0f * uy, cx1, py - 3.5f * uy)

    // ─── Jersey (body) ─────────────
    use jerseyBrush = new SolidBrush(jerseyColor)
    // Shoulders: 8 wide, 2 tall
    g.FillRectangle(jerseyBrush, px - 4.0f * u, py - 3.0f * uy, 8.0f * u, 2.0f * uy)
    // Torso: 10 wide, 2.5 tall
    g.FillRectangle(jerseyBrush, px - 5.0f * u, py - 1.0f * uy, 10.0f * u, 2.5f * uy)

    // ─── Trousers / Goalie pads ────
    if isGoalie then
        // Wide goalie leg pads (cream/white)
        use padBrush = new SolidBrush(goaliePadColor)
        // Hips — slightly wider
        g.FillRectangle(padBrush, px - 5.0f * u, py + 1.5f * uy, 10.0f * u, 1.5f * uy)
        // Leg pads — wide rectangular pads
        g.FillRectangle(padBrush, px - 4.5f * u, py + 3.0f * uy, 4.0f * u, 2.0f * uy)
        g.FillRectangle(padBrush, px + 0.5f * u, py + 3.0f * uy, 4.0f * u, 2.0f * uy)
        // Pad outlines
        use padPen = new Pen(Color.FromArgb(160, 150, 130), max 1.0f (0.4f * u))
        g.DrawRectangle(padPen, px - 4.5f * u, py + 3.0f * uy, 4.0f * u, 2.0f * uy)
        g.DrawRectangle(padPen, px + 0.5f * u, py + 3.0f * uy, 4.0f * u, 2.0f * uy)
    else
        // Normal trousers
        use trouserBrush = new SolidBrush(trouserColor)
        // Hips: 8 wide, 1.5 tall
        g.FillRectangle(trouserBrush, px - 4.0f * u, py + 1.5f * uy, 8.0f * u, 1.5f * uy)
        // Legs: 6 wide, 1 tall
        g.FillRectangle(trouserBrush, px - 3.0f * u, py + 3.0f * uy, 6.0f * u, 1.0f * uy)

    // ─── Skate blades ──────────────
    let skateY = if isGoalie then py + 5.2f * uy else py + 4.2f * uy
    use skatePen = new Pen(skateColor, max 1.0f (0.6f * u))
    g.DrawLine(skatePen, px - 2.5f * u, skateY, px - 0.5f * u, skateY)
    g.DrawLine(skatePen, px + 0.5f * u, skateY, px + 2.5f * u, skateY)

    // ─── Stick ─────────────────────
    // Thicker shaft with tape at the handle end, visible blade at the puck end
    let shaftWidth = max 2.0f (1.6f * u)
    use shaftPen = new Pen(stickBrown, shaftWidth)

    let wobble =
        if stickAnim > 0 then
            sin (float32 stickAnim * 1.5f) * 3.0f * u
        else
            0.0f

    let stickLen = 8.0f * u
    let startX = px + faceDir * 2.0f * u
    let startY = py - 1.0f * uy
    let endX = startX + faceDir * 3.0f * u   // less sideways extension
    let endY = startY - stickLen + wobble     // mostly forward (upward in local space)

    // Shaft
    g.DrawLine(shaftPen, startX, startY, endX, endY)

    // Tape on handle (top 20% of shaft, white)
    let tapeFrac = 0.20f
    let tapeEndX = startX + (endX - startX) * tapeFrac
    let tapeEndY = startY + (endY - startY) * tapeFrac
    use tapePen = new Pen(stickTape, shaftWidth + 0.5f)
    g.DrawLine(tapePen, startX, startY, tapeEndX, tapeEndY)

    // Blade at end of stick (wider, angled forward)
    let bladeLen = 3.0f * u
    let bladeW = max 2.5f (1.8f * u)
    use bladePen = new Pen(stickBrown, bladeW)
    let bladeEndX = endX + faceDir * bladeLen
    let bladeEndY = endY - 1.0f * uy
    g.DrawLine(bladePen, endX, endY, bladeEndX, bladeEndY)

    // Restore transform before drawing active marker (marker should not rotate)
    g.Transform <- savedTransform
    savedTransform.Dispose()

    // ─── Active player marker (drawn in world space, not rotated) ──────
    if isActive then
        let px0 = gameX sx ent.X
        let py0 = gameY sy ent.Y
        let my = py0 - 7.0f * uy
        let ms = 3.5f * sx
        use markerPen = new Pen(activeMarker, 1.5f)
        g.DrawLine(markerPen, px0, my - ms, px0 + ms, my)
        g.DrawLine(markerPen, px0 + ms, my, px0, my + ms)
        g.DrawLine(markerPen, px0, my + ms, px0 - ms, my)
        g.DrawLine(markerPen, px0 - ms, my, px0, my - ms)

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (g: Graphics) sx sy (ball: Entity) =
    let px = gameX sx ball.X
    let py = gameY sy ball.Y
    let r = 2.5f * sx

    use brush = new SolidBrush(puckColor)
    g.FillEllipse(brush, px - r, py - r, r * 2.0f, r * 2.0f)

    use hlBrush = new SolidBrush(puckHighlight)
    let hr = r * 0.4f
    g.FillEllipse(hlBrush, px - hr, py - hr - 0.5f, hr * 2.0f, hr * 2.0f)

    use pen = new Pen(Color.Black, 1.0f)
    g.DrawEllipse(pen, px - r, py - r, r * 2.0f, r * 2.0f)

// ─── Draw HUD ─────────────────────────────────────────────────────────

let drawHud (g: Graphics) (gs: GameState) sx sy rinkBottom width =
    let hudY = rinkBottom + 2.0f
    let hudH = HudHeight * sy

    use bgBrush = new SolidBrush(hudBg)
    g.FillRectangle(bgBrush, 0.0f, hudY, width, hudH)

    use sepPen = new Pen(boardColor, 2.0f)
    g.DrawLine(sepPen, 0.0f, hudY, width, hudY)

    let fontSize = max 6.0f (5.0f * min sx sy)
    use font = new Font("Consolas", fontSize, FontStyle.Bold)
    use smallFont = new Font("Consolas", fontSize * 0.75f, FontStyle.Regular)
    use textBrush = new SolidBrush(hudText)
    use t1Brush = new SolidBrush(team1Color)
    use t2Brush = new SolidBrush(team2Color)

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
    if gs.GoalFlashTimer <= 0<tick> then
        ()
    else

        let alpha = if int gs.GoalFlashTimer % 10 < 5 then 80 else 30
        use overlayBrush = new SolidBrush(Color.FromArgb(alpha, goalFlashColor))
        g.FillRectangle(overlayBrush, 0.0f, 0.0f, width, height)

        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 8.0f (10.0f * scale)
        use font = new Font("Consolas", fontSize, FontStyle.Bold)

        let scorerName =
            match gs.GoalScoredBy with
            | Team1Scored -> teamNames.[gs.Team1Idx]
            | Team2Scored -> teamNames.[gs.Team2Idx]
            | NoGoal -> ""

        let goalStr = $"GOAL! {scorerName}"
        let strSize = g.MeasureString(goalStr, font)
        let tx = (width - strSize.Width) / 2.0f
        let ty = (height - strSize.Height) / 2.0f - 20.0f

        use shadowBrush = new SolidBrush(Color.FromArgb(180, Color.Black))
        g.DrawString(goalStr, font, shadowBrush, tx + 2.0f, ty + 2.0f)
        use goalBrush = new SolidBrush(goalFlashColor)
        g.DrawString(goalStr, font, goalBrush, tx, ty)

        let scoreStr = $"{gs.Team1Score} - {gs.Team2Score}"
        use scoreFont = new Font("Consolas", fontSize * 0.7f, FontStyle.Bold)
        use whiteBrush = new SolidBrush(Color.White)
        drawCentered g scoreFont whiteBrush width (ty + strSize.Height + 4.0f) scoreStr

// ─── Game Over Screen ─────────────────────────────────────────────────

let drawGameOver (g: Graphics) (gs: GameState) width height leagueMode =
    use overlayBrush = new SolidBrush(Color.FromArgb(160, Color.Black))
    g.FillRectangle(overlayBrush, 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    use bigFont = new Font("Consolas", bigSize, FontStyle.Bold)
    use medFont = new Font("Consolas", medSize, FontStyle.Bold)
    use smallFont = new Font("Consolas", smallSize, FontStyle.Regular)
    use whiteBrush = new SolidBrush(Color.White)
    use yellowBrush = new SolidBrush(goalFlashColor)
    use grayBrush = new SolidBrush(Color.FromArgb(180, 180, 180))

    drawCentered g bigFont yellowBrush width (height * 0.25f) "GAME OVER"

    let scoreStr =
        $"{teamNames.[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames.[gs.Team2Idx]}"

    drawCentered g medFont whiteBrush width (height * 0.40f) scoreStr

    let winner =
        match compare gs.Team1Score gs.Team2Score with
        | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
        | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
        | _ -> "IT'S A TIE!"

    drawCentered g medFont yellowBrush width (height * 0.52f) winner

    let instrStr =
        if leagueMode then
            "Press SPACE for standings"
        else
            "Press SPACE to play again  |  ESC to quit"

    drawCentered g smallFont grayBrush width (height * 0.72f) instrStr

// ─── League Matchup Screen ────────────────────────────────────────────

let drawLeagueMatchup (g: Graphics) width height roundNum totalRounds (team1Name: string) (team2Name: string) =
    use bgBrush = new SolidBrush(Color.FromArgb(10, 10, 30))
    g.FillRectangle(bgBrush, 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    use bigFont = new Font("Consolas", bigSize, FontStyle.Bold)
    use medFont = new Font("Consolas", medSize, FontStyle.Bold)
    use smallFont = new Font("Consolas", smallSize, FontStyle.Regular)
    use yellowBrush = new SolidBrush(goalFlashColor)
    use whiteBrush = new SolidBrush(Color.White)
    use grayBrush = new SolidBrush(Color.FromArgb(160, 160, 160))
    use t1Brush = new SolidBrush(team1Color)
    use t2Brush = new SolidBrush(team2Color)

    drawCentered g bigFont yellowBrush width (height * 0.12f) "LEAGUE MODE"
    drawCentered g medFont whiteBrush width (height * 0.28f) $"ROUND {roundNum} of {totalRounds}"
    drawCentered g medFont t1Brush width (height * 0.42f) team1Name
    drawCentered g smallFont grayBrush width (height * 0.52f) "vs"
    drawCentered g medFont t2Brush width (height * 0.60f) team2Name
    drawCentered g smallFont grayBrush width (height * 0.80f) "Press SPACE to start match"

// ─── League Standings Screen ──────────────────────────────────────────

let drawLeagueStandings (g: Graphics) width height (standings: (int * TeamStats) array) isFinal humanTeam =
    use bgBrush = new SolidBrush(Color.FromArgb(10, 10, 30))
    g.FillRectangle(bgBrush, 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    use bigFont = new Font("Consolas", bigSize, FontStyle.Bold)
    use medFont = new Font("Consolas", medSize, FontStyle.Bold)
    use smallFont = new Font("Consolas", smallSize, FontStyle.Regular)
    use yellowBrush = new SolidBrush(goalFlashColor)
    use whiteBrush = new SolidBrush(Color.White)
    use grayBrush = new SolidBrush(Color.FromArgb(160, 160, 160))

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
    use sepPen = new Pen(Color.FromArgb(60, 80, 120), 1.0f)
    let sepY = headerY + medSize * 1.4f
    g.DrawLine(sepPen, tableX, sepY, width - tableX, sepY)

    // Rows
    let dataY = sepY + 4.0f

    standings
    |> Array.iteri (fun rank (teamIdx, stats) ->
        let ry = dataY + float32 rank * rowH
        let isHuman = (teamIdx = humanTeam)

        if isHuman then
            use hlBrush = new SolidBrush(Color.FromArgb(30, 50, 80))
            g.FillRectangle(hlBrush, tableX - 2.0f, ry - 1.0f, width - tableX * 2.0f + 4.0f, rowH)

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

        use winFont = new Font("Consolas", max 7.0f (7.0f * scale), FontStyle.Bold)
        drawCentered g winFont yellowBrush width (height * 0.88f) winnerStr

    let instrStr =
        if isFinal then
            "Press SPACE to return to menu"
        else
            "Press SPACE to continue"

    drawCentered g smallFont grayBrush width (height * 0.94f) instrStr

// ─── Menu Screen ──────────────────────────────────────────────────────

let drawMenu (g: Graphics) width height selectedTeam1 selectedTeam2 activeColumn fastHuman hardMode fivePlayer =
    use bgBrush = new SolidBrush(Color.FromArgb(10, 10, 30))
    g.FillRectangle(bgBrush, 0.0f, 0.0f, width, height)

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    use bigFont = new Font("Consolas", bigSize, FontStyle.Bold)
    use medFont = new Font("Consolas", medSize, FontStyle.Regular)
    use smallFont = new Font("Consolas", smallSize, FontStyle.Regular)
    use whiteBrush = new SolidBrush(Color.White)
    use yellowBrush = new SolidBrush(goalFlashColor)
    use grayBrush = new SolidBrush(Color.FromArgb(140, 140, 140))
    use t1Brush = new SolidBrush(team1Color)
    use t2Brush = new SolidBrush(team2Color)

    // Title + subtitle
    drawCentered g bigFont yellowBrush width (height * 0.06f) "THE FS HOCKEY LEAGUE"

    let sub =
        "Tuomas Hietanen, Influenced by Solar Hockey (c) 1990-1992 Galifir Developments"

    drawCentered g smallFont grayBrush width (height * 0.06f + bigSize * 1.4f) sub

    // Two-column team selection
    let colW = width * 0.42f
    let col1X = width * 0.04f
    let col2X = width * 0.54f
    let listY = height * 0.22f

    let drawColumn colX (headerText: string) (headerBrush: SolidBrush) selectedIdx isActive =
        use headerFont = new Font("Consolas", medSize, FontStyle.Bold)
        g.DrawString(headerText, headerFont, headerBrush, colX, listY)

        if isActive then
            use activePen = new Pen(headerBrush.Color, 1.5f)
            let boxH = medSize * 1.3f + float32 NumTeams * smallSize * 1.6f + 6.0f
            g.DrawRectangle(activePen, colX - 3.0f, listY - 2.0f, colW, boxH)

        for i in 0 .. NumTeams - 1 do
            let ty = listY + medSize * 1.5f + float32 i * smallSize * 1.6f
            let isSelected = (i = selectedIdx)

            if isSelected then
                use selBrush = new SolidBrush(Color.FromArgb(40, 60, 100))
                g.FillRectangle(selBrush, colX, ty - 1.0f, colW - 6.0f, smallSize * 1.4f)

            let brush: Brush = if isSelected then yellowBrush else whiteBrush
            let prefix = if isSelected then "> " else "  "
            g.DrawString($"{prefix}{teamNames.[i]}", smallFont, brush, colX + 4.0f, ty)

    drawColumn col1X "TEAM 1 (LEFT)" t1Brush selectedTeam1 (activeColumn = 0)
    drawColumn col2X "TEAM 2 (RIGHT)" t2Brush selectedTeam2 (activeColumn = 1)

    // Instructions
    let fastStr = if fastHuman then "ON" else "OFF"
    let hardStr = if hardMode then "ON" else "OFF"
    let fiveStr = if fivePlayer then "6v6" else "3v3"

    let instrLines =
        [| "UP/DOWN = Select Team  |  TAB = Switch Column"
           "ENTER = Start Game  |  L = Play League  |  ESC = Quit"
           $"F = Fast Human [{fastStr}]  |  H = Hard Mode [{hardStr}]  |  5 = Players [{fiveStr}]"
           "Hold shoot key longer for harder shot, quick tap for a pass"
           "Player 1: Arrow Keys + RShift/Enter to shoot"
           "Player 2: WASD + Space/Tab to shoot"
           "(Set team to HUMAN PLAYER for keyboard control)" |]

    let baseY = height * 0.82f

    instrLines
    |> Array.iteri (fun i line -> drawCentered g smallFont grayBrush width (baseY + float32 i * smallSize * 1.3f) line)

// ─── Main Render ──────────────────────────────────────────────────────

let renderFrame
    (g: Graphics)
    (gs: GameState)
    width
    height
    menuMode
    selTeam1
    selTeam2
    activeCol
    leagueMode
    fastHuman
    hardMode
    fivePlayer
    =
    g.SmoothingMode <- SmoothingMode.AntiAlias
    g.TextRenderingHint <- Text.TextRenderingHint.ClearTypeGridFit

    let w = float32 width
    let h = float32 height

    if menuMode then
        drawMenu g w h selTeam1 selTeam2 activeCol fastHuman hardMode fivePlayer
    else

        let rinkH = OrigH + HudHeight
        let sx = w / OrigW
        let sy = h / rinkH

        use bgBrush = new SolidBrush(Color.FromArgb(30, 30, 50))
        g.FillRectangle(bgBrush, 0.0f, 0.0f, w, h)

        drawRink g sx sy team1Color team2Color

        let ppt = gs.PlayersPerTeam
        let t2s = gs.Team2Start

        // Helmet colors: human team = gold, CPU = black
        let t1Helmet = if gs.Team1Idx = 0 then helmetGold else helmetBlack
        let t2Helmet = if gs.Team2Idx = 0 then helmetGold else helmetBlack

        // Puck drawn UNDER players so skaters appear on top of it
        drawPuck g sx sy gs.Entities.[gs.BallIdx]

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

        // HUD
        let rinkBottom = gameY sy FieldBottom + 4.0f * sy
        drawHud g gs sx sy rinkBottom w

        // Overlays
        drawGoalFlash g gs w h

        if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
            drawGameOver g gs w h leagueMode
