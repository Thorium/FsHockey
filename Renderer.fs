/// THE FS HOCKEY LEAGUE — Renderer (MonoGame)
/// SpriteBatch drawing: ice rink, players, puck, HUD, goal flash, game-over
module HockeyDemo.Renderer

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp
open HockeyDemo.Physics
open HockeyDemo.Drawing
open HockeyDemo.Game

// ─── Scale / Layout ────────────────────────────────────────────────────

[<Literal>]
let OrigW = 320.0f

[<Literal>]
let OrigH = 200.0f

[<Literal>]
let HudHeight = 48.0f

// ─── Colors (CGA-inspired) ────────────────────────────────────────────

let iceColor = Color(200, 220, 240)
let boardColor = Color(60, 80, 120)
let lineColor = Color(180, 40, 40)
let blueLineColor = Color(40, 80, 180)
let team1Color = Color(220, 60, 60)
let team1Light = Color(255, 120, 120)
let team2Color = Color(60, 100, 220)
let team2Light = Color(120, 160, 255)
let puckColor = Color(20, 20, 20)
let puckHighlight = Color(60, 60, 60)
let hudBg = Color(20, 20, 40)
let hudText = Color(220, 220, 220)
let goalFlashColor = Color(255, 255, 80)
let activeMarkerColor = Color.White
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

// ─── Font System ──────────────────────────────────────────────────────

let mutable private fontSystem: FontSystem = null

let initFonts (_device: GraphicsDevice) =
    let settings = FontSystemSettings()
    fontSystem <- new FontSystem(settings)

    // Discover monospace fonts across platforms (Linux, macOS, Windows)
    let fontPaths =
        [| "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
           "/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf"
           "/usr/share/fonts/truetype/freefont/FreeMono.ttf"
           "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
           "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf"
           "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
           "/usr/share/fonts/dejavu-sans-mono-fonts/DejaVuSansMono.ttf"
           "/System/Library/Fonts/Menlo.ttc"
           "/Library/Fonts/Courier New.ttf"
           "C:\\Windows\\Fonts\\consola.ttf"
           "C:\\Windows\\Fonts\\cour.ttf"
           "C:\\Windows\\Fonts\\lucon.ttf" |]

    let mutable loaded = false

    for path in fontPaths do
        if not loaded && System.IO.File.Exists(path) then
            try
                fontSystem.AddFont(System.IO.File.ReadAllBytes(path))
                loaded <- true
            with
            | _ -> ()

let disposeFonts () =
    if fontSystem <> null then
        fontSystem.Dispose()
        fontSystem <- null

let private getFont (size: float32) =
    if fontSystem <> null then
        Some(fontSystem.GetFont(size))
    else
        None

// ─── Drawing Helpers ──────────────────────────────────────────────────

/// Scale game X-coordinate to screen
let inline gameX (sx: float32) (x: float<px>) = float32 (stripPx x) * sx

/// Scale game Y-coordinate to screen
let inline gameY (sy: float32) (y: float<px>) = float32 (stripPx y) * sy

/// Draw a string centered horizontally at the given Y position
let private drawCentered (sb: SpriteBatch) (font: SpriteFontBase) width y (text: string) (color: Color) =
    let sz = font.MeasureString(text)
    font.DrawText(sb, text, Vector2((width - sz.X) / 2.0f, y), color) |> ignore

/// Draw text at a position (wraps DrawText with ignore)
let private drawText (font: SpriteFontBase) (sb: SpriteBatch) (text: string) (pos: Vector2) (color: Color) =
    font.DrawText(sb, text, pos, color) |> ignore

/// Create font sizes at a given scale factor.
/// Uses the same numeric values as the original GDI+ point sizes.
/// DejaVu Sans Mono (FontStashSharp) renders comparably to Consolas (GDI+)
/// at the same numeric size, so no extra multiplier is needed.
let private mkFonts (scale: float32) =
    let big = max 8.0f (9.0f * scale)
    let med = max 7.0f (6.0f * scale)
    let small = max 6.0f (5.0f * scale)
    struct (big, med, small)

// ─── Draw Rink ────────────────────────────────────────────────────────

let drawRink (sb: SpriteBatch) sx sy leftGoalColor rightGoalColor =
    let rinkW = OrigW * sx
    let rinkH = gameY sy FieldBottom + 4.0f * sy
    let fl, fr = gameX sx FieldLeft, gameX sx FieldRight
    let ft, fb = gameY sy FieldTop, gameY sy FieldBottom
    let gt, gb = gameY sy GoalTop, gameY sy GoalBottom
    let gd = float32 (stripPx GoalDepth) * sx
    let cx = gameX sx CenterX
    let cy = gameY sy CenterY

    // Ice surface
    fillRect sb 0.0f 0.0f rinkW rinkH iceColor

    // Board outline
    drawRect sb fl ft (fr - fl) (fb - ft) 3.0f boardColor

    // Goal nets
    let drawGoalNet x (color: Color) =
        let netColor = Color(color.R, color.G, color.B, 60uy)
        fillRect sb x gt gd (gb - gt) netColor
        drawRect sb x gt gd (gb - gt) 2.0f color

    drawGoalNet (fl - gd) leftGoalColor
    drawGoalNet fr rightGoalColor

    // Center line + circle
    drawLine sb cx ft cx fb 1.5f lineColor
    let circR = 20.0f * sx
    drawEllipse sb cx cy circR circR 1.5f lineColor iceColor

    // Center dot
    fillEllipse sb cx cy 3.0f 3.0f lineColor

    // Blue lines (1/3 and 2/3 of field width)
    let fieldW = stripPx FieldRight - stripPx FieldLeft
    let bl1 = gameX sx (FieldLeft + fieldW / 3.0 * 1.0<px>)
    let bl2 = gameX sx (FieldLeft + fieldW / 3.0 * 2.0<px>)
    drawLine sb bl1 ft bl1 fb 2.0f blueLineColor
    drawLine sb bl2 ft bl2 fb 2.0f blueLineColor

    // Goal lines (red dashed)
    let glx = gameX sx GoalLeftX
    let grx = gameX sx GoalRightX
    drawDashedLine sb glx ft glx fb 1.0f 4.0f lineColor
    drawDashedLine sb grx ft grx fb 1.0f 4.0f lineColor

// ─── Draw Retro Hockey Player ──────────────────────────────────────────

let drawRetroPlayer (sb: SpriteBatch) sx sy (ent: Entity) jerseyColor helmetColor isActive (stickAnim: int) isGoalie (gameTick: int) =
    let screenX = gameX sx ent.X
    let screenY = gameY sy ent.Y
    let u = 0.85f * sx
    let uy = 0.85f * sy

    // ─── Rotation: face direction of DirX/DirY (matches original GDI+ behavior) ───
    // Compute rotation angle from direction vector
    let angleRad =
        if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
            float32 (System.Math.Atan2(float ent.DirX, -(float ent.DirY)))
        else
            0.0f

    // End the current SpriteBatch, begin a new one with rotation transform around the entity position.
    // This mirrors the GDI+ TranslateTransform + RotateTransform approach.
    sb.End()
    let rotMatrix =
        Matrix.CreateTranslation(-screenX, -screenY, 0.0f)
        * Matrix.CreateRotationZ(angleRad)
        * Matrix.CreateTranslation(screenX, screenY, 0.0f)
    sb.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied, transformMatrix = rotMatrix)

    // Draw body at origin-relative coordinates (px, py = screenX, screenY)
    let px = screenX
    let py = screenY

    // Skating leg animation — slow oscillation to look like skating, not running
    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
    let legOffset =
        if speedSq > 16.0 then
            sin (float32 gameTick * 0.08f) * 1.2f * uy * 0.3f
        else
            0.0f

    // ─── Helmet (head)
    fillRect sb (px - 1.5f * u) (py - 5.5f * uy) (3.0f * u) (2.0f * uy) helmetColor

    // Face area (skin visible below helmet)
    fillRect sb (px - 1.0f * u) (py - 3.5f * uy) (2.0f * u) (1.0f * uy) skinColor

    // Goalie face mask
    if isGoalie then
        let maskX = px + 0.3f * u
        fillRect sb maskX (py - 4.5f * uy) (1.2f * u) (1.5f * uy) goalieMaskColor
        let cagePen = Color(100, 100, 100)
        let cageW = max 0.5f (0.3f * u)
        let cx0 = maskX + 0.3f * u
        let cx1 = maskX + 0.9f * u
        drawLine sb cx0 (py - 4.5f * uy) cx0 (py - 3.0f * uy) cageW cagePen
        drawLine sb cx1 (py - 4.5f * uy) cx1 (py - 3.0f * uy) cageW cagePen

    // ─── Jersey (body)
    // Shoulders
    fillRect sb (px - 3.5f * u) (py - 2.5f * uy) (7.0f * u) (1.5f * uy) jerseyColor
    // Torso
    fillRect sb (px - 3.0f * u) (py - 1.0f * uy) (6.0f * u) (2.5f * uy) jerseyColor
    // Jersey stripe
    let stripeColor = Color(255, 255, 255, 80)
    fillRect sb (px - 3.0f * u) (py - 0.5f * uy) (6.0f * u) (0.6f * uy) stripeColor

    // ─── Arms / Gloves
    // Left arm
    fillRect sb (px - 4.0f * u) (py - 2.0f * uy) (1.2f * u) (2.0f * uy) jerseyColor
    fillRect sb (px - 4.0f * u) (py + 0.0f * uy) (1.2f * u) (0.8f * uy) gloveColor
    // Right arm
    fillRect sb (px + 2.8f * u) (py - 2.0f * uy) (1.2f * u) (2.0f * uy) jerseyColor
    fillRect sb (px + 2.8f * u) (py + 0.0f * uy) (1.2f * u) (0.8f * uy) gloveColor

    // ─── Trousers / Goalie pads
    if isGoalie then
        // Hips
        fillRect sb (px - 3.5f * u) (py + 1.5f * uy) (7.0f * u) (1.2f * uy) goaliePadColor
        // Leg pads
        fillRect sb (px - 3.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) goaliePadColor
        fillRect sb (px + 0.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) goaliePadColor
        // Pad outlines
        let outlineColor = Color(160, 150, 130)
        let outlineW = max 1.0f (0.4f * u)
        drawRect sb (px - 3.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) outlineW outlineColor
        drawRect sb (px + 0.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) outlineW outlineColor
    else
        // Hips
        fillRect sb (px - 3.0f * u) (py + 1.5f * uy) (6.0f * u) (1.2f * uy) trouserColor
        // Left leg (animated)
        fillRect sb (px - 2.5f * u) (py + 2.7f * uy + legOffset) (2.2f * u) (1.0f * uy) trouserColor
        // Right leg (animated opposite)
        fillRect sb (px + 0.3f * u) (py + 2.7f * uy - legOffset) (2.2f * u) (1.0f * uy) trouserColor
        // Socks
        fillRect sb (px - 2.2f * u) (py + 3.5f * uy + legOffset) (1.8f * u) (0.5f * uy) sockColor
        fillRect sb (px + 0.4f * u) (py + 3.5f * uy - legOffset) (1.8f * u) (0.5f * uy) sockColor

    // ─── Skate blades
    let skateW = max 1.0f (0.5f * u)

    if isGoalie then
        let skateY = py + 4.8f * uy
        drawLine sb (px - 2.0f * u) skateY (px - 0.5f * u) skateY skateW skateColor
        drawLine sb (px + 0.5f * u) skateY (px + 2.0f * u) skateY skateW skateColor
    else
        let skateYL = py + 4.0f * uy + legOffset
        let skateYR = py + 4.0f * uy - legOffset
        drawLine sb (px - 2.0f * u) skateYL (px - 0.3f * u) skateYL skateW skateColor
        drawLine sb (px + 0.3f * u) skateYR (px + 2.0f * u) skateYR skateW skateColor

    // ─── Stick
    let shaftWidth = max 1.5f (1.2f * u)
    let wobble =
        if stickAnim > 0 then
            sin (float32 stickAnim * 1.5f) * 2.5f * u
        else
            0.0f

    // Stick always points forward (faceDir=1) since the whole sprite rotates
    let faceDir = 1.0f

    let stickLen = 7.0f * u
    let startX = px + faceDir * 2.0f * u
    let startY = py - 0.5f * uy
    let endX = startX + faceDir * 2.5f * u
    let endY = startY - stickLen + wobble

    drawLine sb startX startY endX endY shaftWidth stickBrown

    // Tape on handle
    let tapeFrac = 0.18f
    let tapeEndX = startX + (endX - startX) * tapeFrac
    let tapeEndY = startY + (endY - startY) * tapeFrac
    drawLine sb startX startY tapeEndX tapeEndY (shaftWidth + 0.5f) stickTape

    // Blade
    let bladeLen = 2.5f * u
    let bladeW = max 2.0f (1.4f * u)
    let bladeEndX = endX + faceDir * bladeLen
    let bladeEndY = endY - 0.8f * uy
    drawLine sb endX endY bladeEndX bladeEndY bladeW stickBrown

    // End the rotated SpriteBatch, restart the normal one
    sb.End()
    sb.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

    // ─── Active player marker (drawn in screen space, NOT rotated)
    if isActive then
        let my = screenY - 6.5f * uy
        let ms = 2.0f * sx
        let markerW = max 1.0f (1.2f * sx)
        drawLine sb (screenX - ms) (my - ms) screenX my markerW activeMarkerColor
        drawLine sb screenX my (screenX + ms) (my - ms) markerW activeMarkerColor

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (sb: SpriteBatch) sx sy (ball: Entity) =
    let px = gameX sx ball.X
    let py = gameY sy ball.Y
    let r = 2.5f * sx

    fillEllipse sb px py r r puckColor

    let hr = r * 0.4f
    fillEllipse sb px (py - 0.5f) hr hr puckHighlight

// ─── Draw HUD ─────────────────────────────────────────────────────────

let drawHud (sb: SpriteBatch) (gs: GameState) sx sy rinkBottom width =
    let hudY = rinkBottom + 2.0f
    let hudH = HudHeight * sy

    fillRect sb 0.0f hudY width hudH hudBg
    drawLine sb 0.0f hudY width hudY 2.0f boardColor

    let fontSize = max 12.0f (12.0f * min sx sy)
    let smallSize = fontSize * 0.75f

    match getFont fontSize, getFont smallSize with
    | Some font, Some smallFont ->
        // Team 1 name + score (left)
        drawText smallFont sb teamNames.[gs.Team1Idx] (Vector2(10.0f * sx, hudY + 4.0f)) team1Color
        drawText font sb $"{gs.Team1Score}" (Vector2(10.0f * sx, hudY + 4.0f + fontSize * 1.1f)) team1Color

        // Team 2 name + score (right)
        let t2Name = teamNames.[gs.Team2Idx]
        let t2Size = smallFont.MeasureString(t2Name)
        drawText smallFont sb t2Name (Vector2(width - t2Size.X - 10.0f * sx, hudY + 4.0f)) team2Color
        let s2Str = $"{gs.Team2Score}"
        let s2Size = font.MeasureString(s2Str)
        drawText font sb s2Str (Vector2(width - s2Size.X - 10.0f * sx, hudY + 4.0f + fontSize * 1.1f)) team2Color

        // Clock (center)
        let secs = int gs.ClockSeconds
        let clockStr = $"{secs / 60}:{secs % 60:D2}"
        drawCentered sb font width (hudY + 4.0f) clockStr hudText

        // Period info
        if gs.NumPeriods > 1 then
            let periodStr = $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
            drawCentered sb smallFont width (hudY + 4.0f + fontSize * 1.1f) periodStr hudText
    | _ -> ()

// ─── Goal Flash Overlay ───────────────────────────────────────────────

let drawGoalFlash (sb: SpriteBatch) (gs: GameState) width height =
    if gs.GoalFlashTimer <= 0<tick> then
        ()
    else
        let alpha = if int gs.GoalFlashTimer % 10 < 5 then 80 else 30
        let overlayColor = Color(goalFlashColor.R, goalFlashColor.G, goalFlashColor.B, byte alpha)
        fillRect sb 0.0f 0.0f width height overlayColor

        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0f (20.0f * scale)

        match getFont fontSize with
        | Some font ->
            let scorerName =
                match gs.GoalScoredBy with
                | Team1Scored -> teamNames.[gs.Team1Idx]
                | Team2Scored -> teamNames.[gs.Team2Idx]
                | NoGoal -> ""

            let goalStr = $"GOAL! {scorerName}"
            let strSize = font.MeasureString(goalStr)
            let tx = (width - strSize.X) / 2.0f
            let ty = (height - strSize.Y) / 2.0f - 20.0f

            drawText font sb goalStr (Vector2(tx + 2.0f, ty + 2.0f)) (Color(0, 0, 0, 180))
            drawText font sb goalStr (Vector2(tx, ty)) goalFlashColor

            let scoreStr = $"{gs.Team1Score} - {gs.Team2Score}"

            match getFont (fontSize * 0.7f) with
            | Some scoreFont ->
                drawCentered sb scoreFont width (ty + strSize.Y + 4.0f) scoreStr Color.White
            | None -> ()
        | None -> ()

// ─── Game Over Screen ─────────────────────────────────────────────────

let drawGameOver (sb: SpriteBatch) (gs: GameState) width height leagueMode =
    fillRect sb 0.0f 0.0f width height (Color(0, 0, 0, 160))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    match getFont bigSize, getFont medSize, getFont smallSize with
    | Some bigFont, Some medFont, Some smallFont ->
        drawCentered sb bigFont width (height * 0.25f) "GAME OVER" goalFlashColor

        let scoreStr =
            $"{teamNames.[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames.[gs.Team2Idx]}"

        drawCentered sb medFont width (height * 0.40f) scoreStr Color.White

        let winner =
            match compare gs.Team1Score gs.Team2Score with
            | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
            | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
            | _ -> "IT'S A TIE!"

        drawCentered sb medFont width (height * 0.52f) winner goalFlashColor

        let instrStr =
            if leagueMode then
                "Press SPACE for standings"
            else
                "Press SPACE to play again  |  ESC to quit"

        drawCentered sb smallFont width (height * 0.72f) instrStr (Color(180, 180, 180))
    | _ -> ()

// ─── League Matchup Screen ────────────────────────────────────────────

let drawLeagueMatchup (sb: SpriteBatch) width height roundNum totalRounds (team1Name: string) (team2Name: string) =
    fillRect sb 0.0f 0.0f width height (Color(10, 10, 30))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    match getFont bigSize, getFont medSize, getFont smallSize with
    | Some bigFont, Some medFont, Some smallFont ->
        drawCentered sb bigFont width (height * 0.12f) "LEAGUE MODE" goalFlashColor
        drawCentered sb medFont width (height * 0.28f) $"ROUND {roundNum} of {totalRounds}" Color.White
        drawCentered sb medFont width (height * 0.42f) team1Name team1Color
        drawCentered sb smallFont width (height * 0.52f) "vs" grayColor
        drawCentered sb medFont width (height * 0.60f) team2Name team2Color
        drawCentered sb smallFont width (height * 0.80f) "Press SPACE to start match" grayColor
    | _ -> ()

// ─── League Standings Screen ──────────────────────────────────────────

let drawLeagueStandings (sb: SpriteBatch) width height (standings: (int * TeamStats) array) isFinal humanTeam =
    fillRect sb 0.0f 0.0f width height (Color(10, 10, 30))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    match getFont bigSize, getFont medSize, getFont smallSize with
    | Some bigFont, Some medFont, Some smallFont ->
        let title = if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS"
        drawCentered sb bigFont width (height * 0.04f) title goalFlashColor

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
            drawText medFont sb h (Vector2(hx, headerY)) grayColor)

        // Separator
        let sepY = headerY + medSize * 1.4f
        drawLine sb tableX sepY (width - tableX) sepY 1.0f (Color(60, 80, 120))

        // Rows
        let dataY = sepY + 4.0f

        standings
        |> Array.iteri (fun rank (teamIdx, stats) ->
            let ry = dataY + float32 rank * rowH
            let isHuman = (teamIdx = humanTeam)

            if isHuman then
                fillRect sb (tableX - 2.0f) (ry - 1.0f) (width - tableX * 2.0f + 4.0f) rowH (Color(30, 50, 80))

            let textColor = if isHuman then goalFlashColor else Color.White
            drawText smallFont sb $"{rank + 1}." (Vector2(tableX, ry)) grayColor
            drawText smallFont sb teamNames.[teamIdx] (Vector2(tableX + smallSize * 2.5f, ry)) textColor

            [| $"{stats.Wins}"
               $"{stats.Losses}"
               $"{stats.Draws}"
               $"{stats.Points}"
               $"{stats.GoalsFor}"
               $"{stats.GoalsAgainst}" |]
            |> Array.iteri (fun i v ->
                drawText smallFont sb v (Vector2(tableX + colPositions.[i], ry)) Color.White))

        // Winner announcement
        if isFinal && standings.Length > 0 then
            let winnerIdx, winnerStats = standings.[0]

            let winnerStr =
                $"{teamNames.[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"

            match getFont (max 14.0f (14.0f * scale)) with
            | Some winFont ->
                drawCentered sb winFont width (height * 0.88f) winnerStr goalFlashColor
            | None -> ()

        let instrStr =
            if isFinal then
                "Press SPACE to return to menu"
            else
                "Press SPACE to continue"

        drawCentered sb smallFont width (height * 0.94f) instrStr grayColor
    | _ -> ()

// ─── Menu Screen ──────────────────────────────────────────────────────

let drawMenu (sb: SpriteBatch) width height selectedTeam1 selectedTeam2 activeColumn fastHuman hardMode fivePlayer =
    fillRect sb 0.0f 0.0f width height (Color(10, 10, 30))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    match getFont bigSize, getFont medSize, getFont smallSize with
    | Some bigFont, Some medFont, Some smallFont ->
        let menuGray = Color(140, 140, 140)

        // Title + subtitle
        drawCentered sb bigFont width (height * 0.06f) "THE FS HOCKEY LEAGUE" goalFlashColor

        let sub = "Tuomas Hietanen, 2026"
        drawCentered sb smallFont width (height * 0.06f + bigSize * 1.4f) sub menuGray

        // Two-column team selection
        let colW = width * 0.42f
        let col1X = width * 0.04f
        let col2X = width * 0.54f
        let listY = height * 0.22f

        let drawColumn colX (headerText: string) (headerColor: Color) selectedIdx isActive =
            drawText medFont sb headerText (Vector2(colX, listY)) headerColor

            if isActive then
                let boxH = medSize * 1.3f + float32 NumTeams * smallSize * 1.6f + 6.0f
                drawRect sb (colX - 3.0f) (listY - 2.0f) colW boxH 1.5f headerColor

            for i in 0 .. NumTeams - 1 do
                let ty = listY + medSize * 1.5f + float32 i * smallSize * 1.6f
                let isSelected = (i = selectedIdx)

                if isSelected then
                    fillRect sb colX (ty - 1.0f) (colW - 6.0f) (smallSize * 1.4f) (Color(40, 60, 100))

                let color = if isSelected then goalFlashColor else Color.White
                let prefix = if isSelected then "> " else "  "
                drawText smallFont sb $"{prefix}{teamNames.[i]}" (Vector2(colX + 4.0f, ty)) color

        drawColumn col1X "TEAM 1 (LEFT)" team1Color selectedTeam1 (activeColumn = 0)
        drawColumn col2X "TEAM 2 (RIGHT)" team2Color selectedTeam2 (activeColumn = 1)

        // Instructions
        let fastStr = if fastHuman then "ON" else "OFF"
        let hardStr = if hardMode then "ON" else "OFF"
        let fiveStr = if fivePlayer then "6v6" else "3v3"

        let instrLines =
            [| "UP/DOWN = Select Team  |  TAB = Switch Column"
               "ENTER = Start Game  |  L = Play League  |  ESC = Quit"
               $"F = Fast Human [{fastStr}]  |  H = Hard Mode [{hardStr}]  |  5 = Players [{fiveStr}]"
               "F11 = Toggle Fullscreen"
               "Hold shoot key longer for harder shot, quick tap for a pass"
               "Player 1: Arrow Keys + RShift/Enter to shoot"
               "Player 2: WASD + Space/Tab to shoot"
               "(Set team to HUMAN PLAYER for keyboard control)" |]

        let baseY = height * 0.7f

        instrLines
        |> Array.iteri (fun i line ->
            drawCentered sb smallFont width (baseY + float32 i * smallSize * 1.3f) line menuGray)
    | _ -> ()

// ─── Main Render ──────────────────────────────────────────────────────

let renderFrame
    (sb: SpriteBatch)
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
    let w = float32 width
    let h = float32 height

    if menuMode then
        drawMenu sb w h selTeam1 selTeam2 activeCol fastHuman hardMode fivePlayer
    else
        let rinkH = OrigH + HudHeight
        let sx = w / OrigW
        let sy = h / rinkH

        fillRect sb 0.0f 0.0f w h bgColor

        drawRink sb sx sy team1Color team2Color

        // Ice trail marks (drawn on ice, under players and puck)
        for i in 0 .. gs.TrailMarkCount - 1 do
            let mark = gs.TrailMarks.[i]

            if mark.Life > 0<tick> then
                let alpha = int (float (int mark.Life) / float (int TrailMarkLifetime) * 180.0) + 40
                let alpha = byte (min 220 alpha)
                let trailColor = Color(255uy, 255uy, 255uy, alpha)
                let mx = gameX sx mark.X
                let my = gameY sy mark.Y
                let r = 1.2f * sx
                fillEllipse sb mx my r r trailColor

        let ppt = gs.PlayersPerTeam
        let t2s = gs.Team2Start

        // Helmet colors: human team = gold, CPU = black
        let t1Helmet = if gs.Team1Idx = 0 then helmetGold else helmetBlack
        let t2Helmet = if gs.Team2Idx = 0 then helmetGold else helmetBlack

        // Puck drawn UNDER players so skaters appear on top of it
        drawPuck sb sx sy gs.Entities.[gs.BallIdx]

        // Team 1 players
        for i in 0 .. ppt - 1 do
            let isGoalie = gs.FivePlayerMode && i = 0

            drawRetroPlayer
                sb
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
                sb
                sx
                sy
                gs.Entities.[ei]
                team2Color
                t2Helmet
                (ei = gs.ActivePlayer2)
                gs.StickAnimTimers.[ei]
                isGoalie
                (int gs.GameTick)

        // HUD
        let rinkBottom = gameY sy FieldBottom + 4.0f * sy
        drawHud sb gs sx sy rinkBottom w

        // Overlays
        drawGoalFlash sb gs w h

        if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
            drawGameOver sb gs w h leagueMode
