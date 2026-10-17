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

// ─── Font System ──────────────────────────────────────────────────────

let mutable private fontSystem: FontSystem = null
let mutable private fontLoaded = false

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

    for path in fontPaths do
        if not fontLoaded && System.IO.File.Exists(path) then
            try
                fontSystem.AddFont(System.IO.File.ReadAllBytes(path))
                fontLoaded <- true
            with
            | _ -> ()

    // Bundled fallback font (Roboto Mono, Apache 2.0) next to the executable
    if not fontLoaded then
        let bundledPath = System.IO.Path.Combine(System.AppContext.BaseDirectory, "RobotoMono-Regular.ttf")
        if System.IO.File.Exists(bundledPath) then
            try
                fontSystem.AddFont(System.IO.File.ReadAllBytes(bundledPath))
                fontLoaded <- true
            with
            | _ -> ()

    if not fontLoaded then
        eprintfn "WARNING: No monospace font found. All text will be invisible."
        eprintfn "  Searched system fonts and bundled fallback (RobotoMono-Regular.ttf)."

let disposeFonts () =
    if not (isNull fontSystem) then
        fontSystem.Dispose()
        fontSystem <- null

let private getFont (size: float32) =
    if (not (isNull fontSystem)) && fontLoaded then
        Some(fontSystem.GetFont size)
    else
        None

// ─── Drawing Helpers ──────────────────────────────────────────────────

/// Scale game X-coordinate to screen
let inline gameX (sx: float32) (x: float<px>) = float32 (stripPx x) * sx

/// Scale game Y-coordinate to screen
let inline gameY (sy: float32) (y: float<px>) = float32 (stripPx y) * sy

/// Draw a string centered horizontally at the given Y position
let private drawCentered (sb: SpriteBatch) (font: SpriteFontBase) width y (text: string) (color: Color) =
    let sz = font.MeasureString text
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

let drawRetroPlayer (sb: SpriteBatch) sx sy (offX: float32) (offY: float32) (ent: Entity) jerseyColor helmetColor isActive (stickAnim: int) isGoalie (gameTick: int) =
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
        * Matrix.CreateTranslation(screenX + offX, screenY + offY, 0.0f)
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

    // End the rotated SpriteBatch, restart the caller's offset-translated one
    sb.End()

    sb.Begin(
        SpriteSortMode.Deferred,
        BlendState.NonPremultiplied,
        transformMatrix = Matrix.CreateTranslation(offX, offY, 0.0f)
    )

    // ─── Active player marker (drawn in rink space, NOT rotated)
    if isActive then
        let my = screenY - 6.5f * uy
        let ms = 2.0f * sx
        let markerW = max 1.0f (1.2f * sx)
        drawLine sb (screenX - ms) (my - ms) screenX my markerW activeMarkerColor
        drawLine sb screenX my (screenX + ms) (my - ms) markerW activeMarkerColor

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (sb: SpriteBatch) sx sy (puck: Entity) (animFrame: int) =
    let px = gameX sx puck.X
    let py = gameY sy puck.Y
    let r = 2.5f * sx

    fillEllipse sb px py r r puckColor

    // Spinning highlight: orbits the puck center once per animation cycle
    let phase = float32 animFrame / float32 (PuckAnimFrames * 2) * (2.0f * float32 System.Math.PI)
    let hr = r * 0.4f
    let orbit = r * 0.35f
    let hx = px + cos phase * orbit
    let hy = py - 0.5f + sin phase * orbit
    fillEllipse sb hx hy hr hr puckHighlight

// ─── Draw HUD ─────────────────────────────────────────────────────────

let drawHud (sb: SpriteBatch) (gs: GameState) sx sy rinkBottom width fastHuman hardMode =
    let hudY = rinkBottom + 2.0f
    let team1Color = teamColor false fastHuman hardMode gs.Team1Idx
    let team2Color = teamColor true fastHuman hardMode gs.Team2Idx
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
        let t2Size = smallFont.MeasureString t2Name
        drawText smallFont sb t2Name (Vector2(width - t2Size.X - 10.0f * sx, hudY + 4.0f)) team2Color
        let s2Str = $"{gs.Team2Score}"
        let s2Size = font.MeasureString s2Str
        drawText font sb s2Str (Vector2(width - s2Size.X - 10.0f * sx, hudY + 4.0f + fontSize * 1.1f)) team2Color

        // Clock (center) — counts DOWN to the period end
        let secs = max 0 (int gs.PeriodLength - int gs.ClockSeconds)
        let clockStr = $"{secs / 60}:{secs % 60:D2}"
        drawCentered sb font width (hudY + 4.0f) clockStr hudText

        // Period info ("FINAL RESULT" once the match is over)
        let periodStr =
            if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then "FINAL RESULT"
            elif gs.NumPeriods > 1 then $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
            else ""

        if periodStr <> "" then
            drawCentered sb smallFont width (hudY + 4.0f + fontSize * 1.1f) periodStr hudText
    | _ -> ()

// ─── Pause Overlay ────────────────────────────────────────────────────

let drawPauseOverlay (sb: SpriteBatch) (width: float32) (height: float32) =
    fillRect sb 0.0f 0.0f width height (Color(0, 0, 0, 140))
    let scale = min (width / OrigW) (height / OrigH)

    match getFont (max 10.0f (14.0f * scale)), getFont (max 7.0f (6.0f * scale)) with
    | Some font, Some small ->
        drawCentered sb font width (height * 0.42f) "PAUSED" goalFlashColor
        drawCentered sb small width (height * 0.42f + 40.0f * scale) "Press P to continue" grayColor
    | _ -> ()

// ─── Quit-the-league Prompt ───────────────────────────────────────────

/// Over a league screen: Escape asked to leave the league, whose standings
/// would be lost with it
let drawQuitLeaguePrompt (sb: SpriteBatch) (width: float32) (height: float32) =
    fillRect sb 0.0f 0.0f width height (Color(0, 0, 0, 170))
    let scale = min (width / OrigW) (height / OrigH)

    match getFont (max 10.0f (14.0f * scale)), getFont (max 7.0f (6.0f * scale)) with
    | Some font, Some small ->
        drawCentered sb font width (height * 0.40f) "QUIT THE LEAGUE?" goalFlashColor
        drawCentered sb small width (height * 0.40f + 40.0f * scale) "The standings so far will be lost" grayColor
        drawCentered sb small width (height * 0.40f + 58.0f * scale) "Y: quit the league    N / Esc: keep playing" grayColor
    | _ -> ()

// ─── Period Start Banner ──────────────────────────────────────────────

let drawPeriodFlash (sb: SpriteBatch) (gs: GameState) (width: float32) (height: float32) =
    if gs.PeriodFlashTimer > 0<tick> && gs.GoalFlashTimer <= 0<tick> then
        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0f (20.0f * scale)

        match getFont fontSize with
        | Some font ->
            let banner = $"PERIOD {gs.CurrentPeriod + 1}"
            let strSize = font.MeasureString banner
            let tx = (width - strSize.X) / 2.0f
            let ty = (height - strSize.Y) / 2.0f - 20.0f
            drawText font sb banner (Vector2(tx + 2.0f, ty + 2.0f)) (Color(0, 0, 0, 180))
            drawText font sb banner (Vector2(tx, ty)) goalFlashColor
        | None -> ()

// ─── Goal Flash Overlay ───────────────────────────────────────────────

let drawGoalFlash (sb: SpriteBatch) (gs: GameState) width height =
    if gs.GoalFlashTimer > 0<tick> then
        let alpha = 60 * int gs.GoalFlashTimer / 90
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
            let strSize = font.MeasureString goalStr
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
            match sign (compare gs.Team1Score gs.Team2Score) with
            | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
            | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
            | _ -> "IT'S A TIE!"

        drawCentered sb medFont width (height * 0.52f) winner goalFlashColor

        let instrStr =
            if leagueMode then
                "Press SPACE for standings"
            else
                "Press SPACE for main menu"

        drawCentered sb smallFont width (height * 0.72f) instrStr (Color(180, 180, 180))
    | _ -> ()

// ─── League Matchup Screen ────────────────────────────────────────────

let drawLeagueMatchup (sb: SpriteBatch) width height roundNum totalRounds (team1Idx: int) (team2Idx: int) fastHuman hardMode =
    fillRect sb 0.0f 0.0f width height (Color(10, 10, 30))

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    match getFont bigSize, getFont medSize, getFont smallSize with
    | Some bigFont, Some medFont, Some smallFont ->
        drawCentered sb bigFont width (height * 0.12f) "LEAGUE MODE" goalFlashColor
        drawCentered sb medFont width (height * 0.28f) $"ROUND {roundNum} of {totalRounds}" Color.White
        // Team name in its colour, then its skating speed dimmed as in the
        // menu, centred together; the smaller hint sits on the name's baseline
        let drawTeam (y: float32) (teamIdx: int) (color: Color) =
            let name = teamNames.[teamIdx]
            let hint = $"  (speed {displayedTeamSpeed fastHuman hardMode teamIdx})"
            let nameW = medFont.MeasureString(name).X
            let x = (width - nameW - smallFont.MeasureString(hint).X) / 2.0f
            drawText medFont sb name (Vector2(x, y)) color
            drawText smallFont sb hint (Vector2(x + nameW, y + (medSize - smallSize) * 0.85f)) speedHintColor

        drawTeam (height * 0.42f) team1Idx (teamColor false fastHuman hardMode team1Idx)
        drawCentered sb smallFont width (height * 0.52f) "vs" grayColor
        drawTeam (height * 0.60f) team2Idx (teamColor true fastHuman hardMode team2Idx)
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

        [| "TEAM"; "W"; "L"; "D"; "GF"; "GA"; "PTS" |]
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
               $"{stats.GoalsFor}"
               $"{stats.GoalsAgainst}"
               $"{stats.Points}" |]
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

// The menu is laid out as data — frames, texts and buttons on a 640x420
// virtual screen, scaled to fit and centred — so drawing and mouse
// hit-testing share the same rectangles. Each button carries the action it
// runs (the same actions the menu keys run in Program.fs).

/// Menu text sizes, in virtual pixels.
type MenuTextSize =
    | Title
    | Heading
    | Body
    | Fine

type MenuButtonKind =
    /// Team row in a column list (selected = yellow on blue)
    | TeamRow of selected: bool
    | Exhibition
    | League
    /// On/off option (on = blue)
    | Toggle of on: bool

type MenuElement =
    | Frame of rect: Rectangle * fill: Color * border: Color
    | Text of x: float32 * y: float32 * text: string * color: Color * size: MenuTextSize * centred: bool
    | MenuButton of rect: Rectangle * text: string * hint: string * action: string * kind: MenuButtonKind

[<Literal>]
let private MenuW = 640.0f

[<Literal>]
let private MenuH = 420.0f

/// Virtual-to-screen scale and offset of the menu.
let private menuTransform (width: float32) (height: float32) =
    let s = min (width / MenuW) (height / MenuH)
    struct (s, (width - MenuW * s) / 2.0f, (height - MenuH * s) / 2.0f)

let private menuGray = Color(140, 140, 140)
let private menuBlue = Color(144, 192, 255)
let private leagueGold = Color(255, 220, 80)

/// The menu screen, in virtual pixels.
let menuLayout selectedTeam1 selectedTeam2 activeColumn fastHuman hardMode fivePlayer gamepadOn : MenuElement list =
    let rowH = 16
    let listY = 92
    let column (col: int) (x: int) (selected: int) =
        [ if activeColumn = col then
              yield Frame(Rectangle(x - 6, 62, 302, listY - 62 + NumTeams * rowH + 6), Color(255, 255, 255, 8), Color(58, 58, 88))
          let head = if col = 0 then "TEAM 1 (LEFT)" else "TEAM 2 (RIGHT)"
          yield Text(float32 x, 68.0f, head, teamColor (col = 1) fastHuman hardMode selected, Heading, false)
          for i in 0 .. NumTeams - 1 do
              let speed = $"speed {displayedTeamSpeed fastHuman hardMode i}"
              yield MenuButton(Rectangle(x, listY + i * rowH, 290, rowH - 1), teamNames[i], speed, $"pick:{col}:{i}", TeamRow(i = selected)) ]
    let toggles =
        [ (if fivePlayer then "6 v 6" else "3 v 3"), "5", "players", true
          "Fast human", "F", "fast", fastHuman
          "Hard CPU", "H", "hard", hardMode
          "Gamepads", "G", "gamepad", gamepadOn ]
    let tableY = 332.0f
    let cell (x: float32) (row: int) (text: string) (color: Color) = Text(x, tableY + float32 row * 11.0f, text, color, Fine, false)
    [ yield Text(MenuW / 2.0f, 14.0f, "THE FS HOCKEY LEAGUE", goalFlashColor, Title, true)
      yield Text(MenuW / 2.0f, 44.0f, "Tuomas Hietanen, 2026", menuGray, Fine, true)
      yield! column 0 26 selectedTeam1
      yield! column 1 324 selectedTeam2
      yield MenuButton(Rectangle(20, 262, 294, 30), "EXHIBITION", "Enter", "exhibition", Exhibition)
      yield MenuButton(Rectangle(326, 262, 294, 30), "LEAGUE", "L", "league", League)
      for k, (text, key, action, on) in List.indexed toggles do
          yield MenuButton(Rectangle(20 + k * 152, 300, 144, 22), text, key, action, Toggle on)
      yield cell 150.0f 0 "Skate" menuGray
      yield cell 330.0f 0 "Shoot" menuGray
      for r, (who, skate, shoot) in List.indexed [ "Player 1", "Arrow keys", "RShift / Enter"; "Player 2", "W A S D", "Space / Tab"; "Gamepads", "Stick / D-pad", "A / B / RT" ] do
          yield cell 40.0f (r + 1) who Color.White
          yield cell 150.0f (r + 1) skate Color.White
          yield cell 330.0f (r + 1) shoot Color.White
      yield Text(MenuW / 2.0f, 384.0f, "Pick HUMAN PLAYER for keyboard control - hold shoot for a harder shot", menuGray, Fine, true)
      yield Text(MenuW / 2.0f, 398.0f, "Up/Down select - Tab switch column - P pause - Esc quit - F11 fullscreen", menuGray, Fine, true) ]

/// The action of the menu button under a screen-pixel point, if any.
let menuHit (elements: MenuElement list) (width: float32) (height: float32) (screen: Vector2) : string option =
    let struct (s, ox, oy) = menuTransform width height
    let p = Point(int ((screen.X - ox) / s), int ((screen.Y - oy) / s))
    elements |> List.tryPick (function
        | MenuButton(r, _, _, action, _) when r.Contains p -> Some action
        | _ -> None)

let drawMenu (sb: SpriteBatch) width height (elements: MenuElement list) (mouse: Vector2) =
    fillRect sb 0.0f 0.0f width height (Color(10, 10, 30))
    let struct (s, ox, oy) = menuTransform width height
    let fonts = [ Title, 26.0f; Heading, 15.0f; Body, 13.0f; Fine, 10.0f ]
    let font size = getFont (max 6.0f (s * (fonts |> List.find (fun (k, _) -> k = size) |> snd)))
    let rx (r: Rectangle) = struct (ox + float32 r.X * s, oy + float32 r.Y * s, float32 r.Width * s, float32 r.Height * s)
    let hover (r: Rectangle) =
        let struct (x, y, w, h) = rx r
        mouse.X >= x && mouse.X < x + w && mouse.Y >= y && mouse.Y < y + h
    for e in elements do
        match e with
        | Frame(r, fill, border) ->
            let struct (x, y, w, h) = rx r
            fillRect sb x y w h fill
            drawRect sb x y w h (max 1.0f s) border
        | Text(x, y, text, color, size, centred) ->
            match font size with
            | Some f ->
                let sz = f.MeasureString text
                let px = if centred then ox + x * s - sz.X / 2.0f else ox + x * s
                drawText f sb text (Vector2(px, oy + y * s)) color
            | None -> ()
        | MenuButton(r, text, hint, _, kind) ->
            let struct (x, y, w, h) = rx r
            let lit = hover r
            let fill, border, ink =
                match kind with
                | TeamRow true -> Color(42, 58, 88), Color(42, 58, 88), goalFlashColor
                | TeamRow false -> (if lit then Color(255, 255, 255, 18) else Color.Transparent), Color.Transparent, Color.White
                | Exhibition -> Color(140, 184, 255), Color(192, 216, 255), Color(16, 24, 48)
                | League -> leagueGold, leagueGold, Color(48, 36, 0)
                | Toggle on -> Color(38, 38, 58), (if on then menuBlue else Color(80, 80, 112)), (if on then menuBlue else Color(224, 224, 232))
            let fill =
                match kind with
                | TeamRow _ -> fill
                | _ when lit -> Color.Lerp(fill, Color.White, 0.15f)
                | _ -> fill
            if fill.A > 0uy then fillRect sb x y w h fill
            if border.A > 0uy then drawRect sb x y w h (max 1.0f s) border
            match kind with
            | TeamRow _ ->
                // Name on the left, the dimmed speed hint on the right
                match font Body with
                | Some f ->
                    let ty = y + (h - f.MeasureString(text).Y) / 2.0f
                    drawText f sb text (Vector2(x + 6.0f * s, ty)) ink
                    let hw = f.MeasureString(hint).X
                    drawText f sb hint (Vector2(x + w - hw - 6.0f * s, ty)) speedHintColor
                | None -> ()
            | _ ->
                let big = match kind with Exhibition | League -> Heading | TeamRow _ | Toggle _ -> Body
                match font big, font Fine with
                | Some f, Some fh ->
                    let tw = f.MeasureString text
                    let kw = fh.MeasureString(hint).X + 6.0f * s
                    let tx = x + (w - tw.X - kw) / 2.0f
                    drawText f sb text (Vector2(tx, y + (h - tw.Y) / 2.0f)) ink
                    let hintInk = match kind with Exhibition | League -> Color(ink, 0.7f) | TeamRow _ | Toggle _ -> menuGray
                    drawText fh sb hint (Vector2(tx + tw.X + 6.0f * s, y + (h - fh.MeasureString(hint).Y) / 2.0f)) hintInk
                | _ -> ()

// ─── Main Render ──────────────────────────────────────────────────────

/// `fastHuman` and `hardMode` set the speed class colours of the teams.
let renderFrame (sb: SpriteBatch) (gs: GameState) width height leagueMode fastHuman hardMode =
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

    fillRect sb 0.0f 0.0f w h bgColor

    // HUD is anchored to the bottom edge; the rink is centered in the space
    // above it (horizontally, and vertically on very wide windows). The rink
    // content is drawn in its own batch translated by (offX, offY).
    let hudH = HudHeight * sy
    let offX = (w - OrigW * sx) / 2.0f
    let offY = max 0.0f ((h - hudH - 2.0f - rinkGameH * sy) / 2.0f)

    sb.End()

    sb.Begin(
        SpriteSortMode.Deferred,
        BlendState.NonPremultiplied,
        null,
        null,
        null,
        null,
        System.Nullable(Matrix.CreateTranslation(offX, offY, 0.0f))
    )

    let team1Color = teamColor false fastHuman hardMode gs.Team1Idx
    let team2Color = teamColor true fastHuman hardMode gs.Team2Idx
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
    drawPuck sb sx sy gs.Entities.[gs.PuckIdx] gs.PuckAnimFrame

    // Team 1 players
    for i in 0 .. ppt - 1 do
        let isGoalie = gs.FivePlayerMode && i = 0

        drawRetroPlayer
            sb
            sx
            sy
            offX
            offY
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
            offX
            offY
            gs.Entities.[ei]
            team2Color
            t2Helmet
            (ei = gs.ActivePlayer2)
            gs.StickAnimTimers.[ei]
            isGoalie
            (int gs.GameTick)

    // Back to screen space for the HUD and overlays
    sb.End()
    sb.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)

    // HUD (spans the full width, anchored to the bottom edge)
    let rinkBottom = h - hudH - 2.0f
    drawHud sb gs sx sy rinkBottom w fastHuman hardMode

    // Overlays
    drawGoalFlash sb gs w h
    drawPeriodFlash sb gs w h

    if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
        drawGameOver sb gs w h leagueMode
