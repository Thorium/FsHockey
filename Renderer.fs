/// THE FS HOCKEY LEAGUE — Renderer (Mibo)
/// Declarative frame building with Mibo's fluent 2D draw DSL:
/// ice rink, players, puck, HUD, menu and league screens.
module HockeyDemo.Renderer

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo
open Mibo.Elmish
open Mibo.Elmish.Graphics
open Mibo.Elmish.Graphics2D
open HockeyDemo.Physics
open HockeyDemo.Game

// ─── Scale / Layout ────────────────────────────────────────────────────

[<Literal>]
let OrigW = 320.0f

[<Literal>]
let OrigH = 200.0f

[<Literal>]
let HudHeight = 48.0f

// ─── Render Layers ─────────────────────────────────────────────────────
// Mibo sorts draw commands by layer (insertion order within a layer), so
// the frame can be described declaratively instead of in paint order.

let private LBg = 0<RenderLayer>
let private LRink = 1<RenderLayer>
let private LTrail = 2<RenderLayer>
let private LPuck = 3<RenderLayer>
let private LPlayer = 4<RenderLayer>
let private LMarker = 5<RenderLayer>
let private LHud = 6<RenderLayer>
let private LOverlay = 7<RenderLayer>

// ─── Colors (CGA-inspired) ────────────────────────────────────────────

let iceColor = Color.rgb 200uy 220uy 240uy
let boardColor = Color.rgb 60uy 80uy 120uy
let lineColor = Color.rgb 180uy 40uy 40uy
let blueLineColor = Color.rgb 40uy 80uy 180uy
let team1Color = Color.rgb 220uy 60uy 60uy
let team2Color = Color.rgb 60uy 100uy 220uy
let puckColor = Color.rgb 20uy 20uy 20uy
let puckHighlight = Color.rgb 60uy 60uy 60uy
let hudBg = Color.rgb 20uy 20uy 40uy
let hudText = Color.rgb 220uy 220uy 220uy
let goalFlashColor = Color.rgb 255uy 255uy 80uy
let activeMarkerColor = Color.rgb 90uy 255uy 120uy // green caret over the controlled player
let stickBrown = Color.rgb 139uy 90uy 43uy
let stickTape = Color.rgb 240uy 240uy 240uy
let helmetBlack = Color.rgb 30uy 30uy 30uy
let helmetGold = Color.rgb 200uy 180uy 40uy
let trouserColor = Color.rgb 30uy 30uy 30uy
let skateColor = Color.rgb 80uy 80uy 80uy
let goaliePadColor = Color.rgb 230uy 220uy 200uy
let goalieMaskColor = Color.rgb 220uy 220uy 220uy
let skinColor = Color.rgb 230uy 195uy 160uy
let gloveColor = Color.rgb 60uy 60uy 60uy
let sockColor = Color.rgb 200uy 200uy 210uy
let bgColor = Color.rgb 30uy 30uy 50uy
let grayColor = Color.rgb 160uy 160uy 160uy
let private white = Color.White

// ─── Drawing Helpers ──────────────────────────────────────────────────

let inline private v2 (x: float32) (y: float32) = System.Numerics.Vector2(x, y)

/// Scale game X-coordinate to screen
let inline gameX (sx: float32) (x: float<px>) = float32 (stripPx x) * sx

/// Scale game Y-coordinate to screen
let inline gameY (sy: float32) (y: float<px>) = float32 (stripPx y) * sy

let private fillRect (b: RenderBuffer2D) (x: float32) (y: float32) (w: float32) (h: float32) (color: Color) layer =
    b.fillRect(x, y, w, h, color, layer = layer) |> ignore

// Rect outlines are drawn as four fillRects rather than `.rectOutline`:
// fillRect and text render through the SpriteBatch while outlines/lines/
// triangles go through the PrimitiveBatch, and the two batches only keep
// relative order at flush boundaries. Keeping UI-phase drawing pure
// SpriteBatch preserves exact insertion-order compositing.
let private drawRect (b: RenderBuffer2D) (x: float32) (y: float32) (w: float32) (h: float32) (thickness: float32) (color: Color) layer =
    b.fillRect(x, y, w, thickness, color, layer = layer)
     .fillRect(x, y + h - thickness, w, thickness, color, layer = layer)
     .fillRect(x, y, thickness, h, color, layer = layer)
     .fillRect(x + w - thickness, y, thickness, h, color, layer = layer)
    |> ignore

let private drawLine (b: RenderBuffer2D) (x1: float32) (y1: float32) (x2: float32) (y2: float32) (thickness: float32) (color: Color) layer =
    b.lineThick(v2 x1 y1, v2 x2 y2, color, thickness = thickness, layer = layer) |> ignore

let private fillCircle (b: RenderBuffer2D) (cx: float32) (cy: float32) (r: float32) (color: Color) layer =
    b.fillCircle(v2 cx cy, r, color, layer = layer) |> ignore

/// Draw a dashed line.
let private drawDashedLine (b: RenderBuffer2D) (x1: float32) (y1: float32) (x2: float32) (y2: float32) (thickness: float32) (dashLen: float32) (color: Color) layer =
    let dx = x2 - x1
    let dy = y2 - y1
    let length = sqrt (dx * dx + dy * dy)

    if length > 0.001f then
        let nx = dx / length
        let ny = dy / length
        let mutable t = 0.0f
        let mutable draw = true

        while t < length do
            let segLen = min dashLen (length - t)

            if draw then
                drawLine b (x1 + nx * t) (y1 + ny * t) (x1 + nx * (t + segLen)) (y1 + ny * (t + segLen)) thickness color layer

            t <- t + dashLen
            draw <- not draw

// ─── Text ─────────────────────────────────────────────────────────────
// The spritefont is baked at 32 px; the DSL's `size` argument is a uniform
// scale on the MonoGame backend, so a pixel size maps to size/32.

[<Literal>]
let private BakedFontPx = 32.0f

let private drawText (b: RenderBuffer2D) (font: SpriteFont) (sizePx: float32) (x: float32) (y: float32) (text: string) (color: Color) layer =
    b.text(font, text, v2 x y, sizePx / BakedFontPx, tint = color, layer = layer) |> ignore

/// Measured (width, height) of text at the given pixel size
let private measureText (font: SpriteFont) (sizePx: float32) (text: string) =
    let s = font.MeasureString(text)
    let k = sizePx / BakedFontPx
    struct (s.X * k, s.Y * k)

/// Draw a string centered horizontally at the given Y position
let private drawCentered (b: RenderBuffer2D) (font: SpriteFont) (sizePx: float32) (width: float32) (y: float32) (text: string) (color: Color) layer =
    let struct (tw, _) = measureText font sizePx text
    drawText b font sizePx ((width - tw) / 2.0f) y text color layer

/// Font pixel sizes at a given scale factor (same numeric values as the
/// original GDI+ point sizes).
let private mkFonts (scale: float32) =
    let big = max 8.0f (9.0f * scale)
    let med = max 7.0f (6.0f * scale)
    let small = max 6.0f (5.0f * scale)
    struct (big, med, small)

// ─── Draw Rink ────────────────────────────────────────────────────────

let drawRink (b: RenderBuffer2D) sx sy (offX: float32) (offY: float32) (leftGoalColor: Color) (rightGoalColor: Color) =
    let rinkW = OrigW * sx
    let rinkH = gameY sy FieldBottom + 4.0f * sy
    let fl, fr = offX + gameX sx FieldLeft, offX + gameX sx FieldRight
    let ft, fb = offY + gameY sy FieldTop, offY + gameY sy FieldBottom
    let gt, gb = offY + gameY sy GoalTop, offY + gameY sy GoalBottom
    let gd = float32 (stripPx GoalDepth) * sx
    let cx = offX + gameX sx CenterX
    let cy = offY + gameY sy CenterY

    // Ice surface
    fillRect b offX offY rinkW rinkH iceColor LRink

    // Board outline
    drawRect b fl ft (fr - fl) (fb - ft) 3.0f boardColor LRink

    // Goal nets
    let drawGoalNet x (color: Color) =
        let netColor = Color.create color.R color.G color.B 60uy
        fillRect b x gt gd (gb - gt) netColor LRink
        drawRect b x gt gd (gb - gt) 2.0f color LRink

    drawGoalNet (fl - gd) leftGoalColor
    drawGoalNet fr rightGoalColor

    // Center line + circle
    drawLine b cx ft cx fb 1.5f lineColor LRink
    let circR = 20.0f * sx
    fillCircle b cx cy circR lineColor LRink
    fillCircle b cx cy (circR - 1.5f) iceColor LRink

    // Center dot
    fillCircle b cx cy 3.0f lineColor LRink

    // Blue lines (1/3 and 2/3 of field width)
    let fieldW = stripPx FieldRight - stripPx FieldLeft
    let bl1 = offX + gameX sx (FieldLeft + fieldW / 3.0 * 1.0<px>)
    let bl2 = offX + gameX sx (FieldLeft + fieldW / 3.0 * 2.0<px>)
    drawLine b bl1 ft bl1 fb 2.0f blueLineColor LRink
    drawLine b bl2 ft bl2 fb 2.0f blueLineColor LRink

    // Goal lines (red dashed)
    let glx = offX + gameX sx GoalLeftX
    let grx = offX + gameX sx GoalRightX
    drawDashedLine b glx ft glx fb 1.0f 4.0f lineColor LRink
    drawDashedLine b grx ft grx fb 1.0f 4.0f lineColor LRink

// ─── Draw Retro Hockey Player ──────────────────────────────────────────

let drawRetroPlayer (b: RenderBuffer2D) sx sy (offX: float32) (offY: float32) (ent: Entity) (jerseyColor: Color) (helmetColor: Color) isActive (stickAnim: int) isGoalie (gameTick: int) =
    let px = offX + gameX sx ent.X
    let py = offY + gameY sy ent.Y
    let u = 0.85f * sx
    let uy = 0.85f * sy

    // Rotation: face direction of DirX/DirY. Without SpriteBatch transform
    // matrices, the body parts are rotated point-by-point around the entity
    // center: rects become two triangles, lines rotate their endpoints.
    let angle =
        if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
            float32 (System.Math.Atan2(float ent.DirX, -(float ent.DirY)))
        else
            0.0f

    let sinA = sin angle
    let cosA = cos angle

    let rot (x: float32) (y: float32) =
        let dx = x - px
        let dy = y - py
        v2 (px + dx * cosA - dy * sinA) (py + dx * sinA + dy * cosA)

    /// Axis-aligned rect in body space, rotated around the entity center
    let rectR (x: float32) (y: float32) (w: float32) (h: float32) (color: Color) =
        b.triangle(rot x y, rot (x + w) y, rot (x + w) (y + h), color, layer = LPlayer)
         .triangle(rot x y, rot (x + w) (y + h), rot x (y + h), color, layer = LPlayer)
        |> ignore

    let rectOutlineR (x: float32) (y: float32) (w: float32) (h: float32) (thickness: float32) (color: Color) =
        rectR x y w thickness color
        rectR x (y + h - thickness) w thickness color
        rectR x y thickness h color
        rectR (x + w - thickness) y thickness h color

    let lineR (x1: float32) (y1: float32) (x2: float32) (y2: float32) (thickness: float32) (color: Color) =
        b.lineThick(rot x1 y1, rot x2 y2, color, thickness = thickness, layer = LPlayer)
        |> ignore

    // Skating leg animation — slow oscillation to look like skating, not running
    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY

    let legOffset =
        if speedSq > 16.0 then
            sin (float32 gameTick * 0.08f) * 1.2f * uy * 0.3f
        else
            0.0f

    // ─── Helmet (head)
    rectR (px - 1.5f * u) (py - 5.5f * uy) (3.0f * u) (2.0f * uy) helmetColor

    // Face area (skin visible below helmet)
    rectR (px - 1.0f * u) (py - 3.5f * uy) (2.0f * u) (1.0f * uy) skinColor

    // Goalie face mask
    if isGoalie then
        let maskX = px + 0.3f * u
        rectR maskX (py - 4.5f * uy) (1.2f * u) (1.5f * uy) goalieMaskColor
        let cagePen = Color.rgb 100uy 100uy 100uy
        let cageW = max 0.5f (0.3f * u)
        let cx0 = maskX + 0.3f * u
        let cx1 = maskX + 0.9f * u
        lineR cx0 (py - 4.5f * uy) cx0 (py - 3.0f * uy) cageW cagePen
        lineR cx1 (py - 4.5f * uy) cx1 (py - 3.0f * uy) cageW cagePen

    // ─── Jersey (body)
    // Shoulders
    rectR (px - 3.5f * u) (py - 2.5f * uy) (7.0f * u) (1.5f * uy) jerseyColor
    // Torso
    rectR (px - 3.0f * u) (py - 1.0f * uy) (6.0f * u) (2.5f * uy) jerseyColor
    // Jersey stripe
    let stripeColor = Color.create 255uy 255uy 255uy 80uy
    rectR (px - 3.0f * u) (py - 0.5f * uy) (6.0f * u) (0.6f * uy) stripeColor

    // ─── Arms / Gloves
    // Left arm
    rectR (px - 4.0f * u) (py - 2.0f * uy) (1.2f * u) (2.0f * uy) jerseyColor
    rectR (px - 4.0f * u) (py + 0.0f * uy) (1.2f * u) (0.8f * uy) gloveColor
    // Right arm
    rectR (px + 2.8f * u) (py - 2.0f * uy) (1.2f * u) (2.0f * uy) jerseyColor
    rectR (px + 2.8f * u) (py + 0.0f * uy) (1.2f * u) (0.8f * uy) gloveColor

    // ─── Trousers / Goalie pads
    if isGoalie then
        // Hips
        rectR (px - 3.5f * u) (py + 1.5f * uy) (7.0f * u) (1.2f * uy) goaliePadColor
        // Leg pads
        rectR (px - 3.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) goaliePadColor
        rectR (px + 0.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) goaliePadColor
        // Pad outlines
        let outlineColor = Color.rgb 160uy 150uy 130uy
        let outlineW = max 1.0f (0.4f * u)
        rectOutlineR (px - 3.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) outlineW outlineColor
        rectOutlineR (px + 0.5f * u) (py + 2.7f * uy) (3.0f * u) (2.0f * uy) outlineW outlineColor
    else
        // Hips
        rectR (px - 3.0f * u) (py + 1.5f * uy) (6.0f * u) (1.2f * uy) trouserColor
        // Left leg (animated)
        rectR (px - 2.5f * u) (py + 2.7f * uy + legOffset) (2.2f * u) (1.0f * uy) trouserColor
        // Right leg (animated opposite)
        rectR (px + 0.3f * u) (py + 2.7f * uy - legOffset) (2.2f * u) (1.0f * uy) trouserColor
        // Socks
        rectR (px - 2.2f * u) (py + 3.5f * uy + legOffset) (1.8f * u) (0.5f * uy) sockColor
        rectR (px + 0.4f * u) (py + 3.5f * uy - legOffset) (1.8f * u) (0.5f * uy) sockColor

    // ─── Skate blades
    let skateW = max 1.0f (0.5f * u)

    if isGoalie then
        let skateY = py + 4.8f * uy
        lineR (px - 2.0f * u) skateY (px - 0.5f * u) skateY skateW skateColor
        lineR (px + 0.5f * u) skateY (px + 2.0f * u) skateY skateW skateColor
    else
        let skateYL = py + 4.0f * uy + legOffset
        let skateYR = py + 4.0f * uy - legOffset
        lineR (px - 2.0f * u) skateYL (px - 0.3f * u) skateYL skateW skateColor
        lineR (px + 0.3f * u) skateYR (px + 2.0f * u) skateYR skateW skateColor

    // ─── Stick
    let shaftWidth = max 1.5f (1.2f * u)

    let wobble =
        if stickAnim > 0 then
            sin (float32 stickAnim * 1.5f) * 2.5f * u
        else
            0.0f

    // Stick always points forward (faceDir=1) since the whole body rotates
    let faceDir = 1.0f

    let stickLen = 7.0f * u
    let startX = px + faceDir * 2.0f * u
    let startY = py - 0.5f * uy
    let endX = startX + faceDir * 2.5f * u
    let endY = startY - stickLen + wobble

    lineR startX startY endX endY shaftWidth stickBrown

    // Tape on handle
    let tapeFrac = 0.18f
    let tapeEndX = startX + (endX - startX) * tapeFrac
    let tapeEndY = startY + (endY - startY) * tapeFrac
    lineR startX startY tapeEndX tapeEndY (shaftWidth + 0.5f) stickTape

    // Blade
    let bladeLen = 2.5f * u
    let bladeW = max 2.0f (1.4f * u)
    let bladeEndX = endX + faceDir * bladeLen
    let bladeEndY = endY - 0.8f * uy
    lineR endX endY bladeEndX bladeEndY bladeW stickBrown

    // ─── Active player marker (drawn in rink space, NOT rotated)
    if isActive then
        let my = py - 6.5f * uy
        let ms = 2.0f * sx
        let markerW = max 1.0f (1.2f * sx)
        drawLine b (px - ms) (my - ms) px my markerW activeMarkerColor LMarker
        drawLine b px my (px + ms) (my - ms) markerW activeMarkerColor LMarker

// ─── Draw Puck ────────────────────────────────────────────────────────

let drawPuck (b: RenderBuffer2D) sx sy (offX: float32) (offY: float32) (puck: Entity) (animFrame: int) =
    let px = offX + gameX sx puck.X
    let py = offY + gameY sy puck.Y
    let r = 2.5f * sx

    fillCircle b px py r puckColor LPuck

    // Spinning highlight: orbits the puck center once per animation cycle
    let phase = float32 animFrame / float32 (PuckAnimFrames * 2) * (2.0f * float32 System.Math.PI)
    let hr = r * 0.4f
    let orbit = r * 0.35f
    let hx = px + cos phase * orbit
    let hy = py - 0.5f + sin phase * orbit
    fillCircle b hx hy hr puckHighlight LPuck

// ─── Draw HUD ─────────────────────────────────────────────────────────

let drawHud (b: RenderBuffer2D) (font: SpriteFont) (gs: GameState) sx sy (rinkBottom: float32) (width: float32) =
    let hudY = rinkBottom + 2.0f
    let hudH = HudHeight * sy

    fillRect b 0.0f hudY width hudH hudBg LHud
    // SpriteBatch fill, not a primitive line — keeps the HUD phase batch-pure
    fillRect b 0.0f (hudY - 1.0f) width 2.0f boardColor LHud

    let fontSize = max 12.0f (12.0f * min sx sy)
    let smallSize = fontSize * 0.75f

    // Team 1 name + score (left)
    drawText b font smallSize (10.0f * sx) (hudY + 4.0f) teamNames.[gs.Team1Idx] team1Color LHud
    drawText b font fontSize (10.0f * sx) (hudY + 4.0f + fontSize * 1.1f) $"{gs.Team1Score}" team1Color LHud

    // Team 2 name + score (right)
    let t2Name = teamNames.[gs.Team2Idx]
    let struct (t2W, _) = measureText font smallSize t2Name
    drawText b font smallSize (width - t2W - 10.0f * sx) (hudY + 4.0f) t2Name team2Color LHud
    let s2Str = $"{gs.Team2Score}"
    let struct (s2W, _) = measureText font fontSize s2Str
    drawText b font fontSize (width - s2W - 10.0f * sx) (hudY + 4.0f + fontSize * 1.1f) s2Str team2Color LHud

    // Clock (center) — counts DOWN to the period end
    let secs = max 0 (int gs.PeriodLength - int gs.ClockSeconds)
    let clockStr = $"{secs / 60}:{secs % 60:D2}"
    drawCentered b font fontSize width (hudY + 4.0f) clockStr hudText LHud

    // Period info ("FINAL RESULT" once the match is over)
    let periodStr =
        if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then "FINAL RESULT"
        elif gs.NumPeriods > 1 then $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
        else ""

    if periodStr <> "" then
        drawCentered b font smallSize width (hudY + 4.0f + fontSize * 1.1f) periodStr hudText LHud

// ─── Pause Overlay ────────────────────────────────────────────────────

let drawPauseOverlay (b: RenderBuffer2D) (font: SpriteFont) (width: float32) (height: float32) =
    fillRect b 0.0f 0.0f width height (Color.create 0uy 0uy 0uy 140uy) LOverlay
    let scale = min (width / OrigW) (height / OrigH)
    drawCentered b font (max 10.0f (14.0f * scale)) width (height * 0.42f) "PAUSED" goalFlashColor LOverlay
    drawCentered b font (max 7.0f (6.0f * scale)) width (height * 0.42f + 40.0f * scale) "Press P to continue" grayColor LOverlay

// ─── Period Start Banner ──────────────────────────────────────────────

let drawPeriodFlash (b: RenderBuffer2D) (font: SpriteFont) (gs: GameState) (width: float32) (height: float32) =
    if gs.PeriodFlashTimer > 0<tick> && gs.GoalFlashTimer <= 0<tick> then
        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0f (20.0f * scale)
        let banner = $"PERIOD {gs.CurrentPeriod + 1}"
        let struct (tw, th) = measureText font fontSize banner
        let tx = (width - tw) / 2.0f
        let ty = (height - th) / 2.0f - 20.0f
        drawText b font fontSize (tx + 2.0f) (ty + 2.0f) banner (Color.create 0uy 0uy 0uy 180uy) LOverlay
        drawText b font fontSize tx ty banner goalFlashColor LOverlay

// ─── Goal Flash Overlay ───────────────────────────────────────────────

let drawGoalFlash (b: RenderBuffer2D) (font: SpriteFont) (gs: GameState) (width: float32) (height: float32) =
    if gs.GoalFlashTimer > 0<tick> then
        let alpha = 60 * int gs.GoalFlashTimer / 90
        fillRect b 0.0f 0.0f width height (Color.create goalFlashColor.R goalFlashColor.G goalFlashColor.B (byte alpha)) LOverlay

        let scale = min (width / OrigW) (height / OrigH)
        let fontSize = max 16.0f (20.0f * scale)

        let scorerName =
            match gs.GoalScoredBy with
            | Team1Scored -> teamNames.[gs.Team1Idx]
            | Team2Scored -> teamNames.[gs.Team2Idx]
            | NoGoal -> ""

        let goalStr = $"GOAL! {scorerName}"
        let struct (tw, th) = measureText font fontSize goalStr
        let tx = (width - tw) / 2.0f
        let ty = (height - th) / 2.0f - 20.0f

        drawText b font fontSize (tx + 2.0f) (ty + 2.0f) goalStr (Color.create 0uy 0uy 0uy 180uy) LOverlay
        drawText b font fontSize tx ty goalStr goalFlashColor LOverlay

        let scoreStr = $"{gs.Team1Score} - {gs.Team2Score}"
        drawCentered b font (fontSize * 0.7f) width (ty + th + 4.0f) scoreStr white LOverlay

// ─── Game Over Screen ─────────────────────────────────────────────────

let drawGameOver (b: RenderBuffer2D) (font: SpriteFont) (gs: GameState) (width: float32) (height: float32) leagueMode =
    fillRect b 0.0f 0.0f width height (Color.create 0uy 0uy 0uy 160uy) LOverlay

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    drawCentered b font bigSize width (height * 0.25f) "GAME OVER" goalFlashColor LOverlay

    let scoreStr =
        $"{teamNames.[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames.[gs.Team2Idx]}"

    drawCentered b font medSize width (height * 0.40f) scoreStr white LOverlay

    let winner =
        match sign (compare gs.Team1Score gs.Team2Score) with
        | 1 -> $"{teamNames.[gs.Team1Idx]} WINS!"
        | -1 -> $"{teamNames.[gs.Team2Idx]} WINS!"
        | _ -> "IT'S A TIE!"

    drawCentered b font medSize width (height * 0.52f) winner goalFlashColor LOverlay

    let instrStr =
        if leagueMode then
            "Press SPACE for standings"
        else
            "Press SPACE for main menu"

    drawCentered b font smallSize width (height * 0.72f) instrStr (Color.rgb 180uy 180uy 180uy) LOverlay

// ─── League Matchup Screen ────────────────────────────────────────────

let drawLeagueMatchup (b: RenderBuffer2D) (font: SpriteFont) (width: float32) (height: float32) roundNum totalRounds (team1Name: string) (team2Name: string) =
    fillRect b 0.0f 0.0f width height (Color.rgb 10uy 10uy 30uy) LBg

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    drawCentered b font bigSize width (height * 0.12f) "LEAGUE MODE" goalFlashColor LRink
    drawCentered b font medSize width (height * 0.28f) $"ROUND {roundNum} of {totalRounds}" white LRink
    drawCentered b font medSize width (height * 0.42f) team1Name team1Color LRink
    drawCentered b font smallSize width (height * 0.52f) "vs" grayColor LRink
    drawCentered b font medSize width (height * 0.60f) team2Name team2Color LRink
    drawCentered b font smallSize width (height * 0.80f) "Press SPACE to start match" grayColor LRink

// ─── League Standings Screen ──────────────────────────────────────────

let drawLeagueStandings (b: RenderBuffer2D) (font: SpriteFont) (width: float32) (height: float32) (standings: (int * TeamStats) array) isFinal humanTeam =
    fillRect b 0.0f 0.0f width height (Color.rgb 10uy 10uy 30uy) LBg

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale

    let title = if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS"
    drawCentered b font bigSize width (height * 0.04f) title goalFlashColor LRink

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
        drawText b font medSize hx headerY h grayColor LRink)

    // Separator (SpriteBatch fill, not a primitive line — see drawRect note)
    let sepY = headerY + medSize * 1.4f
    fillRect b tableX (sepY - 0.5f) (width - tableX * 2.0f) 1.0f (Color.rgb 60uy 80uy 120uy) LRink

    // Rows
    let dataY = sepY + 4.0f

    standings
    |> Array.iteri (fun rank (teamIdx, stats) ->
        let ry = dataY + float32 rank * rowH
        let isHuman = (teamIdx = humanTeam)

        if isHuman then
            fillRect b (tableX - 2.0f) (ry - 1.0f) (width - tableX * 2.0f + 4.0f) rowH (Color.rgb 30uy 50uy 80uy) LRink

        let textColor = if isHuman then goalFlashColor else white
        drawText b font smallSize tableX ry $"{rank + 1}." grayColor LTrail
        drawText b font smallSize (tableX + smallSize * 2.5f) ry teamNames.[teamIdx] textColor LTrail

        [| $"{stats.Wins}"
           $"{stats.Losses}"
           $"{stats.Draws}"
           $"{stats.Points}"
           $"{stats.GoalsFor}"
           $"{stats.GoalsAgainst}" |]
        |> Array.iteri (fun i v -> drawText b font smallSize (tableX + colPositions.[i]) ry v white LTrail))

    // Winner announcement
    if isFinal && standings.Length > 0 then
        let winnerIdx, winnerStats = standings.[0]

        let winnerStr =
            $"{teamNames.[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"

        drawCentered b font (max 14.0f (14.0f * scale)) width (height * 0.88f) winnerStr goalFlashColor LTrail

    let instrStr =
        if isFinal then
            "Press SPACE to return to menu"
        else
            "Press SPACE to continue"

    drawCentered b font smallSize width (height * 0.94f) instrStr grayColor LTrail

// ─── Menu Screen ──────────────────────────────────────────────────────

let drawMenu (b: RenderBuffer2D) (font: SpriteFont) (width: float32) (height: float32) selectedTeam1 selectedTeam2 activeColumn fastHuman hardMode fivePlayer gamepadOn =
    fillRect b 0.0f 0.0f width height (Color.rgb 10uy 10uy 30uy) LBg

    let scale = min (width / OrigW) (height / OrigH)
    let struct (bigSize, medSize, smallSize) = mkFonts scale
    let menuGray = Color.rgb 140uy 140uy 140uy

    // Title + subtitle
    drawCentered b font bigSize width (height * 0.06f) "THE FS HOCKEY LEAGUE" goalFlashColor LRink

    let sub = "Tuomas Hietanen, 2026"
    drawCentered b font smallSize width (height * 0.06f + bigSize * 1.4f) sub menuGray LRink

    // Two-column team selection
    let colW = width * 0.42f
    let col1X = width * 0.04f
    let col2X = width * 0.54f
    let listY = height * 0.22f

    let drawColumn colX (headerText: string) (headerColor: Color) selectedIdx isActive =
        drawText b font medSize colX listY headerText headerColor LTrail

        if isActive then
            let boxH = medSize * 1.3f + float32 NumTeams * smallSize * 1.6f + 6.0f
            drawRect b (colX - 3.0f) (listY - 2.0f) colW boxH 1.5f headerColor LRink

        for i in 0 .. NumTeams - 1 do
            let ty = listY + medSize * 1.5f + float32 i * smallSize * 1.6f
            let isSelected = (i = selectedIdx)

            if isSelected then
                fillRect b colX (ty - 1.0f) (colW - 6.0f) (smallSize * 1.4f) (Color.rgb 40uy 60uy 100uy) LRink

            let color = if isSelected then goalFlashColor else white
            let prefix = if isSelected then "> " else "  "
            let label = $"{prefix}{teamNames.[i]}"
            drawText b font smallSize (colX + 4.0f) ty label color LTrail

            // Team skating speed, dimmed — a hint of how hard a CPU opponent
            // is, reflecting the current fast-human/hard-mode settings
            let struct (labelW, _) = measureText font smallSize label
            let spd = displayedTeamSpeed fastHuman hardMode i
            drawText b font smallSize (colX + 4.0f + labelW) ty $" (speed {spd})" (Color.rgb 105uy 105uy 125uy) LTrail

    drawColumn col1X "TEAM 1 (LEFT)" team1Color selectedTeam1 (activeColumn = 0)
    drawColumn col2X "TEAM 2 (RIGHT)" team2Color selectedTeam2 (activeColumn = 1)

    // Instructions
    let fastStr = if fastHuman then "ON" else "OFF"
    let hardStr = if hardMode then "ON" else "OFF"
    let fiveStr = if fivePlayer then "6v6" else "3v3"
    let padStr = if gamepadOn then "ON" else "OFF"

    let instrLines =
        [| "UP/DOWN = Select Team  |  TAB = Switch Column"
           "ENTER = Start Game  |  L = Play League  |  P = Pause  |  ESC = Quit"
           $"F = Fast Human [{fastStr}]  |  H = Hard Mode [{hardStr}]  |  5 = Players [{fiveStr}]"
           $"G = Gamepad [{padStr}]  |  F11 = Toggle Fullscreen"
           "Hold shoot key longer for harder shot, quick tap for a pass"
           "Player 1: Arrow Keys + RShift/Enter, or Gamepad 1"
           "Player 2: WASD + Space/Tab, or Gamepad 2"
           "(Set team to HUMAN PLAYER for keyboard control)" |]

    let baseY = height * 0.7f

    instrLines
    |> Array.iteri (fun i line ->
        drawCentered b font smallSize width (baseY + float32 i * smallSize * 1.3f) line menuGray LTrail)

// ─── Main Render ──────────────────────────────────────────────────────

let renderFrame (b: RenderBuffer2D) (font: SpriteFont) (gs: GameState) (width: float32) (height: float32) leagueMode =
    let w = width
    let h = height

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

    // Rink content (fills via SpriteBatch, players/lines/circles via
    // PrimitiveBatch) is bracketed in an identity camera: BeginCamera and
    // EndCamera are the renderer's batch-flush points, which is what makes
    // the HUD/overlay text drawn after endCamera composite ON TOP of the
    // world primitives. Without the bracket the frame is a single batch
    // stretch and all primitives would draw over all text and fills.
    let identityCam =
        Camera2D.create (Vector2(w / 2.0f, h / 2.0f)) 1.0f (Vector2(w, h))

    b.beginCamera(identityCam, layer = LBg) |> ignore

    fillRect b 0.0f 0.0f w h bgColor LBg

    // HUD is anchored to the bottom edge; the rink is centered in the space
    // above it (horizontally, and vertically on very wide windows).
    let hudH = HudHeight * sy
    let offX = (w - OrigW * sx) / 2.0f
    let offY = max 0.0f ((h - hudH - 2.0f - rinkGameH * sy) / 2.0f)

    drawRink b sx sy offX offY team1Color team2Color

    // Ice trail marks (drawn on ice, under players and puck)
    for i in 0 .. gs.TrailMarkCount - 1 do
        let mark = gs.TrailMarks.[i]

        if mark.Life > 0<tick> then
            let alpha = int (float (int mark.Life) / float (int TrailMarkLifetime) * 180.0) + 40
            let alpha = byte (min 220 alpha)
            let mx = offX + gameX sx mark.X
            let my = offY + gameY sy mark.Y
            fillCircle b mx my (1.2f * sx) (Color.create 255uy 255uy 255uy alpha) LTrail

    let ppt = gs.PlayersPerTeam
    let t2s = gs.Team2Start

    // Helmet colors: human team = gold, CPU = black
    let t1Helmet = if gs.Team1Idx = 0 then helmetGold else helmetBlack
    let t2Helmet = if gs.Team2Idx = 0 then helmetGold else helmetBlack

    // Puck on a lower layer than players, so skaters appear on top of it
    drawPuck b sx sy offX offY gs.Entities.[gs.PuckIdx] gs.PuckAnimFrame

    // Team 1 players
    for i in 0 .. ppt - 1 do
        let isGoalie = gs.FivePlayerMode && i = 0

        drawRetroPlayer
            b
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
            b
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

    // Close the world bracket: flushes both batches so the HUD and overlays
    // below render strictly on top of the rink and players.
    b.endCamera(layer = LMarker) |> ignore

    // HUD (spans the full width, anchored to the bottom edge)
    let rinkBottom = h - hudH - 2.0f
    drawHud b font gs sx sy rinkBottom w

    // Overlays
    drawGoalFlash b font gs w h
    drawPeriodFlash b font gs w h

    if not gs.Playing && gs.ClockSeconds >= gs.PeriodLength then
        drawGameOver b font gs w h leagueMode
