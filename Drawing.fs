/// THE FS HOCKEY LEAGUE — MonoGame Drawing Primitives
/// Helper functions for 2D primitive rendering with MonoGame SpriteBatch.
module HockeyDemo.Drawing

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// ─── Pixel Texture ────────────────────────────────────────────────────
// A 1x1 white texture used for drawing filled rectangles and lines.

let mutable private pixelTexture: Texture2D = null
let mutable private circleTexture: Texture2D = null

/// Initialize the shared pixel and circle textures. Call once at LoadContent.
let initTextures (device: GraphicsDevice) =
    pixelTexture <- new Texture2D(device, 1, 1)
    pixelTexture.SetData([| Color.White |])

    // Pre-generate a circle texture (diameter 64 px) for ellipse drawing
    let diam = 64
    let r = float32 diam / 2.0f
    let data = Array.create (diam * diam) Color.Transparent

    for y in 0 .. diam - 1 do
        for x in 0 .. diam - 1 do
            let dx = float32 x - r + 0.5f
            let dy = float32 y - r + 0.5f

            if dx * dx + dy * dy <= r * r then
                data.[y * diam + x] <- Color.White

    circleTexture <- new Texture2D(device, diam, diam)
    circleTexture.SetData(data)

/// Dispose textures. Call at UnloadContent.
let disposeTextures () =
    if pixelTexture <> null then
        pixelTexture.Dispose()
        pixelTexture <- null

    if circleTexture <> null then
        circleTexture.Dispose()
        circleTexture <- null

// ─── Filled Rectangle ─────────────────────────────────────────────────

/// Draw a filled rectangle.
let fillRect (sb: SpriteBatch) (x: float32) (y: float32) (w: float32) (h: float32) (color: Color) =
    sb.Draw(
        pixelTexture,
        Rectangle(int x, int y, int (ceil w), int (ceil h)),
        color
    )

// ─── Draw Line ────────────────────────────────────────────────────────

/// Draw a line between two points with given thickness and color.
let drawLine (sb: SpriteBatch) (x1: float32) (y1: float32) (x2: float32) (y2: float32) (thickness: float32) (color: Color) =
    let dx = x2 - x1
    let dy = y2 - y1
    let length = sqrt (dx * dx + dy * dy)

    if length > 0.001f then
        let angle = atan2 dy dx

        sb.Draw(
            pixelTexture,
            Vector2(x1, y1),
            System.Nullable(),
            color,
            angle,
            Vector2(0.0f, 0.5f),
            Vector2(length, thickness),
            SpriteEffects.None,
            0.0f
        )

// ─── Draw Rectangle Outline ──────────────────────────────────────────

/// Draw a rectangle outline.
let drawRect (sb: SpriteBatch) (x: float32) (y: float32) (w: float32) (h: float32) (thickness: float32) (color: Color) =
    // Top
    fillRect sb x y w thickness color
    // Bottom
    fillRect sb x (y + h - thickness) w thickness color
    // Left
    fillRect sb x y thickness h color
    // Right
    fillRect sb (x + w - thickness) y thickness h color

// ─── Filled Ellipse ───────────────────────────────────────────────────

/// Draw a filled ellipse (uses pre-generated circle texture, scaled).
let fillEllipse (sb: SpriteBatch) (cx: float32) (cy: float32) (rx: float32) (ry: float32) (color: Color) =
    let diam = float32 circleTexture.Width
    let scaleX = (rx * 2.0f) / diam
    let scaleY = (ry * 2.0f) / diam

    sb.Draw(
        circleTexture,
        Vector2(cx, cy),
        System.Nullable(),
        color,
        0.0f,
        Vector2(diam / 2.0f, diam / 2.0f),
        Vector2(scaleX, scaleY),
        SpriteEffects.None,
        0.0f
    )

// ─── Draw Ellipse Outline ─────────────────────────────────────────────

/// Draw an ellipse outline by drawing a filled ellipse with a cut-out interior.
let drawEllipse (sb: SpriteBatch) (cx: float32) (cy: float32) (rx: float32) (ry: float32) (thickness: float32) (color: Color) (bgColor: Color) =
    fillEllipse sb cx cy rx ry color
    if thickness < rx && thickness < ry then
        fillEllipse sb cx cy (rx - thickness) (ry - thickness) bgColor

// ─── Dashed Line ──────────────────────────────────────────────────────

/// Draw a dashed line.
let drawDashedLine (sb: SpriteBatch) (x1: float32) (y1: float32) (x2: float32) (y2: float32) (thickness: float32) (dashLen: float32) (color: Color) =
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
                let sx = x1 + nx * t
                let sy = y1 + ny * t
                let ex = x1 + nx * (t + segLen)
                let ey = y1 + ny * (t + segLen)
                drawLine sb sx sy ex ey thickness color

            t <- t + dashLen
            draw <- not draw
