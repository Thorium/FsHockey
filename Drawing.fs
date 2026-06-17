/// THE FS HOCKEY LEAGUE — Canvas 2D Drawing Primitives (Fable)
/// Helper functions for 2D primitive rendering on an HTML5 canvas context.
/// Mirrors the MonoGame Drawing module API so the renderer reads the same,
/// but draws through the browser's 2D context instead of a SpriteBatch.
module HockeyDemo.Drawing

open Fable.Core

// ─── Colour ───────────────────────────────────────────────────────────
// A simple RGBA colour carrying 0..255 channels, with a cached CSS string.
// Constructed like the MonoGame Color so renderer code stays familiar:
//   Color(200, 220, 240)         opaque
//   Color(255, 255, 80, 128)     with alpha

type Color(r: int, g: int, b: int, a: int) =
    new(r, g, b) = Color(r, g, b, 255)
    member _.R = r
    member _.G = g
    member _.B = b
    member _.A = a

    /// CSS colour string (rgb / rgba) for fillStyle / strokeStyle.
    member _.Css =
        if a >= 255 then $"rgb({r},{g},{b})"
        else $"rgba({r},{g},{b},{float a / 255.0})"

    static member White = Color(255, 255, 255)
    static member Black = Color(0, 0, 0)
    static member Transparent = Color(0, 0, 0, 0)

// ─── Canvas 2D interop (raw JS via Emit — front-end is Fable-only) ──────
// The 2D context is passed around as `obj`.

[<Emit("$0.fillStyle = $1")>]
let private setFill (ctx: obj) (c: string) : unit = jsNative
[<Emit("$0.strokeStyle = $1")>]
let private setStroke (ctx: obj) (c: string) : unit = jsNative
[<Emit("$0.lineWidth = $1")>]
let private setLineWidth (ctx: obj) (w: float) : unit = jsNative
[<Emit("$0.fillRect($1, $2, $3, $4)")>]
let private fillRectJs (ctx: obj) (x: float) (y: float) (w: float) (h: float) : unit = jsNative
[<Emit("$0.beginPath()")>]
let private beginPath (ctx: obj) : unit = jsNative
[<Emit("$0.moveTo($1, $2)")>]
let private moveTo (ctx: obj) (x: float) (y: float) : unit = jsNative
[<Emit("$0.lineTo($1, $2)")>]
let private lineTo (ctx: obj) (x: float) (y: float) : unit = jsNative
[<Emit("$0.stroke()")>]
let private strokePath (ctx: obj) : unit = jsNative
[<Emit("$0.fill()")>]
let private fillPath (ctx: obj) : unit = jsNative
[<Emit("$0.ellipse($1, $2, $3, $4, 0, 0, 6.283185307179586)")>]
let private ellipseJs (ctx: obj) (cx: float) (cy: float) (rx: float) (ry: float) : unit = jsNative
[<Emit("$0.setLineDash($1)")>]
let private setLineDash (ctx: obj) (segments: float array) : unit = jsNative
[<Emit("$0.lineCap = $1")>]
let private setLineCap (ctx: obj) (cap: string) : unit = jsNative

// ─── Filled Rectangle ───────────────────────────────────────────────────

/// Draw a filled rectangle.
let fillRect (ctx: obj) (x: float) (y: float) (w: float) (h: float) (color: Color) =
    setFill ctx color.Css
    fillRectJs ctx x y w h

// ─── Draw Line ────────────────────────────────────────────────────────

/// Draw a line between two points with given thickness and color.
let drawLine (ctx: obj) (x1: float) (y1: float) (x2: float) (y2: float) (thickness: float) (color: Color) =
    setStroke ctx color.Css
    setLineWidth ctx (max 0.5 thickness)
    setLineCap ctx "butt"
    beginPath ctx
    moveTo ctx x1 y1
    lineTo ctx x2 y2
    strokePath ctx

// ─── Draw Rectangle Outline ──────────────────────────────────────────

/// Draw a rectangle outline by filling its four edge bars (matches the
/// MonoGame implementation so corners join the same way).
let drawRect (ctx: obj) (x: float) (y: float) (w: float) (h: float) (thickness: float) (color: Color) =
    fillRect ctx x y w thickness color                       // top
    fillRect ctx x (y + h - thickness) w thickness color     // bottom
    fillRect ctx x y thickness h color                       // left
    fillRect ctx (x + w - thickness) y thickness h color     // right

// ─── Filled Ellipse ───────────────────────────────────────────────────

/// Draw a filled ellipse.
let fillEllipse (ctx: obj) (cx: float) (cy: float) (rx: float) (ry: float) (color: Color) =
    setFill ctx color.Css
    beginPath ctx
    ellipseJs ctx cx cy (max 0.1 rx) (max 0.1 ry)
    fillPath ctx

// ─── Draw Ellipse Outline ─────────────────────────────────────────────

/// Draw an ellipse outline. (The MonoGame version faked this with a cut-out
/// fill; canvas can stroke directly, so bgColor is accepted but unused.)
let drawEllipse (ctx: obj) (cx: float) (cy: float) (rx: float) (ry: float) (thickness: float) (color: Color) (_bgColor: Color) =
    setStroke ctx color.Css
    setLineWidth ctx (max 0.5 thickness)
    beginPath ctx
    ellipseJs ctx cx cy (max 0.1 rx) (max 0.1 ry)
    strokePath ctx

// ─── Dashed Line ──────────────────────────────────────────────────────

/// Draw a dashed line.
let drawDashedLine (ctx: obj) (x1: float) (y1: float) (x2: float) (y2: float) (thickness: float) (dashLen: float) (color: Color) =
    setLineDash ctx [| dashLen; dashLen |]
    drawLine ctx x1 y1 x2 y2 thickness color
    setLineDash ctx [||]
