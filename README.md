# The FS Hockey League — Web (Fable)

### ▶ [Play now](https://thorium.github.io/FsHockey/)

A retro-style ice hockey game inspired by Solar Hockey (1990-1992). Written in
F# and compiled to JavaScript with [Fable](https://fable.io/), rendering to an
HTML5 canvas. This is the **FableWeb** branch — it runs in the browser and is
deployed to GitHub Pages.

The desktop front-ends (WinForms and MonoGame) live on their own branches. All
three share the same framework-agnostic game logic (`Physics.fs`, `Game.fs`);
only the renderer (`Drawing.fs`, `Renderer.fs`) and entry point (`Program.fs`)
differ per platform.

## Requirements

- .NET 10 SDK
- Node.js 20+ (npm)

## Develop

```
dotnet tool restore   # restore the Fable tool (first time only)
npm install
npm run dev           # Fable watch + Vite dev server
```

Then open the URL Vite prints (usually http://localhost:5173/).

## Build

```
npm run build         # dotnet fable && vite build  →  dist/
```

The static site is emitted to `dist/`. The `FableWeb` branch auto-deploys to
GitHub Pages via `.github/workflows/deploy.yml` on every push.

## Controls

| Action        | Player 1            | Player 2    |
|---------------|---------------------|-------------|
| Move          | Arrow keys          | WASD        |
| Shoot / Pass  | Right Shift / Enter | Space / Tab |

Hold the shoot key longer for a harder shot. A quick tap gives a weaker pass.

## Menu

| Key       | Action                                    |
|-----------|-------------------------------------------|
| Up / Down | Select team                               |
| Tab       | Switch between Team 1 and Team 2 columns  |
| Enter     | Start exhibition match                    |
| L         | Start league tournament                   |
| F         | Toggle fast human players                 |
| H         | Toggle hard mode (stronger CPU)           |
| 5         | Toggle 3v3 / 6v6 player mode              |
| F11       | Toggle fullscreen                         |
| Esc       | Return to menu (during a match)           |

Set a team to **HUMAN PLAYER** for keyboard control. Both teams can be human or CPU.

## Credits

By Tuomas Hietanen. Influenced by Wayne Gretzky Hockey (1988) and Solar Hockey (c) 1990-1992 Galifir Developments.
