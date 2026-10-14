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

## Touch (phones / tablets)

On touch devices the game switches to a touch layout (landscape; in portrait
it asks you to turn the device, and pauses a running match). A tap menu picks
both teams, 3v3 / 6v6, fast human and hard CPU, and starts an exhibition or a
league. In a match, touch
anywhere in the left part of the screen to get a floating skate stick (8
directions); **SHOOT** sits in the bottom-right corner — hold it longer for a
harder shot. With both teams set to Human Player, each player gets a stick in
their bottom corner and SHOOT in their top corner, rimmed in their team's
colour. **PAUSE** / **MENU** are at the top, and **CONTINUE** appears on the
result and league screens. `?touch=1` / `?touch=0` in the URL forces the touch
layout on or off.

## Install as an app

The web build is an installable app (web app manifest + offline service
worker). On Android Chrome use *Install app* / *Add to Home screen*: it then
launches fullscreen in landscape and starts offline after one online launch.
On iPhone/iPad use Safari's *Share → Add to Home Screen*. Desktop Chrome/Edge
offer the install button in the address bar. The service worker is only
registered in production builds (`npm run build`), not under the Vite dev
server.

## Credits

By Tuomas Hietanen. Influenced by Wayne Gretzky Hockey (1988) and Solar Hockey (c) 1990-1992 Galifir Developments.
