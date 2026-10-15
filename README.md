# The FS Hockey League

A retro-style ice hockey game inspired by Solar Hockey (1990-1992). Written in F# with the [Mibo game engine](https://github.com/AngelMunoz/Mibo) (Elmish/MVU, MonoGame DesktopGL backend).

This branch uses Mibo's Elmish architecture: the app flow is an immutable
model-view-update loop, the 60 Hz simulation runs on Mibo's fixed timestep,
keyboard input goes through Mibo's `InputMap` action mapping, the gamepad is
a Mibo subscription (diffed on/off by the menu toggle), and the frame is
described declaratively with the fluent 2D draw DSL on sorted render layers.

The Elmish core (App.fs) references only `Mibo.Core`, so the entire game loop
also runs in Mibo's headless runtime: `dotnet fsi test.fsx` plays through the
major end-user scenarios with simulated input — menu navigation, human
keyboard control, a scored goal with faceoff reset, gamepad stick/d-pad
input, a two-player match, 6v6 + hard-mode options, pause, and a full league
season to the champion screen — no window needed.

There are also MonoGame, WinForms and FableWeb branches.
You can try the Fable version online here: https://thorium.github.io/FsHockey/

## Requirements

- .NET 10 SDK (Windows, macOS, or Linux)

## Build & Run

```
dotnet tool restore
dotnet build
dotnet run
```

(`dotnet tool restore` installs the MonoGame content builder, which bakes the
bundled Roboto Mono font into a spritefont.)

## Controls

| Action        | Player 1            | Player 2    |
|---------------|---------------------|-------------|
| Move          | Arrow keys          | WASD        |
| Shoot / Pass  | Right Shift / Enter | Space / Tab |

Hold the shoot key longer for a harder shot. A quick tap gives a weaker pass.

On Windows the accessibility shortcuts (Shift pressed five times for Sticky Keys,
right Shift held for Filter Keys, Num Lock held for Toggle Keys) are off while
the game window is in front, so their dialogs can't interrupt a match; they come
back when you switch away or quit. Players who have the feature itself turned on
keep it. No admin rights are needed, and nothing is saved to your settings.

## Menu

| Key       | Action                                    |
|-----------|-------------------------------------------|
| Up / Down | Select team                               |
| Tab       | Switch between Team 1 and Team 2 columns  |
| Enter     | Start exhibition match                    |
| L         | Start league tournament                   |
| F         | Toggle fast human players                 |
| H         | Toggle hard mode (stronger CPU)           |
| 5         | Toggle 3v3 / 5v5 player mode             |
| Esc       | Quit                                      |

Set a team to **HUMAN PLAYER** for keyboard control. Both teams can be human or CPU.

The menu is also clickable: click a team in either list to pick it, click
**EXHIBITION** or **LEAGUE** to start, and the toggles below them switch
3v3 / 6v6, fast human, hard CPU and gamepads. Every control shows its
keyboard shortcut, and the keys above keep working.

## Team colours

The jersey colour tells a team's side and how fast it skates:

| Speed class | Team 1 (left) | Team 2 (right) |
|-------------|---------------|----------------|
| Fastest     | Red           | Blue           |
| Medium      | Orange        | Yellow         |
| Slowest     | Red-purple    | Green          |

The classes come from the CPU teams' speeds as the menu shows them (fastest
value, slowest value, anything between), and the Human Player is placed among
them by its own speed. Hard mode speeds every CPU team up alike, so their
classes stay, but the fast Human Player drops to medium. The league's
next-game screen shows each team's speed after its name, as the menu does.


## Credits

By Tuomas Hietanen. Influenced by Wayne Gretzky Hockey (1988) and Solar Hockey (c) 1990-1992 Galifir Developments.

<img width="1191" height="527" alt="image" src="https://github.com/user-attachments/assets/01e121cb-1d32-4087-96b6-ff4b9200188e" />

