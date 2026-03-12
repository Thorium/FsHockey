# The FS Hockey League

A retro-style ice hockey game for Windows, inspired by Solar Hockey (1990-1992). Written in F# with Windows Forms.

## Requirements

- .NET 9 SDK (Windows)

## Build & Run

```
dotnet build
dotnet run
```

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
| 5         | Toggle 3v3 / 5v5 player mode             |
| Esc       | Quit                                      |

Set a team to **HUMAN PLAYER** for keyboard control. Both teams can be human or CPU.

## Teams

Phobos Miners, Titan Orange, Pluto Icemen, Neptune Blue, Saturn Ringers, Jupiter Magnos, Mars Reds, Moon Minerals, Earth Mutants

## Credits

By Tuomas Hietanen. Influenced by Wayne Gretzky Hockey (1988) and Solar Hockey (c) 1990-1992 Galifir Developments.
