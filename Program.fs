/// THE FS HOCKEY LEAGUE — Program Entry Point (Mibo, MonoGame backend)
/// The thin backend shell around the Elmish core in App.fs: view, input
/// subscriptions, fullscreen handling, and the MonoGame host bootstrap.
module HockeyDemo.Program

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish
open Mibo.Elmish.Graphics2D
open Mibo.Input
open HockeyDemo.Physics
open HockeyDemo.Game
open HockeyDemo.App
open HockeyDemo.Renderer

// ─── Fullscreen ───────────────────────────────────────────────────────
// The GraphicsDeviceManager is captured at construction time so F11 can
// toggle borderless fullscreen at the desktop resolution.

let mutable private graphicsManager: GraphicsDeviceManager voption = ValueNone

let private toggleFullscreen () =
    match graphicsManager with
    | ValueNone -> ()
    | ValueSome gdm ->
        if gdm.IsFullScreen then
            gdm.IsFullScreen <- false
            gdm.PreferredBackBufferWidth <- 960
            gdm.PreferredBackBufferHeight <- 620
        else
            let dm = GraphicsAdapter.DefaultAdapter.CurrentDisplayMode
            gdm.HardwareModeSwitch <- false
            gdm.PreferredBackBufferWidth <- dm.Width
            gdm.PreferredBackBufferHeight <- dm.Height
            gdm.IsFullScreen <- true

        gdm.ApplyChanges()

// ─── View ─────────────────────────────────────────────────────────────

let private view (ctx: GameContext) (m: Model) (buffer: RenderBuffer2D) =
    // The asset service caches by path, so this is a lookup, not a load.
    let assets = GameContext.getService<IAssets> ctx
    let font: SpriteFont = assets.Font "Fonts/Ui"
    let w = float32 ctx.WindowWidth
    let h = float32 ctx.WindowHeight

    match m.Mode with
    | Menu ->
        drawMenu buffer font w h m.SelectedTeam1 m.SelectedTeam2 m.ActiveColumn m.FastHuman m.HardMode m.FivePlayerMode m.GamepadEnabled

    | Playing ->
        renderFrame buffer font m.Gs w h false

        if m.Paused then
            drawPauseOverlay buffer font w h

    | LeagueMatchup ->
        match m.League with
        | Some league ->
            let t1, t2 = currentMatchup league

            drawLeagueMatchup buffer font w h (league.CurrentRound + 1) league.Schedule.Length teamNames.[t1] teamNames.[t2]
        | None -> ()

    | LeaguePlaying ->
        renderFrame buffer font m.Gs w h true

        if m.Paused then
            drawPauseOverlay buffer font w h

    | LeagueStandings ->
        m.League
        |> Option.iter (fun league ->
            drawLeagueStandings buffer font w h (getSortedStandings league) false league.HumanTeam)

    | LeagueFinalStandings ->
        m.League
        |> Option.iter (fun league ->
            drawLeagueStandings buffer font w h (getSortedStandings league) true league.HumanTeam)

// ─── Subscriptions ────────────────────────────────────────────────────
// The gamepad subscription is only active while gamepads are enabled in
// the menu — Mibo diffs subscriptions by id on every model change.

let private subscriptions (ctx: GameContext) (m: Model) : Sub<Msg> =
    Sub.batch
        [ InputMapper.subscribeStatic inputMap InputChanged ctx
          if m.GamepadEnabled then
              Gamepad.listen PadChanged ctx ]

// ─── Graphics Smoke Test (`--graphics-test <outDir>`) ─────────────────
// Self-driving capture mode for visual regression checks: an autopilot
// subscription feeds the same InputChanged messages a player's keys would
// produce, and a trailing renderer saves backbuffer PNGs at fixed frames.
// No OS-level input injection — the window drives itself and exits.

/// (frame number, screenshot name) — the trailing renderer runs after the
/// main view has drawn the frame into the backbuffer.
let private screenshotPlan =
    [ 60, "menu"
      115, "period-banner"
      200, "gameplay"
      250, "paused" ]

/// (delay ms before the press, action) — pressed and released like a key.
let private autopilotScript =
    [ 1500, Select //       menu -> exhibition match (PERIOD 1 banner shows)
      1800, TogglePause //  freeze play for a stable overlay shot
      1300, TogglePause
      200, Back //          match -> menu
      500, Back ] //        menu -> quit

let private autopilotSub (ctx: GameContext) : Sub<Msg> =
    let subId = SubId.ofString "HockeyDemo/graphics-test/autopilot"

    let start (dispatch: Dispatch<Msg>) =
        let cts = new System.Threading.CancellationTokenSource()

        Async.Start(
            async {
                for delayMs, action in autopilotScript do
                    do! Async.Sleep delayMs

                    dispatch (
                        InputChanged
                            { ActionState.empty with
                                Started = Set.singleton action
                                Held = Set.singleton action }
                    )

                    do! Async.Sleep 60
                    dispatch (InputChanged { ActionState.empty with Released = Set.singleton action })
            },
            cts.Token
        )

        { new System.IDisposable with
            member _.Dispose() = cts.Cancel() }

    Sub.Active(subId, start)

let private screenshotRenderer (outDir: string) : IRenderer<Model> =
    let mutable frame = 0

    { new IRenderer<Model> with
        member _.Draw(ctx, _model, _gameTime) =
            frame <- frame + 1

            match screenshotPlan |> List.tryFind (fun (f, _) -> f = frame) with
            | Some(_, name) ->
                let gd = MonoGameGameContext.getGraphicsDevice ctx
                let w = gd.PresentationParameters.BackBufferWidth
                let h = gd.PresentationParameters.BackBufferHeight
                let data: Color[] = Array.zeroCreate (w * h)
                gd.GetBackBufferData data
                use tex = new Texture2D(gd, w, h)
                tex.SetData data
                use fs = System.IO.File.Create(System.IO.Path.Combine(outDir, name + ".png"))
                tex.SaveAsPng(fs, w, h)
            | None -> () }

// ─── Entry Point ──────────────────────────────────────────────────────

[<EntryPoint>]
let main argv =
    toggleFullscreenHook <- toggleFullscreen

    let graphicsTestDir =
        match argv with
        | [| "--graphics-test"; dir |] ->
            System.IO.Directory.CreateDirectory(dir) |> ignore
            Some dir
        | _ -> None

    let withTestHarness program =
        match graphicsTestDir with
        | None -> program
        | Some dir ->
            program
            |> Program.withSubscription (fun ctx m ->
                Sub.batch [ subscriptions ctx m; autopilotSub ctx ])
            |> Program.withRenderer (fun () -> screenshotRenderer dir)

    let program =
        Program.mkProgram init update
        |> Program.withConfig (fun cfg ->
            { cfg with
                Width = 960
                Height = 620
                Title = "The FS Hockey League — By Tuomas Hietanen 2026"
                TargetFPS = 60 })
        |> Program.withInput
        |> Program.withSubscription subscriptions
        |> Program.withFixedStep fixedStepConfig
        |> Program.withRenderer (fun () -> Renderer2D.create view)
        |> withTestHarness
        |> MonoGameProgram.ofProgram
        |> MonoGameProgram.withConfig (fun (game, gdm) ->
            game.Content.RootDirectory <- "Content"
            // The MonoGame backend doesn't map GameConfig.MinWidth/MinHeight
            // to a resizable window (only raylib does), so set it directly.
            game.Window.AllowUserResizing <- true
            graphicsManager <- ValueSome gdm)

    use game = new MiboGame<Model, Msg>(program)
    game.Run()
    0
