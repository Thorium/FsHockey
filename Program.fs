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

// ─── Entry Point ──────────────────────────────────────────────────────

[<EntryPoint>]
let main _ =
    toggleFullscreenHook <- toggleFullscreen

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
