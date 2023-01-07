program core_window_letterbox;

{$mode objfpc}{$H+}
uses
cmem, raylib, raymath, math;

const
  WindowWidth = 800;
  WindowHeight = 450;
  GameScreenWidth = 640;
  GameScreenHeight = 480;

var
  Target: TRenderTexture2D;
  Colors: array [0..9] of TColor;
  I: Integer;
  Scale: Single;
  Mouse: TVector2;
  VirtualMouse: TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
   // Enable config flags for resizable window and vertical synchro
  SetConfigFlags(FLAG_WINDOW_RESIZABLE or FLAG_VSYNC_HINT);
  InitWindow(WindowWidth, WindowHeight, 'raylib [core] example - window scale letterbox');
  SetWindowMinSize(320, 240);

  // Render texture initialization, used to hold the rendering result so we can easily resize it
  Target := LoadRenderTexture(GameScreenWidth, GameScreenHeight);
  SetTextureFilter(Target.Texture, TEXTURE_FILTER_BILINEAR);  // Texture scale filter to use

  for I := 0 to High(Colors) do
    Colors[I] := ColorCreate(GetRandomValue(100, 250), GetRandomValue(50, 150), GetRandomValue(10, 100), 255);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Compute required framebuffer scaling
      Scale := Min(GetScreenWidth() / GameScreenWidth, GetScreenHeight() / GameScreenHeight);

      if IsKeyPressed(KEY_SPACE) then
      begin
        // Recalculate random colors for the bars
        for I := 0 to High(Colors) do
          Colors[I] := ColorCreate(GetRandomValue(100, 250), GetRandomValue(50, 150), GetRandomValue(10, 100), 255);
      end;

      // Update virtual mouse (clamped mouse value behind game screen)
      Mouse := GetMousePosition();
      VirtualMouse := Vector2Create(
        (Mouse.X - (GetScreenWidth() - (GameScreenWidth * Scale)) * 0.5) / Scale,
        (Mouse.Y - (GetScreenHeight() - (GameScreenHeight * Scale)) * 0.5) / Scale
      );
      VirtualMouse := Vector2Clamp(VirtualMouse, Vector2Create(0, 0), Vector2Create(GameScreenWidth, GameScreenHeight));
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      // Draw everything in the render texture, note this will not be rendered on screen, yet
      BeginTextureMode(Target);
        ClearBackground(RAYWHITE); // Clear render texture background color

        for I := 0 to High(Colors) do
          DrawRectangle(0, Trunc((GameScreenHeight / 10) * I), GameScreenWidth, Trunc(GameScreenHeight / 10), Colors[I]);

        DrawText(UTF8String('If executed inside a window,'#10'you can resize the window,'#10'and see the screen scaling!'), 10, 25, 20, WHITE);
        DrawText(TextFormat(UTF8String('Default Mouse: [%i , %i]'), Integer(Trunc(Mouse.X)), Integer(Trunc(Mouse.Y))), 350, 25, 20, GREEN);
        DrawText(TextFormat(UTF8String('Virtual Mouse: [%i , %i]'), Integer(Trunc(VirtualMouse.X)), Integer(Trunc(VirtualMouse.Y))), 350, 55, 20, YELLOW);
      EndTextureMode();

      BeginDrawing();
        ClearBackground(BLACK); // Clear screen background

        // Draw render texture to screen, properly scaled
        DrawTexturePro(
          Target.Texture,
          RectangleCreate(0.0, 0.0, Target.Texture.Width, -Target.Texture.Height),
          RectangleCreate(
            (GetScreenWidth() - (GameScreenWidth * Scale)) * 0.5,
            (GetScreenHeight() - (GameScreenHeight * Scale)) * 0.5,
            GameScreenWidth * Scale,
            GameScreenHeight * Scale
          ),
          Vector2Create(0, 0),
          0.0,
          WHITE
        );
        EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadRenderTexture(Target);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

