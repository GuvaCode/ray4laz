program core_3d_camera_split_screen;

{$mode objfpc}{$H+}

uses
  raylib;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  cameraPlayer1, cameraPlayer2: TCamera;
  screenPlayer1, screenPlayer2: TRenderTexture2D;
  splitScreenRect: TRectangle;
  count: Integer;
  spacing: Single;
  offsetThisFrame: Single;
  x, z: Single;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d camera split screen');

  // Setup player 1 camera and screen
  cameraPlayer1.fovy := 45.0;
  cameraPlayer1.up := Vector3Create(0.0, 1.0, 0.0);
  cameraPlayer1.target := Vector3Create(0.0, 1.0, 0.0);
  cameraPlayer1.position := Vector3Create(0.0, 1.0, -3.0);
  cameraPlayer1.projection := CAMERA_PERSPECTIVE;

  screenPlayer1 := LoadRenderTexture(screenWidth div 2, screenHeight);

  // Setup player two camera and screen
  cameraPlayer2.fovy := 45.0;
  cameraPlayer2.up := Vector3Create(0.0, 1.0, 0.0);
  cameraPlayer2.target := Vector3Create(0.0, 3.0, 0.0);
  cameraPlayer2.position := Vector3Create(-3.0, 3.0, 0.0);
  cameraPlayer2.projection := CAMERA_PERSPECTIVE;

  screenPlayer2 := LoadRenderTexture(screenWidth div 2, screenHeight);

  // Build a flipped rectangle the size of the split view to use for drawing later
  splitScreenRect := RectangleCreate(0.0, 0.0, screenPlayer1.texture.width, -screenPlayer1.texture.height);

  // Grid data
  count := 5;
  spacing := 4;

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    // If anyone moves this frame, how far will they move based on the time since the last frame
    // this moves things at 10 world units per second, regardless of the actual FPS
    offsetThisFrame := 10.0 * GetFrameTime();

    // Move Player1 forward and backwards (no turning)
    if IsKeyDown(KEY_W) then
    begin
      cameraPlayer1.position.z := cameraPlayer1.position.z + offsetThisFrame;
      cameraPlayer1.target.z := cameraPlayer1.target.z + offsetThisFrame;
    end
    else if IsKeyDown(KEY_S) then
    begin
      cameraPlayer1.position.z := cameraPlayer1.position.z - offsetThisFrame;
      cameraPlayer1.target.z := cameraPlayer1.target.z - offsetThisFrame;
    end;

    // Move Player2 forward and backwards (no turning)
    if IsKeyDown(KEY_UP) then
    begin
      cameraPlayer2.position.x := cameraPlayer2.position.x + offsetThisFrame;
      cameraPlayer2.target.x := cameraPlayer2.target.x + offsetThisFrame;
    end
    else if IsKeyDown(KEY_DOWN) then
    begin
      cameraPlayer2.position.x := cameraPlayer2.position.x - offsetThisFrame;
      cameraPlayer2.target.x := cameraPlayer2.target.x - offsetThisFrame;
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    // Draw Player1 view to the render texture
    BeginTextureMode(screenPlayer1);
      ClearBackground(SKYBLUE);

      BeginMode3D(cameraPlayer1);

        // Draw scene: grid of cube trees on a plane to make a "world"
        DrawPlane(Vector3Create(0, 0, 0), Vector2Create(50, 50), BEIGE); // Simple world plane

        x := -count * spacing;
        while x <= count * spacing do
        begin
          z := -count * spacing;
          while z <= count * spacing do
          begin
            DrawCube(Vector3Create(x, 1.5, z), 1, 1, 1, LIME);
            DrawCube(Vector3Create(x, 0.5, z), 0.25, 1, 0.25, BROWN);
            z := z + spacing;
          end;
          x := x + spacing;
        end;

        // Draw a cube at each player's position
        DrawCube(cameraPlayer1.position, 1, 1, 1, RED);
        DrawCube(cameraPlayer2.position, 1, 1, 1, BLUE);

      EndMode3D();

      DrawRectangle(0, 0, GetScreenWidth() div 2, 40, Fade(RAYWHITE, 0.8));
      DrawText('PLAYER1: W/S to move', 10, 10, 20, MAROON);

    EndTextureMode();

    // Draw Player2 view to the render texture
    BeginTextureMode(screenPlayer2);
      ClearBackground(SKYBLUE);

      BeginMode3D(cameraPlayer2);

        // Draw scene: grid of cube trees on a plane to make a "world"
        DrawPlane(Vector3Create(0, 0, 0), Vector2Create(50, 50), BEIGE); // Simple world plane

        x := -count * spacing;
        while x <= count * spacing do
        begin
          z := -count * spacing;
          while z <= count * spacing do
          begin
            DrawCube(Vector3Create(x, 1.5, z), 1, 1, 1, LIME);
            DrawCube(Vector3Create(x, 0.5, z), 0.25, 1, 0.25, BROWN);
            z := z + spacing;
          end;
          x := x + spacing;
        end;

        // Draw a cube at each player's position
        DrawCube(cameraPlayer1.position, 1, 1, 1, RED);
        DrawCube(cameraPlayer2.position, 1, 1, 1, BLUE);

      EndMode3D();

      DrawRectangle(0, 0, GetScreenWidth() div 2, 40, Fade(RAYWHITE, 0.8));
      DrawText('PLAYER2: UP/DOWN to move', 10, 10, 20, DARKBLUE);

    EndTextureMode();

    // Draw both views render textures to the screen side by side
    BeginDrawing();
      ClearBackground(BLACK);

      DrawTextureRec(screenPlayer1.texture, splitScreenRect, Vector2Create(0, 0), WHITE);
      DrawTextureRec(screenPlayer2.texture, splitScreenRect, Vector2Create(screenWidth / 2.0, 0), WHITE);

      DrawRectangle(GetScreenWidth() div 2 - 2, 0, 4, GetScreenHeight(), LIGHTGRAY);
    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadRenderTexture(screenPlayer1); // Unload render texture
  UnloadRenderTexture(screenPlayer2); // Unload render texture

  CloseWindow();                      // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
