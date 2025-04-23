{*******************************************************************************************
*
*   raylib [core] example - 3d cmaera split screen
*
*   Example originally created with raylib 3.7, last time updated with raylib 4.0
*
*   Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2021-2024 Jeffery Myers (@JeffM2501)
*   Pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program core_3d_camera_split_screen;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;

var
  cameraPlayer1, cameraPlayer2: TCamera;
  screenPlayer1, screenPlayer2: TRenderTexture;
  splitScreenRect: TRectangle;
  count: integer;
  offsetThisFrame, spacing: single;
  x,z: integer;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d camera split screen');

  // Setup player 1 camera and screen
  cameraPlayer1.fovy := 45.0;
  cameraPlayer1.up.y := 1.0;
  cameraPlayer1.target.y := 1.0;
  cameraPlayer1.position.z := -3.0;
  cameraPlayer1.position.y := 1.0;
  screenPlayer1 := LoadRenderTexture(screenWidth div 2, screenHeight);

  // Setup player two camera and screen
  cameraPlayer2.fovy := 45.0;
  cameraPlayer2.up.y := 1.0;
  cameraPlayer2.target.y := 3.0;
  cameraPlayer2.position.x := -3.0;
  cameraPlayer2.position.y := 3.0;
  screenPlayer2 := LoadRenderTexture(screenWidth div 2, screenHeight);

  // Build a flipped rectangle the size of the split view to use for drawing later
  splitScreenRect := RectangleCreate( 0.0, 0.0, screenPlayer1.texture.width, -screenPlayer1.texture.height );

  // Grid data
  count := 5;
  spacing := 4;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // If anyone moves this frame, how far will they move based on the time since the last frame
      // this moves thigns at 10 world units per second, regardless of the actual FPS
      offsetThisFrame := 10.0*GetFrameTime();

      // Move Player1 forward and backwards (no turning)
      if (IsKeyDown(KEY_W)) then
      begin
        cameraPlayer1.position.z += offsetThisFrame;
        cameraPlayer1.target.z += offsetThisFrame;
      end
      else if (IsKeyDown(KEY_S)) then
      begin
        cameraPlayer1.position.z -= offsetThisFrame;
        cameraPlayer1.target.z -= offsetThisFrame;
      end;

      // Move Player2 forward and backwards (no turning)
      if (IsKeyDown(KEY_UP)) then
      begin
        cameraPlayer2.position.x += offsetThisFrame;
        cameraPlayer2.target.x += offsetThisFrame;
      end
      else if (IsKeyDown(KEY_DOWN)) then
      begin
        cameraPlayer2.position.x -= offsetThisFrame;
        cameraPlayer2.target.x -= offsetThisFrame;
      end;

      // Draw
      BeginTextureMode(screenPlayer1);
          ClearBackground(SKYBLUE);

          BeginMode3D(cameraPlayer1);

          // Draw scene: grid of cube trees on a plane to make a "world"
          DrawPlane(Vector3Create( 0, 0, 0 ), Vector2Create( 50, 50 ), BEIGE); // Simple world plane

          for  x := Trunc(-count* spacing) to Trunc(count*spacing) do
          begin
            for z := Trunc(-count* spacing) to Trunc(count*spacing)  do
            begin
              DrawCube(Vector3Create ( x, 1.5, z ), 1, 1, 1, LIME);
              DrawCube(Vector3Create ( x, 0.5, z ), 0.25, 1, 0.25, BROWN);
            end;
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
              DrawPlane( Vector3Create( 0, 0, 0 ), Vector2Create( 50, 50 ), BEIGE); // Simple world plane

              for x := Trunc(-count*spacing) to Trunc(count*spacing) do
              begin
                for z := Trunc(-count*spacing) to Trunc(count*spacing) do
                begin
                  DrawCube(Vector3Create ( x, 1.5, z ), 1, 1, 1, LIME);
                  DrawCube(Vector3Create ( x, 0.5, z ), 0.25, 1, 0.25, BROWN);
                end;
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

          DrawTextureRec(screenPlayer1.texture, splitScreenRect, Vector2Create( 0, 0 ), WHITE);
          DrawTextureRec(screenPlayer2.texture, splitScreenRect, Vector2Create( screenWidth/2.0, 0 ), WHITE);

          DrawRectangle(GetScreenWidth() div 2 - 2, 0, 4, GetScreenHeight(), LIGHTGRAY);
      EndDrawing();
   end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context

end.

