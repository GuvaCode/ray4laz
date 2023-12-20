{*******************************************************************************************
*
*   raylib [core] example - split screen
*
*   This example has been created using raylib 3.7 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2021 Jeffery Myers (@JeffM2501)
*   Pascal translation 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program core_split_screen;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
//raymath,
//rlgl,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var

   cameraPlayer1 :TCamera;
   cameraPlayer2 :TCamera;

   screenPlayer1: TRenderTexture;
   screenPlayer2: TRenderTexture;
   splitScreenRect: TRectangle;
   offsetThisFrame: single;

   // Scene drawing
 procedure DrawScene;
 var count,x,z: integer;
     spacing: single;
   begin
       count := 5;
       spacing := 4;

       // Grid of cube trees on a plane to make a "world"
       DrawPlane(Vector3Create(0,0,0), Vector2Create(50,50), BEIGE);// // Simple world plane

       for x := -count to count do
       begin
         for z :=-count to count do
          begin
               DrawCube(Vector3Create(x*spacing, 1.5, z*spacing), 1, 1, 1, GREEN);
               DrawCube(Vector3Create(x*spacing, 0.5, z*spacing), 0.25, 1, 0.25, BROWN);
           end;
       end;

       // Draw a cube at each player's position
       DrawCube(cameraPlayer1.position, 1, 1, 1, RED);
       DrawCube(cameraPlayer2.position, 1, 1, 1, BLUE);
   end;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - split screen');


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
  splitScreenRect :=RectangleCreate(0.0, 0.0, screenPlayer1.texture.width, -screenPlayer1.texture.height);
  { 0.0f, 0.0f, (float)screenPlayer1.texture.width, (float)-screenPlayer1.texture.height };


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // If anyone moves this frame, how far will they move based on the time since the last frame
      // this moves thigns at 10 world units per second, regardless of the actual FPS
      offsetThisFrame := 10.0*GetFrameTime;
      // Move Player1 forward and backwards (no turning)
        if IsKeyDown(KEY_W) then
        begin
            cameraPlayer1.position.z += offsetThisFrame;
            cameraPlayer1.target.z += offsetThisFrame;
        end
        else if IsKeyDown(KEY_S) then
        begin
            cameraPlayer1.position.z -= offsetThisFrame;
            cameraPlayer1.target.z -= offsetThisFrame;
        end;

       // Move Player2 forward and backwards (no turning)
        if IsKeyDown(KEY_UP) then
        begin
            cameraPlayer2.position.x += offsetThisFrame;
            cameraPlayer2.target.x += offsetThisFrame;
        end
        else if IsKeyDown(KEY_DOWN) then
        begin
            cameraPlayer2.position.x -= offsetThisFrame;
            cameraPlayer2.target.x -= offsetThisFrame;
        end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      // Draw Player1 view to the render texture
      BeginTextureMode(screenPlayer1);
          ClearBackground(SKYBLUE);
          BeginMode3D(cameraPlayer1);
          DrawScene();
          EndMode3D();
          DrawText('PLAYER1 W/S to move', 10, 10, 20, RED);
      EndTextureMode();

      // Draw Player2 view to the render texture
        BeginTextureMode(screenPlayer2);
            ClearBackground(SKYBLUE);
            BeginMode3D(cameraPlayer2);
            DrawScene();
            EndMode3D();
            DrawText('PLAYER2 UP/DOWN to move', 10, 10, 20, BLUE);
        EndTextureMode();

        // Draw both views render textures to the screen side by side
        BeginDrawing();
            ClearBackground(BLACK);
            DrawTextureRec(screenPlayer1.texture, splitScreenRect, Vector2Create(0,0), WHITE);
            DrawTextureRec(screenPlayer2.texture, splitScreenRect, Vector2Create(screenWidth/2.0,0), WHITE);
        EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadRenderTexture(screenPlayer1); // Unload render texture
  UnloadRenderTexture(screenPlayer2); // Unload render texture

  CloseWindow();        // Close window and OpenGL context

  //--------------------------------------------------------------------------------------
end.

