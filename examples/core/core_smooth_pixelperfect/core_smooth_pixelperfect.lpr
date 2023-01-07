{*******************************************************************************************
*
*   raylib [core] example - smooth pixel-perfect camera
*
*   This example has been created using raylib 3.7 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Giancamillo Alessandroni (@NotManyIdeasDev) and
*   reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2021 Giancamillo Alessandroni (@NotManyIdeasDev) and Ramon Santamaria (@raysan5)
*   pascal translation Gunko Vadim (@GuvaCode)
*
********************************************************************************************}
program core_smooth_pixelperfect;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
raymath,
//rlgl,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  virtualScreenWidth = 160;
  virtualScreenHeight = 90;
  virtualRatio = single(screenWidth)/single(virtualScreenWidth);

  var
    worldSpaceCamera, screenSpaceCamera: TCamera2D;
    target:TRenderTexture2D;
    rec01,rec02,rec03,sourceRec,destRec:TRectangle;
    origin:TVector2;
    rotation,cameraX,cameraY:single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - smooth pixel-perfect camera');

  worldSpaceCamera.zoom := 1.0; // Game world camera
  screenSpaceCamera.zoom := 1.0; // Smoothing camera

  target := LoadRenderTexture(virtualScreenWidth, virtualScreenHeight); // This is where we'll draw all our objects.

  RectangleSet(@rec01,70.0, 35.0, 20.0, 20.0);
  RectangleSet(@rec02,90.0, 55.0, 30.0, 10.0);
  RectangleSet(@rec03,80.0, 65.0, 15.0, 25.0);

  // The target's height is flipped (in the source Rectangle), due to OpenGL reasons
  RectangleSet(@sourceRec,0.0,0.0, target.texture.width, -target.texture.height);
  RectangleSet(@destRec,-virtualRatio, -virtualRatio, screenWidth + (virtualRatio*2), screenHeight + (virtualRatio*2));
  rotation:=0.0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      rotation += 60.0*GetFrameTime();   // Rotate the rectangles, 60 degrees per second

      // Make the camera move to demonstrate the effect
      cameraX := (sin(GetTime)*50.0) - 10.0;
      cameraY := cos(GetTime)*30.0;

      // Set the camera's target to the values computed above
      screenSpaceCamera.target := Vector2Create( cameraX, cameraY );

      // Round worldSpace coordinates, keep decimals into screenSpace coordinates
      worldSpaceCamera.target.x := screenSpaceCamera.target.x;
      screenSpaceCamera.target.x -= worldSpaceCamera.target.x;
      screenSpaceCamera.target.x *= virtualRatio;

      worldSpaceCamera.target.y := screenSpaceCamera.target.y;
      screenSpaceCamera.target.y -= worldSpaceCamera.target.y;
      screenSpaceCamera.target.y *= virtualRatio;
     //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginTextureMode(target);
             ClearBackground(RAYWHITE);

            BeginMode2D(worldSpaceCamera);
                DrawRectanglePro(rec01, origin, rotation, BLACK);
                DrawRectanglePro(rec02, origin, -rotation, RED);
                DrawRectanglePro(rec03, origin, rotation + 45.0, BLUE);
            EndMode2D();
        EndTextureMode();

        BeginDrawing();
            ClearBackground(RED);

            BeginMode2D(screenSpaceCamera);
                DrawTexturePro(target.texture, sourceRec, destRec, origin, 0.0, WHITE);
            EndMode2D();

            DrawText(TextFormat('Screen resolution: %ix%i', screenWidth, screenHeight), 10, 10, 20, DARKBLUE);
            DrawText(TextFormat('World resolution: %ix%i', virtualScreenWidth, virtualScreenHeight), 10, 40, 20, DARKGREEN);
            DrawFPS(GetScreenWidth() - 95, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadRenderTexture(target);    // Unload render texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

