{*******************************************************************************************
*
*   raylib [core] example - World to screen
*
*   Example originally created with raylib 1.3, last time updated with raylib 1.4
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2022 Ramon Santamaria (@raysan5)
*   Pascal translation 2022 Vadim Gunko (@guvacode)
*
********************************************************************************************}
program core_world_screen;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  cubePosition : TVector3;
  cubeScreenPosition : TVector2;
  camera : TCamera;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - World to screen');
  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 10.0, 10.0, 10.0 );
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );
  camera.fovy := 45.0;
  camera.projection := CAMERA_PERSPECTIVE;

  cubePosition := Vector3Create( 0.0, 0.0, 0.0 );
  cubeScreenPosition := Vector2Create( 0.0, 0.0 );



  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera, CAMERA_FREE);

      // Calculate cube screen space position (with a little offset to be in top)
      cubeScreenPosition := GetWorldToScreen(Vector3Create(cubePosition.x, cubePosition.y + 2.5, cubePosition.z), camera);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginMode3D(camera);
          DrawCube(cubePosition, 2.0, 2.0, 2.0, RED);
          DrawCubeWires(cubePosition, 2.0, 2.0, 2.0, MAROON);
          DrawGrid(10, 1.0);
        EndMode3D();
          DrawText('Enemy: 100 / 100', Round(cubeScreenPosition.x - MeasureText('Enemy: 100/100', 20)/2), Round(cubeScreenPosition.y), 20, BLACK);
          DrawText('Text is always on top of the cube', (screenWidth - MeasureText('Text is always on top of the cube', 20)) div 2, 25, 20, GRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

