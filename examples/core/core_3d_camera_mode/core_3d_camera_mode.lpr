program core_3d_camera_mode;

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
  Camera: TCamera3D;
  CubePosition: TVector3;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d camera mode');

  Camera := Default(TCamera3D);
  Camera.Position := Vector3Create(0.0, 10.0, 10.0);  // Camera position
  Camera.Target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);          // Camera up vector (rotation towards target)
  Camera.Fovy := 45.0;                                  // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

  CubePosition := Vector3Create(0.0, 0.0, 0.0);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
        BeginDrawing();

      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);

        DrawCube(CubePosition, 2.0, 2.0, 2.0, RED);
        DrawCubeWires(CubePosition, 2.0, 2.0, 2.0, MAROON);

        DrawGrid(10, 1.0);

      EndMode3D();

      DrawText('Welcome to the third dimension!', 10, 40, 20, DARKGRAY);

      DrawFPS(10, 10);

    EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

