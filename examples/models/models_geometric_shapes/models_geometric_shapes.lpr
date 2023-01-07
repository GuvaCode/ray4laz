program models_geometric_shapes;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Camera: TCamera;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - geometric shapes');

  // Define the camera to look into our 3d world
  Camera := Camera3DCreate(
    Vector3Create(0.0, 10.0, 10.0),
    Vector3Create(0.0, 0.0, 0.0),
    Vector3Create(0.0, 1.0, 0.0),
    45.0,CAMERA_PERSPECTIVE);

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

        DrawCube(Vector3Create(-4.0, 0.0, 2.0), 2.0, 5.0, 2.0, RED);
        DrawCubeWires(Vector3Create(-4.0, 0.0, 2.0), 2.0, 5.0, 2.0, GOLD);
        DrawCubeWires(Vector3Create(-4.0, 0.0, -2.0), 3.0, 6.0, 2.0, MAROON);

        DrawSphere(Vector3Create(-1.0, 0.0, -2.0), 1.0, GREEN);
        DrawSphereWires(Vector3Create(1.0, 0.0, 2.0), 2.0, 16, 16, LIME);

        DrawCylinder(Vector3Create(4.0, 0.0, -2.0), 1.0, 2.0, 3.0, 4, SKYBLUE);
        DrawCylinderWires(Vector3Create(4.0, 0.0, -2.0), 1.0, 2.0, 3.0, 4, DARKBLUE);
        DrawCylinderWires(Vector3Create(4.5, -1.0, 2.0), 1.0, 1.0, 2.0, 6, BROWN);

        DrawCylinder(Vector3Create(1.0, 0.0, -4.0), 0.0, 1.5, 3.0, 8, GOLD);
        DrawCylinderWires(Vector3Create(1.0, 0.0, -4.0), 0.0, 1.5, 3.0, 8, PINK);

        DrawCapsule     (Vector3Create(-3.0, 1.5, -4.0), Vector3Create(-4.0, -1.0, -4.0), 1.2, 8, 8, VIOLET);
        DrawCapsuleWires(Vector3Create(-3.0, 1.5, -4.0), Vector3Create(-4.0, -1.0, -4.0), 1.2, 8, 8, PURPLE);

        DrawGrid(10, 1.0);        // Draw a grid

      EndMode3D();

      DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

