program models_orthographic_projection;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  FOVY_PERSPECTIVE   = 45.0;
  WIDTH_ORTHOGRAPHIC = 10.0;
var
  Camera: TCamera;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - geometric shapes');

  // Define the camera to look into our 3d world
  Camera := Default(TCamera);
  Camera.Position := Vector3Create(0.0, 10.0, 10.0);  // Camera position
  Camera.Target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);          // Camera up vector (rotation towards target)
  Camera.Fovy := FOVY_PERSPECTIVE;                      // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_SPACE) then
       begin
         if Camera.Projection = CAMERA_PERSPECTIVE then
         begin
           Camera.Fovy := WIDTH_ORTHOGRAPHIC;
           Camera.Projection := CAMERA_ORTHOGRAPHIC;
         end else
         begin
           Camera.Fovy := FOVY_PERSPECTIVE;
           Camera.Projection := CAMERA_PERSPECTIVE;
         end;
       end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       BeginMode3D(camera);

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

         DrawGrid(10, 1.0);        // Draw a grid

       EndMode3D();

       DrawText('Press Spacebar to switch camera type', 10, GetScreenHeight() - 30, 20, DARKGRAY);

       if Camera.Projection = CAMERA_ORTHOGRAPHIC then
         DrawText('ORTHOGRAPHIC', 10, 40, 20, BLACK)
       else if Camera.Projection = CAMERA_PERSPECTIVE then
         DrawText('PERSPECTIVE', 10, 40, 20, BLACK);

       DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

