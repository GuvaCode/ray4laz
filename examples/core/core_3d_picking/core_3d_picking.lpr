program core_3d_picking;

{$mode objfpc}{$H+}

uses 
cmem,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;

var
  Camera: TCamera3D;
  CubePosition: TVector3;
  CubeSize: TVector3;
  Ray: TRay;
  Collision: TRayCollision;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d picking');

   // Define the camera to look into our 3d world
  Camera := Default(TCamera3D);
  Camera.Position := Vector3Create(10.0, 10.0, 10.0); // Camera position
  Camera.Target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);          // Camera up vector (rotation towards target)
  Camera.Fovy := 45.0;                                  // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

  CubePosition := Vector3Create(0.0, 1.0, 0.0);
  CubeSize := Vector3Create(2.0, 2.0, 2.0);

  Ray := Default(TRay); // Picking line ray

  Collision := Default(TRayCollision);


  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
          UpdateCamera(@Camera,CAMERA_FIRST_PERSON);

    if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
    begin
      if not Collision.Hit then
      begin
        Ray := GetMouseRay(GetMousePosition(), Camera);

        // Check collision between ray and box
        Collision := GetRayCollisionBox(
          Ray,
          BoundingBoxCreate(
          Vector3Create(CubePosition.X - CubeSize.X / 2, CubePosition.Y - CubeSize.Y / 2, CubePosition.Z - CubeSize.Z / 2),
          Vector3Create(CubePosition.X + CubeSize.X / 2, CubePosition.Y + CubeSize.Y / 2, CubePosition.Z + CubeSize.Z / 2)
          )
        );
      end else
        Collision.Hit := False;
    end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

        BeginMode3D(Camera);

          if Collision.Hit then
          begin
            DrawCube(CubePosition, CubeSize.X, CubeSize.Y, CubeSize.Z, RED);
            DrawCubeWires(CubePosition, CubeSize.X, CubeSize.Y, CubeSize.Z, MAROON);

            DrawCubeWires(CubePosition, CubeSize.X + 0.2, CubeSize.Y + 0.2, CubeSize.Z + 0.2, GREEN);
          end else
          begin
            DrawCube(CubePosition, CubeSize.X, CubeSize.Y, CubeSize.Z, GRAY);
            DrawCubeWires(CubePosition, CubeSize.X, CubeSize.Y, CubeSize.Z, DARKGRAY);
          end;

          DrawRay(Ray, MAROON);
          DrawGrid(10, 1.0);

        EndMode3D();

        DrawText('Try selecting the box with mouse!', 240, 10, 20, DARKGRAY);

        if Collision.Hit then
        DrawText('BOX SELECTED', (screenWidth - MeasureText('BOX SELECTED', 30)) div 2,
      //  DrawText('BOX SELECTED', ScreenWidth - MeasureText('BOX SELECTED', 30) div 2,
          Trunc(ScreenHeight * 0.1), 30, GREEN);

        DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

