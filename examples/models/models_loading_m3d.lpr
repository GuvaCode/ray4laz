program models_loading_m3d;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 600;

var
  camera:TCamera;
  Position: TVector3;
  model:TModel;
  anims: PModelAnimation;
  animsCount, animId, animFrameCounter, i:integer;
begin

  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - M3D model');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 10.0, 10.0, 10.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
  position := Vector3Create( 0, 0, 0 );           // Set model position

  // Load model
  model := LoadModel('resources/models/m3d/Spacesuit.iqm'); // Load the animated model mesh and basic data

  // Load animation data
  animsCount := 0;
  anims := LoadModelAnimations('resources/models/m3d/Spacesuit.iqm', @animsCount);
  animFrameCounter := 0;
  animId := 0;

  SetCameraMode(camera, CAMERA_FREE); // Set free camera mode

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera);
      // Play animation when spacebar is held down
        if (animsCount) >= 1 then
        begin
            if (IsKeyDown(KEY_SPACE)) then
            begin
                Inc(animFrameCounter);//++;
                UpdateModelAnimation(model, anims[animId], animFrameCounter);
                if (animFrameCounter >= anims[animId].frameCount) then animFrameCounter := 0;
            end;

            // Select animation on mouse click
            if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) then
            begin
                animFrameCounter := 0;
                Inc(animId);//++;
                if (animId >= animsCount) then animId := 0;
                UpdateModelAnimation(model, anims[animId], 0);
            end;
        end;

      // Draw
      BeginDrawing();

       ClearBackground(RAYWHITE);
        BeginMode3D(camera);
          DrawModel(model, position, 1.0, WHITE);        // Draw 3d model with texture

        if (animsCount) >= 1  then
            for i := 0 to model.boneCount-1 do
              begin
               // DrawCube(anims[animId].framePoses[animFrameCounter][i].translation, 0.2, 0.2, 0.2, RED);
                end;


           DrawGrid(10, 1.0);         // Draw a grid
          EndMode3D();

            DrawText('PRESS SPACE to PLAY MODEL ANIMATION', 10, GetScreenHeight() - 30, 10, MAROON);
            DrawText('MOUSE LEFT BUTTON to CYCLE THROUGH ANIMATIONS', 10, GetScreenHeight() - 20, 10, DARKGRAY);
            DrawText('(c) Suzanne 3D model by blender', screenWidth - 200, screenHeight - 20, 10, GRAY);


      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

