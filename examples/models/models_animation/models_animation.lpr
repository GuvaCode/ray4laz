program models_animation;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  Camera: TCamera;
  Model: TModel;
  Texture: TTexture2D;
  Position: TVector3;
  AnimsCount: Cardinal;
  Anims: PModelAnimation;
  AnimFrameCounter: Integer;
  I: Integer;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_HIGHDPI);
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [models] example - model animation');

  // Define the camera to look into our 3d world
  Camera := Default(TCamera);
  Camera.Position := Vector3Create(10.0, 10.0, 10.0); // Camera position
  Camera.Target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);          // Camera up vector (rotation towards target)
  Camera.Fovy := 45.0;                                  // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

  Model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/iqm/guy.iqm'));        // Load the animated model mesh and basic data
  Texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/models/iqm/guytex.png')); // Load model texture and set material
  SetMaterialTexture(@Model.Materials[0], MATERIAL_MAP_DIFFUSE, Texture);       // Set model material map texture

  Position := Vector3Create(0.0, 0.0, 0.0); // Set model position

  // Load animation data
  AnimsCount := 0;
  Anims := LoadModelAnimations(PChar(GetApplicationDirectory + 'resources/models/iqm/guyanim.iqm'), @AnimsCount);
  AnimFrameCounter := 0;
  DisableCursor;
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_FIRST_PERSON);

        // Play animation when spacebar is held down
        if IsKeyDown(KEY_SPACE) then
        begin
          Inc(AnimFrameCounter);
          UpdateModelAnimation(Model, Anims[0], AnimFrameCounter);
          if AnimFrameCounter >= Anims[0].FrameCount then
            AnimFrameCounter := 0;
        end;

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

        ClearBackground(RAYWHITE);

        BeginMode3D(Camera);

          DrawModelEx(Model, Position, Vector3Create(1.0, 0.0, 0.0), -90.0, Vector3Create(1.0, 1.0, 1.0), WHITE);

          for I := 0 to Model.BoneCount - 1 do
          begin
            DrawCube(Anims[0].FramePoses[AnimFrameCounter][I].Translation, 0.2, 0.2, 0.2, RED);
          end;

          DrawGrid(10, 1.0); // Draw a grid

        EndMode3D();

        DrawText('PRESS SPACE to PLAY MODEL ANIMATION', 10, 10, 20, MAROON);
        DrawText('(c) Guy IQM 3D model by @culacant', ScreenWidth - 200, ScreenHeight - 20, 10, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(texture);                     // Unload texture
  UnloadModelAnimations(anims, animsCount);   // Unload model animations data
  UnloadModel(model);                         // Unload model
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

