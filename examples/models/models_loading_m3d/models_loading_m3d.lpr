program models_loading_m3d;

{$mode objfpc}{$H+}

uses 
cmem, rlgl,
raylib, raymath, sysutils;

const
  screenWidth = 800;
  screenHeight = 600;

var
  camera:TCamera;
  Position: TVector3;
  model,model2:TModel;
  anims: PModelAnimation;
  drawMesh,drawSkeleton,animPlaying : boolean;
  animsCount: longint;
  animFrameCounter: integer;
  animId: integer;
  i: integer;



begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - M3D model');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 1.5, 1.5, 1.5 ); // Camera position
  camera.target := Vector3Create( 0.0, 0.4, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type
  position := Vector3Create( 0, 0, 0 );                // Set model position

  // Load model
  model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/m3d/cesium_man.m3d')); // Load the animated model mesh and basic data
  model2 := LoadModel(PChar(GetApplicationDirectory + 'resources/models/m3d/cesium_man.m3d')); // Load the animated model mesh and basic data
  drawMesh := true;
  drawSkeleton := true;
  animPlaying := false;   // Store anim state, what to draw

  // Load animation data
  animsCount:= 0;
  animFrameCounter := 0;
  animId := 0;
  anims := LoadModelAnimations(PChar(GetApplicationDirectory + 'resources/models/m3d/cesium_man.m3d'), @animsCount);

  disableCursor;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera,CAMERA_FIRST_PERSON);
      // Play animation when spacebar is held down
      if animsCount>=1 then
       begin
           // Play animation when spacebar is held down (or step one frame with N)
           if IsKeyDown(KEY_SPACE) or IsKeyPressed(KEY_N) then
           begin
              Inc(animFrameCounter);//++;

               if (animFrameCounter >= anims[animId].frameCount) then animFrameCounter := 0;

               UpdateModelAnimation(model, anims[animId], animFrameCounter);
               animPlaying := true;
           end;

    // Select animation by pressing A
            if (IsKeyPressed(KEY_Z)) then
            begin
                animFrameCounter := 0;
                Inc(animId);//++;

                if (animId >= animsCount) then animId := 0;
                UpdateModelAnimation(model, anims[animId], 0);
                animPlaying := true;
            end;
        end;

       // Toggle skeleton drawing
        if (IsKeyPressed(KEY_X)) then drawSkeleton:= not drawSkeleton;

        // Toggle mesh drawing
        if (IsKeyPressed(KEY_M)) then drawMesh:= not drawMesh;


      // Draw
      BeginDrawing();

       ClearBackground(RAYWHITE);
        BeginMode3D(camera);

         // Draw 3d model with texture
         if (drawMesh) then  DrawModel(model, position, 1.0, WHITE);

         // Draw the animated skeleton
         if (drawSkeleton) then
           begin
           // Loop to (boneCount - 1) because the last one is a special "no bone" bone,
           // needed to workaround buggy models
           // without a -1, we would always draw a cube at the origin
           for i := 0 to model.boneCount - 1 do
             begin
             // By default the model is loaded in bind-pose by LoadModel().
             // But if UpdateModelAnimation() has been called at least once
             // then the model is already in animation pose, so we need the animated skeleton
             if (not animPlaying) or (animsCount<=0) then
               begin
               // Display the bind-pose skeleton
               DrawCube(model.bindPose[i].translation, 0.04, 0.04, 0.04, RED);

               if (model.bones[i].parent >= 0) then
                 begin
                   DrawLine3D(model.bindPose[i].translation,
                   model.bindPose[model.bones[i].parent].translation, RED);
                 end;
               end
                 else
               begin
               // Display the frame-pose skeleton
               DrawCube(anims[animId].framePoses[animFrameCounter][i].translation, 0.05, 0.05, 0.05, RED);

               if (anims[animId].bones[i].parent >= 0) then
                 begin


                   DrawLine3D(anims[animId].framePoses[animFrameCounter][i].translation,
                   anims[animId].framePoses[animFrameCounter][anims[animId].bones[i].parent].translation, RED);
                 end;
               end;
             end;
           end;

        DrawGrid(10, 1.0);// Draw a grid

        EndMode3D();
        DrawFPS(10,10);
        DrawText('PRESS SPACE to PLAY MODEL ANIMATION', 10, GetScreenHeight() - 60, 10, MAROON);
        DrawText('PRESS Z to CYCLE THROUGH ANIMATIONS', 10, GetScreenHeight() - 40, 10, DARKGRAY);
        DrawText('PRESS M to toggle MESH, X to toggle SKELETON DRAWING', 10, GetScreenHeight() - 20, 10, DARKGRAY);
        DrawText('(c) SpaceSuit model by Quaternius', GetScreenWidth() - 210, GetScreenHeight() - 20, 10, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  // Unload model animations data
  UnloadModelAnimations(anims, animsCount);
  UnloadModel(model);         // Unload model
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

