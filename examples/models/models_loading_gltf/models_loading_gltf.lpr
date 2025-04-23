{*******************************************************************************************
*
*   raylib [models] example - loading gltf with animations
*
*   LIMITATIONS:
*     - Only supports 1 armature per file, and skips loading it if there are multiple armatures
*     - Only supports linear interpolation (default method in Blender when checked
*       "Always Sample Animations" when exporting a GLTF file)
*     - Only supports translation/rotation/scale animation channel.path,
*       weights not considered (i.e. morph targets)
*
*   Example originally created with raylib 3.7, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2020-2023 Ramon Santamaria (@raysan5)
*   Pascal translation 2023 Vadim Gunko (@GuvaCode)
*
********************************************************************************************}
program models_loading_gltf;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  camera: TCamera;
  model: TModel;
  animsCount, animIndex, animCurrentFrame: longWord;
  modelAnimations: PModelAnimation;
  position: TVector3;
  anim: TModelAnimation;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - loading gltf');

  // Define the camera to look into our 3d world
  camera := default(TCamera);
  camera.position := Vector3Create( 5.0, 5.0, 5.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 2.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;             // Camera mode type

  // Load gltf model
  model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/gltf/robot.glb'));

  // Load gltf model animations
  animsCount := 0;
  animIndex := 0;
  animCurrentFrame := 0;
  modelAnimations := LoadModelAnimations(PChar(GetApplicationDirectory + 'resources/models/gltf/robot.glb'), @animsCount);

  position := Vector3Create( 0.0, 0.0, 0.0 );    // Set model position



  DisableCursor();                    // Limit cursor to relative movement inside the window

  SetTargetFPS(60);                       // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Select current animation
      if (IsKeyPressed(KEY_z)) then animIndex := (animIndex + 1) mod animsCount
      else
        if (IsKeyPressed(KEY_x)) then animIndex := (animIndex + animsCount - 1) mod animsCount;

      // Update model animation
      anim := modelAnimations[animIndex];
      animCurrentFrame := (animCurrentFrame + 1) mod anim.frameCount;
      UpdateModelAnimation(model, anim, animCurrentFrame);

      // Update camera
      UpdateCamera(@camera,CAMERA_THIRD_PERSON);

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(camera);
          DrawModel(model, position, 1.0, WHITE);    // Draw animated model
          DrawGrid(10, 1.0);
      EndMode3D();

      DrawText('Use the ''Z'' or ''X'' keys to switch animation', 10, 10, 20, GRAY);
      DrawText(TextFormat('Animation: %s', anim.name), 10, GetScreenHeight() - 20, 10, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadModel(model);         // Unload model and meshes/material
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

