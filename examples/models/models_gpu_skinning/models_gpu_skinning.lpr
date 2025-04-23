{*******************************************************************************************
*
*   raylib [core] example - Doing skinning on the gpu using a vertex shader
*
*   Example originally created with raylib 4.5, last time updated with raylib 4.5
*
*   Example contributed by Daniel Holden (@orangeduck) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2024 Daniel Holden (@orangeduck)
*   Pascal translation 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program models_gpu_skinning;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath,
//rlgl,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  camera: TCamera;
  characterModel: TModel;
  {%H-}skinningShader: TShader;
  modelAnimations: PModelAnimation;
  animsCount: Integer;
  animIndex, animCurrentFrame: LongWord;
  position: TVector3;
  anim: TModelAnimation;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - GPU skinning');

  // Define the camera to look into our 3d world
  camera := Default(TCamera);
  camera.position := Vector3Create( 5.0, 5.0, 5.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 2.0, 0.0 );   // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );       // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                               // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;           // Camera projection type


  // Load gltf model
  characterModel := LoadModel('resources/models/gltf/greenman.glb'); // Load character model

  // Load skinning shader
  skinningShader := Default(TShader);
  skinningShader := LoadShader(TextFormat('resources/shaders/glsl%i/skinning.vs', GLSL_VERSION),
                               TextFormat('resources/shaders/glsl%i/skinning.fs', GLSL_VERSION));

  characterModel.materials[1].shader := skinningShader;

  // Load gltf model animations
  animsCount := 0;
  animIndex := 0;
  animCurrentFrame := 0;
  modelAnimations := LoadModelAnimations('resources/models/gltf/greenman.glb', @animsCount);

  position := Vector3Create( 0.0, 0.0, 0.0 ); // Set model position

  DisableCursor();  // Limit cursor to relative movement inside the window
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_THIRD_PERSON);

      // Select current animation
      if (IsKeyPressed(KEY_T)) and (animIndex < animsCount - 1)  then Inc(animIndex) else
      if (IsKeyPressed(KEY_G)) and (animIndex >= 1) then Dec(animIndex);

      // Update model animation
      anim := modelAnimations[animIndex];
      animCurrentFrame := animCurrentFrame + 1 mod anim.frameCount;
      UpdateModelAnimationBones(characterModel, anim, animCurrentFrame);

      // Draw
      BeginDrawing();

      ClearBackground(RAYWHITE);

        BeginMode3D(camera);
          // Draw character
          characterModel.transform := MatrixTranslate(position.x, position.y, position.z);
          UpdateModelAnimationBones(characterModel, anim, animCurrentFrame);
          DrawMesh(characterModel.meshes[0], characterModel.materials[1], characterModel.transform);
          DrawGrid(10, 1.0);
        EndMode3D();

        DrawText('Use the T/G to switch animation', 10, 10, 20, GRAY);

      EndDrawing();
    end;

  // De-Initialization
  UnloadModelAnimations(modelAnimations, animsCount);
  UnloadModel(characterModel);         // Unload character model and meshes/material
  UnloadShader(skinningShader);

  CloseWindow();        // Close window and OpenGL context
end.

