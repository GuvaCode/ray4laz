{*******************************************************************************************
*
*   raylib [core] example - Using bones as socket for calculating the positioning of something
*
*   Example originally created with raylib 4.5, last time updated with raylib 4.5
*
*   Example contributed by iP (@ipzaur) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2024 iP (@ipzaur)
*   pascal conversion 2024 Gunko Vadim
*
********************************************************************************************}
program models_bone_socket;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath,
rlgl,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  BONE_SOCKETS = 2;
  BONE_SOCKET_HAT = 0;
  BONE_SOCKET_HAND_R = 1;
  BONE_SOCKET_HAND_L = 2;

var
  camera: TCamera;
  characterModel: TModel;
  equipModel: array[0..BONE_SOCKETS] of TModel;
  showEquip: array[0..2] of Boolean;
  animsCount, i: integer;
  animIndex, animCurrentFrame: LongWord;
  modelAnimations: PModelAnimation;
  anim: TModelAnimation;
  boneSocketIndex: array[0..BONE_SOCKETS] of integer;
  position: TVector3;
  angle: Word;
  characterRotate, inRotation, outRotation, rotate: TQuaternion;
  matrixTransform: TMatrix;
  transform: PTransform;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - bone socket');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 5.0, 5.0, 5.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 2.0, 0.0 );   // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );       // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                               // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;           // Camera projection type

  characterModel := LoadModel('resources/models/gltf/greenman.glb'); // Load character model
  equipModel[0] := LoadModel('resources/models/gltf/greenman_hat.glb');    // Index for the hat model is the same as BONE_SOCKET_HAT
  equipModel[1] := LoadModel('resources/models/gltf/greenman_sword.glb');  // Index for the sword model is the same as BONE_SOCKET_HAND_R
  equipModel[2] := LoadModel('resources/models/gltf/greenman_shield.glb');  // Index for the shield model is the same as BONE_SOCKET_HAND_L

  showEquip[0] := True; // Toggle on/off equip
  showEquip[1] := True;
  showEquip[2] := True;

  // Load gltf model animations
  animsCount := 0;
  animIndex := 0;
  animCurrentFrame := 0;
  modelAnimations := LoadModelAnimations('resources/models/gltf/greenman.glb', @animsCount);

  // indices of bones for sockets
  boneSocketIndex[0] := -1;
  boneSocketIndex[1] := -1;
  boneSocketIndex[2] := -1;

  // search bones for sockets
  for i := 0 to characterModel.boneCount -1 do //(int i = 0; i < characterModel.boneCount; i++)
  begin
    if (TextIsEqual(characterModel.bones[i].name, 'socket_hat')) then
    begin
      boneSocketIndex[BONE_SOCKET_HAT] := i;
      continue;
    end;
    if (TextIsEqual(characterModel.bones[i].name, 'socket_hand_R')) then
    begin
      boneSocketIndex[BONE_SOCKET_HAND_R] := i;
      continue;
    end;
    if (TextIsEqual(characterModel.bones[i].name, 'socket_hand_L')) then
    begin
      boneSocketIndex[BONE_SOCKET_HAND_L] := i;
      continue;
    end;
  end;

  position := Vector3Create( 0.0, 0.0, 0.0 ); // Set model position
  angle := 0;           // Set angle for rotate character

  DisableCursor();                    // Limit cursor to relative movement inside the window
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      UpdateCamera(@camera, CAMERA_THIRD_PERSON);

      // Rotate character
      if (IsKeyDown(KEY_F)) then angle := (angle + 1) mod 360
      else if (IsKeyDown(KEY_H)) then angle := (360 + angle - 1) mod 360;

      // Select current animation
      if (IsKeyPressed(KEY_T)) then animIndex := (animIndex + 1) mod animsCount
      else if (IsKeyPressed(KEY_G)) then animIndex := (animIndex + animsCount - 1) mod animsCount;

      // Toggle shown of equip
      if (IsKeyPressed(KEY_ONE)) then showEquip[BONE_SOCKET_HAT] := not showEquip[BONE_SOCKET_HAT];
      if (IsKeyPressed(KEY_TWO)) then showEquip[BONE_SOCKET_HAND_R] := not showEquip[BONE_SOCKET_HAND_R];
      if (IsKeyPressed(KEY_THREE)) then showEquip[BONE_SOCKET_HAND_L] :=  not showEquip[BONE_SOCKET_HAND_L];

      // Update model animation
      anim := modelAnimations[animIndex];
      animCurrentFrame := (animCurrentFrame + 1) mod anim.frameCount;
      UpdateModelAnimation(characterModel, anim, animCurrentFrame);
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginMode3D(camera);
          // Draw character
          characterRotate := QuaternionFromAxisAngle(Vector3Create( 0.0, 1.0, 0.0 ), angle*DEG2RAD);
          characterModel.transform := MatrixMultiply(QuaternionToMatrix(characterRotate), MatrixTranslate(position.x, position.y, position.z));
          UpdateModelAnimation(characterModel, anim, animCurrentFrame);
          DrawMesh(characterModel.meshes[0], characterModel.materials[1], characterModel.transform);

          // Draw equipments (hat, sword, shield)
          for i:= 0 to BONE_SOCKETS do
          begin
            if (not showEquip[i]) then continue;
            transform := @anim.framePoses[animCurrentFrame][boneSocketIndex[i]];
            inRotation := characterModel.bindPose[boneSocketIndex[i]].rotation;
            outRotation := transform^.rotation;
            // Calculate socket rotation (angle between bone in initial pose and same bone in current animation frame)
            rotate := QuaternionMultiply(outRotation, QuaternionInvert(inRotation));
            matrixTransform := QuaternionToMatrix(rotate);
            // Translate socket to its position in the current animation
            matrixTransform := MatrixMultiply(matrixTransform, MatrixTranslate(transform^.translation.x, transform^.translation.y, transform^.translation.z));
            // Transform the socket using the transform of the character (angle and translate)
            matrixTransform := MatrixMultiply(matrixTransform, characterModel.transform);
            // Draw mesh at socket position with socket angle rotation
            DrawMesh(equipModel[i].meshes[0], equipModel[i].materials[1], matrixTransform);
          end;

          DrawGrid(10, 1.0);
          EndMode3D();

          DrawText('Use the T/G to switch animation', 10, 10, 20, GRAY);
          DrawText('Use the F/H to rotate character left/right', 10, 35, 20, GRAY);
          DrawText('Use the 1,2,3 to toggle shown of hat, sword and shield', 10, 60, 20, GRAY);
       EndDrawing();
    end;

  // De-Initialization
  UnloadModelAnimations(modelAnimations, animsCount);
  UnloadModel(characterModel);         // Unload character model and meshes/material
  for i :=0 to BONE_SOCKETS do UnloadModel(equipModel[i]);
  CloseWindow();        // Close window and OpenGL context
end.

