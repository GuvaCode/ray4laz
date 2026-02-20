program ModelAnimationBoneBlending;

uses
  Raylib, raymath, SysUtils;

const
  GLSL_VERSION = 330;

//----------------------------------------------------------------------------------
// Module Functions Definition (объявлены ДО основной программы)
//----------------------------------------------------------------------------------

// Check if a bone is part of upper body (for selective blending)
function IsUpperBodyBone(const boneName: PChar): Boolean;
begin
  Result := False;

  // Common upper body bone names (adjust based on your model)
  if (TextIsEqual(boneName, 'spine') or TextIsEqual(boneName, 'spine1') or TextIsEqual(boneName, 'spine2') or
      TextIsEqual(boneName, 'chest') or TextIsEqual(boneName, 'upperChest') or
      TextIsEqual(boneName, 'neck') or TextIsEqual(boneName, 'head') or
      TextIsEqual(boneName, 'shoulder') or TextIsEqual(boneName, 'shoulder_L') or TextIsEqual(boneName, 'shoulder_R') or
      TextIsEqual(boneName, 'upperArm') or TextIsEqual(boneName, 'upperArm_L') or TextIsEqual(boneName, 'upperArm_R') or
      TextIsEqual(boneName, 'lowerArm') or TextIsEqual(boneName, 'lowerArm_L') or TextIsEqual(boneName, 'lowerArm_R') or
      TextIsEqual(boneName, 'hand') or TextIsEqual(boneName, 'hand_L') or TextIsEqual(boneName, 'hand_R') or
      TextIsEqual(boneName, 'clavicle') or TextIsEqual(boneName, 'clavicle_L') or TextIsEqual(boneName, 'clavicle_R')) then
  begin
    Result := True;
    Exit;
  end;

  // Check if bone name contains upper body keywords
  if (Pos('spine', boneName) > 0) or (Pos('chest', boneName) > 0) or
     (Pos('neck', boneName) > 0) or (Pos('head', boneName) > 0) or
     (Pos('shoulder', boneName) > 0) or (Pos('arm', boneName) > 0) or
     (Pos('hand', boneName) > 0) or (Pos('clavicle', boneName) > 0) then
  begin
    Result := True;
  end;
end;

// Blend two animations per-bone with selective upper/lower body blending
procedure BlendModelAnimationsBones(model: PModel; anim1: PModelAnimation; frame1: Integer;
  anim2: PModelAnimation; frame2: Integer; blendFactor: Single; upperBodyBlend: Boolean);
var
  firstMeshWithBones, boneCount, boneId, i: Integer;
  boneBlendFactor: Single;
  boneName: PChar;
  isUpperBody: Boolean;
  bindTransform, anim1Transform, anim2Transform: PTransform;
  blended: TTransform;
  bindMatrix, blendedMatrix: TMatrix;
begin
  // Validate inputs
  if (anim1^.boneCount = 0) or (anim1^.framePoses = nil) or
     (anim2^.boneCount = 0) or (anim2^.framePoses = nil) or
     (model^.boneCount = 0) or (model^.bindPose = nil) then
  begin
    Exit;
  end;

  // Clamp blend factor to [0, 1]
  if blendFactor < 0.0 then blendFactor := 0.0;
  if blendFactor > 1.0 then blendFactor := 1.0;

  // Ensure frame indices are valid
  if frame1 >= anim1^.frameCount then frame1 := anim1^.frameCount - 1;
  if frame2 >= anim2^.frameCount then frame2 := anim2^.frameCount - 1;
  if frame1 < 0 then frame1 := 0;
  if frame2 < 0 then frame2 := 0;

  // Find first mesh with bones
  firstMeshWithBones := -1;
  for i := 0 to model^.meshCount - 1 do
  begin
    if model^.meshes[i].boneMatrices <> nil then
    begin
      firstMeshWithBones := i;
      Break;
    end;
  end;

  if firstMeshWithBones = -1 then Exit;

  // Get bone count (use minimum of all to be safe)
  boneCount := model^.boneCount;
  if anim1^.boneCount < boneCount then boneCount := anim1^.boneCount;
  if anim2^.boneCount < boneCount then boneCount := anim2^.boneCount;

  // Blend each bone
  for boneId := 0 to boneCount - 1 do
  begin
    // Determine blend factor for this bone
    boneBlendFactor := blendFactor;

    // If upper body blending is enabled, use different blend factors for upper vs lower body
    if upperBodyBlend then
    begin
      boneName := model^.bones[boneId].name;
      isUpperBody := IsUpperBodyBone(boneName);

      // Upper body: use anim2 (attack), Lower body: use anim1 (walk)
      // blendFactor = 0.0 means full anim1 (walk), 1.0 means full anim2 (attack)
      if isUpperBody then
        boneBlendFactor := blendFactor // Upper body: blend towards anim2 (attack)
      else
        boneBlendFactor := 1.0 - blendFactor; // Lower body: blend towards anim1 (walk) - invert the blend
    end;

    // Get transforms from both animations
    bindTransform := @model^.bindPose[boneId];
    anim1Transform := @anim1^.framePoses[frame1][boneId];
    anim2Transform := @anim2^.framePoses[frame2][boneId];

    // Blend the transforms
    blended.translation := Vector3Lerp(anim1Transform^.translation, anim2Transform^.translation, boneBlendFactor);
    blended.rotation := QuaternionSlerp(anim1Transform^.rotation, anim2Transform^.rotation, boneBlendFactor);
    blended.scale := Vector3Lerp(anim1Transform^.scale, anim2Transform^.scale, boneBlendFactor);

    // Convert bind pose to matrix
    bindMatrix := MatrixMultiply(
                    MatrixMultiply(
                      MatrixScale(bindTransform^.scale.x, bindTransform^.scale.y, bindTransform^.scale.z),
                      QuaternionToMatrix(bindTransform^.rotation)),
                    MatrixTranslate(bindTransform^.translation.x, bindTransform^.translation.y, bindTransform^.translation.z));

    // Convert blended transform to matrix
    blendedMatrix := MatrixMultiply(
                       MatrixMultiply(
                         MatrixScale(blended.scale.x, blended.scale.y, blended.scale.z),
                         QuaternionToMatrix(blended.rotation)),
                       MatrixTranslate(blended.translation.x, blended.translation.y, blended.translation.z));

    // Calculate final bone matrix (similar to UpdateModelAnimationBones)
    model^.meshes[firstMeshWithBones].boneMatrices[boneId] := MatrixMultiply(MatrixInvert(bindMatrix), blendedMatrix);
  end;

  // Copy bone matrices to remaining meshes
  for i := firstMeshWithBones + 1 to model^.meshCount - 1 do
  begin
    if model^.meshes[i].boneMatrices <> nil then
    begin
      Move(model^.meshes[firstMeshWithBones].boneMatrices^,
           model^.meshes[i].boneMatrices^,
           model^.meshes[i].boneCount * SizeOf(TMatrix));
    end;
  end;
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth: Integer = 800;
  screenHeight: Integer = 450;

  camera: TCamera;
  characterModel: TModel;
  skinningShader: TShader;
  animsCount: Integer;
  modelAnimations: PModelAnimation;
  animIndex1: Cardinal;
  animIndex2: Cardinal;
  animCurrentFrame1: Cardinal;
  animCurrentFrame2: Cardinal;
  i: Integer;
  position: TVector3;
  upperBodyBlendMode: Boolean;
  currentAnim1, currentAnim2: TModelAnimation;
  currentBlendFactor: Single;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - animation bone blending');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create(5.0, 5.0, 5.0);      // Camera position
  camera.target := Vector3Create(0.0, 2.0, 0.0);        // Camera looking at point
  camera.up := Vector3Create(0.0, 1.0, 0.0);            // Camera up vector
  camera.fovy := 45.0;                                   // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;               // Camera projection type

  // Load gltf model
  characterModel := LoadModel('resources/models/gltf/greenman.glb');

  // Load skinning shader
  skinningShader := LoadShader(TextFormat('resources/shaders/glsl%i/skinning.vs', GLSL_VERSION),
                               TextFormat('resources/shaders/glsl%i/skinning.fs', GLSL_VERSION));

  characterModel.materials[1].shader := skinningShader;

  // Load gltf model animations
  animsCount := 0;
  modelAnimations := LoadModelAnimations('resources/models/gltf/greenman.glb', @animsCount);

  // Log all available animations for debugging
  TraceLog(LOG_INFO, TextFormat('Found %d animations:', animsCount));
  for i := 0 to animsCount - 1 do
  begin
    TraceLog(LOG_INFO, TextFormat('  Animation %d: %s (%d frames)', i, modelAnimations[i].name, modelAnimations[i].frameCount));
  end;

  // Use specific indices: walk/move = 2, attack = 3
  animIndex1 := 2; // Walk/Move animation (index 2)
  animIndex2 := 3; // Attack animation (index 3)
  animCurrentFrame1 := 0;
  animCurrentFrame2 := 0;

  // Validate indices
  if animIndex1 >= animsCount then animIndex1 := 0;
  if animIndex2 >= animsCount then
  begin
    if animsCount > 1 then animIndex2 := 1
    else animIndex2 := 0;
  end;

  TraceLog(LOG_INFO, TextFormat('Using Walk (index %d): %s', animIndex1, modelAnimations[animIndex1].name));
  TraceLog(LOG_INFO, TextFormat('Using Attack (index %d): %s', animIndex2, modelAnimations[animIndex2].name));

  position := Vector3Create(0.0, 0.0, 0.0); // Set model position
  upperBodyBlendMode := True; // Toggle: true = upper/lower body blending, false = uniform blending (50/50)

  DisableCursor;
  SetTargetFPS(60);

  // Main game loop
  while not WindowShouldClose do
  begin
    // Update
    UpdateCamera(@camera, CAMERA_THIRD_PERSON);

    // Toggle upper/lower body blending mode (SPACE key)
    if IsKeyPressed(KEY_SPACE) then upperBodyBlendMode := not upperBodyBlendMode;

    // Update animation frames
    currentAnim1 := modelAnimations[animIndex1];
    currentAnim2 := modelAnimations[animIndex2];

    animCurrentFrame1 := (animCurrentFrame1 + 1) mod currentAnim1.frameCount;
    animCurrentFrame2 := (animCurrentFrame2 + 1) mod currentAnim2.frameCount;

    // Blend the two animations
    characterModel.transform := MatrixTranslate(position.x, position.y, position.z);

    // When upperBodyBlendMode is ON: upper body = attack (1.0), lower body = walk (0.0)
    // When upperBodyBlendMode is OFF: uniform blend at 0.5 (50% walk, 50% attack)
    if upperBodyBlendMode then currentBlendFactor := 1.0
    else currentBlendFactor := 0.5;

    BlendModelAnimationsBones(@characterModel, @currentAnim1, animCurrentFrame1,
                              @currentAnim2, animCurrentFrame2, currentBlendFactor, upperBodyBlendMode);

    // Draw
    BeginDrawing;
    ClearBackground(RAYWHITE);

    BeginMode3D(camera);

    // Draw character mesh, pose calculation is done in shader (GPU skinning)
    DrawMesh(characterModel.meshes[0], characterModel.materials[1], characterModel.transform);
    DrawGrid(10, 1.0);

    EndMode3D;

    // Draw UI
    DrawText('BONE BLENDING EXAMPLE', 10, 10, 20, DARKGRAY);
    DrawText(TextFormat('Walk (Animation 2): %s', currentAnim1.name), 10, 35, 10, GRAY);
    DrawText(TextFormat('Attack (Animation 3): %s', currentAnim2.name), 10, 50, 10, GRAY);

    if upperBodyBlendMode then
      DrawText('Mode: Upper/Lower Body Blending', 10, 65, 10, GRAY)
    else
      DrawText('Mode: Uniform Blending', 10, 65, 10, GRAY);

    DrawText('SPACE - Toggle blending mode', 10, GetScreenHeight - 20, 10, DARKGRAY);

    EndDrawing;
  end;

  // De-Initialization
  UnloadModelAnimations(modelAnimations, animsCount);
  UnloadModel(characterModel);
  UnloadShader(skinningShader);

  CloseWindow;
end.
