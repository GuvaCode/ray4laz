program ModelAnimationBlending;

uses
  Raylib;

const
  GLSL_VERSION = 330;

var
  screenWidth: Integer = 800;
  screenHeight: Integer = 450;

  camera: TCamera;
  characterModel: TModel;
  animsCount: Integer;
  animIndex0: Cardinal;
  animIndex1: Cardinal;
  animCurrentFrame: Cardinal;
  modelAnimations: PModelAnimation;
  blendFactor: Single;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - Model Animation Blending');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create(8.0, 8.0, 8.0);      // Camera position
  camera.target := Vector3Create(0.0, 2.0, 0.0);        // Camera looking at point
  camera.up := Vector3Create(0.0, 1.0, 0.0);            // Camera up vector
  camera.fovy := 45.0;                                   // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;               // Camera projection type

  // Load gltf model
  characterModel := LoadModel('resources/models/gltf/robot.glb');

  // Load gltf model animations
  animsCount := 0;
  animIndex0 := 0;
  animIndex1 := 0;
  animCurrentFrame := 0;
  modelAnimations := LoadModelAnimations('resources/models/gltf/robot.glb', @animsCount);

  blendFactor := 0.5;

  DisableCursor;
  SetTargetFPS(60);

  // Main game loop
  while not WindowShouldClose do
  begin
    // Update
    UpdateCamera(@camera, CAMERA_THIRD_PERSON);

    // Select current animation
    if IsKeyPressed(KEY_T) then
      animIndex0 := (animIndex0 + 1) mod animsCount
    else if IsKeyPressed(KEY_G) then
      animIndex0 := (animIndex0 + animsCount - 1) mod animsCount;

    if IsKeyPressed(KEY_Y) then
      animIndex1 := (animIndex1 + 1) mod animsCount
    else if IsKeyPressed(KEY_H) then
      animIndex1 := (animIndex1 + animsCount - 1) mod animsCount;

    // Select blend factor
    if IsKeyPressed(KEY_U) then
    begin
      blendFactor := blendFactor - 0.1;
      if blendFactor < 0.0 then blendFactor := 0.0;
    end
    else if IsKeyPressed(KEY_J) then
    begin
      blendFactor := blendFactor + 0.1;
      if blendFactor > 1.0 then blendFactor := 1.0;
    end;

    // Update animation
    animCurrentFrame := animCurrentFrame + 1;

    // Update bones with blending
    UpdateModelAnimationBonesLerp(characterModel,
                                  modelAnimations[animIndex0],
                                  animCurrentFrame,
                                  modelAnimations[animIndex1],
                                  animCurrentFrame,
                                  blendFactor);

    UpdateModelVertsToCurrentBones(characterModel);

    // Draw
    BeginDrawing;
    ClearBackground(RAYWHITE);

    BeginMode3D(camera);

    DrawModel(characterModel, Vector3Create(0.0, 0.0, 0.0), 1.0, WHITE);
    DrawGrid(10, 1.0);

    EndMode3D;

    DrawText('Use the U/J to adjust blend factor', 10, 10, 20, GRAY);
    DrawText('Use the T/G to switch first animation', 10, 30, 20, GRAY);
    DrawText('Use the Y/H to switch second animation', 10, 50, 20, GRAY);
    DrawText(TextFormat('Animations: %s, %s',
             modelAnimations[animIndex0].name,
             modelAnimations[animIndex1].name), 10, 70, 20, BLACK);
    DrawText(TextFormat('Blend Factor: %f', blendFactor), 10, 86, 20, BLACK);

    EndDrawing;
  end;

  // De-Initialization
  UnloadModelAnimations(modelAnimations, animsCount);
  UnloadModel(characterModel);
  CloseWindow;

end.
