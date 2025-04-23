{*******************************************************************************************
*
*   raylib [shaders] example - Shadowmap
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example contributed by @TheManTheMythTheGameDev and reviewed by Ramon Santamaria (@raysan5)
*   Pacal conversion by Gunko Vadim (@GuvaCode)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
********************************************************************************************}
program shaders_shadowmap;

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
  GLSL_VERSION = 330;
  SHADOWMAP_RESOLUTION = 1024;
  cameraSpeed = 0.05;

function LoadShadowmapRenderTexture(width, height: Integer): TRenderTexture2D;
var target: TRenderTexture2D;
begin
  target.id := rlLoadFramebuffer();   // Load an empty framebuffer
  target.texture.width := width;
  target.texture.height := height;
  if (target.id > 0) then
  begin
    rlEnableFramebuffer(target.id);
    // Create depth texture
    // We don't need a color texture for the shadowmap
    target.depth.id := rlLoadTextureDepth(width, height, false);
    target.depth.width := width;
    target.depth.height := height;
    target.depth.format := 19;       //DEPTH_COMPONENT_24BIT?
    target.depth.mipmaps := 1;
    // Attach depth texture to FBO
    rlFramebufferAttach(target.id, target.depth.id, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_TEXTURE2D, 0);
    // Check if fbo is complete with attachments (valid)
    if rlFramebufferComplete(target.id) then TRACELOG(LOG_INFO, 'FBO: [ID %i] Framebuffer object created successfully', target.id);
    rlDisableFramebuffer();
  end
  else TRACELOG(LOG_WARNING, 'FBO: Framebuffer object can not be created');
  result := target;
end;


procedure UnloadShadowmapRenderTexture(target: TRenderTexture2D);
begin
  if (target.id > 0) then
  // NOTE: Depth texture/renderbuffer is automatically
  // queried and deleted before deleting framebuffer
  rlUnloadFramebuffer(target.id);
end;

procedure DrawScene(cube, robot: TModel);
begin
  DrawModelEx(cube, Vector3Zero(), Vector3Create(0.0, 1.0, 0.0), 0.0, Vector3Create( 10.0, 1.0, 10.0 ), BLUE);
  DrawModelEx(cube, Vector3Create( 1.5, 1.0, -1.5 ), Vector3Create( 0.0, 1.0, 0.0 ), 0.0, Vector3One(), WHITE);
  DrawModelEx(robot, Vector3Create(0.0, 0.5, 0.0), Vector3Create( 0.0, 1.0, 0.0), 0.0, Vector3Create( 1.0, 1.0, 1.0), RED);
end;


var
  cam, lightCam: TCamera3D;
  shadowShader: TShader;
  lightDir, cameraPos: TVector3;
  lightColor: TColorB;
  lightColorNormalized: TVector4;
  lightDirLoc, lightColLoc, ambientLoc, lightVPLoc, shadowMapLoc, shadowMapResolution,
  animcount,  i, fc, slot: Integer;
  ambient: array[0..3] of Single;
  cube, robot: TModel;
  robotAnimations: PModelAnimation;
  shadowMap: TRenderTexture2D;
  dt: single;
  lightView, lightProj, lightViewProj: TMatrix;
begin
  // Initialization
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  // Shadows are a HUGE topic, and this example shows an extremely simple implementation of the shadowmapping algorithm,
  // which is the industry standard for shadows. This algorithm can be extended in a ridiculous number of ways to improve
  // realism and also adapt it for different scenes. This is pretty much the simplest possible implementation.
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - shadowmap');

  cam.position := Vector3Create( 10.0, 10.0, 10.0 );
  cam.target := Vector3Zero();
  cam.projection := CAMERA_PERSPECTIVE;
  cam.up := Vector3Create( 0.0, 1.0, 0.0 );
  cam.fovy := 45.0;

  shadowShader := LoadShader(TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/shadowmap.vs'), GLSL_VERSION),
                             TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/shadowmap.fs'), GLSL_VERSION));
   shadowShader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shadowShader, 'viewPos');

   lightDir := Vector3Normalize(Vector3Create( 0.35, -1.0, -0.35 ));
   lightColor := WHITE;
   lightColorNormalized := ColorNormalize(lightColor);
   lightDirLoc := GetShaderLocation(shadowShader, 'lightDir');
   lightColLoc := GetShaderLocation(shadowShader, 'lightColor');
   SetShaderValue(shadowShader, lightDirLoc, @lightDir, SHADER_UNIFORM_VEC3);
   SetShaderValue(shadowShader, lightColLoc, @lightColorNormalized, SHADER_UNIFORM_VEC4);
   ambientLoc := GetShaderLocation(shadowShader, 'ambient');
   ambient[0] := 0.1;
   ambient[1] := 0.1;
   ambient[2] := 0.1;
   ambient[3] := 1.0;
   SetShaderValue(shadowShader, ambientLoc, @ambient, SHADER_UNIFORM_VEC4);
   lightVPLoc := GetShaderLocation(shadowShader, 'lightVP');
   shadowMapLoc := GetShaderLocation(shadowShader, 'shadowMap');
   shadowMapResolution := SHADOWMAP_RESOLUTION;
   SetShaderValue(shadowShader, GetShaderLocation(shadowShader, 'shadowMapResolution'), @shadowMapResolution, SHADER_UNIFORM_INT);

   cube := LoadModelFromMesh(GenMeshCube(1.0, 1.0, 1.0));
   cube.materials[0].shader := shadowShader;
   robot := LoadModel(PChar(GetApplicationDirectory + 'resources/models/robot.glb'));

   for i := 0 to robot.materialCount-1 do
   robot.materials[i].shader := shadowShader;

   animCount := 0;
   robotAnimations := LoadModelAnimations(PChar(GetApplicationDirectory + 'resources/models/robot.glb'), @animCount);

   shadowMap := LoadShadowmapRenderTexture(SHADOWMAP_RESOLUTION, SHADOWMAP_RESOLUTION);
   // For the shadowmapping algorithm, we will be rendering everything from the light's point of view

   lightCam.position := Vector3Scale(lightDir, -15.0);
   lightCam.target := Vector3Zero();
   // Use an orthographic projection for directional lights
   lightCam.projection := CAMERA_ORTHOGRAPHIC;
   lightCam.up := Vector3Create( 0.0, 1.0, 0.0 );
   lightCam.fovy := 20.0;

   SetTargetFPS(60);
   fc := 0;

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      dt := GetFrameTime();
      cameraPos := cam.position;
      SetShaderValue(shadowShader, shadowShader.locs[SHADER_LOC_VECTOR_VIEW], @cameraPos, SHADER_UNIFORM_VEC3);
      UpdateCamera(@cam, CAMERA_ORBITAL);

      Inc(fc);
      fc := fc mod(robotAnimations[0].frameCount);

      UpdateModelAnimation(robot, robotAnimations[0], fc);

      if (IsKeyDown(KEY_LEFT)) then
      begin
        if (lightDir.x < 0.6) then
            lightDir.x += cameraSpeed * 60.0 * dt;
      end;

      if (IsKeyDown(KEY_RIGHT)) then
      begin
        if (lightDir.x > -0.6) then
           lightDir.x -= cameraSpeed * 60.0 * dt;
      end;

      if (IsKeyDown(KEY_UP)) then
      begin
        if (lightDir.z < 0.6) then
            lightDir.z += cameraSpeed * 60.0 * dt;
      end;

      if (IsKeyDown(KEY_DOWN)) then
      begin
        if (lightDir.z > -0.6) then
            lightDir.z -= cameraSpeed * 60.0 * dt;
      end;

      lightDir := Vector3Normalize(lightDir);
      lightCam.position := Vector3Scale(lightDir, -15.0);
      SetShaderValue(shadowShader, lightDirLoc, @lightDir, SHADER_UNIFORM_VEC3);

      // Draw
      BeginDrawing();
      // First, render all objects into the shadowmap
      // The idea is, we record all the objects' depths (as rendered from the light source's point of view) in a buffer
      // Anything that is "visible" to the light is in light, anything that isn't is in shadow
      // We can later use the depth buffer when rendering everything from the player's point of view
      // to determine whether a given point is "visible" to the light
      BeginTextureMode(shadowMap);
        ClearBackground(WHITE);
        BeginMode3D(lightCam);
          lightView := rlGetMatrixModelview();
          lightProj := rlGetMatrixProjection();
          DrawScene(cube, robot);
        EndMode3D();
      EndTextureMode();

      lightViewProj := MatrixMultiply(lightView, lightProj);
      ClearBackground(RAYWHITE);

      SetShaderValueMatrix(shadowShader, lightVPLoc, lightViewProj);

      rlEnableShader(shadowShader.id);
      slot := 10; // Can be anything 0 to 15, but 0 will probably be taken up
      rlActiveTextureSlot(10);
      rlEnableTexture(shadowMap.depth.id);
      rlSetUniform(shadowMapLoc, @slot, SHADER_UNIFORM_INT, 1);

      BeginMode3D(cam);
        // Draw the same exact things as we drew in the shadowmap!
        DrawScene(cube, robot);
      EndMode3D();

      DrawText('Shadows in raylib using the shadowmapping algorithm!', screenWidth - 320, screenHeight - 20, 10, GRAY);
      DrawText('Use the arrow keys to rotate the light!', 10, 10, 30, RED);
      DrawFps(10,40);
      EndDrawing();
    end;

  // De-Initialization
  UnloadShader(shadowShader);
  UnloadModel(cube);
  UnloadModel(robot);
  UnloadModelAnimations(robotAnimations, animCount);
  UnloadShadowmapRenderTexture(shadowMap);
  CloseWindow();        // Close window and OpenGL context
end.

