(*******************************************************************************************
*
*   raylib [shaders] example - deferred rendering
*
*   NOTE: This example requires raylib OpenGL 3.3 or OpenGL ES 3.0
*
*   Example originally created with raylib 4.5, last time updated with raylib 4.5
*
*   Example contributed by Justin Andreas Lacoste (@27justin) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Justin Andreas Lacoste (@27justin)
*   Pascal conversion (c) 2023 Gunko Vadim (@guvacode)
*
********************************************************************************************)
program shaders_deferred_render;

{$mode objfpc}{$H+}

uses 
cmem, raylib, rlgl, rlights, raymath, math;

const
  screenWidth = 800;
  screenHeight = 450;
  CUBE_SCALE = 0.25;
  MAX_CUBES = 30;

type
  TGBuffer = record
    framebuffer: LongWord;
    positionTexture: LongWord;
    normalTexture: LongWord;
    albedoSpecTexture: LongWord;
    depthRenderbuffer: LongWord;
  end;

  TDeferredMode = (DEFERRED_POSITION, DEFERRED_NORMAL, DEFERRED_ALBEDO, DEFERRED_SHADING);

var
  camera: TCamera;
  model, cube: TModel;
  gbufferShader, deferredShader: TShader;
  gBuffer: TGBuffer;
  lights: array [0..MAX_LIGHTS] of TLight;
  cubePositions: array [0..MAX_CUBES] of TVector3;
  cubeRotations: array [0..MAX_CUBES] of Single;
  i: integer;
  mode: TDeferredMode;
  texture: TTexture2D;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - deferred render');

  camera := Default(TCamera);
  camera.position := Vector3Create( 5.0, 4.0, 5.0 );    // Camera position
  camera.target := Vector3Create( 0.0, 1.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 60.0;                                // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;             // Camera projection type

  // Load plane model from a generated mesh
  model := LoadModelFromMesh(GenMeshPlane(10.0, 10.0, 3, 3));
  cube := LoadModelFromMesh(GenMeshCube(2.0, 2.0, 2.0));

  // Load geometry buffer (G-buffer) shader and deferred shader
  gbufferShader := LoadShader('resources/shaders/glsl330/gbuffer.vs',
                              'resources/shaders/glsl330/gbuffer.fs');

  deferredShader := LoadShader('resources/shaders/glsl330/deferred_shading.vs',
                               'resources/shaders/glsl330/deferred_shading.fs');
  deferredShader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(deferredShader, 'viewPosition');

  // Initialize the G-buffer
  gBuffer := Default(TGBuffer);
  gBuffer.framebuffer := rlLoadFramebuffer();

  if gBuffer.framebuffer <= 0 then
  begin
    TraceLog(LOG_WARNING, 'Failed to create framebuffer');
    halt;
  end;

  rlEnableFramebuffer(gBuffer.framebuffer);

  // Since we are storing position and normal data in these textures,
  // we need to use a floating point format.
  gBuffer.positionTexture := rlLoadTexture(nil, screenWidth, screenHeight, RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32, 1);

  gBuffer.normalTexture := rlLoadTexture(nil, screenWidth, screenHeight, RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32, 1);
  // Albedo (diffuse color) and specular strength can be combined into one texture.
  // The color in RGB, and the specular strength in the alpha channel.
  gBuffer.albedoSpecTexture := rlLoadTexture(nil, screenWidth, screenHeight, RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8, 1);

  // Activate the draw buffers for our framebuffer
  rlActiveDrawBuffers(3);

  // Now we attach our textures to the framebuffer.
  rlFramebufferAttach(gBuffer.framebuffer, gBuffer.positionTexture, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_TEXTURE2D, 0);
  rlFramebufferAttach(gBuffer.framebuffer, gBuffer.normalTexture, RL_ATTACHMENT_COLOR_CHANNEL1, RL_ATTACHMENT_TEXTURE2D, 0);
  rlFramebufferAttach(gBuffer.framebuffer, gBuffer.albedoSpecTexture, RL_ATTACHMENT_COLOR_CHANNEL2, RL_ATTACHMENT_TEXTURE2D, 0);

  // Finally we attach the depth buffer.
  gBuffer.depthRenderbuffer := rlLoadTextureDepth(screenWidth, screenHeight, true);
  rlFramebufferAttach(gBuffer.framebuffer, gBuffer.depthRenderbuffer, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_RENDERBUFFER, 0);

  // Make sure our framebuffer is complete.
  // NOTE: rlFramebufferComplete() automatically unbinds the framebuffer, so we don't have
  // to rlDisableFramebuffer() here.
  if not rlFramebufferComplete(gBuffer.framebuffer) then
  begin
    TraceLog(LOG_WARNING, 'Framebuffer is not complete');
    halt;
  end;

  // Now we initialize the sampler2D uniform's in the deferred shader.
  // We do this by setting the uniform's value to the color channel slot we earlier
  // bound our textures to.
  rlEnableShader(deferredShader.id);

    rlSetUniformSampler(rlGetLocationUniform(deferredShader.id, 'gPosition'), 0);
    rlSetUniformSampler(rlGetLocationUniform(deferredShader.id, 'gNormal'), 1);
    rlSetUniformSampler(rlGetLocationUniform(deferredShader.id, 'gAlbedoSpec'), 2);

  rlDisableShader();

  // Assign out lighting shader to model
  model.materials[0].shader := gbufferShader;
  cube.materials[0].shader := gbufferShader;

  // Create lights
  //--------------------------------------------------------------------------------------
  lights[0] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1, -2 ), Vector3Zero(), YELLOW, deferredShader);
  lights[1] := CreateLight(LIGHT_POINT, Vector3Create( 2, 1, 2 ), Vector3Zero(), RED, deferredShader);
  lights[2] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1, 2 ), Vector3Zero(), GREEN, deferredShader);
  lights[3] := CreateLight(LIGHT_POINT, Vector3Create( 2, 1, -2 ), Vector3Zero(), BLUE, deferredShader);

  randomize;
  for  i := 0 to MAX_CUBES -1 do
  begin
    cubePositions[i] := Vector3Create(random(10) - 5,random(5),random(10) - 5);
    cubeRotations[i] := random(360);
  end;

  mode := DEFERRED_SHADING;

  rlEnableDepthTest();
  SetTargetFPS(60);                   // Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera, CAMERA_ORBITAL);

      SetShaderValue(deferredShader, deferredShader.locs[SHADER_LOC_VECTOR_VIEW], @camera.position, SHADER_UNIFORM_VEC3);

      // Check key inputs to enable/disable lights
      if (IsKeyPressed(KEY_Y)) then  lights[0].enabled := not lights[0].enabled;
      if (IsKeyPressed(KEY_R)) then  lights[1].enabled := not lights[1].enabled;
      if (IsKeyPressed(KEY_G)) then  lights[2].enabled := not lights[2].enabled;
      if (IsKeyPressed(KEY_B)) then  lights[3].enabled := not lights[3].enabled;

      // Check key inputs to switch between G-buffer textures
      if (IsKeyPressed(KEY_ONE)) then mode := DEFERRED_POSITION;
      if (IsKeyPressed(KEY_TWO)) then mode := DEFERRED_NORMAL;
      if (IsKeyPressed(KEY_THREE)) then mode := DEFERRED_ALBEDO;
      if (IsKeyPressed(KEY_FOUR)) then mode := DEFERRED_SHADING;

      // Update light values (actually, only enable/disable them)
      for i :=0 to MAX_LIGHTS -1 do UpdateLightValues(deferredShader, lights[i]);

      //----------------------------------------------------------------------------------
      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        // Draw to the geometry buffer by first activating it
        rlEnableFramebuffer(gBuffer.framebuffer);
        rlClearScreenBuffers();  // Clear color and depth buffer

        rlDisableColorBlend();
        BeginMode3D(camera);
          // NOTE: We have to use rlEnableShader here. `BeginShaderMode` or thus `rlSetShader`
          // will not work, as they won't immediately load the shader program.
          rlEnableShader(gbufferShader.id);
          // When drawing a model here, make sure that the material's shaders
          // are set to the gbuffer shader!
          DrawModel(model, Vector3Zero(), 1.0, WHITE);
          DrawModel(cube,  Vector3Create ( 0.0, 1.0, 0.0 ), 1.0, WHITE);

          while i < MAX_CUBES -1 do
          begin
            DrawModelEx(cube, cubePositions[i], Vector3Create( 1, 1, 1 ), cubeRotations[i],
            Vector3Create( CUBE_SCALE, CUBE_SCALE, CUBE_SCALE ), WHITE);
            inc(i);
          end;

          rlDisableShader();
          EndMode3D();
          rlEnableColorBlend();

          // Go back to the default framebuffer (0) and draw our deferred shading.
          rlDisableFramebuffer();
          rlClearScreenBuffers(); // Clear color & depth buffer

          case mode of

          DEFERRED_SHADING:
          begin
          BeginMode3D(camera);
          rlDisableColorBlend();
          rlEnableShader(deferredShader.id);
          // Activate our g-buffer textures
          // These will now be bound to the sampler2D uniforms `gPosition`, `gNormal`,
          // and `gAlbedoSpec`
          rlActiveTextureSlot(0);
          rlEnableTexture(gBuffer.positionTexture);
          rlActiveTextureSlot(1);
          rlEnableTexture(gBuffer.normalTexture);
          rlActiveTextureSlot(2);
          rlEnableTexture(gBuffer.albedoSpecTexture);

          // Finally, we draw a fullscreen quad to our default framebuffer
          // This will now be shaded using our deferred shader
          rlLoadDrawQuad();
          rlDisableShader();
          rlEnableColorBlend();
          EndMode3D();

          // As a last step, we now copy over the depth buffer from our g-buffer to the default framebuffer.
          rlEnableFramebuffer(gBuffer.framebuffer); //glBindFramebuffer(GL_READ_FRAMEBUFFER, gBuffer.framebuffer);
          rlEnableFramebuffer(0); //glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
          rlBlitFramebuffer(0, 0, screenWidth, screenHeight, 0, 0, screenWidth, screenHeight, $00000100);    // GL_DEPTH_BUFFER_BIT
          rlDisableFramebuffer();

          // Since our shader is now done and disabled, we can draw our lights in default
          // forward rendering
          BeginMode3D(camera);
          rlEnableShader(rlGetShaderIdDefault());
          for i:=0 to MAX_LIGHTS -1 do
          begin
            if (lights[i].enabled) then DrawSphereEx(lights[i].position, 0.2, 8, 8, lights[i].color)
            else DrawSphereWires(lights[i].position, 0.2, 8, 8, ColorAlpha(lights[i].color, 0.3));
          end;
          rlDisableShader();
          EndMode3D();
          DrawText('FINAL RESULT', 10, screenHeight - 30, 20, DARKGREEN);
          end;

          DEFERRED_POSITION:
          begin
            Texture.id:=gBuffer.positionTexture;
            Texture.width:=screenWidth;
            Texture.height:=screenHeight;
            DrawTextureRec(Texture, RectangleCreate ( 0, 0, screenWidth, -screenHeight ), Vector2Zero(), RAYWHITE);
            DrawText('POSITION TEXTURE', 10, screenHeight - 30, 20, DARKGREEN);
          end;

          DEFERRED_NORMAL:
          begin
            Texture.id:=gBuffer.normalTexture;
            Texture.width:=screenWidth;
            Texture.height:=screenHeight;
            DrawTextureRec(Texture, RectangleCreate ( 0, 0, screenWidth, -screenHeight ), Vector2Zero(), RAYWHITE);
            DrawText('NORMAL TEXTURE', 10, screenHeight - 30, 20, DARKGREEN);
          end;

          DEFERRED_ALBEDO:
          begin
            Texture.id:=gBuffer.albedoSpecTexture;
            Texture.width:=screenWidth;
            Texture.height:=screenHeight;
            DrawTextureRec(Texture, RectangleCreate ( 0, 0, screenWidth, -screenHeight ), Vector2Zero(), RAYWHITE);
            DrawText('ALBEDO TEXTURE', 10, screenHeight - 30, 20, DARKGREEN);
          end;
          end;

          DrawText('Toggle lights keys: [Y][R][G][B]', 10, 40, 20, DARKGRAY);
          DrawText('Switch G-buffer textures: [1][2][3][4]', 10, 70, 20, DARKGRAY);

          DrawFPS(10, 10);

      EndDrawing();

    end;

  // De-Initialization
  UnloadModel(model);     // Unload the models
  UnloadModel(cube);

  UnloadTexture(Texture);

  UnloadShader(deferredShader);   // Unload shaders
  UnloadShader(gbufferShader);

  // Unload geometry buffer and all attached textures
  rlUnloadFramebuffer(gBuffer.framebuffer);
  rlUnloadTexture(gBuffer.positionTexture);
  rlUnloadTexture(gBuffer.normalTexture);
  rlUnloadTexture(gBuffer.albedoSpecTexture);
  rlUnloadTexture(gBuffer.depthRenderbuffer);

  CloseWindow();          // Close window and OpenGL context
end.

