{*******************************************************************************************
*
*   raylib [models] example - Skybox loading and drawing
*
*   Example originally created with raylib 1.8, last time updated with raylib 4.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2017-2023 Ramon Santamaria (@raysan5)
*   Pascal translation 2023 Vadim Gunko (@GuvaCode)
*
********************************************************************************************}
program models_skybox;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath, rlgl;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
   camera: TCamera;
   skybox: TModel;
   cube: TMesh;
   useHDR: Boolean;
   map: integer;
   cbDat: integer;
   img: TImage;
   shdrCubemap: TShader;
   panorama: TTexture2D;
   skyboxFileName: String;

// Generate cubemap texture from HDR texture
function GenTextureCubemap(shader: TShader; panorama: TTexture2D; size, format: integer) : TTextureCubemap;
var i: integer;
    cubemap: TTextureCubemap;
    rbo, fbo: LongWord;
    matFboProjection: TMatrix;
    fboViews : array [0..5] of  TMatrix;
begin
      cubemap := Default(TTextureCubemap);

      rlDisableBackfaceCulling();     // Disable backface culling to render inside the cube

      // STEP 1: Setup framebuffer
      //------------------------------------------------------------------------------------------
      rbo := rlLoadTextureDepth(size, size, true);
      cubemap.id := rlLoadTextureCubemap(nil, size, format, 1);

      fbo := rlLoadFramebuffer();
      rlFramebufferAttach(fbo, rbo, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_RENDERBUFFER, 0);
      rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X, 0);

      // Check if framebuffer is complete with attachments (valid)
      if rlFramebufferComplete(fbo) then TraceLog(LOG_INFO, 'FBO: [ID %i] Framebuffer object created successfully', fbo);
      //------------------------------------------------------------------------------------------

      // STEP 2: Draw to framebuffer
      //------------------------------------------------------------------------------------------
      // NOTE: Shader is used to convert HDR equirectangular environment map to cubemap equivalent (6 faces)
      rlEnableShader(shader.id);

      // Define projection matrix and send it to shader
      matFboProjection := MatrixPerspective(90.0*DEG2RAD, 1.0, rlGetCullDistanceNear, rlGetCullDistanceFar);
      rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_PROJECTION], matFboProjection);

      // Define view matrix for every side of the cubemap
      fboViews[0] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create(  1.0,  0.0,  0.0 ), Vector3Create( 0.0, -1.0,  0.0 ));
      fboViews[1] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create( -1.0,  0.0,  0.0 ), Vector3Create( 0.0, -1.0,  0.0 ));
      fboViews[2] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create(  0.0,  1.0,  0.0 ), Vector3Create( 0.0,  0.0,  1.0 ));
      fboViews[3] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create(  0.0, -1.0,  0.0 ), Vector3Create( 0.0,  0.0, -1.0 ));
      fboViews[4] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create(  0.0,  0.0,  1.0 ), Vector3Create( 0.0, -1.0,  0.0 ));
      fboViews[5] := MatrixLookAt(Vector3Create( 0.0, 0.0, 0.0 ), Vector3Create(  0.0,  0.0, -1.0 ), Vector3Create( 0.0, -1.0,  0.0 ));

      rlViewport(0, 0, size, size);   // Set viewport to current fbo dimensions

      // Activate and enable texture for drawing to cubemap faces
      rlActiveTextureSlot(0);
      rlEnableTexture(panorama.id);

      for i:=0 to 5 do
      begin
          // Set the view matrix for the current cube face
          rlSetUniformMatrix(shader.locs[SHADER_LOC_MATRIX_VIEW], fboViews[i]);

          // Select the current cubemap face attachment for the fbo
          // WARNING: This function by default enables->attach->disables fbo!!!
          rlFramebufferAttach(fbo, cubemap.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_CUBEMAP_POSITIVE_X + i, 0);
          rlEnableFramebuffer(fbo);

          // Load and draw a cube, it uses the current enabled texture
          rlClearScreenBuffers();
          rlLoadDrawCube();

          // ALTERNATIVE: Try to use internal batch system to draw the cube instead of rlLoadDrawCube
          // for some reason this method does not work, maybe due to cube triangles definition? normals pointing out?
          // TODO: Investigate this issue...
          //rlSetTexture(panorama.id); // WARNING: It must be called after enabling current framebuffer if using internal batch system!
          //rlClearScreenBuffers();
          //DrawCubeV(Vector3Zero(), Vector3One(), WHITE);
          //rlDrawRenderBatchActive();
      end;
      //------------------------------------------------------------------------------------------

      // STEP 3: Unload framebuffer and reset state
      //------------------------------------------------------------------------------------------
      rlDisableShader();          // Unbind shader
      rlDisableTexture();         // Unbind texture
      rlDisableFramebuffer();     // Unbind framebuffer
      rlUnloadFramebuffer(fbo);   // Unload framebuffer (and automatically attached depth texture/renderbuffer)

      // Reset viewport dimensions to default
      rlViewport(0, 0, rlGetFramebufferWidth(), rlGetFramebufferHeight());
      rlEnableBackfaceCulling();
      //------------------------------------------------------------------------------------------

      cubemap.width := size;
      cubemap.height := size;
      cubemap.mipmaps := 1;
      cubemap.format := format;

      result := cubemap;
  end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - skybox loading and drawing');

  // Define the camera to look into our 3d world
  Camera:=Camera3DCreate(Vector3Create(1.0,1.0,1.0),Vector3Create(4.0,1.0,4.0),Vector3Create(0.0,1.0,0.0),45.0,0);

  // Load skybox model
  cube := GenMeshCube(1.0, 1.0, 1.0);
  skybox := LoadModelFromMesh(cube);

  useHDR := false;

  // Load skybox shader and set required locations
  // NOTE: Some locations are automatically set at shader loading
  skybox.materials[0].shader := LoadShader(TextFormat('resources/shaders/glsl%i/skybox.vs', GLSL_VERSION),
                                           TextFormat('resources/shaders/glsl%i/skybox.fs', GLSL_VERSION));

  map := MATERIAL_MAP_CUBEMAP;
  SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'environmentMap'), @map, SHADER_UNIFORM_INT);
  SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'doGamma'), @useHDR, SHADER_UNIFORM_INT);
  SetShaderValue(skybox.materials[0].shader, GetShaderLocation(skybox.materials[0].shader, 'vflipped'), @useHDR, SHADER_UNIFORM_INT);

  // Load cubemap shader and setup required shader locations
  shdrCubemap := LoadShader(TextFormat('resources/shaders/glsl%i/cubemap.vs', GLSL_VERSION),
                            TextFormat('resources/shaders/glsl%i/cubemap.fs', GLSL_VERSION));

  SetShaderValue(shdrCubemap, GetShaderLocation(shdrCubemap, 'equirectangularMap'), @cbDat, SHADER_UNIFORM_INT);

  if useHDR then
   begin
       //TextCopy(PChar(skyboxFileName), 'resources/dresden_square_2k.hdr');

            // Load HDR panorama (sphere) texture
        panorama := LoadTexture(PChar(GetApplicationDirectory + 'resources/dresden_square_2k.hdr'));
       // panorama := LoadTexture(PChar(skyboxFileName));
            // Generate cubemap (texture with 6 quads-cube-mapping) from panorama HDR texture
            // NOTE 1: New texture is generated rendering to texture, shader calculates the sphere->cube coordinates mapping
            // NOTE 2: It seems on some Android devices WebGL, fbo does not properly support a FLOAT-based attachment,
            // despite texture can be successfully created.. so using PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 instead of PIXELFORMAT_UNCOMPRESSED_R32G32B32A32
            skybox.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture := GenTextureCubemap(shdrCubemap, panorama, 1024, PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);

            UnloadTexture(panorama);        // Texture not required anymore, cubemap already generated

   end
   else
   begin
       img := LoadImage('resources/skybox.png');
       skybox.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture := LoadTextureCubemap(img, CUBEMAP_LAYOUT_AUTO_DETECT);    // CUBEMAP_LAYOUT_PANORAMA
       UnloadImage(img);
   end;

  DisableCursor;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_FIRST_PERSON);

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(camera);

          // We are inside the cube, we need to disable backface culling!
          rlDisableBackfaceCulling();
          rlDisableDepthMask();
          DrawModel(skybox, Vector3Create(0, 0, 0), 1.0, WHITE);
          rlEnableBackfaceCulling();
          rlEnableDepthMask();

          DrawGrid(10, 1.0);

      EndMode3D();

      //DrawTextureEx(panorama, Vector2Create( 0, 0 ), 0.0, 0.5, WHITE);

      if (useHDR) then DrawText(TextFormat('Panorama image from hdrihaven.com: %s',
      GetFileName(PChar(GetApplicationDirectory  + skyboxFileName{%H-}))), 10, GetScreenHeight() - 20, 10, BLUE)
      else
        DrawText(TextFormat(': %s', GetFileName(PChar(skyboxFileName))), 10, GetScreenHeight() - 20, 10, BLACK);

      DrawFPS(10, 10);


      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(skybox.materials[0].shader);
  UnloadTexture(skybox.materials[0].maps[MATERIAL_MAP_CUBEMAP].texture);

  UnloadModel(skybox);        // Unload skybox model
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

