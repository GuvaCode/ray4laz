{*******************************************************************************************
*
*   raylib [shaders] example - Hybrid Rendering
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example contributed by Buğra Alptekin Sarı (@BugraAlptekinSari) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2022-2023 Buğra Alptekin Sarı (@BugraAlptekinSari)
*   Pascal translation 2023 Vadim Gunko (@guvacode)
*
********************************************************************************************}
program shaders_hybrid_render;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath, math,
rlgl,
raylib; 

const
  GLSL_VERSION = 330;
  screenWidth = 800;
  screenHeight = 450;

  //------------------------------------------------------------------------------------
  // Declare custom functions required for the example
  //------------------------------------------------------------------------------------
  // Load custom render texture, create a writable depth texture buffer
  function LoadRenderTextureDepthTex(width,height: integer): TRenderTexture2D;
  var target: TRenderTexture2D;
  begin
  target:=Default(TRenderTexture2D);
  target.id := rlLoadFramebuffer();   // Load an empty framebuffer

    if (target.id > 0) then
    begin
        rlEnableFramebuffer(target.id);

        // Create color texture (default to RGBA)
        target.texture.id := rlLoadTexture(nil, width, height, PIXELFORMAT_UNCOMPRESSED_R8G8B8A8, 1);
        target.texture.width := width;
        target.texture.height := height;
        target.texture.format := PIXELFORMAT_UNCOMPRESSED_R8G8B8A8;
        target.texture.mipmaps := 1;

        // Create depth texture buffer (instead of raylib default renderbuffer)
        target.depth.id := rlLoadTextureDepth(width, height, false);
        target.depth.width := width;
        target.depth.height := height;
        target.depth.format := 19;       //DEPTH_COMPONENT_24BIT?
        target.depth.mipmaps := 1;

        // Attach color texture and depth texture to FBO
        rlFramebufferAttach(target.id, target.texture.id, RL_ATTACHMENT_COLOR_CHANNEL0, RL_ATTACHMENT_TEXTURE2D, 0);
        rlFramebufferAttach(target.id, target.depth.id, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_TEXTURE2D, 0);

        // Check if fbo is complete with attachments (valid)
        if (rlFramebufferComplete(target.id)) then TRACELOG(LOG_INFO, 'FBO: [ID %i] Framebuffer object created successfully', target.id);

        rlDisableFramebuffer();
    end
    else TRACELOG(LOG_WARNING, 'FBO: Framebuffer object can not be created');

    result := target;
  end;


  // Unload render texture from GPU memory (VRAM)
  procedure UnloadRenderTextureDepthTex(target: TRenderTexture2D);
  begin
    if (target.id > 0) then
    begin
        // Color texture attached to FBO is deleted
        rlUnloadTexture(target.texture.id);
        rlUnloadTexture(target.depth.id);

        // NOTE: Depth texture is automatically
        // queried and deleted before deleting framebuffer
        rlUnloadFramebuffer(target.id);
   end;
  end;

  //------------------------------------------------------------------------------------
  // Declare custom Structs
  //------------------------------------------------------------------------------------
type
  TRayLocs = record
  camPos: LongWord;
  camDir: LongWord;
  screenCenter: LongWord;
  end;

var
  shdrRaymarch, shdrRaster: TShader;
  marchLocs: TRayLocs;
  screenCenter: TVector2;
  target: TRenderTexture2D;
  camera: TCamera;
  camDist: double;
  camDir: TVector3;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - write depth buffer');

  // This Shader calculates pixel depth and color using raymarch
  shdrRaymarch := LoadShader(nil, TextFormat('resources/shaders/glsl%i/hybrid_raymarch.fs', GLSL_VERSION));

   // This Shader is a standard rasterization fragment shader with the addition of depth writing
   // You are required to write depth for all shaders if one shader does it
   shdrRaster := LoadShader(nil, TextFormat('resources/shaders/glsl%i/hybrid_raster.fs', GLSL_VERSION));

   // Declare Struct used to store camera locs.
   marchLocs := Default(TRayLocs);

   // Fill the struct with shader locs.
   marchLocs.camPos := GetShaderLocation(shdrRaymarch, 'camPos');
   marchLocs.camDir := GetShaderLocation(shdrRaymarch, 'camDir');
   marchLocs.screenCenter := GetShaderLocation(shdrRaymarch, 'screenCenter');

   // Transfer screenCenter position to shader. Which is used to calculate ray direction.
   screenCenter := Vector2Create(screenWidth/2.0, screenHeight/2.0);
   SetShaderValue(shdrRaymarch, marchLocs.screenCenter , @screenCenter , SHADER_UNIFORM_VEC2);

   // Use Customized function to create writable depth texture buffer
    target := LoadRenderTextureDepthTex(screenWidth, screenHeight);

   // Define the camera to look into our 3d world
   camera := Default(TCamera);
   camera.position := Vector3Create( 0.5, 1.0, 1.5 );    // Camera position
   camera.target := Vector3Create( 0.0, 0.5, 0.0 );      // Camera looking at point
   camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
   camera.fovy := 45.0;                                  // Camera field-of-view Y
   camera.projection := CAMERA_PERSPECTIVE;              // Camera projection type


   // Camera FOV is pre-calculated in the camera Distance.
   camDist := 1.0/(tan(camera.fovy*0.5*DEG2RAD));

   SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
   //--------------------------------------------------------------------------------------------



  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_ORBITAL);

      // Update Camera Postion in the ray march shader.
      SetShaderValue(shdrRaymarch, marchLocs.camPos, @(camera.position), RL_SHADER_UNIFORM_VEC3);

      // Update Camera Looking Vector. Vector length determines FOV.
      camDir := Vector3Scale( Vector3Normalize( Vector3Subtract(camera.target, camera.position)) , camDist);
      SetShaderValue(shdrRaymarch, marchLocs.camDir, @(camDir), RL_SHADER_UNIFORM_VEC3);

      // Draw
      // Draw into our custom render texture (framebuffer)
       BeginTextureMode(target);
           ClearBackground(WHITE);

           // Raymarch Scene
           rlEnableDepthTest(); //Manually enable Depth Test to handle multiple rendering methods.
           BeginShaderMode(shdrRaymarch);
             DrawRectangleRec(RectangleCreate(0,0,screenWidth,screenHeight),WHITE);
           EndShaderMode();

           // Raserize Scene
           BeginMode3D(camera);
             BeginShaderMode(shdrRaster);
               DrawCubeWiresV(Vector3Create( 0.0, 0.5, 1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), RED);
               DrawCubeV(Vector3Create( 0.0, 0.5, 1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), PURPLE);
               DrawCubeWiresV(Vector3Create( 0.0, 0.5, -1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), DARKGREEN);
               DrawCubeV(Vector3Create( 0.0, 0.5, -1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), YELLOW);
               DrawGrid(10, 1.0);
             EndShaderMode();
           EndMode3D();
       EndTextureMode();

       // Draw into screen our custom render texture
       BeginDrawing();
           ClearBackground(RAYWHITE);
           DrawTextureRec(target.texture, RectangleCreate( 0, 0, screenWidth, -screenHeight ), Vector2Zero, WHITE);
           DrawFPS(10, 10);
       EndDrawing();

    end;

  // De-Initialization
  UnloadRenderTextureDepthTex(target);
  UnloadShader(shdrRaymarch);
  UnloadShader(shdrRaster);

  CloseWindow();        // Close window and OpenGL context
end.

