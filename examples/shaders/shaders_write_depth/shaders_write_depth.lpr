program shaders_write_depth;

{$mode objfpc}{$H+}

uses 
cmem, rlgl, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

  //------------------------------------------------------------------------------------
  // Define custom functions required for the example
  //------------------------------------------------------------------------------------
  // Load custom render texture, create a writable depth texture buffer
function LoadRenderTextureDepthTex(width, height: Integer): TRenderTexture2D;
var target: TRenderTexture2D;
begin
      target := Default(TRenderTexture2D);
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
          if (rlFramebufferComplete(target.id)) then
          TRACELOG(LOG_INFO, 'FBO: [ID %i] Framebuffer object created successfully', target.id);

          rlDisableFramebuffer();
      end
      else TRACELOG(LOG_WARNING, 'FBO: Framebuffer object can not be created');

      result:= target;
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

var
  shader: TShader;
  target: TRenderTexture2D;
  camera: TCamera;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

  // The shader inverts the depth buffer by writing into it by `gl_FragDepth = 1 - gl_FragCoord.z;`
  shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/write_depth.fs', GLSL_VERSION));

   // Use Customized function to create writable depth texture buffer
  target := LoadRenderTextureDepthTex(screenWidth, screenHeight);

   // Define the camera to look into our 3d world
   camera:= Camera3dCreate(Vector3Create(2.0,2.0,3.0),
                           Vector3Create(0.0,0.5,0.0),
                           Vector3Create(0.0,1.0,0.0), 45, CAMERA_PERSPECTIVE);



   SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_ORBITAL);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      // Draw into our custom render texture (framebuffer)
      BeginTextureMode(target);
          ClearBackground(WHITE);

          BeginMode3D(camera);
              BeginShaderMode(shader);
                  DrawCubeWiresV (Vector3Create( 0.0, 0.5, 1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), RED);
                  DrawCubeV      (Vector3Create( 0.0, 0.5, 1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), PURPLE);
                  DrawCubeWiresV (Vector3Create( 0.0, 0.5,-1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), DARKGREEN);
                  DrawCubeV      (Vector3Create( 0.0, 0.5,-1.0 ), Vector3Create( 1.0, 1.0, 1.0 ), YELLOW);
                  DrawGrid(10, 1.0);
              EndShaderMode();
          EndMode3D();
      EndTextureMode();

      // Draw into screen our custom render texture
      BeginDrawing();
          ClearBackground(RAYWHITE);

          DrawTextureRec(target.texture, RectangleCreate ( 0, 0, screenWidth, -screenHeight ), Vector2Create ( 0, 0 ), WHITE);
          DrawFPS(10, 10);
      EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadRenderTextureDepthTex(target);
  UnloadShader(shader);

  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

