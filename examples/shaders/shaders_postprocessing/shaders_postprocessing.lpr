{*******************************************************************************************
*
*   raylib [shaders] example - Apply a postprocessing shader to a scene
*
*   NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
*         OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version.
*
*   NOTE: Shaders used in this example are #version 330 (OpenGL 3.3), to test this example
*         on OpenGL ES 2.0 platforms (Android, Raspberry Pi, HTML5), use #version 100 shaders
*         raylib comes with shaders ready for both versions, check raylib/shaders install folder
*
*   This example has been created using raylib 1.3 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2015 Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program shaders_postprocessing;

{$mode objfpc}{$H+}



uses 
{uncomment if necessary}
//raymath,
//rlgl,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330; // note: for PLATFORM_RPI, PLATFORM_ANDROID GLSL_VERSION=100 !!!
  MAX_POSTPRO_SHADERS = 12;

  postproShaderText: array of PChar =('GRAYSCALE','POSTERIZATION','DREAM_VISION',
  'PIXELIZER','CROSS_HATCHING','CROSS_STITCHING','PREDATOR_VIEW','SCANLINES',
  'FISHEYE','SOBEL','BLOOM','BLUR');

   FX_GRAYSCALE = 0;
   FX_POSTERIZATION = 1;
   FX_DREAM_VISION = 2;
   FX_PIXELIZER = 3;
   FX_CROSS_HATCHING = 4;
   FX_CROSS_STITCHING = 5;
   FX_PREDATOR_VIEW = 6;
   FX_SCANLINES = 7;
   FX_FISHEYE = 8;
   FX_SOBEL = 9;
   FX_BLOOM = 10;
   FX_BLUR = 11;


var
  camera: TCamera;
  model: TModel;
  texture: TTexture2D;
  position: TVector3;
  shaders: array [0..MAX_POSTPRO_SHADERS] of TShader;
  //shaders: array of TShader;
  currentShader: integer;
  target: TRenderTexture2D;
  i: integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);      // Enable Multi Sampling Anti Aliasing 4x (if available)
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');
  // Define the camera to look into our 3d world
  Camera3DSet(@camera,Vector3Create(2.0,3.0,2.0),Vector3Create(0.0,1.0,0.0),Vector3Create(0.0,1.0,0.0),45.0,0);

  model := LoadModel('resources/models/church.obj'); // Load OBJ model
  texture := LoadTexture('resources/models/church_diffuse.png'); // Load model texture (diffuse map)
  model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture; // Set model diffuse texture

  Vector3Set(@position,0.0,0.0,0.0); // Set model position

  // Load all postpro shaders
  // NOTE 1: All postpro shader use the base vertex shader (DEFAULT_VERTEX_SHADER)
  // NOTE 2: We load the correct shader depending on GLSL version
  // NOTE 3: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  shaders[FX_GRAYSCALE] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/grayscale.fs', GLSL_VERSION));
  shaders[FX_POSTERIZATION] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/posterization.fs]', GLSL_VERSION));
  shaders[FX_DREAM_VISION] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/dream_vision.fs', GLSL_VERSION));
  shaders[FX_PIXELIZER] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/pixelizer.fs', GLSL_VERSION));
  shaders[FX_CROSS_HATCHING] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/cross_hatching.fs', GLSL_VERSION));
  shaders[FX_CROSS_STITCHING] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/cross_stitching.fs', GLSL_VERSION));
  shaders[FX_PREDATOR_VIEW] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/predator.fs', GLSL_VERSION));
  shaders[FX_SCANLINES] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/scanlines.fs', GLSL_VERSION));
  shaders[FX_FISHEYE] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/fisheye.fs', GLSL_VERSION));
  shaders[FX_SOBEL] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/sobel.fs', GLSL_VERSION));
  shaders[FX_BLOOM] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/bloom.fs', GLSL_VERSION));
  shaders[FX_BLUR] := LoadShader(nil, TextFormat('resources/shaders/glsl%i/blur.fs', GLSL_VERSION));


  currentShader := FX_GRAYSCALE;

  // Create a RenderTexture2D to be used for render to texture
  target := LoadRenderTexture(screenWidth, screenHeight);

  // Setup orbital camera


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_ORBITAL);              // Update camera

      if IsKeyPressed(KEY_RIGHT) then inc(currentShader) else
      if IsKeyPressed(KEY_LEFT) then dec(currentShader);

        if (currentShader >= MAX_POSTPRO_SHADERS) then  currentShader := 0
         else
        if (currentShader < 0) then currentShader := MAX_POSTPRO_SHADERS - 1;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginTextureMode(target);       // Enable drawing to texture
        ClearBackground(RAYWHITE);  // Clear texture background

            BeginMode3D(camera);        // Begin 3d mode drawing
                DrawModel(model, position, 0.1, WHITE);   // Draw 3d model with texture
                DrawGrid(10, 1.0);     // Draw a grid
            EndMode3D();                // End 3d mode drawing, returns to orthographic 2d mode
        EndTextureMode();               // End drawing to texture (now we have a texture available for next passes)

        BeginDrawing();
            ClearBackground(RAYWHITE);  // Clear screen background

            // Render generated texture using selected postprocessing shader
            BeginShaderMode(shaders[currentShader]);
                // NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
                DrawTextureRec(target.texture,
                RectangleCreate(0, 0, target.texture.width, -target.texture.height),
                Vector2Create(0,0),WHITE);
            EndShaderMode();

            // Draw 2d shapes and text over drawn texture
            DrawRectangle(0, 9, 580, 30, Fade(LIGHTGRAY, 0.7));

            DrawText('(c) Church 3D model by Alberto Cano', screenWidth - 200, screenHeight - 20, 10, GRAY);
            DrawText('CURRENT POSTPRO SHADER:', 10, 15, 20, BLACK);
            DrawText(postproShaderText[currentShader], 330, 15, 20, RED);
            DrawText('< >', 540, 10, 30, DARKBLUE);
            DrawFPS(700, 15);
        EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  // Unload all postpro shaders
    for i:=0 to MAX_POSTPRO_SHADERS do UnloadShader(shaders[i]);
    UnloadTexture(texture);         // Unload texture
    UnloadModel(model);             // Unload model
    UnloadRenderTexture(target);    // Unload render texture

    CloseWindow();                  // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

