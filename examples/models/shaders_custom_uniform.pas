(******************************************************************************

 raylib [shaders] example - Apply a postprocessing shader and connect a
 custom uniform variable

 NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders
 support, OpenGL 1.1 does not support shaders, recompile raylib to
 OpenGL 3.3 version.

 NOTE: Shaders used in this example are #version 330 (OpenGL 3.3), to test
 this example on OpenGL ES 2.0 platforms (Android, Raspberry Pi,
 HTML5), use #version 100 shaders raylib comes with shaders ready for
 both versions, check raylib/shaders install folder

 This example has been created using raylib 1.3 (www.raylib.com)
 raylib is licensed under an unmodified zlib/libpng license (View raylib.pas
 for details)

 Copyright (c) 2015 Ramon Santamaria (@raysan5)
 Delphi Conversion Copyright (c) 2018 dRez Games (https://drez.games)
*******************************************************************************)

program shaders_custom_uniform;

{$MODE objfpc}

uses cmem, ray_headers, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  cam: TCamera;
  dwarf: TModel;
  texture: TTexture2d;
  position: TVector3;
  shdr: TShader;
  swirlCenterLoc: integer;
  swirlCenter: array [0 .. 1] of single;
  target: TRenderTexture2D;
  mousePosition: TVector2;

begin
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  // Enable Multi Sampling Anti Aliasing 4x (if available)

  InitWindow(screenWidth, screenHeight,
    'raylib [shaders] example - custom uniform variable');

  // Define the camera to look into our 3d world
  cam.position := Vector3Create(3.0, 3.0, 3.0);
  cam.target := Vector3Create(0.0, 1.5, 0.0);
  cam.up := Vector3Create(0.0, 1.0, 0.0);
  cam.fovy := 45.0;
  cam._type := CAMERA_PERSPECTIVE;

  dwarf := LoadModel('resources/models/dwarf.obj'); // Load OBJ model
  texture := LoadTexture('resources/models/dwarf_diffuse.png');
  // Load model texture (diffuse map)
  //dwarf.material.maps[MAP_DIFFUSE].texture := texture;
  SetMaterialTexture(@dwarf.materials[0], MAP_DIFFUSE, texture);
  // Set dwarf model diffuse texture

  position := Vector3Create(0.0, 0.0, 0.0); // Set model position

  shdr := LoadShader('resources/shaders/glsl330/base.vs',
    'resources/shaders/glsl330/swirl.fs'); // Load postpro shader

  // Get variable (uniform) location on the shader to connect with the program
  // NOTE: If uniform variable could not be found in the shader, function
  // returns -1
  swirlCenterLoc := GetShaderLocation(shdr, 'center');

  swirlCenter[0] := screenWidth / 2;
  swirlCenter[1] := screenHeight / 2;

  // Create a RenderTexture2D to be used for render to texture
  target := LoadRenderTexture(screenWidth, screenHeight);

  // Setup orbital camera
  SetCameraMode(cam, CAMERA_ORBITAL); // Set an orbital camera mode

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  // -------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose do // Detect window close button or ESC key
  begin
    // Update
    // -----------------------------------------------------------------------
    mousePosition := GetMousePosition;

    swirlCenter[0] := mousePosition.x;
    swirlCenter[1] := screenHeight - mousePosition.y;

    // Send new value to the shader to be used on drawing
    SetShaderValue(shdr, swirlCenterLoc, @swirlCenter, 2);

    UpdateCamera(@cam); // Update camera
    // -----------------------------------------------------------------------

    // Draw
    // -----------------------------------------------------------------------
    BeginDrawing();

    ClearBackground(RAYWHITE);

    BeginTextureMode(target); // Enable drawing to texture

    BeginMode3d(cam);

    DrawModel(dwarf, position, 2.0, WHITE); // Draw 3d model with texture

    DrawGrid(10, 1.0); // Draw a grid

    EndMode3d();

    DrawText('TEXT DRAWN IN RENDER TEXTURE', 200, 10, 30, RED);

    EndTextureMode();
    // End drawing to texture (now we have a texture available for next passes)

    BeginShaderMode(shdr);

    // NOTE: Render texture must be y-flipped due to default OpenGL
    // coordinates (left-bottom)
    DrawTextureRec(target.texture, RectangleCreate(0, 0, target.texture.width,
     -target.texture.height), Vector2Create(0, 0), WHITE);

    EndShaderMode();

    DrawText('(c) Dwarf 3D model by David Moreno', screenWidth - 200,
      screenHeight - 20, 10, GRAY);

    DrawFPS(10, 10);

    EndDrawing();
    // ----------------------------------------------------------------------
  end;

  // De-Initialization
  // ------------------------------------------------------------------------
  UnloadShader(shdr); // Unload shader
  UnloadTexture(texture); // Unload texture
  UnloadModel(dwarf); // Unload model
  UnloadRenderTexture(target); // Unload render texture

  CloseWindow(); // Close window and OpenGL context

end.
