program shaders_texture_outline;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;
var
  ShdrOutline: TShader;
  Texture: TTexture2D;
  OutlineSize: Single;
  OutlineColor: array of Single;
  TextureSize: array of Single;
  OutlineSizeLoc, OutlineColorLoc, TextureSizeLoc: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - Apply an outline to a texture');

  Texture := LoadTexture('resources/fudesumi.png');
  ShdrOutline := LoadShader(nil, TextFormat('resources/shaders/glsl%i/outline.fs', Integer(GLSL_VERSION)));

  OutlineSize := 2.0;
  OutlineColor := [1.0, 0.0, 0.0, 1.0];     // Normalized RED color
  TextureSize := [Texture.Width, Texture.Height];

  // Get shader locations
  OutlineSizeLoc := GetShaderLocation(ShdrOutline, 'outlineSize');
  OutlineColorLoc := GetShaderLocation(ShdrOutline, 'outlineColor');
  TextureSizeLoc := GetShaderLocation(ShdrOutline, 'textureSize');

  // Set shader values (they can be changed later)
  SetShaderValue(ShdrOutline, OutlineSizeLoc, @OutlineSize, SHADER_UNIFORM_FLOAT);
  SetShaderValue(ShdrOutline, OutlineColorLoc, @OutlineColor[0], SHADER_UNIFORM_VEC4);
  SetShaderValue(ShdrOutline, TextureSizeLoc, @TextureSize[0], SHADER_UNIFORM_VEC2);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      OutlineSize := OutlineSize + GetMouseWheelMove();
      if OutlineSize < 1.0 then
        OutlineSize := 1.0;

      SetShaderValue(ShdrOutline, OutlineSizeLoc, @OutlineSize, SHADER_UNIFORM_FLOAT);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       BeginShaderMode(ShdrOutline);
       DrawTexture(Texture, GetScreenWidth() div 2 - Texture.Width div 2, -30, WHITE);

       EndShaderMode();
       DrawText('Shader-based'#10'texture\noutline', 10, 10, 20, GRAY);
       DrawText(TextFormat('Outline size: %i px', Trunc(OutlineSize)), 10, 120, 20, MAROON);
       DrawFPS(710, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture);
  UnloadShader(ShdrOutline);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

