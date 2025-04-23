program shaders_texture_drawing;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;
var
  imBlank: TImage;
  texture: TTexture;
  shader: TShader;
  time: Single;
  timeLoc: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - texture drawing');
  imBlank := GenImageColor(1024, 1024, BLANK);
  texture := LoadTextureFromImage(imBlank);  // Load blank texture to fill on shader
  UnloadImage(imBlank);

  // NOTE: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
  shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/cubes_panning.fs', GLSL_VERSION));

  time := 0.0;
  timeLoc := GetShaderLocation(shader, 'uTime');
  SetShaderValue(shader, timeLoc, @time, SHADER_UNIFORM_FLOAT);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      time := GetTime();
      SetShaderValue(shader, timeLoc, @time, SHADER_UNIFORM_FLOAT);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

      ClearBackground(RAYWHITE);
        BeginShaderMode(shader);    // Enable our custom shader for next shapes/textures drawings
          DrawTexture(texture, 0, 0, WHITE);  // Drawing BLANK texture, all magic happens on shader
       EndShaderMode();            // Disable our custom shader, return to default shader

       DrawText('BACKGROUND is PAINTED and ANIMATED on SHADER!', 10, 10, 20, MAROON);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------

  UnloadShader(shader);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

