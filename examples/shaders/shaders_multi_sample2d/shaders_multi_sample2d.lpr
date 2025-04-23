program shaders_multi_sample2d;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;
var
  imRed, imBlue: TImage;
  texRed, texBlue: TTexture;
  texBlueLoc, dividerLoc: integer;
  dividerValue: single;
  shader: TShader;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');


  imRed := GenImageColor(800, 450, ColorCreate( 255, 0, 0, 255 ));
  texRed := LoadTextureFromImage(imRed);
  UnloadImage(imRed);

  imBlue := GenImageColor(800, 450, ColorCreate( 0, 0, 255, 255 ));
  texBlue := LoadTextureFromImage(imBlue);
  UnloadImage(imBlue);

  shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/color_mix.fs', GLSL_VERSION));

  // Get an additional sampler2D location to be enabled on drawing
  texBlueLoc := GetShaderLocation(shader, 'texture1');

  // Get shader uniform for divider
  dividerLoc := GetShaderLocation(shader, 'divider');
  dividerValue := 0.5;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
         if (IsKeyDown(KEY_RIGHT)) then dividerValue += 0.01
         else if (IsKeyDown(KEY_LEFT)) then dividerValue -= 0.01;

         if (dividerValue < 0.0) then dividerValue := 0.0
         else if (dividerValue > 1.0) then dividerValue := 1.0;

         SetShaderValue(shader, dividerLoc, @dividerValue, SHADER_UNIFORM_FLOAT);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginShaderMode(shader);

          // WARNING: Additional samplers are enabled for all draw calls in the batch,
          // EndShaderMode() forces batch drawing and consequently resets active textures
          // to let other sampler2D to be activated on consequent drawings (if required)
          SetShaderValueTexture(shader, texBlueLoc, texBlue);

          // We are drawing texRed using default sampler2D texture0 but
          // an additional texture units is enabled for texBlue (sampler2D texture1)
          DrawTexture(texRed, 0, 0, WHITE);

      EndShaderMode();

      DrawText('Use KEY_LEFT/KEY_RIGHT to move texture mixing in shader!', 80, GetScreenHeight() - 40, 20, RAYWHITE);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(shader);       // Unload shader
  UnloadTexture(texRed);      // Unload texture
  UnloadTexture(texBlue);     // Unload texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

