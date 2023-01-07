program shaders_texture_waves;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  Texture: TTexture2D;
  Shader: TShader;
  SecondsLoc, FreqXLoc, FreqYLoc, AmpXLoc, AmpYLoc, SpeedXLoc, SpeedYLoc: Integer;
  FreqX, FreqY, AmpX, AmpY, SpeedX, SpeedY: Single;
  ScreenSize: array [0..1] of Single;
  Seconds: Single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - texture waves');
  // Load texture texture to apply shaders
  Texture := LoadTexture('resources/space.png');

  // Load shader and setup location points and values
  Shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/wave.fs', Integer(GLSL_VERSION)));

  SecondsLoc := GetShaderLocation(Shader, 'secondes');
  FreqXLoc := GetShaderLocation(Shader, 'freqX');
  FreqYLoc := GetShaderLocation(Shader, 'freqY');
  AmpXLoc := GetShaderLocation(Shader, 'ampX');
  AmpYLoc := GetShaderLocation(Shader, 'ampY');
  SpeedXLoc := GetShaderLocation(Shader, 'speedX');
  SpeedYLoc := GetShaderLocation(Shader, 'speedY');

  // Shader uniform values that can be updated at any time
  FreqX := 25.0;
  FreqY := 25.0;
  AmpX := 5.0;
  AmpY := 5.0;
  SpeedX := 8.0;
  SpeedY := 8.0;

  ScreenSize[0] := GetScreenWidth(); ScreenSize[1] := GetScreenHeight();
  SetShaderValue(Shader, GetShaderLocation(Shader, 'size'), @ScreenSize, SHADER_UNIFORM_VEC2);
  SetShaderValue(Shader, FreqXLoc, @FreqX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, FreqYLoc, @FreqY, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, AmpXLoc, @AmpX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, AmpYLoc, @AmpY, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, SpeedXLoc, @SpeedX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, SpeedYLoc, @SpeedY, SHADER_UNIFORM_FLOAT);

  Seconds := 0.0;
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------

      Seconds := Seconds + GetFrameTime();

      SetShaderValue(Shader, SecondsLoc, @Seconds, SHADER_UNIFORM_FLOAT);

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginShaderMode(Shader);

          DrawTexture(Texture, 0, 0, WHITE);
          DrawTexture(Texture, Texture.Width, 0, WHITE);

        EndShaderMode();

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture);
  UnloadShader(Shader);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

