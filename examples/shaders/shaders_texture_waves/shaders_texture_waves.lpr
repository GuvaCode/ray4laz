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
  Shader := LoadShader(nil, TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/wave.fs'), Integer(GLSL_VERSION)));

  secondsLoc := GetShaderLocation(shader, 'seconds');
  freqXLoc := GetShaderLocation(shader, 'freqX');
  freqYLoc := GetShaderLocation(shader, 'freqY');
  ampXLoc := GetShaderLocation(shader, 'ampX');
  ampYLoc := GetShaderLocation(shader, 'ampY');
  speedXLoc := GetShaderLocation(shader, 'speedX');
  speedYLoc := GetShaderLocation(shader, 'speedY');

  // Shader uniform values that can be updated at any time
  freqX := 25.0;
  freqY := 25.0;
  ampX := 5.0;
  ampY := 5.0;
  speedX := 8.0;
  speedY := 8.0;


  ScreenSize[0] := GetScreenWidth();
  ScreenSize[1] := GetScreenHeight();
  SetShaderValue(shader, GetShaderLocation(shader, 'size'), @screenSize, SHADER_UNIFORM_VEC2);
  SetShaderValue(shader, freqXLoc, @freqX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, freqYLoc, @freqY, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, ampXLoc, @ampX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, ampYLoc, @ampY, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, speedXLoc, @speedX, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, speedYLoc, @speedY, SHADER_UNIFORM_FLOAT);
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
          DrawTexture(Texture, 0, Texture.height, WHITE);
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

