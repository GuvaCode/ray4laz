{*******************************************************************************************
*
*   raylib [shaders] example - Hot reloading
*
*   NOTE: This example requires raylib OpenGL 3.3 for shaders support and only #version 330
*         is currently supported. OpenGL ES 2.0 platforms are not supported at the moment.
*
*   Example originally created with raylib 3.0, last time updated with raylib 3.5
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2020-2024 Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}

program shaders_hot_reloading;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
rlgl,
raylib, sysutils;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  fragShaderFileName: PChar;
  fragShaderFileModTime: QWord;
  shader, updatedShader: TShader;
  resolutionLoc, mouseLoc, timeLoc: integer;
  resolution, mousePos: array[0..1] of single;
  totalTime: single;
  shaderAutoReloading: boolean;
  mouse: TVector2;

  currentFragShaderModTime: integer;
  autoStr: PChar;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - hot reloading');

  fragShaderFileName := 'resources/shaders/glsl%i/reload.fs';
  fragShaderFileModTime := GetFileModTime(TextFormat(fragShaderFileName, GLSL_VERSION));

  // Load raymarching shader
  // NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  shader := LoadShader(nil, TextFormat(fragShaderFileName, GLSL_VERSION));

  // Get shader locations for required uniforms
  resolutionLoc := GetShaderLocation(shader, 'resolution');
  mouseLoc := GetShaderLocation(shader, 'mouse');
  timeLoc := GetShaderLocation(shader, 'time');

  resolution[0] := screenWidth;//{ (float)screenWidth, (float)screenHeight };
  resolution[1] := screenHeight;//

  SetShaderValue(shader, resolutionLoc, @resolution, SHADER_UNIFORM_VEC2);

  totalTime := 0.0;
  shaderAutoReloading := false;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      totalTime += GetFrameTime();
      mouse := GetMousePosition();
      mousePos[0] := mouse.x;
      mousePos[1] := mouse.y;

      // Set shader required uniform values
      SetShaderValue(shader, timeLoc, @totalTime, SHADER_UNIFORM_FLOAT);
      SetShaderValue(shader, mouseLoc, @mousePos, SHADER_UNIFORM_VEC2);

      // Hot shader reloading
      if (shaderAutoReloading and (IsMouseButtonPressed(MOUSE_BUTTON_LEFT))) then
      begin
        currentFragShaderModTime := GetFileModTime(TextFormat(fragShaderFileName, GLSL_VERSION));

          // Check if shader file has been modified
          if (currentFragShaderModTime) <> fragShaderFileModTime then
          begin
              // Try reloading updated shader
              updatedShader := LoadShader(nil, TextFormat(fragShaderFileName, GLSL_VERSION));

              if (updatedShader.id) <> rlGetShaderIdDefault() then      // It was correctly loaded
              begin
                  UnloadShader(shader);
                  shader := updatedShader;

                  // Get shader locations for required uniforms
                  resolutionLoc := GetShaderLocation(shader, 'resolution');
                  mouseLoc := GetShaderLocation(shader, 'mouse');
                  timeLoc := GetShaderLocation(shader, 'time');

                  // Reset required uniforms
                  SetShaderValue(shader, resolutionLoc, @resolution, SHADER_UNIFORM_VEC2);
              end;

              fragShaderFileModTime := currentFragShaderModTime;
          end;
      end;

      if (IsKeyPressed(KEY_A)) then shaderAutoReloading := not shaderAutoReloading;


      // Draw
      BeginDrawing();
      ClearBackground(RAYWHITE);

      // We only draw a white full-screen rectangle, frame is generated in shader
      BeginShaderMode(shader);
          DrawRectangle(0, 0, screenWidth, screenHeight, WHITE);
      EndShaderMode();

      if shaderAutoReloading then autoStr := 'AUTO' else
                                  autoStr := 'MANUAL';

      DrawText(TextFormat('PRESS [A] to TOGGLE SHADER AUTOLOADING: %s',
      autoStr), 10, 10, 10, {shaderAutoReloading? RED : }BLACK);

      if not shaderAutoReloading then DrawText('MOUSE CLICK to SHADER RE-LOADING', 10, 30, 10, BLACK);

      EndDrawing();
    end;

  // De-Initialization
  UnloadShader(shader);           // Unload shader
  CloseWindow();        // Close window and OpenGL context
end.

