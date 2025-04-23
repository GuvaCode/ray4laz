{*******************************************************************************************
*
*   raylib [shaders] example - Sieve of Eratosthenes
*
*   Sieve of Eratosthenes, the earliest known (ancient Greek) prime number sieve.
*
*   "Sift the twos and sift the threes,
*    The Sieve of Eratosthenes.
*    When the multiples sublime,
*    the numbers that are left are prime."
*
*   NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
*         OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version.
*
*   NOTE: Shaders used in this example are #version 330 (OpenGL 3.3).
*
*   This example has been created using raylib 2.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by ProfJski and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2019 ProfJski and Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}


program shaders_eratosthenes;

{$mode objfpc}{$H+}

uses raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330; // Note: for android and rbpi glsl version = 100

var
  target: TRenderTexture2D;
  shader: TShader;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - Sieve of Eratosthenes');

  target := LoadRenderTexture(screenWidth, screenHeight);

  // Load Eratosthenes shader
  // NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/eratosthenes.fs', GLSL_VERSION));

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Nothing to do here, everything is happening in the shader
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginTextureMode(target);       // Enable drawing to texture
          ClearBackground(BLACK);     // Clear the render texture

          // Draw a rectangle in shader mode to be used as shader canvas
          // NOTE: Rectangle uses font white character texture coordinates,
          // so shader can not be applied here directly because input vertexTexCoord
          // do not represent full screen coordinates (space where want to apply shader)
          DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), BLACK);
      EndTextureMode();               // End drawing to texture (now we have a blank texture available for the shader)

      BeginDrawing();
          ClearBackground(RAYWHITE);  // Clear screen background

          BeginShaderMode(shader);
              // NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
              DrawTextureRec(target.texture, RectangleCreate( 0, 0, target.texture.width, -target.texture.height ),
              Vector2Create( 0.0, 0.0 ), WHITE);
          EndShaderMode();
      EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(shader);               // Unload shader
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

