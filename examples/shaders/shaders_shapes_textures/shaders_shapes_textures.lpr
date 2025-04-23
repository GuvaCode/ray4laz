program shaders_shapes_textures;

{$mode objfpc}{$H+}

uses 
cmem, raymath, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  Fudesumi: TTexture2D;
  Shader: TShader;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - shapes and texture shaders');

  Fudesumi := LoadTexture('resources/fudesumi.png');

  // Load shader to be used on some parts drawing
  // NOTE 1: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
  // NOTE 2: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  Shader := LoadShader(nil, TextFormat('resources/shaders/shaders/glsl%i/grayscale.fs', Integer(GLSL_VERSION)));

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

      ClearBackground(RAYWHITE);
      // Start drawing with default shader

        DrawText('USING DEFAULT SHADER', 20, 40, 10, RED);

        DrawCircle(80, 120, 35, DARKBLUE);
        DrawCircleGradient(80, 220, 60, GREEN, SKYBLUE);
        DrawCircleLines(80, 340, 80, DARKBLUE);


        // Activate our custom shader to be applied on next shapes/textures drawings
        BeginShaderMode(Shader);

          DrawText('USING CUSTOM SHADER', 190, 40, 10, RED);

          DrawRectangle(250 - 60, 90, 120, 60, RED);
          DrawRectangleGradientH(250 - 90, 170, 180, 130, MAROON, GOLD);
          DrawRectangleLines(250 - 40, 320, 80, 60, ORANGE);

        // Activate our default shader for next drawings
        EndShaderMode();

        DrawText('USING DEFAULT SHADER', 370, 40, 10, RED);

        DrawTriangle(Vector2Create(430, 80),
                     Vector2Create(430 - 60, 150),
                     Vector2Create(430 + 60, 150), VIOLET);

        DrawTriangleLines(Vector2Create(430, 160),
                          Vector2Create(430 - 20, 230),
                          Vector2Create(430 + 20, 230), DARKBLUE);

        DrawPoly(Vector2Create(430, 320), 6, 80, 0, BROWN);

        // Activate our custom shader to be applied on next shapes/textures drawings
        BeginShaderMode(Shader);

          DrawTexture(Fudesumi, 500, -30, WHITE);    // Using custom shader

        // Activate our default shader for next drawings
        EndShaderMode();

        DrawText('(c) Fudesumi sprite by Eiden Marsal', 380, ScreenHeight - 20, 10, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(Shader);       // Unload shader
  UnloadTexture(Fudesumi);    // Unload texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

