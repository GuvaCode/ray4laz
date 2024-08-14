{*******************************************************************************************
*
*   raylib [textures] example - Image Rotation
*
*   Example originally created with raylib 1.0, last time updated with raylib 1.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2014-2024 Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunko Vadim
*
********************************************************************************************}
program textures_image_rotate;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  NUM_TEXTURES = 3;

var
  image45, image90, imageNeg90: TImage;
  textures: array[0.. NUM_TEXTURES] of TTexture ;
  currentTexture: integer;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - texture rotation');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  image45 := LoadImage('resources/raylib_logo.png');
  image90 := LoadImage('resources/raylib_logo.png');
  imageNeg90 := LoadImage('resources/raylib_logo.png');

  ImageRotate(@image45, 45);
  ImageRotate(@image90, 90);
  ImageRotate(@imageNeg90, -90);

  textures[0] := LoadTextureFromImage(image45);
  textures[1] := LoadTextureFromImage(image90);
  textures[2] := LoadTextureFromImage(imageNeg90);

  currentTexture := 0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT) or IsKeyPressed(KEY_RIGHT)) then
      begin
          currentTexture := (currentTexture + 1) mod NUM_TEXTURES; // Cycle between the textures
      end;

      // Draw
      BeginDrawing();
       ClearBackground(RAYWHITE);
       DrawTexture(textures[currentTexture], screenWidth div 2 - textures[currentTexture].width div 2,
       screenHeight div 2 - textures[currentTexture].height div 2, WHITE);
       DrawText('Press LEFT MOUSE BUTTON to rotate the image clockwise', 250, 420, 10, DARKGRAY);
      EndDrawing();
    end;

  // De-Initialization
  for currentTexture := 0 to NUM_TEXTURES -1 do UnloadTexture(textures[currentTexture]);
  CloseWindow();        // Close window and OpenGL context
end.

