{*******************************************************************************************
*
*   raylib [textures] example - SVG loading and texture creation
*
*   NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2022 Dennis Meinen (@bixxy#4258 on Discord)
*   pascal conversion 2024 Gunko Vadim
*
********************************************************************************************}
program textures_svg_loading;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;

var
  image: TImage;
  texture: TTexture;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - svg loading');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  image := LoadImageSvg('resources/test.svg', 400, 350);     // Loaded in CPU memory (RAM)
  texture := LoadTextureFromImage(image);          // Image converted to texture, GPU memory (VRAM)
  UnloadImage(image);   // Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Draw
      BeginDrawing();
      ClearBackground(RAYWHITE);

       DrawTexture(texture, screenWidth div 2 - texture.width div 2, screenHeight div 2 - texture.height div 2, WHITE);

       //Red border to illustrate how the SVG is centered within the specified dimensions
       DrawRectangleLines(trunc(screenWidth / 2 - texture.width / 2) - 1,
                          trunc(screenHeight / 2 - texture.height / 2) - 1,
                          texture.width + 2, texture.height + 2, RED);

       DrawText('this IS a texture loaded from an SVG file!', 300, 410, 10, GRAY);

      EndDrawing();
    end;

  // De-Initialization
  UnloadTexture(texture);       // Texture unloading
  CloseWindow();        // Close window and OpenGL context
end.

