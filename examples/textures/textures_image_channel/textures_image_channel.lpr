{*******************************************************************************************
*
*   raylib [textures] example - Retrive image channel (mask)
*
*   NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
*
*   Example originally created with raylib 5.1-dev, last time updated with raylib 5.1-dev
*
*   Example contributed by Bruno Cabral (github.com/brccabral) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2024-2024 Bruno Cabral (github.com/brccabral) and Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunvo Vadim (@guvacode)
*
********************************************************************************************}
program textures_image_channel;

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
  fudesumiImage, imageAlpha, imageRed, imageGreen, imageBlue, backgroundImage: TImage;
  fudesumiTexture, textureAlpha, textureRed, textureGreen, textureBlue, backgroundTexture: TTexture;
  fudesumiRec, fudesumiPos, redPos, greenPos, bluePos, alphaPos: TRectangle;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - extract channel from image');

  fudesumiImage := LoadImage('resources/fudesumi.png');

  imageAlpha := ImageFromChannel(fudesumiImage, 3);
  ImageAlphaMask(@imageAlpha, imageAlpha);

  imageRed := ImageFromChannel(fudesumiImage, 0);
  ImageAlphaMask(@imageRed, imageAlpha);

  imageGreen := ImageFromChannel(fudesumiImage, 1);
  ImageAlphaMask(@imageGreen, imageAlpha);

  imageBlue := ImageFromChannel(fudesumiImage, 2);
  ImageAlphaMask(@imageBlue, imageAlpha);

  backgroundImage := GenImageChecked(screenWidth, screenHeight, screenWidth div 20, screenHeight div 20, ORANGE, YELLOW);

  fudesumiTexture := LoadTextureFromImage(fudesumiImage);
  textureAlpha := LoadTextureFromImage(imageAlpha);
  textureRed := LoadTextureFromImage(imageRed);
  textureGreen := LoadTextureFromImage(imageGreen);
  textureBlue := LoadTextureFromImage(imageBlue);
  backgroundTexture := LoadTextureFromImage(backgroundImage);

  UnloadImage(fudesumiImage);
  UnloadImage(imageAlpha);
  UnloadImage(imageRed);
  UnloadImage(imageGreen);
  UnloadImage(imageBlue);
  UnloadImage(backgroundImage);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  fudesumiRec := RectangleCreate(0, 0, fudesumiImage.width, fudesumiImage.height);

  fudesumiPos := RectangleCreate(50, 10, fudesumiImage.width*0.8, fudesumiImage.height*0.8);
  redPos := RectangleCreate( 410, 10, fudesumiPos.width / 2, fudesumiPos.height / 2 );
  greenPos := RectangleCreate( 600, 10, fudesumiPos.width / 2, fudesumiPos.height / 2 );
  bluePos := RectangleCreate( 410, 230, fudesumiPos.width / 2, fudesumiPos.height / 2 );
  alphaPos := RectangleCreate( 600, 230, fudesumiPos.width / 2, fudesumiPos.height / 2 );


  // Main game loop
  while not WindowShouldClose() do
    begin
      // Draw
      BeginDrawing();
        DrawTexture(backgroundTexture, 0, 0, WHITE);
        DrawTexturePro(fudesumiTexture, fudesumiRec, fudesumiPos, Vector2Create (0, 0), 0, WHITE);

        DrawTexturePro(textureRed, fudesumiRec, redPos, Vector2Create (0, 0), 0, RED);
        DrawTexturePro(textureGreen, fudesumiRec, greenPos, Vector2Create (0, 0), 0, GREEN);
        DrawTexturePro(textureBlue, fudesumiRec, bluePos, Vector2Create (0, 0), 0, BLUE);
        DrawTexturePro(textureAlpha, fudesumiRec, alphaPos, Vector2Create (0, 0), 0, WHITE);

      EndDrawing();
    end;

  // De-Initialization
  UnloadTexture(backgroundTexture);
  UnloadTexture(fudesumiTexture);
  UnloadTexture(textureRed);
  UnloadTexture(textureGreen);
  UnloadTexture(textureBlue);
  UnloadTexture(textureAlpha);
  CloseWindow();        // Close window and OpenGL context
end.

