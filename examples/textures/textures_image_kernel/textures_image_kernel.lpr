{*******************************************************************************************
*
*   raylib [textures] example - Image loading and texture creation
*
*   NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
*
*   Example originally created with raylib 1.3, last time updated with raylib 1.3
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2024 Karim Salem (@kimo-s)
*   pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}

program textures_image_kernel;

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

  gaussiankernel: array[0..2, 0..2] of single = (
  (1.0, 2.0, 1.0),
  (2.0, 4.0, 2.0),
  (1.0, 2.0, 1.0));

  sobelkernel: array[0..2, 0..2] of single = (
  (1.0, 0.0, -1.0),
  (2.0, 0.0, -2.0),
  (1.0, 0.0, -1.0));

  sharpenkernel: array[0..2, 0..2] of single = (
  (0.0, -1.0, 0.0),
  (-1.0,  5.0, -1.0),
  (0.0, -1.0, 0.0));


procedure NormalizeKernel(kernel: PSingle; size: integer);
var sum: single; i: integer;
begin
  sum := 0.0;
  for i:=0 to size do sum += kernel[i];// (int i = 0; i < size; i++) sum += kernel[i];

  if (sum <> 0.0) then
  begin
    for i:=0 to size do kernel[i] /= sum;
  end;
end;

var
  image, catSharpend, catSobel, catGaussian: TImage;
  texture, catSharpendTexture, catSobelTexture, catGaussianTexture: TTexture;
  i: integer;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

  image := LoadImage('resources/cat.png');     // Loaded in CPU memory (RAM)

  NormalizeKernel(@gaussiankernel, 9);
  NormalizeKernel(@sharpenkernel, 9);
  NormalizeKernel(@sobelkernel, 9);

  catSharpend := ImageCopy(image);
  ImageKernelConvolution(@catSharpend, @sharpenkernel, 9);

  catSobel := ImageCopy(image);
  ImageKernelConvolution(@catSobel, @sobelkernel, 9);

  catGaussian := ImageCopy(image);

  for i :=0 to 6 do
  begin
    ImageKernelConvolution(@catGaussian, @gaussiankernel, 9);
  end;

   ImageCrop(@image, RectangleCreate( 0, 0, 200, 450 ));
   ImageCrop(@catGaussian, RectangleCreate( 0, 0, 200, 450 ));
   ImageCrop(@catSobel, RectangleCreate( 0, 0, 200, 450 ));
   ImageCrop(@catSharpend, RectangleCreate( 0, 0, 200, 450 ));

   // Images converted to texture, GPU memory (VRAM)
   texture := LoadTextureFromImage(image);
   catSharpendTexture := LoadTextureFromImage(catSharpend);
   catSobelTexture := LoadTextureFromImage(catSobel);
   catGaussianTexture := LoadTextureFromImage(catGaussian);

   // Once images have been converted to texture and uploaded to VRAM,
   // they can be unloaded from RAM
   UnloadImage(image);
   UnloadImage(catGaussian);
   UnloadImage(catSobel);
   UnloadImage(catSharpend);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawTexture(catSharpendTexture, 0, 0, WHITE);
        DrawTexture(catSobelTexture, 200, 0, WHITE);
        DrawTexture(catGaussianTexture, 400, 0, WHITE);
        DrawTexture(texture, 600, 0, WHITE);
      EndDrawing();
    end;

  // De-Initialization
  UnloadTexture(texture);
  UnloadTexture(catGaussianTexture);
  UnloadTexture(catSobelTexture);
  UnloadTexture(catSharpendTexture);
  CloseWindow();        // Close window and OpenGL context
end.

