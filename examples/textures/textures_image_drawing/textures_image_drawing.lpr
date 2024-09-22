program textures_image_drawing;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  Cat, Parrots: TImage;
  Font: TFont;
  Texture: TTexture2D;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] examples - texture source and destination rectangles');
  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)

  Cat := LoadImage(PChar(GetApplicationDirectory + 'resources/cat.png')); // Load image in CPU memory (RAM)
  ImageCrop(@Cat, RectangleCreate(100, 10, 280, 380));      // Crop an image piece
  ImageFlipHorizontal(@Cat);                                  // Flip cropped image horizontally
  ImageResize(@Cat, 150, 200);                                // Resize flipped-cropped image

  Parrots := LoadImage(PChar(GetApplicationDirectory + 'resources/parrots.png')); // Load image in CPU memory (RAM)

  // Draw one image over the other with a scaling of 1.5f
  ImageDraw(@Parrots, Cat, RectangleCreate(0, 0, Cat.Width, Cat.Height), RectangleCreate(30, 40, Cat.Width * 1.5, Cat.Height * 1.5), WHITE);
  ImageCrop(@Parrots, RectangleCreate(0, 50, Parrots.Width, Parrots.Height - 100)); // Crop resulting image

  // Draw on the image with a few image draw methods
  ImageDrawPixel(@Parrots, 10, 10, RAYWHITE);
  ImageDrawCircle(@Parrots, 10, 10, 5, RAYWHITE);
  ImageDrawRectangle(@Parrots, 5, 20, 10, 10, RAYWHITE);

  UnloadImage(cat); // Unload image from RAM

  // Load custom font for frawing on image
  Font := LoadFont(PChar(GetApplicationDirectory + 'resources/custom_jupiter_crash.png'));

  // Draw over image using custom font
  ImageDrawTextEx(@Parrots, Font, 'PARROTS & CAT', Vector2Create(300, 230), Font.BaseSize, -2, WHITE);

  UnloadFont(Font); // Unload custom font (already drawn used on image)

  Texture := LoadTextureFromImage(Parrots); // Image converted to texture, uploaded to GPU memory (VRAM)
  UnloadImage(Parrots); // Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
         ClearBackground(RAYWHITE);

      DrawTexture(Texture, ScreenWidth div 2 - Texture.Width div 2, ScreenHeight div 2 - Texture.Height div 2 - 40, WHITE);
      DrawRectangleLines(ScreenWidth div 2 - Texture.Width div 2, ScreenHeight div 2 - Texture.Height div 2 - 40, Texture.Width, Texture.Height, DARKGRAY);

      DrawText('We are drawing only one texture from various images composed!', 240, 350, 10, DARKGRAY);
      DrawText('Source images have been cropped, scaled, flipped and copied one over the other.', 190, 370, 10, DARKGRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

