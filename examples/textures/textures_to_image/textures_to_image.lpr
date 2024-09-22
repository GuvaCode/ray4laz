program textures_to_image;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Texture: TTexture2D;
  Image: TImage;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [textures] example - texture to image');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)

  Image := LoadImage(PChar(GetApplicationDirectory + 'resources/raylib_logo.png')); // Loaded in CPU memory (RAM)
  Texture :=  LoadTextureFromImage(Image); // Image converted to texture, GPU memory (RAM -> VRAM)
  UnloadImage(Image); // Unload image data from CPU memory (RAM)

  Image := LoadImageFromTexture(Texture); // Load image from GPU texture (VRAM -> RAM)
  UnloadTexture(Texture); // Unload texture from GPU memory (VRAM)

  Texture := LoadTextureFromImage(Image); // Recreate texture from retrieved image data (RAM -> VRAM)

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------

    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawTexture(Texture, ScreenWidth div 2 - Texture.Width div 2, ScreenHeight div 2 - Texture.Height div 2, WHITE);

      DrawText('this IS a texture loaded from an image!', 300, 370, 10, GRAY);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading

  CloseWindow(); // Close window and OpenGL context
end.

