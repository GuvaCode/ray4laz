program textures_image_loading;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Texture: TTexture2D;
  Image: TImage;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - image loading');
  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)

  Image := LoadImage(PChar(GetApplicationDirectory + 'resources/raylib_logo.png')); // Loaded in CPU memory (RAM)
  Texture :=  LoadTextureFromImage(Image); // Image converted to texture, GPU memory (VRAM)
  UnloadImage(Image); // Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
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

       DrawTexture(Texture, ScreenWidth div 2 - Texture.Width div 2, ScreenHeight div 2 - Texture.Height div 2, WHITE);
       DrawText('this IS a texture loaded from an image!', 360, 370, 10, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

