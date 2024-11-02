program embedded_files_loading;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib,
raylogo; // array of image

const
  screenWidth = 800;
  screenHeight = 450;

var Image: TImage;
    Texture: TTexture2D;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

  Image.data := @IMAGE_DATA;
  Image.width := IMAGE_WIDTH;
  Image.height := IMAGE_HEIGHT;
  Image.format := IMAGE_FORMAT;
  Image.mipmaps := 1;

  // Image converted to Texture (VRAM) to be drawn
  texture := LoadTextureFromImage(image);


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawTexture(texture, screenWidth div 2 - texture.width div 2, 40, WHITE);

        DrawText('raylib logo loaded from array image', 220, 320, 20, LIGHTGRAY);

      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

