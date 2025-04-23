program textures_image_text;

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
  Texture: TTexture2D;
  Parrots: TImage;
  Font: TFont;
  Position: TVector2;
  ShowFont: Boolean;

begin
  // Initialization
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [texture] example - image text drawing');

  Parrots := LoadImage(PChar(GetApplicationDirectory + 'resources/parrots.png')); // Load image in CPU memory (RAM)

  // TTF Font loading with custom generation parameters
  Font := LoadFontEx(PChar(GetApplicationDirectory + 'resources/KAISG.ttf'), 64, nil, 0);

  // Draw over image using custom font
  ImageDrawTextEx(@Parrots, font, '[Parrots font drawing]', Vector2Create(20.0, 20.0), Font.BaseSize, 0.0, RED);

  Texture := LoadTextureFromImage(Parrots);  // Image converted to texture, uploaded to GPU memory (VRAM)
  UnloadImage(Parrots);   // Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM

  Position := Vector2Create(ScreenWidth / 2 - Texture.Width / 2, ScreenHeight / 2 - Texture.Height / 2 - 20);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    if IsKeyDown(KEY_SPACE) then
      ShowFont := True
    else
      ShowFont := False;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      if not ShowFont then
      begin
        // Draw texture with text already drawn inside
        DrawTextureV(Texture, Position, WHITE);

        // Draw text directly using sprite font
        DrawTextEx(Font, '[Parrots font drawing]',
          Vector2Create(Position.X + 20, Position.Y + 20 + 280), Font.BaseSize, 0.0, WHITE);
      end else
        DrawTexture(Font.Texture, ScreenWidth div 2 - Font.Texture.Width div 2, 50, BLACK);

      DrawText('PRESS SPACE to SHOW FONT ATLAS USED', 290, 420, 10, DARKGRAY);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading

  UnloadFont(Font); // Unload custom font

  CloseWindow(); // Close window and OpenGL context
end.

