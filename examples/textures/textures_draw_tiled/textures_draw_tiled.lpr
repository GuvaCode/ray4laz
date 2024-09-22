program textures_draw_tiled;

{$mode objfpc}{$H+}

uses 
cmem,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  OPT_WIDTH = 220; // Max width for the options container
  MARGIN_SIZE = 8; // Size for the margins
  COLOR_SIZE = 16; // Size of the color select buttons

var
  TexPattern: TTexture2D;
  RecPattern: array of TRectangle;
  Colors: array of TColor;
  ColorRec: array of TRectangle;
  X, Y, I: Integer;
  ActivePattern, ActiveCol: Integer;
  Scale, Rotation: Single;
  Mouse: TVector2;
procedure DrawTextureTiled(Texture: TTexture2D; Source, Dest: TRectangle; Origin: TVector2; Rotation, Scale: Single; Tint: TColor);
var
  TileWidth, TileHeight: Integer;
  Dy, Dx: Integer;
begin
  if (Texture.Id <= 0) or (Scale <= 0.0) then exit;  // Wanna see a infinite loop?!...just delete this line!
  if (Source.Width = 0) or (Source.Height = 0) then exit;

  TileWidth := Trunc(Source.Width * Scale);
  TileHeight := Trunc(Source.Height * Scale);
  if (Dest.Width < TileWidth) and (Dest.Height < TileHeight) then
  begin
    // Can fit only one tile
    DrawTexturePro(
      Texture,
      RectangleCreate(Source.X, Source.Y, (Dest.Width / TileWidth) * Source.Width, (Dest.Height / TileHeight) * Source.Height),
      RectangleCreate(Dest.X, Dest.Y, Dest.Width, Dest.Height),
      Origin, Rotation, Tint);
  end
  else if Dest.Width <= TileWidth then
  begin
    // Tiled vertically (one column)
    Dy := 0;
    while Dy + TileHeight < Dest.Height do
    begin
      DrawTexturePro(
        Texture,
        RectangleCreate(Source.X, source.Y, (Dest.Width / TileWidth) * Source.Width, Source.Height),
        RectangleCreate(Dest.X, Dest.Y + Dy, Dest.Width, TileHeight),
        Origin, Rotation, Tint);
      Dy := Dy + TileHeight;
    end;

    // Fit last tile
    if Dy < Dest.Height then
      DrawTexturePro(
        Texture,
        RectangleCreate(Source.X, Source.Y, (Dest.Width / TileWidth) * Source.Width, ((Dest.Height - Dy) / TileHeight) * Source.Height),
        RectangleCreate(Dest.X, Dest.Y + Dy, Dest.Width, Dest.Height - Dy),
        Origin, Rotation, Tint);
  end
  else if Dest.Height <= TileHeight then
  begin
    // Tiled horizontally (one row)
    Dx := 0;
    while Dx + TileWidth < Dest.Width do
    begin
      DrawTexturePro(
        Texture,
        RectangleCreate(Source.X, Source.Y, Source.Width, (Dest.Height / TileHeight) * Source.Height),
        RectangleCreate(Dest.X + Dx, Dest.Y, TileWidth, Dest.Height),
        Origin, Rotation, Tint);
      Dx := Dx + TileWidth;
    end;

    // Fit last tile
    if Dx < Dest.Width then
      DrawTexturePro(
        Texture,
        RectangleCreate(Source.X, Source.Y, ((Dest.Width - Dx) / TileWidth) * Source.Width, (Dest.Height / TileHeight) * Source.Height),
        RectangleCreate(Dest.X + Dx, Dest.Y, Dest.Width - Dx, Dest.Height),
        Origin, Rotation, Tint);
  end else
  begin
    // Tiled both horizontally and vertically (rows and columns)
    Dx := 0;
    while Dx + TileWidth < Dest.Width do
    begin
      Dy := 0;
      while Dy + TileHeight < Dest.Height do
      begin
        DrawTexturePro(Texture, Source, RectangleCreate(Dest.X + Dx, Dest.Y + Dy, TileWidth, TileHeight), Origin, Rotation, Tint);
        Dy := Dy + TileHeight;
      end;

      if Dy < Dest.Height then
        DrawTexturePro(
          Texture,
          RectangleCreate(Source.X, Source.Y, Source.Width, ((Dest.Height - Dy) / TileHeight) * Source.Height),
          RectangleCreate(Dest.X + Dx, Dest.Y + Dy, TileWidth, Dest.Height - Dy),
          Origin, Rotation, Tint);

      Dx := Dx + TileWidth;
    end;

    // Fit last column of tiles
    if Dx < Dest.Width then
    begin
      Dy := 0;
      while Dy + TileHeight < Dest.Height do
      begin
        DrawTexturePro(
          Texture,
          RectangleCreate(Source.X, Source.Y, ((Dest.Width - Dx) / TileWidth) * Source.Width, Source.Height),
          RectangleCreate(Dest.X + Dx, Dest.Y + Dy, Dest.Width - Dx, TileHeight),
          Origin, Rotation, Tint);
        Dy := Dy + TileHeight;
      end;

      // Draw final tile in the bottom right corner
      if Dy < Dest.Height then
        DrawTexturePro(
          Texture,
          RectangleCreate(Source.X, Source.Y, ((Dest.Width - Dx) / TileWidth) * Source.Width, ((Dest.Height - Dy) / TileHeight) * Source.Height),
          RectangleCreate(Dest.X + Dx, Dest.Y + Dy, Dest.Width - Dx, Dest.Height - Dy),
          Origin, Rotation, Tint);
    end;
  end;
end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - Draw part of a texture tiled');
  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  TexPattern := LoadTexture(PChar(GetApplicationDirectory + 'resources/patterns.png'));
  SetTextureFilter(TexPattern, TEXTURE_FILTER_TRILINEAR); // Makes the texture smoother when upscaled

  // Coordinates for all patterns inside the texture
  RecPattern := [
    RectangleCreate(3, 3, 66, 66),
    RectangleCreate(75, 3, 100, 100),
    RectangleCreate(3, 75, 66, 66),
    RectangleCreate(7, 156, 50, 50),
    RectangleCreate(85, 106, 90, 45),
    RectangleCreate(75, 154, 100, 60)
  ];

  // Setup colors
  Colors := [BLACK, MAROON, ORANGE, BLUE, PURPLE, BEIGE, LIME, RED, DARKGRAY, SKYBLUE];
  SetLength(ColorRec, Length(Colors));

  // Calculate rectangle for each color
  X := 0;
  Y := 0;
  for I := 0 to High(Colors) do
  begin
    ColorRec[I].X := 2.0 + MARGIN_SIZE + X;
    ColorRec[I].Y := 22.0 + 256.0 + MARGIN_SIZE + y;
    ColorRec[I].Width := COLOR_SIZE * 2.0;
    ColorRec[I].Height := COLOR_SIZE;

    if I = (Length(Colors) div 2 - 1) then
    begin
      X := 0;
      Y := Y + COLOR_SIZE + MARGIN_SIZE;
    end else
      X := X + (COLOR_SIZE * 2 + MARGIN_SIZE);
  end;

  ActivePattern := 0; ActiveCol := 0;
  Scale := 1.0; Rotation := 0.0;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Handle mouse
        if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
        begin
          Mouse := GetMousePosition();

          // Check which pattern was clicked and set it as the active pattern
          for I := 0 to High(RecPattern) do
          begin
            if CheckCollisionPointRec(Mouse, RectangleCreate(2 + MARGIN_SIZE + RecPattern[I].X, 40 + MARGIN_SIZE + RecPattern[i].Y, RecPattern[I].Width, RecPattern[i].Height)) then
            begin
              ActivePattern := I;
              break;
            end;
          end;

          // Check to see which color was clicked and set it as the active color
          for I := 0 to High(Colors) do
          begin
            if CheckCollisionPointRec(Mouse, ColorRec[I]) then
            begin
              ActiveCol := I;
              break;
            end;
          end;
        end;

        // Handle keys

        // Change scale
        if IsKeyPressed(KEY_UP) then
          Scale := Scale + 0.25;
        if IsKeyPressed(KEY_DOWN) then
          Scale := Scale - 0.25;
        if Scale > 10.0 then
          Scale := 10.0
        else if Scale <= 0.0 then
          Scale := 0.25;

        // Change rotation
        if IsKeyPressed(KEY_LEFT) then
          Rotation := Rotation - 25.0;
        if IsKeyPressed(KEY_RIGHT) then
          Rotation := Rotation + 25.0;

        // Reset
        if IsKeyPressed(KEY_SPACE) then
        begin
          Rotation := 0.0;
          Scale := 1.0;
        end;
        //-------------------------------------------------------------------------------------------

        // Draw
        //-------------------------------------------------------------------------------------------
        BeginDrawing();
          ClearBackground(RAYWHITE);

          // Draw the tiled area
          DrawTextureTiled(TexPattern, RecPattern[ActivePattern], RectangleCreate(OPT_WIDTH + MARGIN_SIZE, MARGIN_SIZE, GetScreenWidth() - OPT_WIDTH - 2.0 * MARGIN_SIZE, GetScreenHeight() - 2.0 * MARGIN_SIZE),
          Vector2Create(0.0, 0.0), Rotation, Scale, Colors[ActiveCol]);

          // Draw options
          DrawRectangle(MARGIN_SIZE, MARGIN_SIZE, OPT_WIDTH - MARGIN_SIZE, GetScreenHeight() - 2 * MARGIN_SIZE, ColorAlpha(LIGHTGRAY, 0.5));

          DrawText('Select Pattern', 2 + MARGIN_SIZE, 30 + MARGIN_SIZE, 10, BLACK);
          DrawTexture(TexPattern, 2 + MARGIN_SIZE, 40 + MARGIN_SIZE, BLACK);
          DrawRectangle(2 + MARGIN_SIZE + Trunc(RecPattern[ActivePattern].X), 40 + MARGIN_SIZE + Trunc(RecPattern[ActivePattern].Y), Trunc(RecPattern[ActivePattern].Width), Trunc(RecPattern[ActivePattern].Height), ColorAlpha(DARKBLUE, 0.3));

          DrawText('Select Color', 2+MARGIN_SIZE, 10+256+MARGIN_SIZE, 10, BLACK);
          for I := 0 to High(Colors) do
          begin
            DrawRectangleRec(ColorRec[I], Colors[I]);
            if ActiveCol = I then
              DrawRectangleLinesEx(ColorRec[I], 3, ColorAlpha(WHITE, 0.5));
          end;

          DrawText('Scale (UP/DOWN to change)', 2 + MARGIN_SIZE, 80 + 256 + MARGIN_SIZE, 10, BLACK);
          DrawText(TextFormat('%.2fx', scale), 2 + MARGIN_SIZE, 92 + 256 + MARGIN_SIZE, 20, BLACK);

          DrawText('Rotation (LEFT/RIGHT to change)', 2 + MARGIN_SIZE, 122 + 256 + MARGIN_SIZE, 10, BLACK);
          DrawText(TextFormat('%.0f degrees', rotation), 2 + MARGIN_SIZE, 134 + 256 + MARGIN_SIZE, 20, BLACK);

          DrawText('Press [SPACE] to reset', 2 + MARGIN_SIZE, 164 + 256 + MARGIN_SIZE, 10, DARKBLUE);

          // Draw FPS
          DrawText(TextFormat('%i FPS', GetFPS()), 2 + MARGIN_SIZE, 2 + MARGIN_SIZE, 20, BLACK);
        EndDrawing();

        // De-Initialization
  end;
  //--------------------------------------------------------------------------------------
  UnloadTexture(TexPattern); // Texture unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

