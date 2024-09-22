program text_font_filters;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  msg: string = 'Loaded Font';
var
  Font: TFont;
  FontSize: Single;
  FontPosition, TextSize: TVector2;
  CurrentFontFilter: Integer;
  DroppedFiles: TFilePathList;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - font filters');
  // TTF Font loading with custom generation parameters
  Font := LoadFontEx(PChar(GetApplicationDirectory + 'resources/KAISG.ttf'), 96, nil, 0);

  // Generate mipmap levels to use trilinear filtering
  // NOTE: On 2D drawing it won't be noticeable, it looks like FILTER_BILINEAR
  GenTextureMipmaps(@Font.Texture);

  FontSize := Font.BaseSize;
  FontPosition := Vector2Create(40.0, ScreenHeight / 2.0 - 80.0);
  TextSize := Vector2Create(0.0, 0.0);

  // Setup texture scaling filter
  SetTextureFilter(Font.Texture, TEXTURE_FILTER_POINT);
  CurrentFontFilter := 0;      // TEXTURE_FILTER_POINT

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      FontSize := FontSize + GetMouseWheelMove() * 4.0;

      // Choose font texture filter method
      if IsKeyPressed(KEY_ONE) then
      begin
        SetTextureFilter(Font.Texture, TEXTURE_FILTER_POINT);
        CurrentFontFilter := 0;
      end
      else if IsKeyPressed(KEY_TWO) then
      begin
        SetTextureFilter(Font.Texture, TEXTURE_FILTER_BILINEAR);
        CurrentFontFilter := 1;
      end
      else if IsKeyPressed(KEY_THREE) then
      begin
        // NOTE: Trilinear filter won't be noticed on 2D drawing
        SetTextureFilter(Font.Texture, TEXTURE_FILTER_TRILINEAR);
        CurrentFontFilter := 2;
      end;

      TextSize := MeasureTextEx(Font, PAnsiChar(UTF8String(Msg)), FontSize, 0);

      if IsKeyDown(KEY_LEFT) then
        FontPosition.X := FontPosition.X - 10
      else if IsKeyDown(KEY_RIGHT) then
        FontPosition.X := FontPosition.X + 10;

      // Load a dropped TTF file dynamically (at current fontSize)
      if IsFileDropped() then
      begin
        DroppedFiles := LoadDroppedFiles();

        // NOTE: We only support first ttf file dropped
        if IsFileExtension(DroppedFiles.Paths[0], '.ttf') then
        begin
          UnloadFont(Font);
          Font := LoadFontEx(DroppedFiles.Paths[0], Trunc(FontSize), nil, 0);
        end;

        UnloadDroppedFiles(DroppedFiles);    // Unload filepaths from memory
      end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       DrawText('Use mouse wheel to change font size', 20, 20, 10, GRAY);
       DrawText('Use KEY_RIGHT and KEY_LEFT to move text', 20, 40, 10, GRAY);
       DrawText('Use 1, 2, 3 to change texture filter', 20, 60, 10, GRAY);
       DrawText('Drop a new TTF font for dynamic loading', 20, 80, 10, DARKGRAY);

       DrawTextEx(Font, PChar(Msg), FontPosition, FontSize, 0, BLACK);

       // TODO: It seems texSize measurement is not accurate due to chars offsets...
       //DrawRectangleLines(Trunc(FontPosition.X), Trunc(FontPosition.Y), Trunc(TextSize.X), Trunc(TextSize.Y), RED);

       DrawRectangle(0, ScreenHeight - 80, ScreenWidth, 80, LIGHTGRAY);
       DrawText(TextFormat('Font size: %02.02f', FontSize), 20, ScreenHeight - 50, 10, DARKGRAY);
       DrawText(TextFormat('Text size: [%02.02f, %02.02f]', TextSize.X, TextSize.Y), 20, ScreenHeight - 30, 10, DARKGRAY);
       DrawText('CURRENT TEXTURE FILTER:', 250, 400, 20, GRAY);

       if CurrentFontFilter = 0 then
         DrawText('POINT', 570, 400, 20, BLACK)
       else if CurrentFontFilter = 1 then
         DrawText('BILINEAR', 570, 400, 20, BLACK)
       else if CurrentFontFilter = 2 then
         DrawText('TRILINEAR', 570, 400, 20, BLACK);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadFont(Font);      // Font unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

