(*******************************************************************************************
*
*   raylib [text] example - unicode ranges
*
*   Example complexity rating: [★★★★] 4/4
*
*   Example originally created with raylib 5.5, last time updated with raylib 5.6
*
*   Example contributed by Vadim Gunko (@GuvaCode) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 Vadim Gunko (@GuvaCode) and Ramon Santamaria (@raysan5)
*   Pascal version Gunko Vadim
*
********************************************************************************************)
program text_unicode_ranges;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math, raylib;

//--------------------------------------------------------------------------------------
// Module Functions Declaration
//--------------------------------------------------------------------------------------
procedure AddCodepointRange(var font: TFont; const fontPath: PChar; start, stop: Integer);
var
  rangeSize, currentRangeSize, updatedCodepointCount, i: Integer;
  updatedCodepoints: PInteger;
begin
  rangeSize := stop - start + 1;
  currentRangeSize := font.glyphCount;

  // TODO: Load glyphs from provided vector font (if available),
  // add them to existing font, regenerating font image and texture

  updatedCodepointCount := currentRangeSize + rangeSize;
  updatedCodepoints := GetMem(updatedCodepointCount * SizeOf(Integer));

  try
    // Get current codepoint list
    for i := 0 to currentRangeSize - 1 do
      updatedCodepoints[i] := font.glyphs[i].value;

    // Add new codepoints to list (provided range)
    for i := currentRangeSize to updatedCodepointCount - 1 do
      updatedCodepoints[i] := start + (i - currentRangeSize);

    UnloadFont(font);
    font := LoadFontEx(fontPath, 32, updatedCodepoints, updatedCodepointCount);
  finally
    FreeMem(updatedCodepoints);
  end;
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  font: TFont;
  unicodeRange, prevUnicodeRange: Integer;
  atlasScale: Single;
  fontRect: TRectangle;
  i: Integer;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [text] example - unicode ranges');

  // Load font with default Unicode range: Basic ASCII [32-127]
  font := LoadFont('resources/NotoSansTC-Regular.ttf');
  SetTextureFilter(font.texture, TEXTURE_FILTER_BILINEAR);

  unicodeRange := 0;           // Track the ranges of codepoints added to font
  prevUnicodeRange := 0;       // Previous Unicode range to avoid reloading every frame

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    if unicodeRange <> prevUnicodeRange then
    begin
      UnloadFont(font);

      // Load font with default Unicode range: Basic ASCII [32-127]
      font := LoadFont('resources/NotoSansTC-Regular.ttf');

      // Add required ranges to loaded font
      case unicodeRange of
        {
        5:
        begin
          // Unicode range: Devanari, Arabic, Hebrew
          // WARNING: Glyphs not available on provided font!
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $900, $97f);  // Devanagari
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $600, $6ff);  // Arabic
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $5d0, $5ea);  // Hebrew
        end;
        }
        4:
        begin
          // Unicode range: CJK (Japanese and Chinese)
          // WARNING: Loading thousands of codepoints requires lot of time!
          // A better strategy is prefilter the required codepoints for the text
          // in the game and just load the required ones
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $4e00, $9fff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $3400, $4dbf);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $3000, $303f);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $3040, $309f);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $30A0, $30ff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $31f0, $31ff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $ff00, $ffef);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $ac00, $d7af);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $1100, $11ff);
        end;
        3:
        begin
          // Unicode range: Cyrillic
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $400, $4ff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $500, $52f);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $2de0, $2Dff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $a640, $A69f);
        end;
        2:
        begin
          // Unicode range: Greek
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $370, $3ff);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $1f00, $1fff);
        end;
        1:
        begin
          // Unicode range: European Languages
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $c0, $17f);
          AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $180, $24f);
          //AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $1e00, $1eff);
          //AddCodepointRange(font, 'resources/NotoSansTC-Regular.ttf', $2c60, $2c7f);
        end;
      end;

      prevUnicodeRange := unicodeRange;
      SetTextureFilter(font.texture, TEXTURE_FILTER_BILINEAR); // Set font atlas scale filter
    end;

    if IsKeyPressed(KEY_ZERO) then unicodeRange := 0
    else if IsKeyPressed(KEY_ONE) then unicodeRange := 1
    else if IsKeyPressed(KEY_TWO) then unicodeRange := 2
    else if IsKeyPressed(KEY_THREE) then unicodeRange := 3
    else if IsKeyPressed(KEY_FOUR) then unicodeRange := 4;
    //else if IsKeyPressed(KEY_FIVE) then unicodeRange := 5;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('ADD CODEPOINTS: [1][2][3][4]', 20, 20, 20, MAROON);
      DrawText('Press Key: 1-4', 20, 40, 20, MAROON);
      // Render test strings in different languages
      DrawTextEx(font, '> English: Hello World!', Vector2Create(50, 70), 32, 1, DARKGRAY); // English
      DrawTextEx(font, '> Español: Hola mundo!', Vector2Create(50, 120), 32, 1, DARKGRAY); // Spanish
      DrawTextEx(font, '> Ελληνικά: Γειά σου κόσμε!', Vector2Create(50, 170), 32, 1, DARKGRAY); // Greek
      DrawTextEx(font, '> Русский: Привет мир!', Vector2Create(50, 220), 32, 0, DARKGRAY); // Russian
      DrawTextEx(font, '> 中文: 你好世界!', Vector2Create(50, 270), 32, 1, DARKGRAY);        // Chinese
      DrawTextEx(font, '> 日本語: こんにちは世界!', Vector2Create(50, 320), 32, 1, DARKGRAY); // Japanese
      //DrawTextEx(font, 'देवनागरी: होला मुंडो!', Vector2Create(50, 350), 32, 1, DARKGRAY);     // Devanagari (glyphs not available in font)

      // Draw font texture scaled to screen
      atlasScale := 380.0 / font.texture.width;
      fontRect.x := 400.0;
      fontRect.y := 16.0;
      fontRect.width := font.texture.width * atlasScale;
      fontRect.height := font.texture.height * atlasScale;

      DrawRectangleRec(fontRect, BLACK);
      DrawTexturePro(font.texture,
        RectangleCreate(0, 0, font.texture.width, font.texture.height),
        fontRect,
        Vector2Create(0, 0), 0.0, WHITE);
      DrawRectangleLines(400, 16, 380, 380, RED);

      DrawText(TextFormat('ATLAS SIZE: %ix%i px (x%02.2f)', font.texture.width, font.texture.height, atlasScale), 20, 380, 20, BLUE);
      DrawText(TextFormat('CODEPOINTS GLYPHS LOADED: %i', font.glyphCount), 20, 410, 20, LIME);

      // Display font attribution
      DrawText('Font: Noto Sans TC. License: SIL Open Font License 1.1', screenWidth - 300, screenHeight - 20, 10, GRAY);

      if prevUnicodeRange <> unicodeRange then
      begin
        DrawRectangle(0, 0, screenWidth, screenHeight, Fade(WHITE, 0.8));
        DrawRectangle(0, 125, screenWidth, 200, GRAY);
        DrawText('GENERATING FONT ATLAS...', 120, 210, 40, BLACK);
      end;

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadFont(font);        // Unload font resource

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.


