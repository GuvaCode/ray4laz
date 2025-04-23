program text_font_loading;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Msg: string;
  FontBm, FontTtf: TFont;
  UseTtf: Boolean;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - font loading');

  // Define characters to draw
  // NOTE: raylib supports UTF-8 encoding, following list is actually codified as UTF8 internally
  Msg := '!\"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHI'#10'JKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn'#10'opqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ'#10'ÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷'#10'øùúûüýþÿ';

  // NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)
  // BMFont (AngelCode) : Font data and image atlas have been generated using external program
  FontBm := LoadFont(PChar(GetApplicationDirectory + 'resources/pixantiqua.fnt'));

  // TTF font : Font data and atlas are generated directly from TTF
  // NOTE: We define a font base size of 32 pixels tall and up-to 250 characters
  FontTtf := LoadFontEx(PChar(GetApplicationDirectory + 'resources/pixantiqua.ttf'), 32, nil, 250);

  // Set line spacing for multiline text (when line breaks are included '\n')
  SetTextLineSpacing(48);

  useTtf := false;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyDown(KEY_SPACE) then
        UseTtf := True
      else
        UseTtf := False;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawText('Hold SPACE to use TTF generated font', 20, 20, 20, LIGHTGRAY);

      if not UseTtf then
      begin
        DrawTextEx(FontBm, PChar(Msg), Vector2Create(20.0, 100.0), FontBm.BaseSize, 2, MAROON);
        DrawText('Using BMFont (Angelcode) imported', 20, GetScreenHeight() - 30, 20, GRAY);
      end else
      begin
        DrawTextEx(fontTtf, PChar(Msg),Vector2Create(20.0, 100.0), FontBm.BaseSize, 2, LIME);
        DrawText('Using TTF font generated', 20, GetScreenHeight() - 30, 20, GRAY);
      end;
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadFont(FontBm);     // AngelCode Font unloading
  UnloadFont(FontTtf);    // TTF Font unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

