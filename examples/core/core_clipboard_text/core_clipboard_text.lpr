program core_clipboard_text;

{$mode objfpc}{$H+}
{$DEFINE RAYGUI_NO_RICONS}
{$UNDEF RAYGUI_NO_RICONS}
uses
  raylib, raygui;

const
  MAX_TEXT_SAMPLES = 5;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  sampleTexts: array[0..MAX_TEXT_SAMPLES-1] of string;
  clipboardText: PChar;
  inputBuffer: array[0..255] of Char;
  textBoxEditMode: Boolean;
  btnCutPressed, btnCopyPressed, btnPastePressed, btnClearPressed, btnRandomPressed: Boolean;
  i: Integer;
  tempStr: string;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - clipboard text');

  // Define some sample texts
  sampleTexts[0] := 'Hello from raylib!';
  sampleTexts[1] := 'The quick brown fox jumps over the lazy dog';
  sampleTexts[2] := 'Clipboard operations are useful!';
  sampleTexts[3] := 'raylib is a simple and easy-to-use library';
  sampleTexts[4] := 'Copy and paste me!';

  clipboardText := nil;
  // Initialize input buffer with random initial string
  FillChar(inputBuffer, SizeOf(inputBuffer), 0);
  tempStr := 'Hello from raylib!';
  Move(tempStr[1], inputBuffer[0], Length(tempStr));

  // UI required variables
  textBoxEditMode := false;

  btnCutPressed := false;
  btnCopyPressed := false;
  btnPastePressed := false;
  btnClearPressed := false;
  btnRandomPressed := false;

  // Set UI style
  GuiSetStyle(DEFAULT, TEXT_SIZE, 20);

  //GuiSetIconScale(2);

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    // Handle button interactions
    if btnCutPressed then
    begin
      SetClipboardText(inputBuffer);
      clipboardText := GetClipboardText();
      FillChar(inputBuffer, SizeOf(inputBuffer), 0); // Clear full buffer properly
    end;

    if btnCopyPressed then
    begin
      SetClipboardText(inputBuffer); // Copy text to clipboard
      clipboardText := GetClipboardText(); // Get text from clipboard
    end;

    if btnPastePressed then
    begin
      // Paste text from clipboard
      clipboardText := GetClipboardText();
      if clipboardText <> nil then
      begin
        FillChar(inputBuffer, SizeOf(inputBuffer), 0);
        Move(clipboardText^, inputBuffer[0], StrLen(clipboardText));
      end;
    end;

    if btnClearPressed then
    begin
      FillChar(inputBuffer, SizeOf(inputBuffer), 0); // Clear full buffer properly
    end;

    if btnRandomPressed then
    begin
      // Get random text from sample list
      tempStr := sampleTexts[GetRandomValue(0, MAX_TEXT_SAMPLES - 1)];
      FillChar(inputBuffer, SizeOf(inputBuffer), 0);
      Move(tempStr[1], inputBuffer[0], Length(tempStr));
    end;

    // Quick cut/copy/paste with keyboard shortcuts
    if IsKeyDown(KEY_LEFT_CONTROL) or IsKeyDown(KEY_RIGHT_CONTROL) then
    begin
      if IsKeyPressed(KEY_X) then
      begin
        SetClipboardText(inputBuffer);
        FillChar(inputBuffer, SizeOf(inputBuffer), 0);
      end;

      if IsKeyPressed(KEY_C) then
        SetClipboardText(inputBuffer);

      if IsKeyPressed(KEY_V) then
      begin
        clipboardText := GetClipboardText();
        if clipboardText <> nil then
        begin
          FillChar(inputBuffer, SizeOf(inputBuffer), 0);
          Move(clipboardText^, inputBuffer[0], StrLen(clipboardText));
        end;
      end;
    end;

    // Reset button states
    btnCutPressed := false;
    btnCopyPressed := false;
    btnPastePressed := false;
    btnClearPressed := false;
    btnRandomPressed := false;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      // Draw instructions
      GuiLabel(RectangleCreate(50, 20, 700, 36), 'Use the BUTTONS or KEY SHORTCUTS:');
      DrawText('[CTRL+X] - CUT | [CTRL+C] COPY | [CTRL+V] | PASTE', 50, 60, 20, MAROON);

      // Draw text box
      if GuiTextBox(RectangleCreate(50, 120, 652, 40), @inputBuffer[0], 256, textBoxEditMode) <> 0 then
        textBoxEditMode := not textBoxEditMode;

      // Random text button
      if GuiButton(RectangleCreate(50 + 652 + 8, 120, 40, 40), '#77#') <> 0 then
        btnRandomPressed := true;

      // Draw buttons
      if GuiButton(RectangleCreate(50, 180, 158, 40), '#17#CUT') <> 0 then
        btnCutPressed := true;

      if GuiButton(RectangleCreate(50 + 165, 180, 158, 40), '#16#COPY') <> 0 then
        btnCopyPressed := true;

      if GuiButton(RectangleCreate(50 + 165*2, 180, 158, 40), '#18#PASTE') <> 0 then
        btnPastePressed := true;

      if GuiButton(RectangleCreate(50 + 165*3, 180, 158, 40), '#143#CLEAR') <> 0 then
        btnClearPressed := true;

      // Draw clipboard status
      GuiSetState(STATE_DISABLED);
      GuiLabel(RectangleCreate(50, 260, 700, 40), 'Clipboard current text data:');
      GuiSetStyle(TEXTBOX, TEXT_READONLY, 1);
      GuiTextBox(RectangleCreate(50, 300, 700, 40), clipboardText, 256, false);
      GuiSetStyle(TEXTBOX, TEXT_READONLY, 0);
      GuiLabel(RectangleCreate(50, 360, 700, 40), 'Try copying text from other applications and pasting here!');
      GuiSetState(STATE_NORMAL);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
