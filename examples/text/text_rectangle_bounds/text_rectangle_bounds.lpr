program text_rectangle_bounds;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  Text: string;
  Resizing, WordWrap: Boolean;
  Container, Resizer: TRectangle;
  MinWidth, MinHeight, MaxWidth, MaxHeight: Single;
  Mouse, LastMouse: TVector2;
  BorderColor: TColor;
  Font: TFont;
  Width, Height: Single;

  // Draw text using font inside rectangle limits with support for text selection
  procedure DrawTextBoxedSelectable(Font: TFont; Text: PChar; Rec: TRectangle; FontSize, Spacing: Single;
    WordWrap: Boolean; Tint: TColor; SelectStart, SelectLength: Integer; SelectTint, SelectBackTint: TColor);
  var
    Length: Integer;
    TextOffsetY, TextOffsetX: Single;
    ScaleFactor: Single;
    State: (MEASURE_STATE, DRAW_STATE);
    StartLine, EndLine, Lastk: Integer;
    CodepointByteCount, Codepoint, Index: Integer;
    GlyphWidth: Single;
    IsGlyphSelected: Boolean;
    I, K, Tmp: Integer;
  begin
    Length := TextLength(Text); // Total length in bytes of the text, scanned by codepoints in loop

    TextOffsetY := 0.0; // Offset between lines (on line break '\n')
    TextOffsetX := 0.0; // Offset X to next character to draw

    ScaleFactor := FontSize / Font.BaseSize;  // Character rectangle scaling factor

    // Word/character wrapping mechanism variables
    if WordWrap then
      State := MEASURE_STATE
    else
      State := DRAW_STATE;

    StartLine := -1; // Index where to begin drawing (where a line begins)
    EndLine := -1; // Index where to stop drawing (where a line ends)
    Lastk := -1; // Holds last value of the character position

    I := 0;
    K := 0;
    while I < Length do
    begin
      // Get next codepoint from byte string and glyph index in font
      CodepointByteCount := 0;
      Codepoint := GetCodepoint(@Text[I], @CodepointByteCount);
      Index := GetGlyphIndex(Font, Codepoint);

      // NOTE: Normally we exit the decoding sequence as soon as a bad byte is found (and return 0x3f)
      // but we need to draw all of the bad bytes using the '?' symbol moving one byte
      if Codepoint = $3f then
        CodepointByteCount := 1;
      I := I + (CodepointByteCount - 1);

      GlyphWidth := 0;
      if Codepoint <> 10 then
      begin
        if Font.Glyphs[Index].AdvanceX = 0 then
          GlyphWidth := Font.Recs[Index].Width * scaleFactor
        else
          GlyphWidth := Font.Glyphs[Index].AdvanceX * ScaleFactor;

        if I + 1 < Length then
          GlyphWidth := GlyphWidth + Spacing;
      end;

      // NOTE: When wordWrap is ON we first measure how much of the text we can draw before going outside of the rec container
      // We store this info in startLine and endLine, then we change states, draw the text between those two variables
      // and change states again and again recursively until the end of the text (or until we get outside of the container).
      // When wordWrap is OFF we don't need the measure state so we go to the drawing state immediately
      // and begin drawing on the next line before we can get outside the container.
      if State = MEASURE_STATE then
      begin
        // TODO: There are multiple types of spaces in UNICODE, maybe it's a good idea to add support for more
        // Ref: http://jkorpela.fi/chars/spaces.html
        if (Codepoint = 32) or (Codepoint = 9) or (Codepoint = 10) then
          EndLine := I;

        if TextOffsetX + GlyphWidth > Rec.Width then
        begin
          if EndLine < 1 then
            EndLine := I;
          if I = EndLine then
            EndLine := EndLine - CodepointByteCount;
          if (StartLine + CodepointByteCount) = EndLine then
            EndLine := I - CodepointByteCount;

          if State = MEASURE_STATE then
            State := DRAW_STATE
          else
            State := MEASURE_STATE;
        end
        else if I + 1 = Length then
        begin
          EndLine := I;
          if State = MEASURE_STATE then
            State := DRAW_STATE
          else
            State := MEASURE_STATE;
        end
        else if Codepoint = 10 then
          if State = MEASURE_STATE then
            State := DRAW_STATE
          else
            State := MEASURE_STATE;

        if State = DRAW_STATE then
        begin
          TextOffsetX := 0;
          I := StartLine;
          GlyphWidth := 0;

          // Save character position when we switch states
          Tmp := Lastk;
          Lastk := K - 1;
          K := Tmp;
        end;
      end else
      begin
        if Codepoint = 10 then
        begin
          if not WordWrap then
          begin
            TextOffsetY := TextOffsetY + (Font.BaseSize + Font.BaseSize / 2) * ScaleFactor;
            TextOffsetX := 0;
          end;
        end else
        begin
          if not WordWrap and ((TextOffsetX + GlyphWidth) > Rec.Width) then
          begin
            TextOffsetY := TextOffsetY + (Font.BaseSize + Font.BaseSize / 2) * ScaleFactor;
            TextOffsetX := 0;
          end;

          // When text overflows rectangle height limit, just stop drawing
          if TextOffsetY + Font.BaseSize * ScaleFactor > Rec.Height then
            break;

          // Draw selection background
          IsGlyphSelected := False;
          if (SelectStart >= 0) and (K >= SelectStart) and (K < SelectStart + SelectLength) then
          begin
            DrawRectangleRec(RectangleCreate(Rec.X + TextOffsetX - 1, Rec.Y + TextOffsetY, GlyphWidth, Font.BaseSize * ScaleFactor), SelectBackTint);
            IsGlyphSelected := True;
          end;

          // Draw current character glyph
          if (Codepoint <> 32) and (Codepoint <> 9) then
          begin
            if IsGlyphSelected then
              DrawTextCodepoint(Font, Codepoint, Vector2Create(Rec.X + TextOffsetX, Rec.Y + TextOffsetY), FontSize, SelectTint)
            else
              DrawTextCodepoint(Font, Codepoint, Vector2Create(Rec.X + TextOffsetX, Rec.Y + TextOffsetY), FontSize, Tint);
          end;
        end;

        if WordWrap and (I = EndLine) then
        begin
          TextOffsetY := TextOffsetY + (Font.BaseSize + Font.BaseSize / 2) * ScaleFactor;
          textOffsetX := 0;
          StartLine := EndLine;
          EndLine := -1;
          GlyphWidth := 0;
          SelectStart := SelectStart + Lastk - K;
          K := Lastk;

          if State = MEASURE_STATE then
            State := DRAW_STATE
          else
            State := MEASURE_STATE;
        end;
      end;

      if (TextOffsetX <> 0) or (Codepoint <> 32) then
        TextOffsetX := TextOffsetX + GlyphWidth;  // avoid leading spaces

      Inc(I);
      Inc(K);
    end;
  end;

// Draw text using font inside rectangle limits
procedure DrawTextBoxed(Font: TFont; Text: PChar; Rec: TRectangle; FontSize, Spacing: Single;
  WordWrap: Boolean; Tint: TColor);
begin
  DrawTextBoxedSelectable(Font, Text, Rec, FontSize, Spacing, WordWrap, Tint, 0, 0, WHITE, WHITE);
end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - draw text inside a rectangle');

  Text := 'Text cannot escape'#9'this container'#9'...word wrap also works when active so here''s ' +
    'a long text for testing.'#10#10'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ' +
    'tempor incididunt ut labore et dolore magna aliqua. Nec ullamcorper sit amet risus nullam eget felis eget.';


  Resizing := False;
  WordWrap := True;

  Container := RectangleCreate(25.0, 25.0, ScreenWidth - 50.0, ScreenHeight - 250.0);
  Resizer := RectangleCreate(Container.X + Container.Width - 17, Container.Y + Container.Height - 17, 14, 14);

  // Minimum width and heigh for the container rectangle
  MinWidth := 60;
  MinHeight := 60;
  MaxWidth := ScreenWidth - 50.0;
  MaxHeight := ScreenHeight - 160.0;

  LastMouse := Vector2Create(0.0, 0.0); // Stores last mouse coordinates
  BorderColor := MAROON;         // Container border color
  Font := GetFontDefault();       // Get default system font

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_SPACE) then
        WordWrap := not wordWrap;

      Mouse := GetMousePosition();

      // Check if the mouse is inside the container and toggle border color
      if CheckCollisionPointRec(Mouse, Container) then
        BorderColor := Fade(MAROON, 0.4)
      else if not Resizing then
        BorderColor := MAROON;

      // Container resizing logic
      if Resizing then
      begin
        if IsMouseButtonReleased(MOUSE_BUTTON_LEFT) then
          Resizing := false;

        Width := Container.Width + (Mouse.X - LastMouse.X);

        if Width > MinWidth then
          if Width < MaxWidth then
            Container.Width := Width
          else
            Container.Width := MaxWidth
        else
          Container.Width := MinWidth;

        Height := Container.Height + (Mouse.Y - LastMouse.Y);

        if Height > MinHeight then
          if Height < MaxHeight then
            Container.Height := Height
          else
            Container.Height := MaxHeight
        else
          Container.Height := MinHeight;
      end else
      begin
        // Check if we're resizing
        if IsMouseButtonDown(MOUSE_BUTTON_LEFT) and CheckCollisionPointRec(Mouse, Resizer) then
          Resizing := True;
      end;

      // Move resizer rectangle properly
      Resizer.X := Container.X + Container.Width - 17;
      Resizer.Y := Container.Y + Container.Height - 17;

      LastMouse := Mouse; // Update mouse
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawRectangleLinesEx(Container, 3, BorderColor); // Draw container border

      // Draw text in container (add some padding)
      DrawTextBoxed(Font, PChar(Text), RectangleCreate(Container.X + 4, Container.Y + 4, Container.Width - 4, Container.Height - 4), 20.0, 2.0, WordWrap, GRAY);

      DrawRectangleRec(Resizer, BorderColor); // Draw the resize box

      // Draw bottom info
      DrawRectangle(0, ScreenHeight - 54, ScreenWidth, 54, GRAY);
      DrawRectangleRec(RectangleCreate(382.0, ScreenHeight - 34.0, 12.0, 12.0), MAROON);

      DrawText('Word Wrap: ', 313, ScreenHeight - 115, 20, BLACK);
      if WordWrap then
        DrawText('ON', 447, ScreenHeight - 115, 20, RED)
      else
        DrawText('OFF', 447, ScreenHeight - 115, 20, BLACK);

      DrawText('Press [SPACE] to toggle word wrap', 218, ScreenHeight - 86, 20, GRAY);

      DrawText('Click hold & drag the    to resize the container', 155, ScreenHeight - 38, 20, RAYWHITE);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

