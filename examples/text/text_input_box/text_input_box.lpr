program text_input_box;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_INPUT_CHARS = 9;

var
  Name: string;
  TextBox: TRectangle;
  MouseOnText: Boolean;
  FramesCounter: Integer;
  Key: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - input box');

  Name := '';
  TextBox := RectangleCreate(ScreenWidth / 2.0 - 100, 180, 225, 50);

  FramesCounter := 0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //---------------------------------------------------------------------------------
      if CheckCollisionPointRec(GetMousePosition(), TextBox) then
         MouseOnText := True
       else
         MouseOnText := False;

       if MouseOnText then
       begin
         // Set the window's cursor to the I-Beam
         SetMouseCursor(MOUSE_CURSOR_IBEAM);

         // Get char pressed (unicode character) on the queue
         Key := GetCharPressed();

         // Check if more characters have been pressed on the same frame
         while Key > 0 do
         begin
           // NOTE: Only allow keys in range [32..125]
           if (Key >= 32) and (Key <= 125) and (Length(Name) < MAX_INPUT_CHARS) then
             Name := Name + Char(Key);

             Key := GetCharPressed();  // Check next character in the queue
         end;

         if IsKeyPressed(KEY_BACKSPACE) and (Length(Name) > 0) then
           Delete(Name, Length(Name), 1);
       end else
         SetMouseCursor(MOUSE_CURSOR_DEFAULT);

       if MouseOnText then
         Inc(FramesCounter)
       else
         FramesCounter := 0;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        ClearBackground(RAYWHITE);

        DrawText('PLACE MOUSE OVER INPUT BOX!', 240, 140, 20, GRAY);

        DrawRectangleRec(TextBox, LIGHTGRAY);
        if MouseOnText then
          DrawRectangleLines(Trunc(TextBox.X), Trunc(TextBox.Y), Trunc(TextBox.Width), Trunc(TextBox.Height), RED)
        else
          DrawRectangleLines(Trunc(TextBox.X), Trunc(TextBox.Y), Trunc(TextBox.Width), Trunc(TextBox.Height), DARKGRAY);

        DrawText(PAnsiChar(Name), Trunc(TextBox.X) + 5, Trunc(TextBox.Y) + 8, 40, MAROON);

        DrawText(TextFormat('INPUT CHARS: %i/%i', Length(Name), MAX_INPUT_CHARS), 315, 250, 20, DARKGRAY);

        if MouseOnText then
        begin
          if Length(Name) < MAX_INPUT_CHARS then
          begin
            // Draw blinking underscore char
            if ((FramesCounter div 20) mod 2) = 0 then
              DrawText('_', Trunc(TextBox.X) + 8 + MeasureText(PChar(Name), 40), Trunc(TextBox.Y) + 12, 40, MAROON);
          end else
            DrawText('Press BACKSPACE to delete chars...', 230, 300, 20, GRAY);
        end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

