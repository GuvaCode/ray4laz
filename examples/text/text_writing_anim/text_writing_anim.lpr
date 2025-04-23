program text_writing_anim;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Message: string;
  FramesCounter: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - text writing anim');

  Message := 'This sample illustrates a text writing'#10'animation effect! Check it out! ;)';

  FramesCounter := 0;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyDown(KEY_SPACE) then
        FramesCounter := FramesCounter + 8
      else
        FramesCounter := FramesCounter + 1;

      if IsKeyPressed(KEY_ENTER) then
        FramesCounter := 0;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText(TextSubtext(PChar(Message), 0, FramesCounter div 10), 210, 160, 20, MAROON);

        DrawText('PRESS [ENTER] to RESTART!', 240, 260, 20, LIGHTGRAY);
        DrawText('PRESS [SPACE] to SPEED UP!', 239, 300, 20, LIGHTGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

