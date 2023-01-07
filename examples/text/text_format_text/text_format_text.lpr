program text_format_text;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Score, Hiscore, Lives: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - text formatting');

  Score := 100020;
  Hiscore := 200450;
  Lives := 5;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
       ClearBackground(RAYWHITE);
       DrawText(TextFormat('Score: %08i', Score), 200, 80, 20, RED);
       DrawText(TextFormat('HiScore: %08i', Hiscore), 200, 120, 20, GREEN);
       DrawText(TextFormat('Lives: %02i', Lives), 200, 160, 40, BLUE);
       DrawText(TextFormat('Elapsed Time: %02.02f ms', Single(GetFrameTime() * 1000)), 200, 220, 20, BLACK);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

