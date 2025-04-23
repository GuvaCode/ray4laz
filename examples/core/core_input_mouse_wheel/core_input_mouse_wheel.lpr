program core_input_mouse_wheel;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  BoxPositionY: Integer;
  ScrollSpeed: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - input mouse wheel');

  BoxPositionY := ScreenHeight div 2 - 40;
  ScrollSpeed := 4; // Scrolling speed in pixels

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      BoxPositionY := BoxPositionY - Trunc(GetMouseWheelMove() * ScrollSpeed);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawRectangle(ScreenWidth div 2 - 40, BoxPositionY, 80, 80, MAROON);

        DrawText('Use mouse wheel to move the cube up and down!', 10, 10, 20, GRAY);
        DrawText(TextFormat('Box position Y: %03i', BoxPositionY), 10, 40, 20, LIGHTGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

