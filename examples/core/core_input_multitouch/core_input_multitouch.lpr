program core_input_multitouch;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_TOUCH_POINTS = 10;

var
  touchPositions : array [0..MAX_TOUCH_POINTS] of TVector2;
  i : integer;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - input multitouch');
  //  Vector2 touchPositions[MAX_TOUCH_POINTS] = { 0 };
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Get multiple touchpoints
      for i:=0 to MAX_TOUCH_POINTS -1 do touchPositions[i] := GetTouchPosition(i);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        for i := 0 to MAX_TOUCH_POINTS -1 do
        begin
        // Make sure point is not (0, 0) as this means there is no touch for it
        if ((touchPositions[i].x > 0) and (touchPositions[i].y > 0)) then
          begin
            // Draw circle and touch index number
            DrawCircleV(touchPositions[i], 34, ORANGE);
            DrawText(TextFormat('%d', i), Trunc(touchPositions[i].x - 10), Trunc(touchPositions[i].y - 70), 40, BLACK);
          end;
        end;

        DrawText('touch the screen at multiple locations to get multiple balls', 10, 10, 20, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

