program shapes_lines_bezier;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
var
  Start, _End: TVector2;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - cubic-bezier lines');

  Start := Vector2Create(0, 0);
  _End := Vector2Create(ScreenWidth, ScreenHeight);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
        Start := GetMousePosition()
      else if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
        _End := GetMousePosition();
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText('USE MOUSE LEFT-RIGHT CLICK to DEFINE LINE START and END POINTS', 15, 20, 20, GRAY);
        DrawLineBezier(Start, _End, 2.0, RED);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

