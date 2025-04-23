program core_input_mouse;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  BallPosition: TVector2;
  BallColor: TColor;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - mouse input');
  BallPosition := Vector2Create(100.0, 100.0);
  BallColor := DARKBLUE;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      BallPosition := GetMousePosition();

      if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
      BallColor := MAROON
      else if IsMouseButtonPressed(MOUSE_BUTTON_MIDDLE) then
      BallColor := LIME
      else if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
      BallColor := DARKBLUE
      else if IsMouseButtonPressed(MOUSE_BUTTON_SIDE) then
      BallColor := PURPLE
      else if IsMouseButtonPressed(MOUSE_BUTTON_EXTRA) then
      BallColor := YELLOW
      else if IsMouseButtonPressed(MOUSE_BUTTON_FORWARD) then
      BallColor := ORANGE
      else if IsMouseButtonPressed(MOUSE_BUTTON_BACK) then
      BallColor := BEIGE;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawCircleV(BallPosition, 40, BallColor);

      DrawText('move ball with mouse and click mouse button to change color', 10, 10, 20, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

