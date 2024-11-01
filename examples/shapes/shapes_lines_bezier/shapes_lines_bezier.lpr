program shapes_lines_bezier;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;
var
  startPoint, endPoint, mouse: TVector2;
  moveStartPoint, moveEndPoint: boolean;
  PointColor: TColorB;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - cubic-bezier lines');

  startPoint := Vector2Create(30, 30);
  endPoint := Vector2Create(ScreenWidth-30, ScreenHeight-30);
  moveStartPoint := false;
  moveEndPoint := false;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      mouse := GetMousePosition();

      if (CheckCollisionPointCircle(mouse, startPoint, 10.0) and IsMouseButtonDown(MOUSE_BUTTON_LEFT)) then moveStartPoint := true
      else
      if (CheckCollisionPointCircle(mouse, endPoint, 10.0) and IsMouseButtonDown(MOUSE_BUTTON_LEFT)) then moveEndPoint := true;

      if (moveStartPoint) then
      begin
        startPoint := mouse;
        if (IsMouseButtonReleased(MOUSE_BUTTON_LEFT)) then moveStartPoint := false;
      end;

      if (moveEndPoint) then
      begin
        endPoint := mouse;
        if (IsMouseButtonReleased(MOUSE_BUTTON_LEFT)) then moveEndPoint := false;
      end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        ClearBackground(RAYWHITE);

        DrawText('MOVE START-END POINTS WITH MOUSE', 15, 20, 20, GRAY);

        // Draw line Cubic Bezier, in-out interpolation (easing), no control points
        DrawLineBezier(startPoint, endPoint, 4.0, BLUE);

        if moveStartPoint then PointColor := RED else PointColor := Blue;

        DrawCircleV(startPoint,
        ifthen(CheckCollisionPointCircle(mouse, startPoint, 10.0),14.0,8.0),PointColor);

        if moveEndPoint then PointColor := RED else PointColor := Blue;

        DrawCircleV(EndPoint,
        ifthen(CheckCollisionPointCircle(mouse, EndPoint, 10.0),14.0,8.0),PointColor);


      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

