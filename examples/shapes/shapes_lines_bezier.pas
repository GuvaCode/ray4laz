program shapes_lines_bezier;

{$MODE objfpc}

uses cmem, ray_header, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  start,
  _end : TVector2;
begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - cubic-bezier lines');
  start := Vector2Create(0,0);
  _end := Vector2Create(screenWidth, screenHeight);
  SetTargetFPS(60);
  while  not WindowShouldClose() do
  begin
    if IsMouseButtonDown(MOUSE_LEFT_BUTTON) then start := GetMousePosition
    else if IsMouseButtonDown(MOUSE_RIGHT_BUTTON) then _end := GetMousePosition;
    BeginDrawing;
      ClearBackground(RAYWHITE);
      DrawText('USE MOUSE LEFT-RIGHT CLICK to DEFINE LINE START and END POINTS', 15, 20, 20, GRAY);
      DrawLineBezier(start, _end, 2.0, RED);
    EndDrawing;
  end;
  CloseWindow;
end.
