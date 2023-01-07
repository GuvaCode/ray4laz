program core_input_keys;

{$MODE objfpc}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  ballPosition : TVector2;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - input keys');

  ballPosition := Vector2Create(screenWidth / 2, screenHeight / 2);

  SetTargetFPS(60);
  while not WindowShouldClose() do
  begin
    // Update
    // -----------------------------------------------------------------------
    if (IsKeyDown(KEY_RIGHT)) then ballPosition.x += 2;
    if (IsKeyDown(KEY_LEFT)) then ballPosition.x -= 2;
    if (IsKeyDown(KEY_UP)) then ballPosition.y -= 2;
    if (IsKeyDown(KEY_DOWN)) then ballPosition.y += 2;

    // Draw
    // -----------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('move the ball with arrow keys', 10, 10, 20, DARKGRAY);

      DrawCircleV(ballPosition, 50, MAROON);

    EndDrawing();
  end;
  CloseWindow();
end.
