program core_basic_window;

{$MODE objfpc}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - basic window');
  SetTargetFPS(60);
  while not WindowShouldClose() do
  begin
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('Congrats! You created your first window!', 190, 200, 20, LIGHTGRAY);

    EndDrawing();
  end;
  CloseWindow();        // Close window and OpenGL context
end.
