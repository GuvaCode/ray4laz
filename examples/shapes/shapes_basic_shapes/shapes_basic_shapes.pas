program shapes_basic_shapes;

{$MODE objfpc}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - basic shapes drawing');
  SetTargetFPS(60);
  while not WindowShouldClose() do
  begin
    BeginDrawing();

        ClearBackground(RAYWHITE);

        DrawText('some basic shapes available on raylib', 20, 20, 20, DARKGRAY);

        DrawCircle(Trunc(screenWidth/4), 120, 35, DARKBLUE);

        DrawRectangle(Trunc(screenWidth/4*2) - 60, 100, 120, 60, RED);
        DrawRectangleLines(Trunc(screenWidth/4*2) - 40, 320, 80, 60, ORANGE);  // NOTE: Uses QUADS internally, not lines
        DrawRectangleGradientH(Trunc(screenWidth/4*2) - 90, 170, 180, 130, MAROON, GOLD);

        DrawTriangle(Vector2Create(Trunc(screenWidth/4*3), 80),
                     Vector2Create(Trunc(screenWidth/4*3) - 60, 150),
                     Vector2Create(Trunc(screenWidth/4*3) + 60, 150), VIOLET);

        DrawPoly(Vector2Create(Trunc(screenWidth/4*3), 320), 6, 80, 0, BROWN);

        DrawCircleGradient(Trunc(screenWidth/4), 220, 60, GREEN, SKYBLUE);

        // NOTE: We draw all LINES based shapes together to optimize internal drawing,
        // this way, all LINES are rendered in a single draw pass
        DrawLine(18, 42, screenWidth - 18, 42, BLACK);
        DrawCircleLines(Trunc(screenWidth/4), 340, 80, DARKBLUE);
        DrawTriangleLines(Vector2Create(Trunc(screenWidth/4*3), 160),
                          Vector2Create(Trunc(screenWidth/4*3) - 20, 230),
                          Vector2Create(Trunc(screenWidth/4*3) + 20, 230), DARKBLUE);
    EndDrawing();
  end;
  CloseWindow();        // Close window and OpenGL context
end.
