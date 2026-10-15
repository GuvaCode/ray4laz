program shapes_basic_shapes;

{$MODE objfpc}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  rotation: single;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - basic shapes');
  SetTargetFPS(60);

  rotation := 0.0;

  while not WindowShouldClose() do
  begin
    rotation := rotation + 0.2;

    BeginDrawing();

        ClearBackground(RAYWHITE);

        DrawText('some basic shapes available on raylib', 20, 20, 20, DARKGRAY);

        DrawCircle(screenWidth div 5, 120, 35, DARKBLUE);
        DrawCircleGradient(Vector2Create(screenWidth / 5.0, 220.0), 60, GREEN, SKYBLUE);

        DrawCircleLines(screenWidth div 5, 340, 80, DARKBLUE);
        DrawEllipse(screenWidth div 5, 120, 25, 20, YELLOW);
        DrawEllipseLines(screenWidth div 5, 120, 30, 25, YELLOW);

        DrawRectangle(screenWidth div 4 * 2 - 60, 100, 120, 60, RED);
        DrawRectangleGradientH(screenWidth div 4 * 2 - 90, 170, 180, 130, MAROON, GOLD);
        DrawRectangleLines(screenWidth div 4 * 2 - 40, 320, 80, 60, ORANGE);

        DrawTriangle(Vector2Create(screenWidth / 4.0 * 3.0, 80.0),
                     Vector2Create(screenWidth / 4.0 * 3.0 - 60.0, 150.0),
                     Vector2Create(screenWidth / 4.0 * 3.0 + 60.0, 150.0), VIOLET);

        DrawTriangleLines(Vector2Create(screenWidth / 4.0 * 3.0, 160.0),
                          Vector2Create(screenWidth / 4.0 * 3.0 - 20.0, 230.0),
                          Vector2Create(screenWidth / 4.0 * 3.0 + 20.0, 230.0), DARKBLUE);

        DrawPoly(Vector2Create(screenWidth / 4.0 * 3.0, 330.0), 6, 80, rotation, BROWN);
        DrawPolyLines(Vector2Create(screenWidth / 4.0 * 3.0, 330.0), 6, 90, rotation, BROWN);
        DrawPolyLinesEx(Vector2Create(screenWidth / 4.0 * 3.0, 330.0), 6, 85, rotation, 6, BEIGE);

        DrawLine(18, 42, screenWidth - 18, 42, BLACK);

    EndDrawing();
  end;
  CloseWindow();
end.
