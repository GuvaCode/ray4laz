program shapes_raylogo_laz;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

const
 screenWidth = 800;
 screenHeight = 450;

begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - raylib logo using shapes');
 SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
  BeginDrawing();
  ClearBackground(RAYWHITE);
  DrawRectangle(screenWidth div 2 - 128, screenHeight div 2 - 128, 256, 256, DARKBLUE);
            DrawRectangle(screenWidth div 2 - 112, screenHeight div 2 - 112, 224, 224, RAYWHITE);
            DrawText('raylib', screenWidth div 2 - 38, screenHeight div 2+11 , 50, DARKBLUE);
            DrawText('Lazarus', screenWidth div 2 - 102, screenHeight div 2+ 50 , 50, DARKBLUE);
            DrawText('this is NOT a texture!', 350, 370, 10, GRAY);
  DrawText('raylib in lazarus !!!', 20, 20, 20, SKYBLUE);

  EndDrawing(); 
 end;
CloseWindow(); 

end.

