program snake;

{$mode objfpc}{$H+}

uses cmem,
{uncomment if necessary}
//ray_math, 
//ray_rlgl, 
ray_header; 

const
 screenWidth = 800;
 screenHeight = 450;
 SNAKE_LENGTH = 256;
 SQUARE_SIZE = 31;

type
  Snake_ = record
      position: TVector2;
      size: TVector2;
      speed: TVector2;
      color: TColor;
 end;

Food = record
      position: TVector2 ;
      size: TVector2;
      active: boolean;
      color: TColor;
  end;

var
 framesCounter : integer = 0;
 gameOver : boolean = false;
 pause : boolean = false;
 fruit : food;
 snakes :  array of snake_;
 snakePosition: TVector2;
 //static Vector2 snakePosition[SNAKE_LENGTH] = { 0 };
allowMove: boolean = false;
offset: tVector2;
counterTail: integer = 0;


begin

 InitWindow(screenWidth, screenHeight, 'raylib pascal - basic window');
 SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
  BeginDrawing();
  ClearBackground(RAYWHITE);

  DrawText('raylib in lazarus !!!', 20, 20, 20, SKYBLUE);

  EndDrawing(); 
 end;
CloseWindow(); 

end.

