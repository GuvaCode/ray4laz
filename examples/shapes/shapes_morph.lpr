program shapes_morph;

{$mode objfpc}{$H+}

uses cmem,
{uncomment if necessary}
//raymath,
//rlgl,
raylib;

const
 screenWidth = 800;
 screenHeight = 450;
 x : Word = 0;
 y : Word = 0;
 IncAngle = 12;
 XMove = 7;
 YMove = 8;

var
 SineMove : array[0..255] of integer; { Sine Table for Movement }
 CosineMove : array[0..255] of integer; { CoSine Table for Movement }
 SineTable : array[0..449] of integer; { Sine Table. 449 = 359 + 180 }
 CenterX, CenterY : Integer;
 CountAngle : Word;
 CountLong : Word;
 IncLong :Word;



 procedure CalculateTables;
var
  wCount : Word;
begin
  { Precalculted Values for movement }
  for wCount := 0 to 255 do
  begin
    SineMove[wCount] := round( sin( pi*wCount/128 ) * 45 );
    CosineMove[wCount] := round( cos( pi*wCount/128 ) * 60 );
  end;
  { Precalculated Sine table. Only One table because cos(i) = sin(i + 90) }
  for wCount := 0 to 449 do
  begin
    SineTable[wCount] := round( sin( pi*wCount/180 ) * 128);
  end;
end;

procedure PlotPoint(XCenter, YCenter, Radius, Angle: Word);
var
  X, Y : Word;
begin
  X := ( Radius * SineTable[90 + Angle]);
  {$ASMMODE intel}
 asm
   sar x,7
  end;
  X := CenterX + XCenter + X;
  Y := ( Radius * SineTable[Angle] );
  asm
    sar y,7
  end;
 Y := CenterY + YCenter + Y;
  if (X < screenWidth ) and ( Y < screenHeight ) then DrawPixel(X,Y,RED);
end;


begin
  InitWindow(screenWidth, screenHeight, 'raylib pascal - basic window');
  CenterX := screenWidth div 2; // shr is the same as /2 but is alot quicker
  CenterY := screenHeight div 2;
  CalculateTables;

 SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
  IncLong := 2;
  CountLong := 20;
  BeginDrawing();



  ClearBackground(BLACK);

  { Draw Circle }
  repeat
    CountAngle := 0;
    repeat
      PlotPoint(CosineMove[( x + ( 200 - CountLong )) mod 255],
                SineMove[( y + ( 200 - CountLong )) mod 255], CountLong, CountAngle);
      inc(CountAngle, IncAngle);
    until CountAngle >= 360;
    { Another Circle, eventually another color }
    inc(CountLong, IncLong);
    if ( CountLong mod 3 ) = 0 then
    begin
      inc(IncLong);
    end;
  until CountLong >= 270;
  { move x and y co-ordinates}
  x := XMove + x mod 255;
  y := YMove + y mod 255;

  EndDrawing();
 end;
CloseWindow(); 

end.

