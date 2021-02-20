program shapes_starfield;

{$mode objfpc}{$H+}

uses cmem, ray_header, math;

type

  { TStar }

  TStar = class
  private
    X: Single;//Integer;
    Y: Single;//Integer;
    StarLayer: Byte;
  public
    procedure Moved(Speed: Integer; Rect: TRectangle);
  end;

  { TStarField }

  TStarField = class
  private
    Stars: array of TStar;
    StarCount: Integer;
    ClientRect: TRectangle;
  public
    constructor Create(StarCnt: Integer; Rect: TRectangle);
    destructor Destroy; override;
    procedure Move(Speed: Integer);
    procedure Render;
  end;

const
 screenWidth = 800;
 screenHeight = 450;

var Star:TStarField;
    Rect:TRectangle;
    LogoRec:TRectangle;

procedure TStar.Moved(Speed: Integer; Rect: TRectangle);
begin
case StarLayer of
    1: X := X + Speed;
    2: X := X + Speed * 2;
    3: X := X + Speed * 4;
 end;
 if (X > Rect.Width) then
 begin
    X:= Rect.x;
    Y:= Rect.y + Random(Round(Rect.Height));
 end;
end;

{ TStarField }

constructor TStarField.Create(StarCnt: Integer; Rect: TRectangle);
var
  Loop: Integer;
  Layer: Byte;
begin
  ClientRect := Rect;
  StarCount := StarCnt;
  SetLength(Stars, StarCount);
  Layer := 1;
  for Loop := 0 to StarCount - 1 do begin
    Stars[Loop] := TStar.Create;
    with Stars[Loop] do begin
      StarLayer := Layer;
       X := Random(Round(ClientRect.Width));
       Y := Rect.y + (2 * ClientRect.Height);
       Inc(Layer);
       if Layer > 3 then
         Layer := 1;
     end;
   end;
end;

destructor TStarField.Destroy;
var
  Loop: Integer;
begin
  for Loop := 0 to StarCount - 1 do
    Stars[Loop].Free;
  inherited Destroy;
end;

procedure TStarField.Move(Speed: Integer);
var Loop: Integer;
begin
  for Loop := 0 to StarCount - 1 do Stars[Loop].Moved(Speed, ClientRect);
end;

procedure TStarField.Render;
var Loop: Integer;
begin
  for Loop := 0 to StarCount - 1 do
    DrawPixel(Round(Stars[Loop].X),Round(Stars[Loop].Y),ColorCreate($FF, $FF, $FF, Random($FF) or $70));
end;

begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}
 InitWindow(screenWidth, screenHeight, 'raylib pascal - basic window');
 SetTargetFPS(60);
 Rect.X := 0;
 Rect.Y := 0;
 Rect.Width := screenWidth;
 Rect.Height := screenHeight - (2 * Rect.Y);
 LogoRec:=RectangleCreate(screenWidth div 2 - 128, screenHeight div 2 - 128, 256,256);

 Star:=TStarField.Create(200,Rect);

 while not WindowShouldClose() do 
begin
  //update
  Star.Move(1);
  BeginDrawing();
  ClearBackground(BLACK);
  Star.Render;
  DrawRectangleLinesEx( LogoRec,14,RAYWHITE);

  DrawText('raylib', screenWidth div 2 - 44, screenHeight div 2 + 48, 50, RAYWHITE);
  DrawText('3.5', screenWidth div 2 + 74, screenHeight div 2 + 90, 20, RAYWHITE);
  EndDrawing();
 end;
CloseWindow(); 
end.

