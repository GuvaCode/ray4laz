program motion;

{$mode objfpc}{$H+}

uses
 {$IFDEF LINUX}cthreads, {$IFEND}
 Classes, SysUtils, CustApp, raylib, raygui, math;

type
  TStarfieldMode = (smForward, smBackward);

  TStar = record
    X, Y, Z: Single;     // x,y,z
    Speed: Double;       // Speed
    Color: TColorB;   // Color
  end;

 { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FBall: TTexture2D;
    X1, X2, Y1, Y2: Single;
    FSprites: Integer;
    SelectIndex: integer;
    EditMode: boolean;
    frameCounter: integer;

    FStars: array of TStar;
    ScrollMode: TStarfieldMode;
    SpeedMultiplier: Single;
    RotationSpeed: Single;
    CenterX, CenterY: Integer;
    MaxZ, MinZ, FocusZ: Single;
    procedure Balls;
    procedure InitStars;
    procedure UpdateStars;
    procedure RotatePoint(var X, Y: Single; Angle: Single);
    function Project3Dto2D(X, Y, Z: Double; out ScX, ScY: Integer; out Size: Integer): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = 'spiral motion';


{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags
  FBall := LoadTexture('resources/raysan.png');
  FSprites := 0;
  // Init float val
  X1 := 0;   X2 := 0;    Y1 := 0;  Y2 := 0;

  SelectIndex := 0;

   ScrollMode := smForward;   // forward
   SpeedMultiplier := 2.0;    // speed
   RotationSpeed := 0;        // Rotation = 0

   CenterX := 800 div 2;
   CenterY := 600 div 2;

   Randomize;
   InitStars;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
end;

procedure TRayApplication.DoRun;
var
  i: Integer;
  ScreenX, ScreenY, StSize: Integer;
  Brightness: Integer;
  col: TColorB;
  Is_Visible: Boolean;
begin

  while (not WindowShouldClose) do // Detect window close button or ESC key
  begin
    // Update your variables here
    Inc(frameCounter);
    UpdateStars;
    // Draw
    BeginDrawing();
      ClearBackground(BLACK);


    for i := 0 to Length(FStars) - 1 do
     begin
       // 3D position to 2D
       Is_Visible := Project3Dto2D(FStars[i].X, FStars[i].Y, FStars[i].Z, ScreenX, ScreenY, StSize);

       if Is_Visible then
       begin
         Brightness := Max(40, Round(255 * (1 - (FStars[i].Z / (MaxZ/1.5)))));
         col := ColorCreate(Brightness, Brightness, Brightness, Brightness);
         if StSize <= 1 then
             DrawPixel(ScreenX,ScreenY,col)
         else
         DrawRectangle(
             ScreenX ,
             ScreenY ,
             StSize div 2 + 1,
             StSize div 2 + 1, col);
       end;
     end;


    Balls;

    GuiLabel(RectangleCreate(10,10,100,20),'Motion Type:');

    if GuiDropdownBox(RectangleCreate( 10, 10, 125, 30 ),
    'Simple;Lissajous;Spirale;Fleur;Papillon;Rose;Spirographe;Harmonique Compose;Double Pendule;Tourbillon Hypnotique;Superposition d''Harmoniques;Spirale Logarithmique;Fractale Simple;Kaléidoscope', @SelectIndex, EditMode) <> 0
    then EditMode := not EditMode;

    EndDrawing();
  end;

  // Stop program loop
  Terminate;
end;

procedure TRayApplication.Balls;
var
  x, y, px, py, i: Integer;
  radius, angle, bigRadius, distance: Single;
begin
  case SelectIndex of
      0: //mtSimple
      begin
        x := Round(320 + 280 * Cos(X1));
        y := Round(240 + 240 * Sin(Y1));
      end;

      1: //mtLissajous
      begin
        x := Round(320 + 280 * Sin(X1) * Cos(X2));
        y := Round(240 + 240 * Sin(Y1) * Cos(Y2));
      end;

      2: //mtSpiral
      begin
        x := Round(320 + X1 * Cos(X1) * 0.014  + X2 * Sin(X2));
        y := Round(240 + Y1 * Sin(Y1) * 0.010  + Y2 * Cos(Y2));
      end;

      3: //mtFlower
      begin
        x := Round(320 + 280 * Cos(X1) * Sin(X2 * 0.014));
        y := Round(240 + 240 * Sin(Y1) * Cos(Y2 * 0.010));
      end;

      4: //mtButterfly
      begin
        x := Round(320 + 280 * Sin(X1) * Cos(Y1) * Sin(X2));
        y := Round(240 + 240 * Sin(X1) * Sin(Y1) * Cos(Y2));
      end;

      5: //mtRose
      begin
        radius := 280 * Cos(0.014 * X1) * Sin(0.010 * X2);
        x := Round(320 + radius * Cos(X1));
        y := Round(240 + radius * Sin(Y1));
      end;

      6: //mtSpirograph
      begin
        bigRadius := 145 + 30 * Sin(X2);
        radius := 0.014 * 10 + 5 * Cos(Y2);
        distance := 0.010 * 10;
        x := Round(320 + (bigRadius-radius) * Cos(X1) + distance * Cos(((bigRadius-radius)/radius) * X1));
        y := Round(240 + (bigRadius-radius) * Sin(Y1) - distance * Sin(((bigRadius-radius)/radius) * Y1));
      end;

      7: //mtHarmonic
      begin
        x := Round(320 + 280 * Sin(X1) + 100 * Sin(X2 * 0.014));
        y := Round(240 + 200 * Cos(Y1) + 80 * Cos(Y2 * 0.010));
      end;

      8: //mtDoublePendule
      begin
        x := Round(320 + 150 * Sin(X1) + 130 * Sin(X1 + X2));
        y := Round(240 + 150 * Cos(Y1) + 130 * Cos(Y1 + Y2));
      end;

      9: //mtTourbillon:
      begin
        radius := 200 * (Sin(X1) * Sin(X2) + Cos(Y1) * Cos(Y2));
        angle := X1 * 3 + Y1 * 2;
        x := Round(320 + radius * Cos(angle));
        y := Round(240 + radius * Sin(angle));
      end;

      10: //mtSuperposition:
      begin
        x := Round(320 + 280 * Sin(X1) * Cos(X2 * 2) + 50 * Sin(X1 * 3 + X2 * 2));
        y := Round(240 + 240 * Sin(Y1) * Cos(Y2 * 3) + 40 * Sin(Y1 * 2 + Y2 * 5));
      end;

      11: //mtSpiraleLog:
      begin
        radius := 0.2 * Exp(0.1 * X1) * (1 + 0.3 * Sin(X2 * 10));
        angle := Y1 + 0.2 * Sin(Y2 * 2);
        x := Round(320 + radius * Cos(angle));
        y := Round(240 + radius * Sin(angle));
      end;

      12: //mtFractale:
      begin
        x := Round(320 + 300 * Sin(X1) * Sin(X1 * X2 * 0.01));
        y := Round(240 + 300 * Cos(Y1) * Cos(Y1 * Y2 * 0.01));
      end;

      13: //mtKaleidoscope:
      begin
        angle := X1 + Sin(X2);
        radius := 200 * Sin(Y1 * 3) * Cos(Y2 * 2);
        x := Round(320 + radius * Cos(angle));
        y := Round(240 + radius * Sin(angle));

      end;
 end;


  for i :=0 to 20 do
  begin
    PY :=  Trunc(Y + cos((frameCounter + i*8)/ 32.0) * (Y/4.0) ) ;
    PX :=  Trunc(X + sin((frameCounter + i*8)/ 32.0) * (X/4.0) ) ;
    DrawTexture(FBall,PX,PY,WHITE);
  end;
  X1 := X1 + 0.017;
  X2 := X2 + 0.014;
  Y1 := Y1 + 0.018;
  Y2 := Y2 + 0.010;
end;

procedure TRayApplication.InitStars;
const
  STAR_COUNT = 1000;  //
  MAX_Z_DISTANCE = 2000.0;
var
  i: Integer;
begin
  SetLength(FStars, STAR_COUNT);

  MinZ := 1.0;
  MaxZ := MAX_Z_DISTANCE;
  FocusZ := 200.0; // 200

  for i := 0 to STAR_COUNT - 1 do
  begin
    // Random position in 3D space
    FStars[i].X := Random(2000) - 1000;  //  -1000 to 1000
    FStars[i].Y := Random(2000) - 1000;  //  -1000 to 1000
    FStars[i].Z := MinZ + Random * (MaxZ - MinZ); // Range from MinZ to MaxZ
    FStars[i].Speed := 1.0 + Random(4);
    FStars[i].Color := ColorCreate(255, 255, 255, 255);
  end;

end;

procedure TRayApplication.UpdateStars;
var
  i: Integer;
  SpeedFactor: Double;
begin
  SpeedFactor := SpeedMultiplier;

  for i := 0 to Length(FStars) - 1 do
  begin

    if RotationSpeed <> 0 then
      RotatePoint(FStars[i].X, FStars[i].Y, RotationSpeed);

    case ScrollMode of
      smForward:
        begin
          FStars[i].Z := FStars[i].Z - FStars[i].Speed * SpeedFactor;
          // reset z position
          if FStars[i].Z < MinZ then
          begin
            FStars[i].X := Random(2000) - 1000;
            FStars[i].Y := Random(2000) - 1000;
            FStars[i].Z := MaxZ;
          end;
        end;

      smBackward:
        begin
          FStars[i].Z := FStars[i].Z + FStars[i].Speed * SpeedFactor;
          if FStars[i].Z > MaxZ then
          begin
            FStars[i].X := Random(2000) - 1000;
            FStars[i].Y := Random(2000) - 1000;
            FStars[i].Z := MinZ;
          end;
        end;
    end;
  end;
end;

procedure TRayApplication.RotatePoint(var X, Y: Single; Angle: Single);
var
  NewX, NewY: Single;
  Cosine, Sine: Single;
begin
  Cosine := Cos(Angle);
  Sine := Sin(Angle);

  NewX := X * Cosine - Y * Sine;
  NewY := X * Sine + Y * Cosine;

  X := NewX;
  Y := NewY;
end;

function TRayApplication.Project3Dto2D(X, Y, Z: Double; out ScX, ScY: Integer;
  out Size: Integer): Boolean;
var
  Scale: Single;
begin
  Result := False;

  if Z <= 0 then Exit;  // ! if Div  0

  Scale := FocusZ / Z;
  ScX := CenterX + Round(X * Scale);
  ScY := CenterY + Round(Y * Scale);
  Size := Max(1, Round(2 * (1 - Z / MaxZ)));
  Result := (ScX >= 0) and (ScX < 800) and
            (ScY >= 0) and (ScY < 600);

end;





destructor TRayApplication.Destroy;
begin
  // De-Initialization
  UnloadTexture(FBall);
  CloseWindow(); // Close window and OpenGL context

  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)


  inherited Destroy;
end;

var
  Application: TRayApplication;
begin
  Application:=TRayApplication.Create(nil);
  Application.Title:=AppTitle;
  Application.Run;
  Application.Free;
end.

