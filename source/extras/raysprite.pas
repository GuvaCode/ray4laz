unit RaySprite;

{$mode ObjFPC}{$H+}

interface

uses
  RayLib, RayMath, Classes, SysUtils, Math;

type
  TCollideMethod = (cmRadius, cmRect, cmQuadrangle, cmPolygon);
//  TAnimPlayMode = (pmForward, pmBackward, pmPingPong);
  TJumpState = (jsNone, jsJumping, jsFalling);

  TSprite = class;

  { TSpriteEngine }

  TSpriteEngine = class
  private
    FCamera: TCamera2D;
    FSpriteList: TList;
    FDeadList: TList;
    FWorldX, FWorldY: Single;
    function GetSprite(const Index: Integer): TSprite;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Sprite: TSprite);
    procedure Remove(const Sprite: TSprite);
    procedure Change(Sprite: TSprite; Dest: TSpriteEngine);
    procedure Move(MoveCount: Single);
    procedure Draw;
    procedure Collision;
    procedure Clear;
    procedure ClearDead;
    property Camera: TCamera2D read FCamera write FCamera;
    property Items[const Index: Integer]: TSprite read GetSprite; default;
    property Count: Integer read GetCount;
    property WorldX: Single read FWorldX write FWorldX;
    property WorldY: Single read FWorldY write FWorldY;
  end;

  TSprite = class(TObject)
  private
    FAngle: Single;
    FRed: Byte;
    FGreen: Byte;
    FBlue: Byte;
    FAlpha: Byte;
    FDoCollision: Boolean;
    FEngine: TSpriteEngine;
    FIsDead: Boolean;
    FMirrorX: Boolean;
    FMirrorY: Boolean;
    FMoved: Boolean;
    FName: string;
    FTexture: TTexture;
    FScaleX: Single;
    FScaleY: Single;
    FTag: Integer;
    FVisible: Boolean;
    FOffsetX: Single;
    FOffsetY: Single;
    FX: Single;
    FY: Single;
    FZ: Integer;

    procedure SetRed(AValue: Byte);
    procedure SetGreen(AValue: Byte);
    procedure SetBlue(AValue: Byte);
    procedure SetAlpha(AValue: Byte);

    procedure SetName(AValue: string);

    procedure SetX(AValue: Single);
    procedure SetY(AValue: Single);
    procedure SetZ(AValue: Integer);
  public
    constructor Create(const AParent: TSpriteEngine; const Name: String); virtual;
    destructor Destroy; override;
    procedure Assign(const AValue: TSprite); virtual;
    procedure Collision(const Other: TSprite); overload; virtual;
    procedure Collision; overload; virtual;
    procedure Dead; virtual;
    procedure OnCollision(const Sprite: TSprite); virtual;
    procedure Move(const {%H-}MoveCount: Single); virtual;
    procedure Draw; virtual;
    procedure SetColor(const Color: TColorB); overload;
    procedure SetColor(Red, Green, Blue: Byte; Alpha: Byte=255); overload;

    procedure AngleToTarget(TargetX, TargetY: Single);
    procedure MoveTowards(TargetX, TargetY, Distance: Single);

    property Red: Byte read FRed write SetRed default 255;
    property Green: Byte read FGreen write SetGreen default 255;
    property Blue: Byte read FBlue write SetBlue default 255;
    property Alpha: Byte read FAlpha write SetAlpha default 255;
    property Name: string read FName write SetName;
    property Moved: Boolean read FMoved write FMoved;
    property DoCollision: Boolean read FDoCollision write FDoCollision;
    property Visible: Boolean read FVisible write FVisible;
    property IsDead: Boolean read FIsDead;
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    property Z: Integer read FZ write SetZ;
    property Angle: Single read FAngle write FAngle;
    property ScaleX: Single read FScaleX write FScaleX;
    property ScaleY: Single read FScaleY write FScaleY;
    property OffsetX: Single read FOffsetX write FOffsetX;
    property OffsetY: Single read FOffsetY write FOffsetY;
    property MirrorX: Boolean read FMirrorX write FMirrorX;
    property MirrorY: Boolean read FMirrorY write FMirrorY;
    property Engine: TSpriteEngine read FEngine write FEngine;
    property Tag: Integer read FTag write FTag;
    property Texture: TTexture read FTexture write FTexture;
  end;

implementation

{ TSpriteEngine }

function TSpriteEngine.GetSprite(const Index: Integer): TSprite;
begin
  if (FSpriteList <> nil) and (Index >= 0) and (Index < FSpriteList.Count) then
  Result := TSprite(FSpriteList[Index])  //Result := FSpriteList[Index]
    else
  Result := nil;
end;

function TSpriteEngine.GetCount: Integer;
begin
  if FSpriteList <> nil then
  Result := FSpriteList.Count
    else
  Result := 0;
end;

constructor TSpriteEngine.Create;
begin
  inherited;
  FSpriteList := TList.Create;
  FDeadList := TList.Create;
  FWorldX := 0;
  FWorldY := 0;
end;

destructor TSpriteEngine.Destroy;
begin
  while FSpriteList.Count > 0 do
  begin
    TSprite(FSpriteList.Items[FSpriteList.Count - 1]).Free;
  end;
  FSpriteList.Free;
  FDeadList.Free;

  inherited Destroy;
end;

procedure TSpriteEngine.Add(const Sprite: TSprite);
var
  L, H, Dif, I: Integer;
begin
  L := 0;
  H := FSpriteList.Count - 1;
  while (L <= H) do
  begin
    I := (L + H) div 2;
    Dif := TSprite(FSpriteList.Items[I]).FZ - Sprite.FZ;
    if (Dif < 0) then L := I + 1
      else
    H := I - 1;
  end;
  FSpriteList.Insert(L, Sprite);
end;

procedure TSpriteEngine.Remove(const Sprite: TSprite);
begin
  FSpriteList.Remove(Sprite);
end;

procedure TSpriteEngine.Change(Sprite: TSprite; Dest: TSpriteEngine);
begin
  Dest.Add(Sprite);
  Sprite.Engine := Dest;
  FSpriteList.Remove(Sprite);
end;

procedure TSpriteEngine.Move(MoveCount: Single);
var
  i: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
  TSprite(FSpriteList.Items[i]).Move(MoveCount);
end;

procedure TSpriteEngine.Draw;
var
  i: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
  TSprite(FSpriteList.Items[i]).Draw;
end;

procedure TSpriteEngine.Collision;
var
  i, j: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    for j := i + 1 to FSpriteList.Count - 1 do
    begin
      if (TSprite(FSpriteList.Items[i]).DoCollision) and (TSprite(FSpriteList.Items[j]).DoCollision) then
      TSprite(FSpriteList.Items[i]).Collision(TSprite(FSpriteList.Items[j]));
    end;
  end;
end;

procedure TSpriteEngine.Clear;
begin
  while Count > 0 do
  Items[Count - 1].Free;
end;

procedure TSpriteEngine.ClearDead;
begin
  while FDeadList.Count > 0 do
  TSprite(FDeadList.Items[FDeadList.Count - 1]).Free;
end;

{ TSprite }

procedure TSprite.SetX(AValue: Single);
begin
  if FX=AValue then Exit;
  FX:=AValue;
end;

procedure TSprite.SetAlpha(AValue: Byte);
begin
  if FAlpha=AValue then Exit;
  FAlpha:=AValue;
end;

procedure TSprite.SetBlue(AValue: Byte);
begin
  if FBlue=AValue then Exit;
  FBlue:=AValue;
end;

procedure TSprite.SetGreen(AValue: Byte);
begin
  if FGreen=AValue then Exit;
  FGreen:=AValue;
end;

procedure TSprite.SetName(AValue: string);
begin
  Self.FName := AValue;
end;

procedure TSprite.SetRed(AValue: Byte);
begin
  if FRed=AValue then Exit;
  FRed:=AValue;
end;

procedure TSprite.SetY(AValue: Single);
begin
  if FY=AValue then Exit;
  FY:=AValue;
end;

procedure TSprite.SetZ(AValue: Integer);
begin
  if FZ <> AValue then
  begin
    FZ := AValue;
    FEngine.FSpriteList.Remove(Self);
    FEngine.Add(Self);
  end;
end;

constructor TSprite.Create(const AParent: TSpriteEngine; const Name: String);
begin
  inherited Create;
  FEngine := AParent;
  FX := 200;
  FY := 200;
  FZ := 0;
  FName := Name;
  FZ := 0;
  FRed := 255;
  FGreen := 255;
  FBlue := 255;
  FAlpha := 255;
  FAngle := 0;
  FScaleX := 1;
  FScaleY := 1;
  FOffsetX := 0;
  FOffsetY := 0;
  FMirrorX := False;
  FMirrorY := False;
  FDoCollision := False;
  FIsDead := False;
  FMoved := True;
  FVisible := True;
  FTag := 0;
  Engine.Add(Self);
end;

destructor TSprite.Destroy;
begin
  UnloadTexture(FTexture);
  Engine.Remove(Self);
  Engine.FDeadList.Remove(Self);
  inherited Destroy;
end;

procedure TSprite.Assign(const AValue: TSprite);
begin

end;

procedure TSprite.Collision(const Other: TSprite);
begin

end;

procedure TSprite.Collision;
begin

end;

procedure TSprite.Dead;
begin

end;

procedure TSprite.OnCollision(const Sprite: TSprite);
begin

end;

procedure TSprite.Move(const MoveCount: Single);
begin
  if not FMoved then Exit;
end;

procedure TSprite.Draw;
var
  Source: TRectangle;
  Dest: TRectangle;

begin
  if (Texture.width <=0) and (Texture.height <= 0) then
  begin
    TraceLog(LOG_ERROR, PChar('Sprite:' + FName + 'not texture'));
    Exit;
  end;

  if Assigned(FEngine) then
  begin
    if (not FMirrorX) and (not FMirrorY) then RectangleSet(@Source, 0, 0, FTexture.width, FTexture.height);
    if (FMirrorX) and (not FMirrorY) then RectangleSet(@Source, 0, 0, -FTexture.width, FTexture.height);
    if (not FMirrorX) and (FMirrorY) then RectangleSet(@Source, 0, 0, FTexture.width, -FTexture.height);
    if (FMirrorX) and (FMirrorY) then RectangleSet(@Source, 0, 0, -FTexture.width, -FTexture.height);

    RectangleSet(@Dest, FX, FY, FTexture.width * FScaleX, FTexture.height * FScaleY);

    DrawTexturePro(FTexture, Source, Dest, Vector2Create(FOffsetX * ScaleX, FOffsetY * ScaleY),
    FAngle, ColorCreate(Fred,FGreen,FBlue,FAlpha));
  end;
end;

procedure TSprite.SetColor(const Color: TColorB);
begin
 FRed := Color.r;
 FGreen := Color.g;
 FBlue := Color.b;
 FAlpha := Color.a;
end;

procedure TSprite.SetColor(Red, Green, Blue: Byte; Alpha: Byte);
begin
  FRed := Red;
  FGreen := Green;
  FBlue := Blue;
  FAlpha := Alpha;
end;

procedure TSprite.AngleToTarget(TargetX, TargetY: Single);
begin
  FAngle := Vector2Angle(Vector2Create(FX,FY),Vector2Create(TargetX, TargetY));
end;

procedure TSprite.MoveTowards(TargetX, TargetY, Distance: Single);
var Towards:TVector2;
begin
  Towards:=Vector2MoveTowards(Vector2Create(FX,FY), Vector2Create(TargetX, TargetY), Distance);
  Fx := Towards.x;
  Fy := Towards.y;
end;




end.

