unit ray_sprite;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
 ray_headers, Classes;

type

{ TRay2DEngine }

TRay2DEngine = class
 private
   FList: TList;
   FDeadList: TList;
   FVisibleHeight: Integer;
   FVisibleWidth: Integer;
   FWorld: TVector3;
   procedure SetWorldX(Value: Single);
   procedure SetWorldY(Value: Single);
public
   procedure Draw();
   procedure ClearDeadSprites;
   procedure Move(TimeGap: Double);
   procedure SetZOrder();
   constructor Create;
   destructor Destroy; override;
   property VisibleWidth: Integer read FVisibleWidth write FVisibleWidth;
   property VisibleHeight: Integer read FVisibleHeight write FVisibleHeight;
   property WorldX: Single read FWorld.X write SetWorldX;
   property WorldY: Single read FWorld.Y write SetWorldY;
 end;

   TPattern = record
    Height, Width: Integer;
  end;

   { TRayTexture }
   TRayTexture = class
  public
    Count: Integer;
    TextureName: array of string;
    Texture: array of TTexture2D;
    Pattern: array of TPattern;
    function LoadFromFile(FileName: String; Width, Height: Integer): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

   { TRaySprite }

   TRaySprite = class
  private
    FAnimated: Boolean;
    FVector: TVector3;
    FZ: Single;
    FScale: Single;
  protected
    FEngine: TRay2DEngine;
    FTextureName: string;
    FTextureIndex: Integer;
    procedure SetTextureName(Value: string);
    procedure SetTextureIndex(Value: Integer);
  public
    FTexture: TRayTexture;
    Alpha: Byte;
    Angle: Single;
    IsSpriteDead: Boolean;
    Visible: Boolean;
    Pattern: TRectangle;
    procedure Draw();
    procedure DoMove(TimeGap: Double); virtual;
    procedure Dead();
    procedure SetOrder(Value: Single);
    procedure SetScale(Value: Single);

    constructor Create(Engine: TRay2DEngine; Texture: TRayTexture); virtual;
    constructor CreateEx(Texture: TRayTexture); virtual;
    destructor Destroy; override;

    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureName: string read FTextureName write SetTextureName;
    property X: Single read FVector.X write FVector.X;
    property Y: Single read FVector.Y write FVector.Y;
    property Z: Single read FZ write SetOrder;
    property Scale: Single read FScale write SetScale;
  end;


  { TRayAnimatedSprite }
  TRayAnimatedSprite = class(TRaySprite)
  protected
    FDoAnimated: Boolean;
   // FSplited: array of TRect;
    FPatternIndex: Integer;
    FPatternHeight: Integer;
    FPatternWidth: Integer;
    procedure SetPatternHeight(Value: Integer);
    procedure SetPatternWidth(Value: Integer);
  public
    AnimLooped: Boolean;
    AnimStart: Integer;
    AnimCount: Integer;
    AnimSpeed: Single;
    AnimPos: Single;

    procedure Draw();
    procedure DoMove(TimeGap: Double); override;
    procedure DoAnim(Looped: Boolean; Start: Integer; Count: Integer; Speed: Single);

    constructor Create(Engine: TRay2DEngine; Texture: TRayTexture); override;
    destructor Destroy; override;

    property PatternHeight: Integer read FPatternHeight write SetPatternHeight;
    property PatternWidth: Integer read FPatternWidth write SetPatternWidth;
  end;


implementation

{ TRayAnimatedSprite }

procedure TRayAnimatedSprite.SetPatternHeight(Value: Integer);
begin
  FPatternHeight := Value;
  Pattern.Height := Value;
end;

procedure TRayAnimatedSprite.SetPatternWidth(Value: Integer);
begin
  FPatternWidth := Value;
  Pattern.Width := Value;
end;

function SetPattern(ATexture: TTexture2D; PatternIndex, PatternWidth, PatternHeight: Integer): TRectangle;
var  FTexWidth,FTexHeight:integer;
  ColCount, RowCount, FPatternIndex:integer;
  Left,Right, Top, Bottom:integer;
  FWidth,FHeight:integer;
  X1,Y1,X2,Y2:integer;
begin

   FTexWidth := ATexture.Width;
   FTexHeight := ATexture.Height;

   ColCount := FTexWidth div PatternWidth;
   RowCount := FTexHeight div PatternHeight;

   FPatternIndex := PatternIndex;

  if FPatternIndex < 0 then
    FPatternIndex := 0;

  if FPatternIndex >= RowCount * ColCount then
    FPatternIndex := RowCount * ColCount - 1;

   Left := (FPatternIndex mod ColCount) * PatternWidth;
   Right := Left + PatternWidth;
   Top := (FPatternIndex div ColCount) * PatternHeight;
   Bottom := Top + PatternHeight;

   FWidth := Right - Left;
   FHeight := Bottom - Top;
   X1 := Left;
   Y1 := Top;
   X2 := (Left + FWidth);
   Y2 := (Top + FHeight);
  Result :=RectangleCreate(Round(X1), Round(Y1), Round(X2), Round(Y2));
end;

procedure TRayAnimatedSprite.Draw();
var Scales: Single;
    Origin: TVector2;
    Source: TRectangle;
    Dest: TRectangle;
begin
 if Visible then
   if (FEngine <> nil) and (TextureIndex <> -1) then
    begin
      if (X > FEngine.WorldX - FTexture.Pattern[FTextureIndex].Width) and
      (Y > FEngine.WorldY - FTexture.Pattern[FTextureIndex].Height) and
      (X < FEngine.WorldX + FEngine.VisibleWidth) and
      (Y < FEngine.WorldY + FEngine.VisibleHeight) then
    begin
     Scales:=FScale;
     Origin:=Vector2Create (FPatternWidth/2*Scale,FPatternHeight/2*Scale);

     Source:=SetPattern(FTexture.Texture[FTextureIndex],FPatternIndex,
     Round(FPatternWidth), Round(FPatternHeight));

      Dest:=RectangleCreate(
      X+FPatternWidth/2*scale,
      Y+FPatternHeight/2*scale,
      FPatternWidth*scale,FPatternHeight*scale);

      DrawTextureTiled(FTexture.Texture[FTextureIndex],Source,Dest,Origin,Angle,Scales,White);
     end;
    end;
end;

procedure TRayAnimatedSprite.DoMove(TimeGap: Double);
begin
  inherited DoMove(TimeGap);
    if AnimSpeed > 0 then
  begin
    AnimPos := AnimPos + AnimSpeed;
    FPatternIndex := Trunc(AnimPos);
    if (Round(AnimPos) > AnimStart + AnimCount) then
    begin
      if (Round(AnimPos)) = AnimStart + AnimCount then
        if AnimLooped then
        begin
          AnimPos := AnimStart;
          FPatternIndex := Round(AnimPos);
        end
        else
        begin
          AnimPos := AnimStart + AnimCount - 2;
          FPatternIndex := Round(AnimPos);
        end;
    end;
    if FDoAnimated = True then
    begin
      if Round(AnimPos) >= AnimCount  then
      begin
        FDoAnimated := False;
        AnimLooped := False;
        AnimSpeed := 0;
        AnimCount := 0;
        AnimPos := AnimStart;
        FPatternIndex := Round(AnimPos);
      end;
    end;
    if Round(AnimPos) < AnimStart then
    begin
      AnimPos := AnimStart;
      FPatternIndex := Trunc(AnimPos);
    end;
    if Round(AnimPos) > AnimCount then
    begin
      AnimPos := AnimStart;
      FPatternIndex := Round(AnimPos);
    end;
  end; // if AnimSpeed > 0
end;

procedure TRayAnimatedSprite.DoAnim(Looped: Boolean; Start: Integer;
  Count: Integer; Speed: Single);
begin
  FDoAnimated := True;
  AnimLooped := Looped;
  AnimStart := Start;
  AnimCount := Count;
  AnimSpeed := Speed;
end;

constructor TRayAnimatedSprite.Create(Engine: TRay2DEngine; Texture: TRayTexture
  );
begin
  inherited Create(Engine, Texture);
  FAnimated := True;
end;

destructor TRayAnimatedSprite.Destroy;
begin
  inherited Destroy;
end;

{ TRaySprite }

procedure TRaySprite.SetTextureName(Value: string);
var i: Integer;
begin
  FTextureName := Value;
  for i := 0 to Length(FTexture.TextureName) - 1 do
  begin
   if LowerCase(FTextureName) = LowerCase(FTexture.TextureName[i]) then
    begin
      TextureIndex := i;
      Pattern.Height := FTexture.Pattern[i].Height;
      Pattern.Width := FTexture.Pattern[i].Width;
      Exit;
    end;
  end;
  TextureIndex := -1;
end;

procedure TRaySprite.SetTextureIndex(Value: Integer);
begin
  FTextureIndex := Value;
  Pattern.Height := FTexture.Pattern[FTextureIndex].Height;
  Pattern.Width := FTexture.Pattern[FTextureIndex].Width;
end;

procedure TRaySprite.Draw();
var Scales: Single;
    Origin: TVector2;
    Source: TRectangle;
    Dest: TRectangle;
begin
 if Visible then
   if (FEngine <> nil) and (TextureIndex <> -1) then
    begin
      if (X > FEngine.WorldX - FTexture.Pattern[FTextureIndex].Width) and
      (Y > FEngine.WorldY - FTexture.Pattern[FTextureIndex].Height) and
      (X < FEngine.WorldX + FEngine.VisibleWidth) and
      (Y < FEngine.WorldY + FEngine.VisibleHeight) then
      begin
      Scales:=FScale;
      Origin:=Vector2Create(FTexture.Texture[FTextureIndex].Width/2*scale,FTexture.Texture[FTextureIndex].Height/2*scale);
      Source:=RectangleCreate(0,0,FTexture.Texture[FTextureIndex].Width*scale,  FTexture.Texture[FTextureIndex].Height*scale);

      Dest:=RectangleCreate(
      X+FTexture.Texture[FTextureIndex].Width/2*scale,
      Y+FTexture.Texture[FTextureIndex].Height/2*scale,
      FTexture.Texture[FTextureIndex].Width*scale,FTexture.Texture[FTextureIndex].Height*scale);
      DrawTextureTiled(FTexture.Texture[FTextureIndex],Source,Dest,Origin,Angle,Scales,White);
      end;
    end;
end;

procedure TRaySprite.DoMove(TimeGap: Double);
begin

end;

procedure TRaySprite.Dead();
begin
   if IsSpriteDead = False then
  begin
    IsSpriteDead := True;
    FEngine.FDeadList.Add(Self);
    Self.Visible := False;
  end;
end;

procedure TRaySprite.SetOrder(Value: Single);
begin
   if FZ <> Value then FZ := Value;
  FEngine.SetZOrder;
end;

procedure TRaySprite.SetScale(Value: Single);
begin
 If Value >=1.0 then
 FScale := Value;
end;

constructor TRaySprite.Create(Engine: TRay2DEngine; Texture: TRayTexture);
begin
  FAnimated := False;
  FEngine := Engine;
  FEngine.FList.Add(Self);
  FTexture := Texture;
  Alpha := 255;
  Visible := True;
  Scale:=1.0;
end;

constructor TRaySprite.CreateEx(Texture: TRayTexture);
begin
 FAnimated := False;
 FTexture := Texture;
 FEngine := nil;
 Alpha := 255;
 Visible := True;
end;

destructor TRaySprite.Destroy;
begin
  inherited Destroy;
end;

{ TRayTexture }
function TRayTexture.LoadFromFile(FileName: String; Width, Height: Integer
  ): Boolean;
begin
   if not FileExists(PChar(FileName)) then
  begin
    Result := False;
    Exit;
  end;
  SetLength(Texture, Count + 1);
  SetLength(TextureName, Count + 1);
  SetLength(Pattern, Count + 1);
  Inc(Count);
  TextureName[Count - 1] := FileName;
  Pattern[Count - 1].Height := Height;
  Pattern[Count - 1].Width := Width;
  Texture[Count - 1] := LoadTexture(PChar(FileName));
  Result := True;
end;

constructor TRayTexture.Create;
begin
//
end;

destructor TRayTexture.Destroy;
var  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TextureName[i] := '';
    UnloadTexture(Texture[i]);// := //nil;
    Pattern[i].Height := 0;
    Pattern[i].Width := 0;
  end;
  SetLength(TextureName, 0);
  SetLength(Texture, 0);
  SetLength(Pattern, 0);
  Count := 0;
  inherited Destroy;
end;

{ TRay2DEngine }
procedure TRay2DEngine.SetWorldX(Value: Single);
begin
  FWorld.X := Value;
end;

procedure TRay2DEngine.SetWorldY(Value: Single);
begin
  FWorld.Y := Value;
end;

procedure TRay2DEngine.Draw();
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if TRaySprite(FList.Items[i]).FAnimated = False then
      TRaySprite(FList.Items[i]).Draw
    else
     TRayAnimatedSprite(FList.Items[i]).Draw;
  end;
end;

procedure TRay2DEngine.ClearDeadSprites;
var i: Integer;
begin
 for i := 0 to FDeadList.Count - 1 do
  begin
    if FDeadList.Count >= 1 then
      if TRaySprite(FDeadList.Items[i]).IsSpriteDead = True then
      TRaySprite(FDeadList.Items[i]).FEngine.FList.Remove(FDeadList.Items[i]);
  end;
  FDeadList.Clear;
end;

procedure TRay2DEngine.Move(TimeGap: Double);
var i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if TRaySprite(FList.Items[i]).FAnimated = False then
       TRaySprite(FList.Items[i]).DoMove(TimeGap)
    else
      TRayAnimatedSprite(FList.Items[i]).DoMove(TimeGap);
  end;
end;

procedure TRay2DEngine.SetZOrder();
var i: Integer; Done: Boolean;
begin
  Done := False;
  repeat
    for i := FList.Count - 1 downto 0 do
    begin
      if i = 0 then
      begin
        Done := True;
        Break;
      end;
      if TRaySprite(FList.Items[i]).Z < TRaySprite(FList.Items[i - 1]).Z then
      begin
       FList.Move(i, i - 1);
       Break;
      end;
    end;
  until Done;
end;

constructor TRay2DEngine.Create;
begin
  FList := TList.Create;
  FDeadList := TList.Create;
end;

destructor TRay2DEngine.Destroy;
var  i: Integer;
begin
 for i := 0 to FList.Count - 1 do
  TRaySprite(FList.Items[i]).Destroy;
  FList.Destroy;
  FDeadList.Destroy;
 // inherited Destroy;
end;

end.

