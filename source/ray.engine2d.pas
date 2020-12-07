unit ray.engine2D;
{$mode objfpc}{$H+}
interface

uses
 Classes,ray.headers, ray.typesEx;

type
  TJumpState = (jsNone, jsJumping, jsFalling);
  TCollideMode = (cmCircle, cmRect, cmQuadrangle, cmPolygon);

 { TSpriteEngine }
 TSpriteEngine = class
  private
    FVisibleHeight: Integer;
    FVisibleWidth: Integer;
    FWorld: TVector3;
    FCamera: TCamera2D;
    procedure SetCamera(Value: TCamera2D);
    procedure SetWorldX(Value: Single);
    procedure SetWorldY(Value: Single);
  public
    List: TList;
    DeadList: TList;
    procedure Draw();
    procedure ClearDeadSprites;
    procedure Move(MoveCount: Double);
    procedure SetZOrder();
    constructor Create;
    destructor Destroy; override;

    property Camera: TCamera2D read FCamera write SetCamera;
    property WorldX: Single read FWorld.X write SetWorldX;
    property WorldY: Single read FWorld.Y write SetWorldY;
    property VisibleWidth: Integer read FVisibleWidth write FVisibleWidth;
    property VisibleHeight: Integer read FVisibleHeight write FVisibleHeight;
  end;

 TPattern = record
   Height, Width: Integer;
 end;

 { TRayTexture }
 TSpriteTexture = class
 public
   Count: Integer;
   TextureName: array of string;
   Texture: array of TTexture2D;
   Pattern: array of TPattern;
   function LoadFromFile(FileName: String; Width, Height: Integer): Boolean;
   constructor Create;
   destructor Destroy; override;
 end;

 TSprite = class
  private
    FAnimated: Boolean;
    FCollideMode: TCollideMode;
    FCollidePolygon: TPolygon;
    FCollidePos: TPoint;
    FCollideQuadrangle: TPoint4;
    FCollideRadius: Integer;
    FCollideRect: TRect;
    FCollisioned: Boolean;
    FVector: TVector3;
    FZ: Single;
    FScale: Single;
  protected
    FEngine: TSpriteEngine;
    FTextureName: string;
    FTextureIndex: Integer;
    procedure SetTextureName(Value: string);
    procedure SetTextureIndex(Value: Integer);
  public
    FTexture: TSpriteTexture;
    Alpha: Single;
    Angle: Single;
    IsSpriteDead: Boolean;
    DrawMode: Integer;
    ScaleX, ScaleY: Single;
    Visible: Boolean;
    Pattern: TRect;
    procedure Draw();
    procedure DoDraw; virtual;
    procedure DoMove(MoveCount: Double); virtual;
    procedure DoCollision(const Sprite: TSprite); virtual;
    procedure Dead();
    procedure SetOrder(Value: Single);
    procedure SetScale(Value: Single);
    procedure Collision(const Other: TSprite); overload; virtual;
    procedure Collision; overload; virtual;
    constructor Create(Engine: TSpriteEngine; Texture: TSpriteTexture); virtual;
    constructor CreateEx(Engine: TSpriteEngine; Texture: TSpriteTexture); virtual;
    destructor Destroy; override;

    property CollidePos: TPoint read FCollidePos write FCollidePos;
    property CollideRadius: Integer read FCollideRadius write FCollideRadius;
    property CollideRect: TRect read FCollideRect write FCollideRect;
    property CollideQuadrangle: TPoint4 read FCollideQuadrangle write FCollideQuadrangle;
    property CollidePolygon: TPolygon read FCollidePolygon write FCollidePolygon;
    property CollideMode: TCollideMode read FCollideMode write FCollideMode;
    property Collisioned: Boolean read FCollisioned write FCollisioned;

    property TextureIndex: Integer read FTextureIndex write SetTextureIndex;
    property TextureName: string read FTextureName write SetTextureName;
    property X: Single read FVector.X write FVector.X;
    property Y: Single read FVector.Y write FVector.Y;
    property Z: Single read FZ write SetOrder;
    property Scale: Single read FScale write SetScale;
  end;

  { TAnimatedSprite }

  TAnimatedSprite = class(TSprite)
  protected
    FDoAnimated: Boolean;
    FSplited: array of TRect;
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

    PatternCount: Integer;
    PatternDeltaX: Integer;
    PatternDeltaY: Integer;

    procedure Split();
    procedure Split2();
    procedure Split3();

    procedure Draw();
    procedure DoDraw; override;
    procedure DoMove(MoveCount: Double); override;

    procedure DoAnim(Looped: Boolean; Start: Integer; Count: Integer;
      Speed: Single);

    constructor Create(Engine: TSpriteEngine; Texture: TSpriteTexture); override;
    destructor Destroy; override;

    property PatternHeight: Integer read FPatternHeight write SetPatternHeight;
    property PatternWidth: Integer read FPatternWidth write SetPatternWidth;
  end;

const EmptyStr: string = '';

implementation

{ TAnimatedSprite }

procedure TAnimatedSprite.SetPatternHeight(Value: Integer);
begin
  FPatternHeight := Value;
  Pattern.Bottom := Value;
end;

procedure TAnimatedSprite.SetPatternWidth(Value: Integer);
begin
  FPatternWidth := Value;
  Pattern.Right := Value;
end;

procedure TAnimatedSprite.Split();
  var
    i: Integer;
  begin
    SetLength(FSplited, PatternCount + 1);
    for i := 0 to PatternDeltaY - 1 do
    begin
      if i = 0 then
      begin
        FSplited[PatternCount].Left := 0;
        FSplited[PatternCount].Top := 0;
        FSplited[PatternCount].Right := PatternWidth;
        FSplited[PatternCount].Bottom := PatternHeight;
        Inc(PatternCount);
      end; // if i = 0
      if i >= 1 then
      begin
        SetLength(FSplited, PatternCount + 1);
        FSplited[PatternCount].Left := 0;
        FSplited[PatternCount].Top := PatternHeight * i;
        FSplited[PatternCount].Right := PatternWidth;
        FSplited[PatternCount].Bottom := PatternHeight * (i + 1);
        Inc(PatternCount);
      end; // if i >= 1
    end; // for
  end;


procedure TAnimatedSprite.Split2();
var i: Integer;
  begin
    SetLength(FSplited, PatternCount + 1);
    for i := 0 to PatternDeltaX - 1 do
    begin
      if i = 0 then
      begin
        FSplited[PatternCount].Left := 0;
        FSplited[PatternCount].Top := 0;
        FSplited[PatternCount].Right := PatternWidth;
        FSplited[PatternCount].Bottom := PatternHeight;
        Inc(PatternCount);
      end; // if i = 0
      if i >= 1 then
      begin
        SetLength(FSplited, PatternCount + 1);
        FSplited[PatternCount].Left := PatternWidth * (i);
        FSplited[PatternCount].Top := 0;
        FSplited[PatternCount].Right := PatternWidth * (i + 1);
        FSplited[PatternCount].Bottom := PatternHeight;
        Inc(PatternCount);
      end; // if i >= 1
    end; // for
  end;

procedure TAnimatedSprite.Split3();
var j: Integer;

  procedure CallSpliter(NextTop, NextHeight: Integer);
  var
    i: Integer;
  begin
    for i := 0 to PatternDeltaX - 1 do
    begin
      if i = 0 then
      begin
        SetLength(FSplited, PatternCount + 1);
        FSplited[PatternCount].Left := 0;
        FSplited[PatternCount].Top := NextTop;
        FSplited[PatternCount].Right := PatternWidth;
        FSplited[PatternCount].Bottom := NextHeight;
        Inc(PatternCount);
      end; // if i = 0
      if i >= 1 then
      begin
        SetLength(FSplited, PatternCount + 1);
        FSplited[PatternCount].Left := PatternWidth * (i);
        FSplited[PatternCount].Top := NextTop;
        FSplited[PatternCount].Right := PatternWidth * (i + 1);
        FSplited[PatternCount].Bottom := NextHeight;
        Inc(PatternCount);
      end; // if i >= 1
    end; // for
  end;

begin
  for j := 0 to PatternDeltaY - 1 do
  begin
    if j = 0 then
    begin
      CallSpliter(0, PatternHeight);
    end
    else
    begin
      CallSpliter(PatternHeight * j, PatternHeight * (j + 1));
    end;
  end;
end;

procedure TAnimatedSprite.Draw();
begin
      if (Visible) and  (TextureIndex <> -1)  then
  begin
    if Assigned(FEngine) then
    begin
      if (X > FEngine.WorldX - FTexture.Pattern[FTextureIndex].Width) and
      (Y > FEngine.WorldY - FTexture.Pattern[FTextureIndex].Height) and (X < FEngine.WorldX +
        FEngine.VisibleWidth) and (Y < FEngine.WorldY + FEngine.VisibleHeight) then
      begin
        DoDraw;
      end;
    end else
    begin
    //drawcustom
    end;
  end;
end;

procedure TAnimatedSprite.DoDraw;
var SourceRec, DestRec: TRectangle; Origin:TVector2;
begin
  inherited DoDraw;
  if not Visible then Exit;
  if TextureIndex <= -1 then Exit;
  BeginBlendMode(BLEND_ALPHA);

  SourceRec:= RectangleCreate(0,0,FTexture.Pattern[FTextureIndex].width,
  FTexture.Pattern[FTextureIndex].height);

  //asprite2d_Draw(FTexture.Texture[FTextureIndex],
  //          FEngine.FCamera.X + X, FEngine.FCamera.Y + Y, PatternWidth,
  //          PatternHeight, Angle, Trunc(AnimPos), Alpha);

  DestRec:= RectangleCreate(Round(FEngine.FCamera.target.X + X),
  Round(FEngine.FCamera.target.y +Y),
  FTexture.Pattern[FTextureIndex].width,FTexture.Pattern[FTextureIndex].height);

  Origin:= Vector2Create( FTexture.Pattern[FTextureIndex].width div 2,FTexture.Pattern[FTextureIndex].height div 2);
  DrawTexturePro(FTexture.Texture[FTextureIndex], sourceRec, destRec, origin, Angle,Fade(RAYWHITE,alpha));
  EndBlendMode();
end;

procedure TAnimatedSprite.DoMove(MoveCount: Double);
begin
  inherited DoMove(MoveCount);
    if AnimSpeed > 0 then
  begin
    AnimPos := AnimPos + AnimSpeed;
    FPatternIndex := Trunc(AnimPos);

    if (Trunc(AnimPos) > AnimStart + AnimCount) then
    begin
      if (Trunc(AnimPos)) = AnimStart + AnimCount then
       if AnimLooped then
        begin
          AnimPos := AnimStart;
          FPatternIndex := Trunc(AnimPos);
        end
        else
        begin
          AnimPos := AnimStart + AnimCount - 1;
          FPatternIndex := Trunc(AnimPos);
        end;
    end;

    if FDoAnimated = True then
    begin
      if Trunc(AnimPos) >= AnimCount + 1 then
      begin
        FDoAnimated := False;
        AnimLooped := False;
        AnimSpeed := 0;
        AnimCount := 0;
        AnimPos := AnimStart;
        FPatternIndex := Trunc(AnimPos);
      end;
    end;

    if Trunc(AnimPos) < AnimStart then
    begin
      AnimPos := AnimStart;
      FPatternIndex := Trunc(AnimPos);
    end;

    if Trunc(AnimPos) > AnimCount then
    begin
      AnimPos := AnimStart;
      FPatternIndex := Trunc(AnimPos);
    end;
  end; // if AnimSpeed > 0
end;

procedure TAnimatedSprite.DoAnim(Looped: Boolean; Start: Integer;
  Count: Integer; Speed: Single);
begin
  FDoAnimated := True;
  AnimLooped := Looped;
  AnimStart := Start;
  AnimCount := Count;
  AnimSpeed := Speed
end;

constructor TAnimatedSprite.Create(Engine: TSpriteEngine;
  Texture: TSpriteTexture);
begin
  inherited Create(Engine, Texture);
  FAnimated := True;
end;

destructor TAnimatedSprite.Destroy;
var
  i: Integer;
begin
  for i := 0 to PatternCount - 1 do
  begin
    FSplited[i].Left := 0;
    FSplited[i].Top := 0;
    FSplited[i].Right := 0;
    FSplited[i].Bottom := 0;
  end;
  SetLength(FSplited, 0);
  inherited Destroy;
end;

{ TSprite }

procedure TSprite.SetTextureName(Value: string);
var
   i: Integer;
  begin
    FTextureName := Value;
    for i := 0 to Length(FTexture.TextureName) - 1 do
    begin
      if lowercase(FTextureName) = lowercase(FTexture.TextureName[i]) then
      begin
        TextureIndex := i;
        Pattern.Right := FTexture.Pattern[i].Height;
        Pattern.Bottom := FTexture.Pattern[i].Width;
        Exit;
      end;
    end;
    TextureIndex := -1;
end;

procedure TSprite.SetTextureIndex(Value: Integer);
begin
  FTextureIndex := Value;
  Pattern.Right := FTexture.Pattern[FTextureIndex].Height;
  Pattern.Bottom := FTexture.Pattern[FTextureIndex].Width;
end;

procedure TSprite.Draw();
begin
   if (Visible) and  (TextureIndex <> -1)  then
  begin
    if Assigned(FEngine) then
    begin
      if (X > FEngine.WorldX - FTexture.Pattern[FTextureIndex].Width) and
      (Y > FEngine.WorldY - FTexture.Pattern[FTextureIndex].Height) and (X < FEngine.WorldX +
        FEngine.VisibleWidth) and (Y < FEngine.WorldY + FEngine.VisibleHeight) then
      begin
        DoDraw;
      end;
    end else
    begin
    //drawcustom
    end;

  end;
end;

procedure TSprite.DoDraw;
var SourceRec, DestRec: TRectangle;
    Origin:TVector2;
begin
  if not Visible then Exit;
  if TextureIndex <= -1 then Exit;
  BeginBlendMode(BLEND_ALPHA);


  SourceRec:= RectangleCreate(0,0,FTexture.Pattern[FTextureIndex].width,
  FTexture.Pattern[FTextureIndex].height);


  {  DestRec:= RectangleCreate(Round(FEngine.FCamera.target.X + X),
  Round(FEngine.FCamera.target.y +Y),
  FTexture.Pattern[FTextureIndex].width,FTexture.Pattern[FTextureIndex].height); }

  DestRec:= RectangleCreate(Round(X),Round(Y),
  FTexture.Pattern[FTextureIndex].width,FTexture.Pattern[FTextureIndex].height);

  Origin:= Vector2Create( FTexture.Pattern[FTextureIndex].width div 2,FTexture.Pattern[FTextureIndex].height div 2);
  DrawTexturePro(FTexture.Texture[FTextureIndex], sourceRec, destRec, origin, Angle,Fade(RAYWHITE,alpha));
  EndBlendMode();
end;

procedure TSprite.DoMove(MoveCount: Double);
begin
end;

procedure TSprite.DoCollision(const Sprite: TSprite);
begin
end;

procedure TSprite.Dead();
begin
   if IsSpriteDead = False then
  begin
    IsSpriteDead := True;
    FEngine.DeadList.Add(Self);
    Self.Visible := False;
  end
end;

procedure TSprite.SetOrder(Value: Single);
begin
  if FZ <> Value then FZ := Value;
  FEngine.SetZOrder;
end;

procedure TSprite.SetScale(Value: Single);
begin
  FScale := Value;
  ScaleX := FScale;
  ScaleY := FScale;
end;

procedure TSprite.Collision(const Other: TSprite);
var IsCollide: Boolean; Delta: Real;
begin
  IsCollide := False;
  if (FCollisioned) and (Other.FCollisioned) and (not IsSpriteDead) and (not Other.IsSpriteDead) then
  begin
    case FCollideMode of
      cmCircle:
        begin
          Delta := Sqrt(Sqr(Self.FCollidePos.X - Other.FCollidePos.X) + Sqr(Self.FCollidePos.Y -
            Other.FCollidePos.Y));
          IsCollide := (Delta < (Self.FCollideRadius + Other.FCollideRadius));
        end;
      cmRect:
        begin
          IsCollide := OverlapRect(Self.FCollideRect, Other.FCollideRect);
        end;
      cmQuadrangle:
        begin
          IsCollide := OverlapQuadrangle(Self.FCollideQuadrangle, Other.FCollideQuadrangle);
        end;
      cmPolygon:
        begin
          IsCollide := OverlapPolygon(Self.FCollidePolygon, Other.FCollidePolygon);
        end;
    end;

    if IsCollide then
    begin
      DoCollision(Other);
      Other.DoCollision(Self);
    end;
  end;

end;

procedure TSprite.Collision;
var
  I: Integer;
begin
  if (FEngine <> nil) and (not IsSpriteDead) and (Collisioned) then
  begin
    for I := 0 to FEngine.List.Count-1 do
     Self.Collision(TSprite(FEngine.List.Items[i]));
  end;
end;

constructor TSprite.Create(Engine: TSpriteEngine; Texture: TSpriteTexture);
begin
  FAnimated := false;
  FEngine := Engine;
  FEngine.List.Add(Self);
  FTexture := Texture;
  Pattern.Left := 0;
  Pattern.Top := 0;
  Alpha := 255;
  ScaleX := 1.0;
  ScaleY := 1.0;
  Visible := True;
  FCollideMode := cmCircle;
  FCollisioned:=False;
end;

constructor TSprite.CreateEx(Engine: TSpriteEngine; Texture: TSpriteTexture);
begin
  FAnimated := False;
  FTexture := Texture;
  FEngine := Engine;
  Pattern.Left := 0;
  Pattern.Top := 0;
  Alpha := 255;
  ScaleX := 1.0;
  ScaleY := 1.0;
  Visible := True;
  FCollisioned:=False;
  FCollideMode := cmCircle;

end;

destructor TSprite.Destroy;
begin
  inherited Destroy;
end;

{ TRayTexture }
function TSpriteTexture.LoadFromFile(FileName: String; Width, Height: Integer
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
  Texture[Count - 1].width:=Width;
  Texture[Count - 1].height:=Height;
  Result := True;
end;

constructor TSpriteTexture.Create;
begin
end;

destructor TSpriteTexture.Destroy;
 var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    TextureName[i] := EmptyStr;
    UnloadTexture(Texture[i]);
    Pattern[i].Height := 0;
    Pattern[i].Width := 0;
  end;
  SetLength(TextureName, 0);
  SetLength(Texture, 0);
  SetLength(Pattern, 0);
  Count := 0;
  inherited Destroy;
end;


{ TSpriteEngine }
procedure TSpriteEngine.SetCamera(Value: TCamera2D);
begin
  FCamera := Value;
end;

procedure TSpriteEngine.SetWorldX(Value: Single);
begin
  FWorld.X := Value;
end;

procedure TSpriteEngine.SetWorldY(Value: Single);
begin
  FWorld.Y := Value;
end;

procedure TSpriteEngine.Draw();
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    if TSprite(List.Items[i]).FAnimated = False then
    begin
      TSprite(List.Items[i]).Draw;
    end
    else
    begin
      TAnimatedSprite(List.Items[i]).Draw;
    end;
  end;
end;

procedure TSpriteEngine.ClearDeadSprites;
var
  i: Integer;
 begin
  for i := 0 to DeadList.Count - 1 do
  begin
    if DeadList.Count >= 1 then
    begin
      if TSprite(DeadList.Items[i]).IsSpriteDead = True then
      begin
        TSprite(DeadList.Items[i]).FEngine.List.Remove(DeadList.Items[i]);
      end;
    end;
  end;
  DeadList.Clear;
end;

procedure TSpriteEngine.Move(MoveCount: Double);
var
  i: Integer;
 begin
  for i := 0 to List.Count - 1 do
  begin
    if TSprite(List.Items[i]).FAnimated = False then
    begin
      TSprite(List.Items[i]).DoMove(MoveCount);
    end
    else
    begin
      TAnimatedSprite(List.Items[i]).DoMove(MoveCount);
    end;
  end;
 end;

procedure TSpriteEngine.SetZOrder();
var
  i: Integer;
  Done: Boolean;
 begin
  Done := False;
  repeat
    for i := List.Count - 1 downto 0 do
    begin
      if i = 0 then
      begin
        Done := True;
        break;
      end;
      if TSprite(List.Items[i]).Z < TSprite(List.Items[i - 1]).Z then
      begin
        List.Move(i, i - 1);
        break;
      end;
    end;
  until Done;
 end;

constructor TSpriteEngine.Create;
 begin
  List := TList.Create;
  DeadList := TList.Create;
 end;

destructor TSpriteEngine.Destroy;
  var
  i: Integer;
 begin
  for i := 0 to List.Count - 1 do
  begin
    TSprite(List.Items[i]).Destroy;
  end;
  List.Destroy;
  DeadList.Destroy;
  inherited Destroy;
end;

end.


