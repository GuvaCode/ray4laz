unit Ray.Sprites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
TCollideMode = (cmCircle, cmRect, cmQuadrangle, cmPolygon);

TAnimPlayMode = (pmForward, pmBackward, pmPingPong);

TJumpState = (jsNone, jsJumping, jsFalling);

TImageType = (itSingleImage, itSpriteSheet);

TGUIType = (gtNormal, gtForm, gtButton, gtScrollBar, gtEdit);

TTileMode = (tmHorizontal, tmVertical, tmFull);

TFrameRec =record
  FrameName: string;
  Frames: array of Cardinal;
end;

ESpriteError = class(Exception);

//TSpriteEngine = class;

//TSpriteClass = class of TSprite;

  { TAnimations }

  TAnimations = class
    private
      FrameData: array of TFrameRec;
      SearchObjects: array of Integer;
      SearchDirty: Boolean;
      function GetItem(Index: Integer): TFrameRec;
      function GetItemCount(): Integer;
      procedure InitSearchObjects();
      procedure SwapSearchObjects(Index1, Index2: Integer);
      function CompareSearchObjects(Obj1, Obj2: TFrameRec): Integer;
      function SplitSearchObjects(Start, Stop: Integer): Integer;
      procedure SortSearchObjects(Start, Stop: Integer);
      procedure UpdateSearchObjects();
      function GetFrame(const Name: string): TFrameRec;
    public
      property Items[Index: Integer]: TFrameRec read GetItem; Default;
      property ItemCount: Integer read GetItemCount;
      property Frame[const Name: string]: TFrameRec read GetFrame;
      function IndexOf(const Name: string): Integer; overload;
      procedure Remove(Index: Integer);
      procedure AddFrames(FrameName: string; Frames: array of Cardinal); overload;
      procedure RemoveAll();
      procedure MarkSearchDirty();
      constructor Create();
      destructor Destroy(); override;
    end;


implementation

{ TAnimations }
function TAnimations.GetItem(Index: Integer): TFrameRec;
begin
  if (Index >= 0) and (Index < Length(FrameData)) then
  Result := FrameData[Index];
end;

function TAnimations.GetItemCount(): Integer;
begin
  Result := Length(FrameData);
end;

procedure TAnimations.InitSearchObjects();
var
  I: Integer;
begin
  if (Length(FrameData) <> Length(SearchObjects)) then
    SetLength(SearchObjects, Length(FrameData));
  for I := 0 to Length(FrameData) - 1 do
    SearchObjects[I] := I;
end;

procedure TAnimations.SwapSearchObjects(Index1, Index2: Integer);
var
  Aux: Integer;
begin
  Aux := SearchObjects[Index1];
  SearchObjects[Index1] := SearchObjects[Index2];
  SearchObjects[Index2] := Aux;
end;


function TAnimations.CompareSearchObjects(Obj1, Obj2: TFrameRec): Integer;
begin
  Result := CompareText(Obj1.FrameName, Obj2.FrameName);
end;

function TAnimations.SplitSearchObjects(Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: TFrameRec;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FrameData[SearchObjects[Start]];
  while (Left <= Right) do
  begin
    while (Left <= Stop) and (CompareSearchObjects(FrameData[SearchObjects[Left]], Pivot) < 0) do
      Inc(Left);
    while (Right > Start) and (CompareSearchObjects(FrameData[SearchObjects[Right]], Pivot) >= 0) do
      Dec(Right);
    if (Left < Right) then
      SwapSearchObjects(Left, Right);
  end;
  SwapSearchObjects(Start, Right);
  Result := Right;
end;

procedure TAnimations.SortSearchObjects(Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if (Start < Stop) then
  begin
    SplitPt := SplitSearchObjects(Start, Stop);
    SortSearchObjects(Start, SplitPt - 1);
    SortSearchObjects(SplitPt + 1, Stop);
  end;
end;

procedure TAnimations.UpdateSearchObjects();
begin
  InitSearchObjects();
  SortSearchObjects(0, Length(SearchObjects) - 1);
  SearchDirty := False;
end;

function TAnimations.GetFrame(const Name: string): TFrameRec;
  var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if (Index <> -1) then
    Result := FrameData[Index];
end;

function TAnimations.IndexOf(const Name: string): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  if (SearchDirty) then
    UpdateSearchObjects();
  Result := -1;
  Lo := 0;
  Hi := Length(SearchObjects) - 1;
  while (Lo <= Hi) do
  begin
    Mid := (Lo + Hi) div 2;
    if (CompareText(FrameData[SearchObjects[Mid]].FrameName, Name) = 0) then
    begin
      Result := SearchObjects[Mid];
      Break;
    end;
    if (CompareText(FrameData[SearchObjects[Mid]].FrameName, Name) > 0) then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

procedure TAnimations.Remove(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FrameData)) then
    Exit;
  for I := Index to Length(FrameData) - 2 do
    FrameData[I] := FrameData[I + 1];
  SetLength(FrameData, Length(FrameData) - 1);
  SearchDirty := True;
end;

procedure TAnimations.AddFrames(FrameName: string; Frames: array of Cardinal);
var
  I, Index: Integer;
begin
  Index := Length(FrameData);
  SetLength(FrameData, Index + 1);
  FrameData[Index].FrameName := FrameName;
  SetLength(FrameData[Index].Frames, High(Frames) + 1);
  for I := 0 to High(Frames) do
    FrameData[Index].Frames[I] := Frames[I];
  SearchDirty := True;
end;

procedure TAnimations.RemoveAll();
begin
  SetLength(FrameData, 0);
  SearchDirty := True;
end;

procedure TAnimations.MarkSearchDirty();
begin
  SearchDirty := True;
end;

constructor TAnimations.Create();
begin
   SearchDirty := False;
end;

destructor TAnimations.Destroy();
begin
  RemoveAll();
  inherited Destroy();
end;

end.

