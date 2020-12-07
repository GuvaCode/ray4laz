unit Ray.Canvas;

{$mode objfpc}{$H+}

interface

uses
  Ray.Headers;

type
  TBlendingEffect = (Alpha=0,Additive,Multiplied);

  { TGameCanvas }

  TGameCanvas = class
    procedure Draw(ATexture: TTexture; X, Y: Single);
  end;

implementation

{ TGameCanvas }

procedure TGameCanvas.Draw(ATexture: TTexture; X, Y: Single);
begin
  DrawTexture(ATexture,Round(X),Round(Y),WHITE);
end;

end.

