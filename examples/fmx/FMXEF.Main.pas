unit FMXEF.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm2 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    LayoutClient: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure LayoutClientResize(Sender: TObject);
  private
    FRayHandle: THandle;
    procedure SetHiddenBorderForExternalWindow(hWnd: THandle);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  FMX.Game, FMX.Platform.Win, raylib, Winapi.Windows;

{$R *.fmx}

procedure TForm2.SetHiddenBorderForExternalWindow(hWnd: THandle);
var
  CurrentStyle: Longint;
  NewStyle: Longint;
begin
  if (hWnd = 0) then
    Exit;

  CurrentStyle := GetWindowLong(hWnd, GWL_STYLE);

  NewStyle := CurrentStyle and not (WS_BORDER or WS_CAPTION or WS_THICKFRAME or WS_SIZEBOX);

  SetWindowLong(hWnd, GWL_STYLE, NewStyle);

  SetWindowPos(hWnd, 0, Trunc(LayoutClient.Position.X), Trunc(LayoutClient.Position.Y),
    Trunc(LayoutClient.Width), Trunc(LayoutClient.Height), SWP_NOZORDER or SWP_FRAMECHANGED);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FRayHandle := 0;
  Run(FmxHandleToHWND(Handle),
    procedure(RayHandle: THandle)
    begin
      FRayHandle := RayHandle;
      SetWindowState(FLAG_WINDOW_TRANSPARENT or FLAG_MSAA_4X_HINT);
      SetHiddenBorderForExternalWindow(RayHandle);
    end);
end;

procedure TForm2.LayoutClientResize(Sender: TObject);
begin
  if FRayHandle <> 0 then
  begin
    //SetWindowPos(FRayHandle, 0, Trunc(LayoutClient.Position.X), Trunc(LayoutClient.Position.Y),
    //  Trunc(LayoutClient.Width), Trunc(LayoutClient.Height), SWP_NOZORDER);
    SetWindowPosition(Trunc(LayoutClient.Position.X), Trunc(LayoutClient.Position.Y));
    SetWindowSize(Trunc(LayoutClient.Width), Trunc(LayoutClient.Height));
  end;
  //LayoutClient
end;

end.

