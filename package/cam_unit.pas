unit cam_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, SrcEditorIntf;

type
  TDialogMode = (dmFunction,dmProcedure);

  { TcamFrm }
  TcamFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CamPosXSpin: TFloatSpinEdit;
    CamNameEdit: TEdit;
    CamFOVYSpin: TFloatSpinEdit;
    CamPosYSpin: TFloatSpinEdit;
    CamPosZSpin: TFloatSpinEdit;
    camTargetXSpin: TFloatSpinEdit;
    camTargetYSpin: TFloatSpinEdit;
    camTargetZSpin: TFloatSpinEdit;
    camUpXSpin: TFloatSpinEdit;
    camUpYSpin: TFloatSpinEdit;
    camUpZSpin: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDialogMode: TDialogMode;
    FDialogText: String;
  public
    property DialogText: String read FDialogText write FDialogText;
    property DialogMode: TDialogMode read FDialogMode write FDialogMode;
  end;

var
  camFrm: TcamFrm;
  Editor: TSourceEditorInterface;

implementation

{$R *.lfm}

{ TcamFrm }

procedure TcamFrm.FormCreate(Sender: TObject);
begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor=nil then exit;
end;

procedure TcamFrm.FormShow(Sender: TObject);
begin
  if DialogMode = dmFunction then GroupBox4.Enabled:=False
  else GroupBox4.Enabled:=True;
end;

procedure TcamFrm.Button1Click(Sender: TObject);
begin
Close;
end;

procedure TcamFrm.Button2Click(Sender: TObject);
begin
    case DialogMode of
    dmProcedure:
    begin
    DialogText:='TCamera3DSet(@'+CamNameEdit.Text+','+
    'Vector3Create('+
    FloatToStr(CamPosXSpin.Value)+','+
    FloatToStr(CamPosYSpin.Value)+','+
    FloatToStr(CamPosZSpin.Value)+'),'+
    'Vector3Create('+
    FloatToStr(camTargetXSpin.Value)+','+
    FloatToStr(camTargetYSpin.Value)+','+
    FloatToStr(camTargetZSpin.Value)+'),'+
    'Vector3Create('+
    FloatToStr(camUpXSpin.Value)+','+
    FloatToStr(camUpYSpin.Value)+','+
    FloatToStr(camUpZSpin.Value)+'),'+
    FloatToStr(CamFOVYSpin.Value) +',CAMERA_PERSPECTIVE);';
    Editor.CutToClipboard;
    Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,DialogText);
    end;
    dmFunction:
    begin
    DialogText:='TCamera3DCreate('+
    'Vector3Create('+
    FloatToStr(CamPosXSpin.Value)+','+
    FloatToStr(CamPosYSpin.Value)+','+
    FloatToStr(CamPosZSpin.Value)+'),'+
    'Vector3Create('+
    FloatToStr(camTargetXSpin.Value)+','+
    FloatToStr(camTargetYSpin.Value)+','+
    FloatToStr(camTargetZSpin.Value)+'),'+
    'Vector3Create('+
    FloatToStr(camUpXSpin.Value)+','+
    FloatToStr(camUpYSpin.Value)+','+
    FloatToStr(camUpZSpin.Value)+'),'+
    FloatToStr(CamFOVYSpin.Value) +',CAMERA_PERSPECTIVE);';
    Editor.CutToClipboard;
    Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,DialogText);
    end;

    end;
    Close;
end;

end.

