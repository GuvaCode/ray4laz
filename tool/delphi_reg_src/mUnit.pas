unit mUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    AddEnvBtn: TButton;
    Image1: TImage;
    RemEnvBtn: TButton;
    Label1: TLabel;

    procedure AddEnvBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetSystemEnvironmentVariable(const Name, Value: string);
    procedure DeleteSystemEnvironmentVariable(const Name: string);
    function IsEnvironmentAdd(const Name: string): boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Registry;
{$R *.dfm}

procedure TForm1.AddEnvBtnClick(Sender: TObject);
var
  ProgramPath: string;
begin
// ѕолучаем путь к директории программы
  ProgramPath := ExtractFileDir(ParamStr(0));

  // ”станавливаем переменную окружени€
  SetSystemEnvironmentVariable('RAY4LAZ_PATH', ProgramPath);


  ShowMessage('RAY4LAZ_PATH = ' + ProgramPath);
end;

procedure TForm1.DeleteSystemEnvironmentVariable(const Name: string);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER; // или HKEY_LOCAL_MACHINE дл€ всех пользователей
    if Reg.OpenKey('Environment', False) then
    begin
      if Reg.ValueExists(Name) then
      begin
        Reg.DeleteValue(Name);

        // ”ведомл€ем систему об изменении
        S := 'Environment';
        SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
          NativeInt(PChar(S)), SMTO_ABORTIFHUNG, 5000, nil);
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RemEnvBtn.Enabled := IsEnvironmentAdd('RAY4LAZ_PATH');
  AddEnvBtn.Enabled := not RemEnvBtn.Enabled;
end;

function TForm1.IsEnvironmentAdd(const Name: string): boolean;
begin
  showMessage(GetEnvironmentVariable('RAY4LAZ_PATH'));
  if GetEnvironmentVariable('RAY4LAZ_PATH') <> '' then result := True else
  result := False;
end;

procedure TForm1.SetSystemEnvironmentVariable(const Name, Value: string);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER; // или HKEY_LOCAL_MACHINE дл€ всех пользователей
    if Reg.OpenKey('Environment', True) then
    begin
      Reg.WriteString(Name, Value);
      Reg.CloseKey;

      // ”ведомл€ем систему об изменении
      S := 'Environment';
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
        NativeInt(PChar(S)), SMTO_ABORTIFHUNG, 5000, nil);
    end;
  finally
    Reg.Free;
  end;
end;

end.
