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
    procedure RemEnvBtnClick(Sender: TObject);
    procedure UpdateBtnState;
  private
    procedure SetSystemEnvironmentVariable(const Name, Value: string);
    procedure DeleteSystemEnvironmentVariable(const Name: string);
    function IsEnvironmentAdd(const Name: string): boolean;
    function IsEnvVarInRegistry(const Name: string): boolean; // Новая функция
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
  // Получаем путь к директории программы
  ProgramPath := ExtractFileDir(ParamStr(0));
  // Устанавливаем переменную окружения
  SetSystemEnvironmentVariable('RAY4LAZ_PATH', ProgramPath);
  UpdateBtnState;
end;

procedure TForm1.DeleteSystemEnvironmentVariable(const Name: string);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    // Альтернативный путь: попробуем открыть с расширенными правами
    if Reg.OpenKey('\Environment', False) then  // Добавлен слеш
    begin
      if Reg.ValueExists(Name) then
      begin
        Reg.DeleteValue(Name);

        // Уведомляем систему
        S := 'Environment';
        SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
          LPARAM(PChar(S)), SMTO_ABORTIFHUNG, 5000, nil);

        SetEnvironmentVariable(PChar(Name), nil);
        // ShowMessage('Переменная удалена');
      end;
      Reg.CloseKey;
    end
    else
      ShowMessage('Не удалось открыть ключ');
  finally
    Reg.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateBtnState;
end;

// Новая функция для проверки наличия переменной в реестре
function TForm1.IsEnvVarInRegistry(const Name: string): boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Environment', False) then
    begin
      Result := Reg.ValueExists(Name);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TForm1.IsEnvironmentAdd(const Name: string): boolean;
begin
  // Используем проверку через реестр вместо GetEnvironmentVariable
  Result := IsEnvVarInRegistry(Name);
end;

procedure TForm1.RemEnvBtnClick(Sender: TObject);
begin
  DeleteSystemEnvironmentVariable('RAY4LAZ_PATH');
  UpdateBtnState;
end;

procedure TForm1.SetSystemEnvironmentVariable(const Name, Value: string);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Environment', True) then
    begin
      Reg.WriteString(Name, Value);
      Reg.CloseKey;

      // Уведомляем систему об изменении
      S := 'Environment';
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,
        NativeInt(PChar(S)), SMTO_ABORTIFHUNG, 5000, nil);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.UpdateBtnState;
begin
  RemEnvBtn.Enabled := IsEnvironmentAdd('RAY4LAZ_PATH');
  AddEnvBtn.Enabled := not RemEnvBtn.Enabled;
end;

end.
