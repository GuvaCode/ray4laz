unit ray4delphi_reg;

interface

uses
  System.SysUtils, System.Classes, ToolsAPI, DesignIntf, DesignEditors,
  Vcl.Forms, PlatformAPI, Vcl.Menus, Vcl.ActnList, Vcl.Graphics,
  Vcl.Dialogs, System.UITypes, Vcl.FileCtrl;

type
  { Базовый класс для создателей проектов raylib }
  TRayBaseProjectCreator = class(TInterfacedObject, IOTACreator,
    IOTAProjectCreator50, IOTAProjectCreator80, IOTAProjectCreator)
  private
    FPlatform: string;
    FProjectName: string;
  protected
    function GetProjectFileName: string; virtual; abstract;
    function GetProjectSource: string; virtual; abstract;
    procedure SetProjectOptions(const Project: IOTAProject); virtual;
    function GetRaylibPath: string; virtual;
  public
    constructor Create(const AProjectName, APlatform: string);
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAProjectCreator }
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    procedure NewDefaultSourceModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    { IOTAProjectCreator80 }
    function GetProjectPersonality: string;
  end;

  { Создатель простого проекта raylib }
  TRaySimpleProjectCreator = class(TRayBaseProjectCreator)
  protected
    function GetProjectFileName: string; override;
    function GetProjectSource: string; override;
    procedure SetProjectOptions(const Project: IOTAProject); override;
  end;

  { Создатель проекта raylib с TCustomApplication }
  TRayCustomAppProjectCreator = class(TRayBaseProjectCreator)
  protected
    function GetProjectFileName: string; override;
    function GetProjectSource: string; override;
    procedure SetProjectOptions(const Project: IOTAProject); override;
  end;

  { Файл исходного кода проекта }
  TRayProjectSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const ASource: string);
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  { Репозиторий визард для простого проекта (для File > New > Other) }
  TRaySimpleRepositoryWizard = class(TNotifierObject, IOTAWizard,
    IOTARepositoryWizard, IOTAProjectWizard, IOTARepositoryWizard160)
  protected
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: THandle;
    function GetDesigner: string;
    { IOTAProjectWizard }
    function GetPersonality: string;
    { IOTARepositoryWizard160 }
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetFrameworkTypes: TArray<string>;
    function GetPlatforms: TArray<string>;
    procedure SetSelectedPersonality(const APersonality: string);
    procedure SetSelectedFrameworkType(const AFrameworkType: string);
    procedure SetSelectedPlatform(const APlatform: string);
  end;

  { Репозиторий визард для проекта с TCustomApplication (для File > New > Other) }
  TRayCustomAppRepositoryWizard = class(TNotifierObject, IOTAWizard,
    IOTARepositoryWizard, IOTAProjectWizard, IOTARepositoryWizard160)
  protected
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: THandle;
    function GetDesigner: string;
    { IOTAProjectWizard }
    function GetPersonality: string;
    { IOTARepositoryWizard160 }
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetFrameworkTypes: TArray<string>;
    function GetPlatforms: TArray<string>;
    procedure SetSelectedPersonality(const APersonality: string);
    procedure SetSelectedFrameworkType(const AFrameworkType: string);
    procedure SetSelectedPlatform(const APlatform: string);
  end;

  TRayPlatformAlias = class
  public
    class function GetPlatformName: string;
  end;

procedure Register;

implementation

{$R RAY_ICON.res}

uses
  System.TypInfo, System.StrUtils, Winapi.Windows,
  System.IOUtils, System.Win.Registry, Vcl.ActnPopup;

resourcestring
  sRaySimpleProject = 'raylib Simple Project';
  sRaySimpleProjectDesc = 'A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)';
  sRayCustomAppProject = 'raylib Project with TCustomApplication';
  sRayCustomAppProjectDesc = 'raylib project using TCustomApplication style';
  sRayAuthor = 'Ray4Delphi Team';
  sRaySimpleComment = 'Creates a new simple raylib project';
  sRayCustomComment = 'Creates a new raylib project with TCustomApplication';
  sRayPage = 'raylib Projects';
  sRaySimpleIDString = 'Ray4Delphi.SimpleRepositoryWizard';
  sRayCustomIDString = 'Ray4Delphi.CustomRepositoryWizard';
  sRaySimpleMenuIDString = 'Ray4Delphi.SimpleMenuWizard';
  sRayCustomMenuIDString = 'Ray4Delphi.CustomMenuWizard';


const
  sProjectPersonality = sDelphiPersonality;
  sProjectsCategoryName = 'Projects'; // Имя корневой категории проектов

var
  SimpleWizardIndex: Integer = -1;
  CustomWizardIndex: Integer = -1;
  GalleryCategory: IOTAGalleryCategory = nil;
  ProjectDirectorySelected: string = '';  // Глобальная переменная для хранения выбранной директории

{ Вспомогательные функции }
function GetActiveProjectGroup: IOTAProjectGroup;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  begin
    for i := 0 to ModuleServices.ModuleCount - 1 do
    begin
      Module := ModuleServices.Modules[i];
      if Supports(Module, IOTAProjectGroup, ProjectGroup) then
      begin
        Result := ProjectGroup;
        Exit;
      end;
    end;
  end;
end;

function GetCurrentProjectDirectory: string;
var
  ProjectGroup: IOTAProjectGroup;
  ActiveProject: IOTAProject;
  ModuleServices: IOTAModuleServices;
begin
  Result := GetCurrentDir;

  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  begin
    // Сначала пытаемся получить активный проект
    ActiveProject := ModuleServices.GetActiveProject;
    if ActiveProject <> nil then
    begin
      Result := ExtractFilePath(ActiveProject.FileName);
      if Result <> '' then
        Exit;
    end;

    // Если нет активного проекта, ищем проект группу
    ProjectGroup := GetActiveProjectGroup;
    if (ProjectGroup <> nil) and (ProjectGroup.FileName <> '') then
    begin
      Result := ExtractFilePath(ProjectGroup.FileName);
      if Result <> '' then
        Exit;
    end;
  end;
end;

function GenerateUniqueProjectName(const BaseName: string; const ProjectDir: string): string;
var
  i: Integer;
  ProjectFileName: string;
  IsUnique: Boolean;
begin
  Result := '';

  i := 1;
  while Result = '' do
  begin
    ProjectFileName := ProjectDir + BaseName + IntToStr(i) + '.dpr';

    // Проверяем уникальность имени
    IsUnique := True;

    // Проверяем существование файла на диске
    if TFile.Exists(ProjectFileName) then
      IsUnique := False;

    if IsUnique then
      Result := ProjectFileName
    else
      Inc(i);

    // Предотвращаем бесконечный цикл
    if i > 1000 then
    begin
      Result := ProjectDir + BaseName + IntToStr(Random(1000)) + '.dpr';
      Break;
    end;
  end;

  // Показываем сообщение о том, куда сохраняется проект
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
    Format('Ray4Delphi: Creating project in: %s', [ProjectDir]));
end;

{ TRayBaseProjectCreator }

constructor TRayBaseProjectCreator.Create(const AProjectName, APlatform: string);
begin
  inherited Create;
  FProjectName := AProjectName;
  FPlatform := APlatform;
end;

function TRayBaseProjectCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TRayBaseProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRayBaseProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRayBaseProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProjectGroup;
end;

function TRayBaseProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TRayBaseProjectCreator.GetFileName: string;
begin
  Result := GetProjectFileName;
end;

function TRayBaseProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRayBaseProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

procedure TRayBaseProjectCreator.NewDefaultModule;
begin
  // Ничего не делаем
end;

procedure TRayBaseProjectCreator.NewDefaultSourceModule;
begin
  // Ничего не делаем
end;

function TRayBaseProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRayBaseProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // Ничего не делаем
end;

function TRayBaseProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TRayProjectSourceFile.Create(GetProjectSource);
end;

procedure TRayBaseProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  // Ничего не делаем
end;

function TRayBaseProjectCreator.GetProjectPersonality: string;
begin
  Result := sProjectPersonality;
end;

{ Получение пути к raylib }
(*function TRayBaseProjectCreator.GetRaylibPath: string;
begin
  Result := '';

  Result := GetEnvironmentVariable('RAY4LAZ_PATH');
  if (Result <> '') and DirectoryExists(Result) then
  begin
    //Result := '';
    Result := IncludeTrailingPathDelimiter(Result) + 'source';

    (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(result);
    if DirectoryExists(Result) then
      Exit;
  end;

  { Сообщаем пользователю, что используется путь по умолчанию }
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
  'Please ensure raylib source files are there or set RAY4LAZ_PATH environment variable.');
end;
*)

function TRayBaseProjectCreator.GetRaylibPath: string;
var
  EnvPath: string;
begin
  Result := '';

  { Получаем путь из переменной окружения }
  EnvPath := GetEnvironmentVariable('RAY4LAZ_PATH');

  { Отладочная информация }
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
    'RAY4LAZ_PATH = "' + EnvPath + '"');

  { Проверяем существование пути }
  if (EnvPath <> '') and DirectoryExists(EnvPath) then
  begin
    { Проверяем наличие подпапки source }
    Result := IncludeTrailingPathDelimiter(EnvPath) + 'source';

    if DirectoryExists(Result) then
    begin
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Found raylib source at: ' + Result);
      Exit;
    end
    else
    begin
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Warning: "source" subdirectory not found in ' + EnvPath);
    end;
  end
  else
  begin
    (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
      'Warning: RAY4LAZ_PATH directory does not exist: ' + EnvPath);
  end;

  { Сообщаем пользователю, что используется путь по умолчанию }
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
    'Please ensure raylib source files are there or set RAY4LAZ_PATH environment variable.');
end;


{ Установка опций проекта и переключение на Release }
procedure TRayBaseProjectCreator.SetProjectOptions(const Project: IOTAProject);
var
  ProjectOptions: IOTAProjectOptions;
  ProjectOptionsConfig: IOTAProjectOptionsConfigurations;
  RaylibPath: string;
  I: Integer;
  Config: IOTABuildConfiguration;
  ReleaseConfig: IOTABuildConfiguration;
begin
  if Project = nil then Exit;

  RaylibPath := GetRaylibPath;
  ProjectOptions := Project.ProjectOptions;

  if ProjectOptions = nil then Exit;

  { Устанавливаем путь к raylib }
  ProjectOptions.Values['UnitDir'] := RaylibPath;
   // ProjectOptions.Values['OutputDir'] := '';
  { Пытаемся переключить на Release конфигурацию }
  if Supports(ProjectOptions, IOTAProjectOptionsConfigurations, ProjectOptionsConfig) then
  begin
    ReleaseConfig := nil;

    { Ищем конфигурацию Release }
    for I := 0 to ProjectOptionsConfig.ConfigurationCount - 1 do
    begin
      Config := ProjectOptionsConfig.Configurations[I];
      if Config <> nil then
      begin
        { Проверяем имя конфигурации }
        if CompareText(Config.Name, 'Release') = 0 then
        begin
          ReleaseConfig := Config;
          Break;
        end;
      end;
    end;

    { Если нашли Release, делаем её активной }
    if ReleaseConfig <> nil then
    begin
      ProjectOptionsConfig.ActiveConfiguration := ReleaseConfig;
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Ray4Delphi: Switched to Release configuration');

      // Убедимся, что путь к raylib установлен и в активной конфигурации
      ProjectOptions.Values['UnitDir'] := RaylibPath;
      //   ProjectOptions.Values['OutputDir'] := '';
      //
    end
    else
    begin
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Ray4Delphi: Release configuration not found, keeping current');
    end;
  end;

  ProjectOptions.ModifiedState := True;
end;

{ TRaySimpleProjectCreator }

function TRaySimpleProjectCreator.GetProjectFileName: string;
begin
  if ProjectDirectorySelected = '' then
    ProjectDirectorySelected := GetCurrentProjectDirectory;

  ProjectDirectorySelected := IncludeTrailingPathDelimiter(ProjectDirectorySelected);
  Result := GenerateUniqueProjectName('raylib_game', ProjectDirectorySelected);
end;

function TRaySimpleProjectCreator.GetProjectSource: string;
var
  Source: string;
  Plat: string;
begin
  Plat := TRayPlatformAlias.GetPlatformName;
  Source :=
    'program ' + ExtractFileName(ChangeFileExt(GetProjectFileName, '')) + ';' + sLineBreak +
    sLineBreak +
    '{$APPTYPE CONSOLE}' + sLineBreak +
    '{$R *.res}' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils,' + sLineBreak +
    '  raylib;' + sLineBreak +
    sLineBreak +
    'const' + sLineBreak +
    '  screenWidth = 800;' + sLineBreak +
    '  screenHeight = 450;' + sLineBreak +
    sLineBreak +
    'begin' + sLineBreak +
    '  // Initialization' + sLineBreak +
    '  InitWindow(screenWidth, screenHeight, ''raylib - simple project'');' + sLineBreak +
    '  SetTargetFPS(60);' + sLineBreak +
    sLineBreak +
    '  // Main game loop' + sLineBreak +
    '  while not WindowShouldClose() do' + sLineBreak +
    '  begin' + sLineBreak +
    '    // Update' + sLineBreak +
    '    // TODO: Add update logic here' + sLineBreak +
    sLineBreak +
    '    // Draw' + sLineBreak +
    '    BeginDrawing();' + sLineBreak +
    '      ClearBackground(RAYWHITE);' + sLineBreak +
    '      DrawText(''Congrats! You created your first raylib window in Delphi!'', 190, 200, 20, LIGHTGRAY);' + sLineBreak +
    '    EndDrawing();' + sLineBreak +
    '  end;' + sLineBreak +
    sLineBreak +
    '  // De-Initialization' + sLineBreak +
    '  CloseWindow();' + sLineBreak +
    'end.' + sLineBreak;

  {$IFDEF WIN64}
  if Plat = 'Win64' then
    Source := StringReplace(Source, '{$APPTYPE CONSOLE}', '{$APPTYPE CONSOLE}' + sLineBreak + '{$SetPEFlags $20}', []);
  {$ENDIF}

  Result := Source;
end;

procedure TRaySimpleProjectCreator.SetProjectOptions(const Project: IOTAProject);
begin
  inherited;
end;

{ TRayCustomAppProjectCreator }

function TRayCustomAppProjectCreator.GetProjectFileName: string;
begin
  if ProjectDirectorySelected = '' then
    ProjectDirectorySelected := GetCurrentProjectDirectory;

  ProjectDirectorySelected := IncludeTrailingPathDelimiter(ProjectDirectorySelected);
  Result := GenerateUniqueProjectName('raylib_custom', ProjectDirectorySelected);
end;

function TRayCustomAppProjectCreator.GetProjectSource: string;
var
  Source: string;
  Plat: string;
begin
  Plat := TRayPlatformAlias.GetPlatformName;
  Source :=
    'program ' + ExtractFileName(ChangeFileExt(GetProjectFileName, '')) + ';' + sLineBreak +
    sLineBreak +
    '{$APPTYPE CONSOLE}' + sLineBreak +
    '{$R *.res}' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils,' + sLineBreak +
    '  raylib;' + sLineBreak +
    sLineBreak +
    'type' + sLineBreak +
    '  TRayApplication = class' + sLineBreak +
    '  public' + sLineBreak +
    '    constructor Create;' + sLineBreak +
    '    destructor Destroy; override;' + sLineBreak +
    '    procedure Run;' + sLineBreak +
    '    procedure DoRun; virtual;' + sLineBreak +
    '  end;' + sLineBreak +
    sLineBreak +
    'const' + sLineBreak +
    '  AppTitle = ''raylib - custom application'';' + sLineBreak +
    sLineBreak +
    '{ TRayApplication }' + sLineBreak +
    sLineBreak +
    'constructor TRayApplication.Create;' + sLineBreak +
    'begin' + sLineBreak +
    '  inherited;' + sLineBreak +
    '  InitWindow(800, 600, AppTitle);' + sLineBreak +
    '  SetTargetFPS(60);' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'destructor TRayApplication.Destroy;' + sLineBreak +
    'begin' + sLineBreak +
    '  CloseWindow();' + sLineBreak +
    '  TraceLog(LOG_INFO, ''Application closed'');' + sLineBreak +
    '  inherited;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'procedure TRayApplication.DoRun;' + sLineBreak +
    'begin' + sLineBreak +
    '  while not WindowShouldClose() do' + sLineBreak +
    '  begin' + sLineBreak +
    '    BeginDrawing();' + sLineBreak +
    '      ClearBackground(RAYWHITE);' + sLineBreak +
    '      DrawText(''Delphi + raylib = awesome!'', 190, 200, 20, DARKGRAY);' + sLineBreak +
    '    EndDrawing();' + sLineBreak +
    '  end;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'procedure TRayApplication.Run;' + sLineBreak +
    'begin' + sLineBreak +
    '  DoRun;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'var' + sLineBreak +
    '  Application: TRayApplication;' + sLineBreak +
    'begin' + sLineBreak +
    '  Application := TRayApplication.Create;' + sLineBreak +
    '  try' + sLineBreak +
    '    Application.Run;' + sLineBreak +
    '  finally' + sLineBreak +
    '    Application.Free;' + sLineBreak +
    '  end;' + sLineBreak +
    'end.' + sLineBreak;

  {$IFDEF WIN64}
  if Plat = 'Win64' then
    Source := StringReplace(Source, '{$APPTYPE CONSOLE}', '{$APPTYPE CONSOLE}' + sLineBreak + '{$SetPEFlags $20}', []);
  {$ENDIF}

  Result := Source;
end;

procedure TRayCustomAppProjectCreator.SetProjectOptions(const Project: IOTAProject);
begin
  inherited;
end;

{ TRayProjectSourceFile }

constructor TRayProjectSourceFile.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

function TRayProjectSourceFile.GetSource: string;
begin
  Result := FSource;
end;

function TRayProjectSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

{ TRaySimpleRepositoryWizard }

function TRaySimpleRepositoryWizard.GetIDString: string;
begin
  Result := sRaySimpleIDString;
end;

function TRaySimpleRepositoryWizard.GetName: string;
begin
  Result := sRaySimpleProject;
end;

function TRaySimpleRepositoryWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;


{ Получение пути к raylib }
function GetRaylibPath: string;
begin
  Result := '';

  Result := GetEnvironmentVariable('RAY4LAZ_PATH');
  if (Result <> '') and DirectoryExists(Result) then
  begin
    Result := IncludeTrailingPathDelimiter(Result);
    if DirectoryExists(Result) then
      Exit;
  end;
end;

procedure TRaySimpleRepositoryWizard.Execute;
var
  Project: IOTAProject;
  ModuleServices: IOTAModuleServices;
  Creator: TRaySimpleProjectCreator;
  Sr, Ds: String;
begin


  Creator := TRaySimpleProjectCreator.Create('', TRayPlatformAlias.GetPlatformName);
  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  { Создаем модуль и получаем созданный проект }
  Project := ModuleServices.CreateModule(Creator) as IOTAProject;

  { Устанавливаем пути к raylib и переключаем на Release }
  if Project <> nil then
    Creator.SetProjectOptions(Project);



    Sr := GetRaylibPath + 'libs\x86_64-win64\libraylib.dll';
    Ds := GetCurrentProjectDirectory + 'Win64\Release\' + 'libraylib.dll';

    if not DirectoryExists(GetCurrentProjectDirectory+'Win64') then
     CreateDir(GetCurrentProjectDirectory+'Win64');

    if not DirectoryExists(GetCurrentProjectDirectory+'Win64\Release') then
      CreateDir(GetCurrentProjectDirectory+'Win64\Release');

    CopyFile(PWideChar(Sr), PWideChar(Ds), True);

    Sr := GetRaylibPath + 'libs\i686-win32\libraylib.dll';
    Ds := GetCurrentProjectDirectory + 'Win32\Release\' + 'libraylib.dll';

    if not DirectoryExists(GetCurrentProjectDirectory+'Win32') then
      CreateDir(GetCurrentProjectDirectory+'Win32');

    if not DirectoryExists(GetCurrentProjectDirectory+'Win32\Release') then
      CreateDir(GetCurrentProjectDirectory+'Win32\Release');


    CopyFile(PWideChar(Sr), PWideChar(Ds), True);
  Project.MarkModified;

  //GetRaylibPath

  Project.Save(False,True);
  Project.MarkModified;
  Project.Show;
end;

function TRaySimpleRepositoryWizard.GetAuthor: string;
begin
  Result := sRayAuthor;
end;

function TRaySimpleRepositoryWizard.GetComment: string;
begin
  Result := sRaySimpleProjectDesc;
end;

function TRaySimpleRepositoryWizard.GetPage: string;
begin
  Result := sRayPage;
end;

function TRaySimpleRepositoryWizard.GetGlyph: THandle;
begin
  { Загрузка иконки с указанным именем }
  Result := LoadIcon(HInstance, 'RAY_ICON');

  { Если иконка не найдена, используем стандартную }
  if Result = 0 then
    Result := LoadIcon(0, IDI_APPLICATION);
end;

function TRaySimpleRepositoryWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TRaySimpleRepositoryWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{ IOTARepositoryWizard160 для TRaySimpleRepositoryWizard }
function TRaySimpleRepositoryWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  CategoryManager: IOTAGalleryCategoryManager;
  RootCategory: IOTAGalleryCategory;
begin
  Result := nil;

  { Получаем менеджер категорий }
  if Supports(BorlandIDEServices, IOTAGalleryCategoryManager, CategoryManager) then
  begin
    { Получаем корневую категорию "Projects" }
    RootCategory := CategoryManager.FindCategory(sProjectsCategoryName);

    if RootCategory <> nil then
    begin
      { Пытаемся найти существующую категорию для Ray4Delphi }
      Result := CategoryManager.FindCategory('Ray4Delphi.Projects');

      { Если категория не найдена, создаем новую }
      if Result = nil then
        Result := CategoryManager.AddCategory(RootCategory, 'Ray4Delphi.Projects', 'Ray4Delphi Projects');
    end;
  end;
end;

function TRaySimpleRepositoryWizard.GetFrameworkTypes: TArray<string>;
begin
  { Возвращаем пустой массив - не используем фреймворки }
  Result := nil;
end;

function TRaySimpleRepositoryWizard.GetPlatforms: TArray<string>;
begin
  { Поддерживаемые платформы }
  Result := TArray<string>.Create('Win64', 'Linux64');
end;

procedure TRaySimpleRepositoryWizard.SetSelectedPersonality(const APersonality: string);
begin
  { Не требуется для нашей реализации }
end;

procedure TRaySimpleRepositoryWizard.SetSelectedFrameworkType(const AFrameworkType: string);
begin
  { Не требуется для нашей реализации }
end;

procedure TRaySimpleRepositoryWizard.SetSelectedPlatform(const APlatform: string);
begin
 //ShowMessage(APlatform);
end;

{ TRayCustomAppRepositoryWizard }

function TRayCustomAppRepositoryWizard.GetIDString: string;
begin
  Result := sRayCustomIDString;
end;

function TRayCustomAppRepositoryWizard.GetName: string;
begin
  Result := sRayCustomAppProject;
end;

function TRayCustomAppRepositoryWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TRayCustomAppRepositoryWizard.Execute;
var
  Project: IOTAProject;
  ModuleServices: IOTAModuleServices;
  Creator: TRayCustomAppProjectCreator;
  sr, ds: String;
begin


  Creator := TRayCustomAppProjectCreator.Create('', TRayPlatformAlias.GetPlatformName);
  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  { Создаем модуль и получаем созданный проект }
  Project := ModuleServices.CreateModule(Creator) as IOTAProject;

  { Устанавливаем пути к raylib и переключаем на Release }
  if Project <> nil then
    Creator.SetProjectOptions(Project);

  Project.ProjectOptions.ModifiedState := True;
  Project.Save(False,True);


  Sr := GetRaylibPath + 'libs\x86_64-win64\libraylib.dll';
  Ds := GetCurrentProjectDirectory + 'Win64\Release\' + 'libraylib.dll';

  if not DirectoryExists(GetCurrentProjectDirectory+'Win64') then
    CreateDir(GetCurrentProjectDirectory+'Win64');

  if not DirectoryExists(GetCurrentProjectDirectory+'Win64\Release') then
    CreateDir(GetCurrentProjectDirectory+'Win64\Release');

  CopyFile(PWideChar(Sr), PWideChar(Ds), True);

  Sr := GetRaylibPath + 'libs\i686-win32\libraylib.dll';
  Ds := GetCurrentProjectDirectory + 'Win32\Release\' + 'libraylib.dll';

  if not DirectoryExists(GetCurrentProjectDirectory+'Win32') then
    CreateDir(GetCurrentProjectDirectory+'Win32');

  if not DirectoryExists(GetCurrentProjectDirectory+'Win32\Release') then
    CreateDir(GetCurrentProjectDirectory+'Win32\Release');


  CopyFile(PWideChar(Sr), PWideChar(Ds), True);

  Project.Save(False,True);
  Project.MarkModified;
  Project.Show;

end;

function TRayCustomAppRepositoryWizard.GetAuthor: string;
begin
  Result := sRayAuthor;
end;

function TRayCustomAppRepositoryWizard.GetComment: string;
begin
  Result := sRayCustomAppProjectDesc;
end;

function TRayCustomAppRepositoryWizard.GetPage: string;
begin
  Result := sRayPage;
end;

function TRayCustomAppRepositoryWizard.GetGlyph: THandle;
begin
  { Загрузка иконки с указанным именем }
  Result := LoadIcon(HInstance, 'RAY_ICON');

  { Если иконка не найдена, можно загрузить стандартную }
  if Result = 0 then
    Result := LoadIcon(0, IDI_APPLICATION);
end;

function TRayCustomAppRepositoryWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TRayCustomAppRepositoryWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

{ IOTARepositoryWizard160 для TRayCustomAppRepositoryWizard }
function TRayCustomAppRepositoryWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  CategoryManager: IOTAGalleryCategoryManager;
  RootCategory: IOTAGalleryCategory;
begin
  Result := nil;

  { Получаем менеджер категорий }
  if Supports(BorlandIDEServices, IOTAGalleryCategoryManager, CategoryManager) then
  begin
    { Получаем корневую категорию "Projects" }
    RootCategory := CategoryManager.FindCategory(sProjectsCategoryName);

    if RootCategory <> nil then
    begin
      { Пытаемся найти существующую категорию для Ray4Delphi }
      Result := CategoryManager.FindCategory('Ray4Delphi.Projects');

      { Если категория не найдена, создаем новую }
      if Result = nil then
        Result := CategoryManager.AddCategory(RootCategory, 'Ray4Delphi.Projects', 'Ray4Delphi Projects');
    end;
  end;
end;

function TRayCustomAppRepositoryWizard.GetFrameworkTypes: TArray<string>;
begin
  { Возвращаем пустой массив - не используем фреймворки }
  Result := nil;
end;

function TRayCustomAppRepositoryWizard.GetPlatforms: TArray<string>;
begin
  { Поддерживаемые платформы }
  Result := TArray<string>.Create('Win64','Linux64');
end;

procedure TRayCustomAppRepositoryWizard.SetSelectedPersonality(const APersonality: string);
begin
  { Не требуется для нашей реализации }
end;

procedure TRayCustomAppRepositoryWizard.SetSelectedFrameworkType(const AFrameworkType: string);
begin
  { Не требуется для нашей реализации }
end;

procedure TRayCustomAppRepositoryWizard.SetSelectedPlatform(const APlatform: string);
begin
  { Не требуется для нашей реализации }
end;


{ TRayPlatformAlias }

class function TRayPlatformAlias.GetPlatformName: string;
begin
  {$IFDEF WIN32}
  Result := 'Win32';
  {$ELSE}
  Result := 'Win64';
  {$ENDIF}
end;

{ Register procedure }

procedure Register;
begin

  { Регистрируем два пункта для диалога File > New > Other (репозиторий) }
  if SimpleWizardIndex = -1 then
    SimpleWizardIndex := (BorlandIDEServices as IOTAWizardServices).AddWizard(TRaySimpleRepositoryWizard.Create);

  if CustomWizardIndex = -1 then
    CustomWizardIndex := (BorlandIDEServices as IOTAWizardServices).AddWizard(TRayCustomAppRepositoryWizard.Create);


end;

initialization
  SimpleWizardIndex := -1;
  CustomWizardIndex := -1;

  GalleryCategory := nil;
  ProjectDirectorySelected := '';  // Сбрасываем при старте

finalization
  if SimpleWizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(SimpleWizardIndex);

  if CustomWizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(CustomWizardIndex);

  GalleryCategory := nil;
  ProjectDirectorySelected := '';  // Сбрасываем при завершении
end.
