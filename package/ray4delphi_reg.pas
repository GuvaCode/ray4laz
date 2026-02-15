unit ray4delphi_reg;

interface

uses
  System.SysUtils, System.Classes, ToolsAPI, DesignIntf, DesignEditors,
  Vcl.Forms, PlatformAPI, Vcl.Menus, Vcl.ActnList, Vcl.Graphics,
  Vcl.Dialogs, System.UITypes;

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
  end;

  { Создатель проекта raylib с TCustomApplication }
  TRayCustomAppProjectCreator = class(TRayBaseProjectCreator)
  protected
    function GetProjectFileName: string; override;
    function GetProjectSource: string; override;
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

  { Визард для пункта меню Tools }
  TShaderToysMenuWizard = class(TNotifierObject, IOTAWizard)
  private
    FMenuItem: TMenuItem;
    procedure ShaderToysClick(Sender: TObject);
  protected
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
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
  sShaderToysMenuText = 'ShaderToys converter';
  sShaderToysIDString = 'Ray4Delphi.ShaderToysWizard';
  sShaderToysMessage = 'Coming Soon';

const
  sProjectPersonality = sDelphiPersonality;
  sProjectsCategoryName = 'Projects'; // Имя корневой категории проектов

var
  SimpleWizardIndex: Integer = -1;
  CustomWizardIndex: Integer = -1;
  ShaderToysWizardIndex: Integer = -1;

  { Категория для галереи Welcome Page }
  GalleryCategory: IOTAGalleryCategory = nil;

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

function GenerateUniqueProjectName(const BaseName: string): string;
var
  ProjectGroup: IOTAProjectGroup;
  ProjectDir: string;
  i: Integer;
  ProjectFileName: string;
  ExistingProjects: TArray<string>;
  IsUnique: Boolean;
begin
  Result := '';


  ProjectGroup := ProjectGroup;
  if Assigned(ProjectGroup) then
  begin
    ProjectDir := ExtractFilePath(ProjectGroup.FileName);
    if ProjectDir = '' then
      ProjectDir := GetCurrentDir;
  end
  else
    ProjectDir := GetCurrentDir;

  ProjectDir := IncludeTrailingPathDelimiter(ProjectDir);

  i := 1;
  while Result = '' do
  begin
    ProjectFileName := ProjectDir + BaseName + IntToStr(i) + '.dpr';

    { Проверяем уникальность имени }
    IsUnique := True;

    { Проверяем существование файла на диске }
    if TFile.Exists(ProjectFileName) then
      IsUnique := False
    else
    begin
      { Проверяем, не используется ли это имя в открытых проектах }
      for var ExistingFile in ExistingProjects do
      begin
        if CompareText(ExistingFile, ProjectFileName) = 0 then
        begin
          IsUnique := False;
          Break;
        end;
      end;
    end;

    if IsUnique then
      Result := ProjectFileName
    else
      Inc(i);

    { Предотвращаем бесконечный цикл }
    if i > 1000 then
    begin
      Result := ProjectDir + BaseName + IntToStr(Random(1000)) + '.dpr';
      Break;
    end;
  end;
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
function TRayBaseProjectCreator.GetRaylibPath: string;
var
  Registry: TRegistry;
  PossiblePaths: TArray<string>;
  Path: string;
  PackageServices: IOTAPackageServices;
  I: Integer;
  PackageInfo: IOTAPackageInfo;
  PackagePath: string;
begin
  Result := '';

  { Вариант 1: Получаем путь из пакета Ray4Delphi через PackageServices }
  if Supports(BorlandIDEServices, IOTAPackageServices, PackageServices) then
  begin
    for I := 0 to PackageServices.PackageCount - 1 do
    begin
      PackageInfo := PackageServices.Package[I];
      if PackageInfo <> nil then
      begin
        { Проверяем, что это наш пакет Ray4Delphi }
        if (Pos('ray4delphi', LowerCase(PackageInfo.GetName)) > 0) or
           (Pos('ray4delphi', LowerCase(PackageInfo.GetFileName)) > 0) then
        begin
          { Получаем путь к папке с пакетом }
          PackagePath := ExtractFilePath(PackageInfo.GetFileName);

          { Для отладки - показываем найденный путь }
          (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
            'Ray4Delphi: Found package at ' + PackagePath);

          { Формируем путь к исходникам: ..\source относительно папки пакета }
          Result := IncludeTrailingPathDelimiter(
            ExtractFilePath(ExcludeTrailingPathDelimiter(PackagePath))) + 'source';

          { Проверяем существует ли папка source }
          if DirectoryExists(Result) then
          begin
            (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
              'Ray4Delphi: Found source at ' + Result);
            Exit;
          end
          else
          begin
            { Если source не найден, пробуем другие варианты }
            Result := '';
          end;
        end;
      end;
    end;
  end;

  { Вариант 2: Ищем через ModuleServices как запасной вариант }
  if Result = '' then
  begin
    var ModuleServices: IOTAModuleServices;
    if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    begin
      for var j := 0 to ModuleServices.ModuleCount - 1 do
      begin
        if Supports(ModuleServices.Modules[j], IOTAPackageInfo, PackageInfo) then
        begin
          if (Pos('ray4delphi', LowerCase(PackageInfo.GetName)) > 0) or
             (Pos('ray4delphi', LowerCase(PackageInfo.GetFileName)) > 0) then
          begin
            PackagePath := ExtractFilePath(PackageInfo.GetFileName);
            Result := IncludeTrailingPathDelimiter(
              ExtractFilePath(ExcludeTrailingPathDelimiter(PackagePath))) + 'source';

            if DirectoryExists(Result) then
              Exit
            else
              Result := '';
          end;
        end;
      end;
    end;
  end;

  { Вариант 3: Проверяем переменную окружения RAYLIB_PATH }
  Result := GetEnvironmentVariable('RAYLIB_PATH');
  if (Result <> '') and DirectoryExists(Result) then
  begin
    Result := IncludeTrailingPathDelimiter(Result) + 'src';
    if DirectoryExists(Result) then
      Exit;
  end;

  { Вариант 4: Проверяем типичные места установки }
  PossiblePaths := TArray<string>.Create(
    'C:\raylib\src',
    'C:\Program Files\raylib\src',
    'C:\Program Files (x86)\raylib\src',
    'C:\Dev\raylib\src',
    'C:\Projects\raylib\src',
    'D:\raylib\src',
    ExtractFilePath(ParamStr(0)) + 'raylib\src'
  );

  for Path in PossiblePaths do
  begin
    if DirectoryExists(Path) then
    begin
      Result := Path;
      Exit;
    end;
  end;

  { Вариант 5: Ищем в реестре (для установленных версий) }
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('SOFTWARE\raylib') or
       Registry.OpenKeyReadOnly('SOFTWARE\Wow6432Node\raylib') then
    begin
      Result := Registry.ReadString('InstallDir');
      Registry.CloseKey;
      if (Result <> '') and DirectoryExists(Result) then
      begin
        Result := IncludeTrailingPathDelimiter(Result) + 'src';
        if DirectoryExists(Result) then
          Exit;
      end;
    end;
  finally
    Registry.Free;
  end;

  { Если ничего не нашли, используем путь по умолчанию }
  Result := 'C:\Project\ray4laz\source'; // Путь по умолчанию

  { Сообщаем пользователю, что используется путь по умолчанию }
  (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
    'Ray4Delphi: Using default source path ' + Result +
    '. Please ensure raylib source files are there or set RAYLIB_PATH environment variable.');
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
     // ;
        //ProjectOptions.Values['UnitDir'] := RaylibPath;
        { Проверяем имя конфигурации }
      //  if CompareText(Config.Name, '') = 0 then
      //  begin
          ReleaseConfig := Config;
       //   Break;
       // end;

         ProjectOptionsConfig.ActiveConfiguration := ReleaseConfig;
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Ray4Delphi: Switched to Release configuration');

       ProjectOptions.Values['UnitDir'] := RaylibPath;


      end;
    end;

    { Если нашли Release, делаем её активной }
    if ReleaseConfig <> nil then
    begin
      ProjectOptionsConfig.ActiveConfiguration := ReleaseConfig;
      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
        'Ray4Delphi: Switched to Release configuration');

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
//  Result := GenerateUniqueProjectName('raylib_game', self.GetProjectFileName);
   Result := GenerateUniqueProjectName('raylib_game');
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

{ TRayCustomAppProjectCreator }

function TRayCustomAppProjectCreator.GetProjectFileName: string;
begin
  Result := GenerateUniqueProjectName('raylib_custom');
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

procedure TRaySimpleRepositoryWizard.Execute;
var
  Project: IOTAProject;
  ModuleServices: IOTAModuleServices;
  Creator: TRaySimpleProjectCreator;
begin
  Creator := TRaySimpleProjectCreator.Create('', TRayPlatformAlias.GetPlatformName);
  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  { Создаем модуль и получаем созданный проект }
  Project := ModuleServices.CreateModule(Creator) as IOTAProject;

  { Устанавливаем пути к raylib и переключаем на Release }
  if Project <> nil then
    Creator.SetProjectOptions(Project);

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
begin
  Creator := TRayCustomAppProjectCreator.Create('', TRayPlatformAlias.GetPlatformName);
  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  { Создаем модуль и получаем созданный проект }
  Project := ModuleServices.CreateModule(Creator) as IOTAProject;

  { Устанавливаем пути к raylib и переключаем на Release }
  if Project <> nil then
    Creator.SetProjectOptions(Project);

  Project.ProjectOptions.ModifiedState := True;
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

{ TShaderToysMenuWizard }

constructor TShaderToysMenuWizard.Create;
var
  NTAServices: INTAServices;
  ToolsMenu: TMenuItem;
  i: Integer;
begin
  inherited Create;
  FMenuItem := nil;

  { Добавляем пункт в меню Tools }
  if Supports(BorlandIDEServices, INTAServices, NTAServices) then
  begin
    { Ищем меню Tools }
    for i := 0 to NTAServices.MainMenu.Items.Count - 1 do
    begin
      if NTAServices.MainMenu.Items[i].Name = 'ToolsMenu' then
      begin
        ToolsMenu := NTAServices.MainMenu.Items[i];

        { Создаем пункт меню }
        FMenuItem := TMenuItem.Create(ToolsMenu);
        FMenuItem.Caption := sShaderToysMenuText;
        FMenuItem.OnClick := ShaderToysClick;
        ToolsMenu.Add(FMenuItem);
        Break;
      end;
    end;
  end;
end;

destructor TShaderToysMenuWizard.Destroy;
begin
  { Удаляем пункт меню }
  if FMenuItem <> nil then
    FMenuItem.Free;
  inherited;
end;

procedure TShaderToysMenuWizard.ShaderToysClick(Sender: TObject);
begin
  ShowMessage(sShaderToysMessage);
end;

function TShaderToysMenuWizard.GetIDString: string;
begin
  Result := sShaderToysIDString;
end;

function TShaderToysMenuWizard.GetName: string;
begin
  Result := sShaderToysMenuText;
end;

function TShaderToysMenuWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TShaderToysMenuWizard.Execute;
begin
  { Этот метод вызывается при выборе визарда в меню Help }
  { Но мы используем свой обработчик OnClick }
end;

{ TRayPlatformAlias }

class function TRayPlatformAlias.GetPlatformName: string;
begin
 //Result := 'Win64';
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

  { Регистрируем пункт меню ShaderToys converter }
  if ShaderToysWizardIndex = -1 then
    ShaderToysWizardIndex := (BorlandIDEServices as IOTAWizardServices).AddWizard(TShaderToysMenuWizard.Create);
end;

initialization
  SimpleWizardIndex := -1;
  CustomWizardIndex := -1;
  ShaderToysWizardIndex := -1;

  GalleryCategory := nil;

finalization
  if SimpleWizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(SimpleWizardIndex);

  if CustomWizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(CustomWizardIndex);

  if ShaderToysWizardIndex <> -1 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(ShaderToysWizardIndex);

  { Очищаем ссылку на категорию }
  GalleryCategory := nil;
end.
