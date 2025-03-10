unit ray4laz_customAppPrj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LazIDEIntf, ProjectIntf, MenuIntf, SrcEditorIntf;

type
  { TRay4LazCustAppProjectDescriptor }
  TRay4LazCustAppProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  procedure Register;

  resourcestring
  rsAboutCustAppPrj = 'A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)';
  rsNameCustAppPrj  = 'raylib project using TCustomApplication.';

implementation

procedure Register;
begin
  RegisterProjectDescriptor(TRay4LazCustAppProjectDescriptor.Create);
end;

{ TRay4LazCustAppProjectDescriptor }

constructor TRay4LazCustAppProjectDescriptor.Create;
begin
  inherited Create;
  Name := rsNameCustAppPrj;
  Flags := Flags -[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement] + [pfUseDefaultCompilerOptions];
end;

function TRay4LazCustAppProjectDescriptor.GetLocalizedName: string;
begin
  Result:= rsNameCustAppPrj;
end;

function TRay4LazCustAppProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:= rsAboutCustAppPrj;
end;

function TRay4LazCustAppProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  Source: string;
  MainFile: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  MainFile := AProject.CreateProjectFile('Project1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;


  Source:='program Project' + IntToStr(AProject.FileCount) + ';' + LineEnding +
  {$IFDEF WINDOWS}
  LineEnding +
  {$IFDEF CPU32}'//note: copy the raylib.dll file from the ''ray4laz/libs/x86_32-windows'' folder to your project folder.' + {$ENDIF}
  {$IFDEF CPU64}'//note: copy the raylib.dll file from the ''ray4laz/libs/x86_64-windows'' folder to your project folder.' + {$ENDIF}
  {$ENDIF}
   LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    LineEnding +
    'uses'  + LineEnding +
    '{$IFDEF LINUX} cthreads,{$ENDIF}'  + LineEnding +
    {$IFDEF DARWIN}
    ' CocoaAll,' + LineEnding +{$ENDIF}
    ' Classes, SysUtils, CustApp, raylib;'  + LineEnding +
    LineEnding +
    'type' + LineEnding +
    '  { TRayApplication }' + LineEnding +
    '  TRayApplication = class(TCustomApplication)' + LineEnding +
    '  protected' + LineEnding +
    '    procedure DoRun; override;' + LineEnding +
    '  public' + LineEnding +
    '    constructor Create(TheOwner: TComponent); override;' + LineEnding +
    '    destructor Destroy; override;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    '  const AppTitle = ''raylib - basic window'';' + LineEnding +
    LineEnding +
    '{ TRayApplication }' + LineEnding +
    LineEnding +
    'constructor TRayApplication.Create(TheOwner: TComponent);' + LineEnding +
    'begin' + LineEnding +
    '  inherited Create(TheOwner);' + LineEnding +
    LineEnding +
    '  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags' + LineEnding +
    LineEnding +
    '  SetTargetFPS(60); // Set our game to run at 60 frames-per-second' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'procedure TRayApplication.DoRun;' + LineEnding +
    'begin' + LineEnding +
    LineEnding +
    '  while (not WindowShouldClose) do // Detect window close button or ESC key' + LineEnding +
    '  begin' + LineEnding +
    '    // Update your variables here' + LineEnding +
    LineEnding +
    '    // Draw' + LineEnding +
    '    BeginDrawing();'  + LineEnding +
    '      ClearBackground(RAYWHITE);' + LineEnding +
    '      DrawText(''Congrats! You created your first window!'', 190, 200, 20, LIGHTGRAY);' + LineEnding +
    '    EndDrawing();' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    '  // Stop program loop' + LineEnding +
    '  Terminate;' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'destructor TRayApplication.Destroy;' + LineEnding +
    'begin' + LineEnding +
    '  // De-Initialization' + LineEnding +
    '  CloseWindow(); // Close window and OpenGL context' + LineEnding +
    LineEnding +
    '  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)' + LineEnding +
    '  TraceLog(LOG_INFO, ''your first window is close and destroy'');' + LineEnding +
    LineEnding +
    '  inherited Destroy;' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'var' + LineEnding +
    '  Application: TRayApplication;' + LineEnding +
    'begin' + LineEnding +
    '  Application:=TRayApplication.Create(nil);' + LineEnding +
    '  Application.Title:=AppTitle;' + LineEnding +
    '  Application.Run;' + LineEnding +
    '  Application.Free;'  + LineEnding +
    'end.' + LineEnding + LineEnding;

  AProject.MainFile.SetSourceText(Source);
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)';
  {$IFDEF DARWIN}
  Aproject.LazCompilerOptions.PassLinkerOptions := True;
  {$IFDEF CPUAARCH64}
  AProject.LazCompilerOptions.CustomOptions:='''-WM11.0''';
  {$ENDIF}
  AProject.LazCompilerOptions.LinkerOptions := '''-framework IOKit''';
  {$ENDIF}
  AProject.LazCompilerOptions.TargetFilename:= 'Project' + IntToStr(AProject.FileCount);
  AProject.AddPackageDependency('ray4laz');
end;

function TRay4LazCustAppProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=inherited CreateStartFiles(AProject);
end;

end.

