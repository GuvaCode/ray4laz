unit ray4laz_simplePrj;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, LazIDEIntf, ProjectIntf, MenuIntf, SrcEditorIntf;

type
  { TRay4LazSimpleProjectDescriptor }
  TRay4LazSimpleProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  procedure Register;

  resourcestring
  rsAboutSimplePrj = 'A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)';
  rsNameSimplePrj  = 'raylib simple project';

implementation

procedure Register;
begin
 RegisterProjectDescriptor(TRay4LazSimpleProjectDescriptor.Create);
end;


{ TRay4LazSimpleProjectDescriptor }
constructor TRay4LazSimpleProjectDescriptor.Create;
begin
  inherited Create;
  Name := rsNameSimplePrj;
  Flags := Flags -[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement] + [pfUseDefaultCompilerOptions];
end;

function TRay4LazSimpleProjectDescriptor.GetLocalizedName: string;
begin
  Result:= rsNameSimplePrj;
end;

function TRay4LazSimpleProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:= rsAboutSimplePrj;
end;

function TRay4LazSimpleProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  Source: string;
  MainFile: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  MainFile := AProject.CreateProjectFile('game.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;
  Source:='program Game;' + LineEnding +
  {$IFDEF WINDOWS}
  LineEnding +
  {$IFDEF CPU32}'//note: copy the raylib.dll file from the ''ray4laz/libs/x86_32-windows'' folder to your project folder.' +{$ENDIF}
  {$IFDEF CPU64}'//note: copy the raylib.dll file from the ''ray4laz/libs/x86_64-windows'' folder to your project folder.' +{$ENDIF}
  {$ENDIF}
    LineEnding +
    '{$mode objfpc}{$H+}' + LineEnding +
    LineEnding +
    'uses ' + LineEnding +
    'cmem, ' + LineEnding +
    '{uncomment if necessary}' + LineEnding +
    '//raymath, ' + LineEnding +
    '//rlgl, ' + LineEnding +
    'raylib; ' + LineEnding + LineEnding +
    'const' +  LineEnding +
    '  screenWidth = 800;'+ LineEnding +
    '  screenHeight = 450;'+ LineEnding  + LineEnding +
    'begin' + LineEnding +
    '  // Initialization'+ LineEnding +
    '  InitWindow(screenWidth, screenHeight, ''raylib - simple project'');'+ LineEnding +
    '  SetTargetFPS(60);// Set our game to run at 60 frames-per-second'+ LineEnding +  LineEnding +

    '  // Main game loop'+ LineEnding +
    '  while not WindowShouldClose() do'+ LineEnding +
    '    begin'+ LineEnding +
    '      // Update'+ LineEnding +
    '      // TODO: Update your variables here'+ LineEnding + LineEnding +
    '      // Draw'+ LineEnding +
    '      BeginDrawing();'+ LineEnding +
    '        ClearBackground(RAYWHITE);'+ LineEnding +
    '        DrawText(''Congrats! You created your first window!'', 190, 200, 20, LIGHTGRAY);'+ LineEnding +
    '      EndDrawing();'+ LineEnding +
    '    end;'+ LineEnding + LineEnding +

    '  // De-Initialization'+ LineEnding +
    '  CloseWindow();        // Close window and OpenGL context'+ LineEnding +

    'end.'+ LineEnding + LineEnding;

  AProject.MainFile.SetSourceText(Source);
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)';// + PathDelim+ 'ray4laz_dsgn';
  AProject.LazCompilerOptions.TargetFilename:= 'game';
  AProject.AddPackageDependency('ray4laz');
end;

function TRay4LazSimpleProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=inherited CreateStartFiles(AProject);
end;

end.

