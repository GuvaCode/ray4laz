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
  rsNameSimplePrj  = 'Raylib Simple Project';

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
    '  //--------------------------------------------------------------------------------------' + LineEnding +
    '  InitWindow(screenWidth, screenHeight, ''raylib - simple project'');'+ LineEnding +
    '  SetTargetFPS(60);// Set our game to run at 60 frames-per-second'+ LineEnding +
    '  //--------------------------------------------------------------------------------------'+ LineEnding +
    '  // Main game loop'+ LineEnding +
    '  while not WindowShouldClose() do'+ LineEnding +
    '    begin'+ LineEnding +
    '      // Update'+ LineEnding +
    '      //----------------------------------------------------------------------------------'+ LineEnding +
    '      // TODO: Update your variables here'+ LineEnding +
    '      //----------------------------------------------------------------------------------'+ LineEnding + LineEnding +
    '      // Draw'+ LineEnding +
    '      //----------------------------------------------------------------------------------'+ LineEnding +
    '      BeginDrawing();'+ LineEnding +
    '        ClearBackground(RAYWHITE);'+ LineEnding +
    '        DrawText(''raylib in lazarus !!!'', 20, 20, 10, DARKGRAY);'+ LineEnding +
    '      EndDrawing();'+ LineEnding +
    '    end;'+ LineEnding +
    '  // De-Initialization'+ LineEnding +
    '  //--------------------------------------------------------------------------------------'+ LineEnding +
    '  CloseWindow();        // Close window and OpenGL context'+ LineEnding +
    '  //--------------------------------------------------------------------------------------'+ LineEnding +
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

