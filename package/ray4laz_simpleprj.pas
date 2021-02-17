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
  Result:=GetLocalizedName + LineEnding +  LineEnding + rsAboutSimplePrj;
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
    'uses ' +'cmem, ray_header, math;' + LineEnding +  LineEnding +
    'const' +  LineEnding +
    ' screenWidth = 800;'+ LineEnding +
    ' screenHeight = 450;'+ LineEnding  + LineEnding +
    'begin' + LineEnding +
    '{$IFDEF DARWIN}' + LineEnding +
    'SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);' + LineEnding +
    '{$IFEND}' + LineEnding + LineEnding +
    ' InitWindow(screenWidth, screenHeight, ''raylib pascal - basic window'');' + LineEnding +
    ' SetTargetFPS(60);' + LineEnding + LineEnding +
    ' while not WindowShouldClose() do ' + LineEnding +
    ' begin'+ LineEnding +
    '  BeginDrawing();' + LineEnding +
    '  ClearBackground(RAYWHITE);' + LineEnding  + LineEnding +
    '  DrawText(''raylib in lazarus !!!'', 20, 20, 20, SKYBLUE);'   + LineEnding +   LineEnding +
    '  EndDrawing(); '   + LineEnding +
    ' end;' + LineEnding +
    'CloseWindow(); ' + LineEnding +
    LineEnding +
     'end.' + LineEnding + LineEnding;

  AProject.MainFile.SetSourceText(Source);
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)' + PathDelim+ 'ray4laz_dsgn';
  AProject.LazCompilerOptions.TargetFilename:= 'game';
  AProject.AddPackageDependency('ray4laz');
end;

function TRay4LazSimpleProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=inherited CreateStartFiles(AProject);
end;

end.

