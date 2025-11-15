unit ray4laz_misc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LazFileUtils, fileutil,
  PackageIntf, MenuIntf, System.UITypes, Dialogs, Forms;

type
  { TEventClass }
  TEventClass = class
   procedure DoSomething(Sender:TObject);
  end;

 function  RayUsed : boolean;

procedure Register;

 var EventCode : TEventClass;
     SectionRay: TIDEMenuSection;
     SectionTool: TIDEMenuSection;
     SectionToolMenu: TIDEMenuSection;
     SectionRayMenu: TIDEMenuSection;


 resourcestring
   rsMnuMisc        = 'raylib Misc ...';
   rsHelpCheat      = 'Cheatsheet ...';
   rsInsertClr      = 'Color create from dialog';
   rsRayWiki        = 'raylib Wiki';
   rsRlTools        = 'ray4laz tools ..';
   rsCompilePkg     = 'Compilation package ray4laz';
   rsCopyLibs       = 'Copy libraries to the executable folder';
//   rsCopyDll1       = 'Copy raylib.dll';
//   rsCopyDll2       = 'Copy r3d.dll';
//   rsCopyDll3       = 'Copy raygizmo.dll';
//   rsCopyDll4       = 'Copy raymedia.dll';
   rsOnlyWin        = 'Only for windows.';
   rsOpenConfig     = 'Open configuration file';
   rsCopyTrue       = 'Copy successful';
   rsCopyFalse      = 'Copy error';
//   rsDownloadFFmpeg = 'Download the necessary ffmpeg libraries';
//   rsSSL            = 'Copy OpenSSL to Lazarus directory';
   srToy            = 'Shadertoy converter';

implementation
uses uShaderTool, IDEWindowIntf, IDEExternToolIntf, IDEMsgIntf,
  LazIDEIntf, ProjectIntf, SrcEditorIntf, MacroIntf;

function RayUsed: boolean;
var
lPkgList: TFPList; i: Integer;
begin
 result:=false;
 PackageEditingInterface.GetRequiredPackages(LazarusIDE.ActiveProject, lPkgList, [pirNotRecursive]);
 if lPkgList = nil then exit;
 try
   for i := 0 to lPkgList.Count - 1 do
   begin
     if ExtractFileName(ExtractFileNameWithoutExt(TIDEPackage(lPkgList[i]).Filename)) = 'ray4laz'
    then result:=true;
   end;
 finally
  lPkgList.Free;
 end;
end;

procedure RayFunction(Sender: TObject);
var  Editor: TSourceEditorInterface;
     Pos: TPoint;
  procedure insertXY(Text:String);
   begin
    Editor.CutToClipboard;
    Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,Text);
    Pos := editor.CursorTextXY;
    Pos.X := Pos.x-1;
    Editor.CursorTextXY := Pos;
   end;

 begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor=nil then exit;

  case (sender as TIDEMenuItem).Name of
  'Vector2Create'       : insertXY('Vector2Create()');
  'Vector2Set'          : insertXY('Vector2Set()');
  'Vector3Create'       : insertXY('Vector3Create()');
  'Vector3Set'          : insertXY('Vector3Set()');
  'Vector4Create'       : insertXY('Vector4Create()');
  'Vector4Set'          : insertXY('Vector4Set()');
  'QuaternionCreate'    : insertXY('QuaternionCreate()');
  'QuaternionSet'       : insertXY('QuaternionSet()');
  'ColorCreate'         : insertXY('ColorCreate()');
  'ColorSet'            : insertXY('ColorSet()');
  'RectangleCreate'     : insertXY('RectangleCreate()');
  'RectangleSet'        : insertXY('RectangleSet()');
  'BoundingBoxCreate'   : insertXY('BoundingBoxCreate()');
  'BoundingBoxSet'      : insertXY('BoundingBoxSet()');
  'Camera3DCreate'      : insertXY('Camera3DCreate()');
  'Camera3DSet'         : insertXY('Camera3DSet()');
  'ShowCheatsheet'      : OpenURL('https://www.raylib.com/cheatsheet/cheatsheet.html');
  'ShowWiki'            : OpenURL('https://github.com/raysan5/raylib/wiki');
  end;
 end;

procedure ShowColorDialog(Sender: TObject);
var ColorDialog: TColorDialog;
    C: TColor;
    Editor: TSourceEditorInterface;
    Txt:String;
begin
  Editor:=SourceEditorManagerIntf.ActiveEditor;
  if Editor=nil then exit;
  ColorDialog:=TColorDialog.Create(nil);
  if ColorDialog.Execute then
  begin
     c := ColorToRGB(ColorDialog.Color);
     Txt:=Format('ColorCreate(%d,%d,%d,255)', [Red(c), Green(c), Blue(c)]);
     Editor.CutToClipboard;
     Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,Txt);
  end;
  if not Assigned(ColorDialog) then ColorDialog.Free;
end;

function MyGetProjectTargetFile: string;
begin
  Result:='$(TargetFile)';
  if not IDEMacros.SubstituteMacros(Result) then
    raise Exception.Create('unable to retrieve target file of project');
end;

function GetOSlibFolder: string;
begin
Result:='$(TargetCPU)-$(TargetOS)';
if not IDEMacros.SubstituteMacros(Result) then
  raise Exception.Create('unable to retrieve target file of project');
end;

function GetTargetOS: string;
begin
Result:='$(TargetOS)';
if not IDEMacros.SubstituteMacros(Result) then
  raise Exception.Create('unable to retrieve target file of project');
end;

function GetRay4lazDir: string;
var Pkg: TIDEPackage;
begin
  Pkg:=PackageEditingInterface.FindPackageWithName('ray4laz');
  if Pkg <> nil then result := ExtractFilePath(ExcludeTrailingPathDelimiter(Pkg.DirectoryExpanded));
end;

function GetLazdir: string;
begin
Result:='$(LazarusDir)';
if not IDEMacros.SubstituteMacros(Result) then
  raise Exception.Create('unable to retrieve target file of project');
end;

procedure CopyByName(dllFile: String);
var Src, Dst: string;
begin
 Src := GetRay4lazDir  + 'libs' + PathDelim + GetOslibFolder + PathDelim + dllFile;
 Dst := ExtractFilePath(MyGetProjectTargetFile)+ dllFile;
 if CopyFile(Src, Dst) then
 IDEMessagesWindow.AddCustomMessage(mluHint, rsCopyTrue, dllFile, 0,0) else
 IDEMessagesWindow.AddCustomMessage(mluError, rsCopyFalse);
end;

{
procedure CopyRaylibToProject(Sender: Tobject);
const
  dllFile = 'libraylib.dll';
begin
 CopyByName(dllFile);
end;
}

{
procedure CopyR3DToProject(Sender: Tobject);
const
  dllFile = 'libr3d.dll';
begin
 CopyByName(dllFile);
end;
}

{
procedure CopyGizmoToProject(Sender: Tobject);
const
  dllFile = 'libraygizmo.dll';
begin
 CopyByName(dllFile);
end;
}

{
procedure CopyMediaToProject(Sender: Tobject);
const
  dllFile = 'libraymedia.dll';
begin
 CopyByName(dllFile);
end;
}
{
procedure CopySSl(dllFile: string);
var Src, Dst : string;
begin
 Src := GetRay4lazDir  + 'package' + PathDelim + 'openssl' + PathDelim + GetOslibFolder + PathDelim + dllFile;
 Dst := GetLazdir + dllFile;

 if CopyFile(Src, Dst) then
   IDEMessagesWindow.AddCustomMessage(mluHint, rsCopyTrue, dllFile, 0,0) else
   IDEMessagesWindow.AddCustomMessage(mluError, rsCopyFalse);
end;
}
{
procedure CopySSlAll(Sender: TObject);
begin
  CopySSl('ssleay32.dll');
  CopySSl('libeay32.dll');
end;
}

procedure CompileRay4laz(Sender: TObject);
var Pkg: TIDEPackage;
begin
  Pkg:=PackageEditingInterface.FindPackageWithName('ray4laz');
  if Pkg<>nil then
  if PackageEditingInterface.DoCompilePackage(Pkg,[pcfCleanCompile], False) <> mrOk then
  exit;
end;

procedure ShowSettingFile(Sender: TObject);
begin
  if LazarusIDE.DoOpenEditorFile(GetRay4lazDir + 'source' +
  PathDelim + 'raylib.inc',-1,-1,[ofAddToRecent])<>mrOk then exit;
end;
{
procedure DownloadFFmpeg(Sender: TObject);
begin
 try
   if GetTargetOS = 'win32' then
   begin
     ShowHttpDownloader('Downloading avcodec-61.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win32/bin/avcodec-61.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avcodec-61.dll') ;
     IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avcodec-61.dll',0,0, rsDownloadFFmpeg );

      ShowHttpDownloader('Downloading avformat-61.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win32/bin/avformat-61.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avformat-61.dll');
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avformat-61.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading avutil-59.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win32/bin/avutil-59.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avutil-59.dll');
     IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avutil-59.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading swresample-5.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win32/bin/swresample-5.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'swresample-5.dll');
     IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'swresample-5.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading swscale-8.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win32/bin/swscale-8.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'swscale-8.dll');
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'swscale-8.dll',0,0, rsDownloadFFmpeg );
   end else

 if GetTargetOS = 'win64' then
   begin
     ShowHttpDownloader('Downloading avcodec-61.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win64/bin/avcodec-61.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avcodec-61.dll') ;
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avcodec-61.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading avformat-61.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win64/bin/avformat-61.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avformat-61.dll');
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avformat-61.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading avutil-59.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win64/bin/avutil-59.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'avutil-59.dll');
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'avutil-59.dll',0,0, rsDownloadFFmpeg );

     ShowHttpDownloader('Downloading swresample-5.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win64/bin/swresample-5.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'swresample-5.dll');

     ShowHttpDownloader('Downloading swscale-8.dll',
       'https://github.com/GuvaCode/ffmpeg_7.1_win7/raw/refs/heads/main/ffmpeg-7.1-shared-win64/bin/swscale-8.dll',
      ExtractFilePath(MyGetProjectTargetFile) + 'swscale-8.dll');
      IDEMessagesWindow.AddCustomMessage(mluHint, 'Download', 'swscale-8.dll',0,0, rsDownloadFFmpeg );
   end else
   IDEMessagesWindow.AddCustomMessage(mluWarning, rsOnlyWin);


   except
     on E: Exception do ShowMessage(E.Message);
   end;

end;
}

procedure ShowShaderToy(Sender: TObject);
begin
 IDEWindowCreators.ShowForm(ShaderToyConverterFormCreator.FormName, false );
end;


procedure Register;
begin
 // register dockable Window
 ShaderToyConverterFormCreator:=IDEWindowCreators.Add(
   'ShaderToyConverterForm',
   @CreateShaderToyConverterForm, nil,
   '250', '250', '', '');

 SectionRay:=RegisterIDEMenuSection(SrcEditMenuSectionFirstStatic,'RayTool');
 SectionRayMenu:= RegisterIDESubMenu(SectionRay,'RayTool',rsMnuMisc,nil ,nil,'cc_class');

 SectionToolMenu:= RegisterIDESubMenu(SectionRayMenu,'rltool',rsRlTools,nil ,nil,'pkg_properties');
 RegisterIDEMenuCommand(SectionToolMenu, 'ShaderConverter', srToy, nil, @ShowShaderToy,nil, 'laz_wand');
 RegisterIDEMenuCommand(SectionToolMenu, 'Sep0','-',nil,nil);
 RegisterIDEMenuCommand(SectionToolMenu, 'Compile', rsCompilePkg, nil, @CompileRay4laz,nil, 'pkg_compile');
 RegisterIDEMenuCommand(SectionToolMenu, 'Sep1','-',nil,nil);
 RegisterIDEMenuCommand(SectionToolMenu, 'OpenSetting', rsOpenConfig, nil, @ShowSettingFile,nil, 'menu_build_file');

 {
 SectionTool:= RegisterIDESubMenu(SectionToolMenu,'rltoolSub',rsCopyLibs,nil ,nil,'pkg_lrs');
 RegisterIDEMenuCommand(SectionTool, 'Sep2','-',nil,nil);
 RegisterIDEMenuCommand(SectionTool, 'CopyDll1', rsCopyDll1, nil, @CopyRaylibToProject,nil, 'pkg_lrs');
 RegisterIDEMenuCommand(SectionTool, 'CopyDll2', rsCopyDll2, nil, @CopyR3DToProject,nil, 'pkg_lrs');
 RegisterIDEMenuCommand(SectionTool, 'CopyDll3', rsCopyDll3, nil, @CopyGizmoToProject,nil, 'pkg_lrs');
 RegisterIDEMenuCommand(SectionTool, 'Sep3','-',nil,nil);
 RegisterIDEMenuCommand(SectionTool, 'CopyDll4', rsCopyDll4, nil, @CopyMediaToProject,nil, 'pkg_lrs');
 RegisterIDEMenuCommand(SectionTool, 'DownloadLibs', rsDownloadFFmpeg , nil, @DownloadFFmpeg,nil, 'menu_exporthtml');
 RegisterIDEMenuCommand(SectionTool, 'DownloadLibs1', rsSSL , nil, @CopySSlAll,nil, 'pkg_lrs');
 }

 RegisterIDEMenuCommand(SectionToolMenu, 'Sep4','-',nil,nil);
 RegisterIDEMenuCommand(SectionToolMenu, 'ShowCheatsheet', rsHelpCheat , nil, @RayFunction, nil, 'ce_interface');
 RegisterIDEMenuCommand(SectionToolMenu, 'ShowWiki', rsRayWiki , nil, @RayFunction, nil, 'menu_information');

 RegisterIDEMenuCommand(SectionRayMenu, 'Spl0','-',nil,nil);
 RegisterIDEMenuCommand(SectionRayMenu, 'InsertColor', rsInsertClr , nil, @ShowColorDialog, nil, 'tcolordialog');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl1','-',nil,nil);
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Create', 'Vector2Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Create', 'Vector3Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector4Create', 'Vector4Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'QuaternionCreate', 'QuaternionCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'ColorCreate', 'ColorCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleCreate', 'RectangleCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'BoundingBoxCreate', 'BoundingBoxCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Camera3DCreate', 'Camera3DCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl2','-',nil,nil);
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Set', 'Vector2Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Set', 'Vector3Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector4Set', 'Vector4Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'QuaternionSet', 'QuaternionSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'ColorSet', 'ColorSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleSet', 'RectangleSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'BoundingBoxSet', 'BoundingBoxSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Camera3DSet', 'Camera3DSet', nil, @RayFunction,nil, 'cc_procedure');

 SectionRay.AddHandlerOnShow(@EventCode.DoSomething,true);
end;

procedure TEventClass.DoSomething(Sender: TObject);
begin
 SectionRayMenu.Visible:=RayUsed;
  {
  if (GetTargetOS = 'win64') or (GetTargetOS = 'win32') then
  begin
    SectionTool.Visible := true;
  end else SectionTool.Visible := false;
  }
end;

initialization
 EventCode := TEventClass.Create;
finalization
 EventCode.Free;

end.

