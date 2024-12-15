unit ray4laz_misc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LazFileUtils, PackageIntf, LazIDEIntf,
  ProjectIntf, MenuIntf, SrcEditorIntf, Dialogs, Forms;

type
  { TEventClass }
  TEventClass = class
   procedure DoSomething(Sender:TObject);
  end;

 function  RayUsed : boolean;

procedure Register;

 var EventCode : TEventClass;
     SectionRay: TIDEMenuSection;
     SectionRayMenu: TIDEMenuSection;

 resourcestring
   rsMnuMisc        = 'raylib Misc ...';
   rsHelpCheat      = 'Cheatsheet ...';
   rsInsertClr      = 'ColorCreate from dialog';
   rsRayWiki        = 'raylib Wiki';

implementation

function RayUsed: boolean;
var
lPkgList: TFPList;
i: Integer;
begin
 result:=false;
 if  (LazarusIDE.ActiveProject = nil) or (PackageEditingInterface.GetPackageCount <= 0 ) then exit;
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

  procedure insertXY(Text:String);
   begin
    Editor.CutToClipboard;
    Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,Text);
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
end;


procedure Register;
begin
 SectionRay:=RegisterIDEMenuSection(SrcEditMenuSectionFirstStatic,'RayTool');
 SectionRayMenu:= RegisterIDESubMenu(SectionRay,'RayTool',rsMnuMisc,nil ,nil,'cc_class');

 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Create', 'Vector2Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Create', 'Vector3Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector4Create', 'Vector4Create', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'QuaternionCreate', 'QuaternionCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'ColorCreate', 'ColorCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleCreate', 'RectangleCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'BoundingBoxCreate', 'BoundingBoxCreate', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Camera3DCreate', 'Camera3DCreate', nil, @RayFunction,nil, 'cc_function');

 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Set', 'Vector2Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Set', 'Vector3Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector4Set', 'Vector4Set', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'QuaternionSet', 'QuaternionSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'ColorSet', 'ColorSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleSet', 'RectangleSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'BoundingBoxSet', 'BoundingBoxSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Camera3DSet', 'Camera3DSet', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl0','-',nil,nil);

 RegisterIDEMenuCommand(SectionRayMenu, 'InsertColor', rsInsertClr , nil, @ShowColorDialog, nil, 'tcolordialog');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl1','-',nil,nil);

 RegisterIDEMenuCommand(SectionRayMenu, 'ShowCheatsheet', rsHelpCheat , nil, @RayFunction, nil, 'ce_interface');
 RegisterIDEMenuCommand(SectionRayMenu, 'ShowWiki', rsRayWiki , nil, @RayFunction, nil, 'menu_information');

 SectionRay.AddHandlerOnShow(@EventCode.DoSomething,true);
end;

procedure TEventClass.DoSomething(Sender: TObject);
begin
 SectionRayMenu.Visible:=RayUsed;
end;


initialization
 EventCode := TEventClass.Create;
finalization
 EventCode.Free;

end.

