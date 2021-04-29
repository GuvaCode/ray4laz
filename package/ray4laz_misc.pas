unit ray4laz_misc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLIntf, LazFileUtils, PackageIntf, LazIDEIntf,
  ProjectIntf, MenuIntf, SrcEditorIntf, Dialogs;

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
   rsHelpCheat      = 'Cheatsheet ...';
   rsInsertClr      = 'ColorCreate from dialog';

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
  'Vector2Create'     : insertXY('Vector2Create()');
  'Vector2Set'        : insertXY('Vector2Set()');
  'Vector3Create'     : insertXY('Vector3Create()');
  'Vector3Set'        : insertXY('Vector3Set()');
  'ColorCreate'       : insertXY('ColorCreate()');
  'TColorSet'         : insertXY('TColorSet()');
  'RectangleCreate'   : insertXY('RectangleCreate()');
  'RectangleSet'      : insertXY('RectangleSet()');
  'TCamera3DCreate'   : insertXY('TCamera3DCreate()');
  'TCamera3DSet'      : insertXY('TCamera3DSet()');
  'ShowCheatsheet'    : OpenURL('https://www.raylib.com/cheatsheet/cheatsheet.html');
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
     //function ColorCreate(aR: byte; aG: byte; aB: byte; aA: byte): TColor;
     //procedure TColorSet(aColor: PColor; aR: byte; aG: byte; aB: byte; aA: byte);

     c := ColorToRGB(ColorDialog.Color);
     Txt:=Format('ColorCreate(%d,%d,%d,255)', [Red(c), Green(c), Blue(c)]);
     Editor.CutToClipboard;
     Editor.ReplaceText(editor.CursorTextXY,editor.CursorTextXY,Txt);

     end;
end;

procedure Register;
begin
  SectionRay:=RegisterIDEMenuSection(SrcEditMenuSectionFirstStatic,'RayTool');
 SectionRayMenu:= RegisterIDESubMenu(SectionRay,'RayTool','Raylib Misc ...',nil ,nil,'cc_class');

 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Create', 'Vector2Create', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Create', 'Vector3Create', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'ColorCreate', 'ColorCreate', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleCreate', 'RectangleCreate', nil, @RayFunction,nil, 'cc_procedure');
 RegisterIDEMenuCommand(SectionRayMenu, 'TCamera3DCreate', 'TCamera3DCreate', nil, @RayFunction,nil, 'cc_procedure');

 RegisterIDEMenuCommand(SectionRayMenu, 'Vector2Set', 'Vector2Set', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Vector3Set', 'Vector3Set', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'TColorSet', 'TColorSet', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'RectangleSet', 'RectangleSet', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'TCamera3DSet', 'TCamera3DSet', nil, @RayFunction,nil, 'cc_function');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl0','-',nil,nil);

 RegisterIDEMenuCommand(SectionRayMenu, 'InsertColor', rsInsertClr , nil, @ShowColorDialog, nil, 'tcolordialog');
 RegisterIDEMenuCommand(SectionRayMenu, 'Spl1','-',nil,nil);
 RegisterIDEMenuCommand(SectionRayMenu, 'ShowCheatsheet', rsHelpCheat , nil, @RayFunction, nil, 'ce_interface');

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

