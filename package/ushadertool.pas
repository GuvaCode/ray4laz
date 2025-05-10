unit uShaderTool;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, SynHighlighterGLSL,
  ExtCtrls, ActnList, ButtonPanel, StdActns, SynEdit, RegExpr, Clipbrd, LazLoggerBase,
  SynEditKeyCmds, LCLType, Menus, SynEditMiscClasses, SynEditMarkupHighAll, IDEWindowIntf;

type
  { TShaderToyConverterForm }
  TShaderToyConverterForm = class(TForm)
    actCopy: TAction;
    actConvert: TAction;
    actCut: TAction;
    actInsertAsUnit: TAction;
    actCompileFs: TAction;
    actCompileVs: TAction;
    actSelAll: TAction;
    actPaste: TAction;
    actNew: TAction;
    ActionList: TActionList;
    convertBtn: TPanelBitBtn;
    actFileOpen: TFileOpen;
    actFileSaveAs: TFileSaveAs;
    ImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem16: TMenuItem;
    Separator1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ResultEditor: TSynEdit;
    InputEditor: TSynEdit;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actCompileFsExecute(Sender: TObject);
    procedure actCompileFsUpdate(Sender: TObject);
    procedure actCompileVsExecute(Sender: TObject);
    procedure actConvertExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actFileSaveAsAccept(Sender: TObject);
    procedure actInsertAsUnitExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actFileOpenAccept(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ShaderCompileFs(FragmentShader: string);

    function ConvertShaderToyToRaylib(const ShaderCode: string): string;
    function StringListToConstDeclaration(sl: TStrings; const ConstName: string): string;
    function GenerateConstUnit(const UnitName_, ConstName: string;
      sl: TStrings; const UsesClause: string = ''): string;
    function IsSystemSupport: Boolean;
  public

  end;

const
  fDefaultFilter = 'GLSL Files (*.fs;*.vs;*.glsl,*.vert,*.frag,*.geom,*.tesc,*.tese,*.comp)|*.fs;*.vs;*.glsl;*.vert;*.frag;*.geom;*.tesc;*.tese;*.comp';
  fSaveFilter = 'GLSL Shader Files(*.fs;*.vs)|*.fs;*.vs)';

resourcestring
  rsAboutConvert = 'Automatically converted from ShaderToy for use with raylib.';
  rsInputVar = 'Input variables from raylib';
  rsUniformVar = 'Uniform variables ShaderToy';
  rsMainFunction = 'The main function that raylib calls';
  rsCallOrig = 'Calling the original function from ShaderToy';
  rsCallOrigInNormal = 'Call the original function from ShaderToy with normalized coordinates';

var
  ShaderToyConverterForm: TShaderToyConverterForm;
  ShaderToyConverterFormCreator: TIDEWindowCreator; // set by Register procedure
  higGlsl: TSynGLSLSyn;
  ErrorMarkUp: TSynEditMarkupHighlightAll;

procedure ShowShaderToyConverterForm(Sender: TObject);
procedure CreateShaderToyConverterForm(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);


implementation
 uses SynEditTypes, LazIDEIntf, SrcEditorIntf, ProjectIntf, Process,
      MacroIntf, PackageIntf, IDEExternToolIntf, IDEMsgIntf, strutils, math;

 procedure ShowShaderToyConverterForm(Sender: TObject);
 begin
  // if Assigned(IDEWindowCreators.ShowForm(ShaderToyConverterFormCreator.FormName, true)) then
   // ShaderToyConverterForm.InitSearch;
 end;

 procedure CreateShaderToyConverterForm(Sender: TObject; aFormName: string;
   var AForm: TCustomForm; DoDisableAutoSizing: boolean);
 begin
   // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'ShaderToyConverterForm')<>0 then begin
    DebugLn(['ERROR: CreateInstantSearchForm: there is already a form with '
      +'this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TShaderToyConverterForm, DoDisableAutoSizing,
    {LazarusIDE.OwningComponent} Application);
  AForm.Name:=aFormName;
  ShaderToyConverterForm:=AForm as TShaderToyConverterForm;
 end;

{$R *.lfm}
{ TShaderToyConverterForm }

procedure TShaderToyConverterForm.FormCreate(Sender: TObject);
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;

begin
  higGlsl := TSynGLSLSyn.Create(Self);
  ResultEditor.Highlighter := higGlsl;
  InputEditor.Highlighter := higGlsl;
  actFileOpen.Dialog.Filter:=fDefaultFilter;
  actFileSaveAs.Dialog.Filter:=fSaveFilter;



  SynMarkup := TSynEditMarkupHighlightAllCaret(ResultEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.FrameColor := clSilver;
  SynMarkup.MarkupInfo.Background := clGray;
 // SynMarkup.MarkupInfo.FrameStyle:= slsWaved;
 // SynMarkup.MarkupInfo.FrameEdges := sfeBottom;
  SynMarkup.WaitTime := 100; // millisec
  SynMarkup.Trim := True;     // no spaces, if using selection
  SynMarkup.FullWord := True; // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := False;

  ErrorMarkUp := TSynEditMarkupHighlightAll(ResultEditor.MarkupByClass[TSynEditMarkupHighlightAll]);
  ErrorMarkUp.MarkupInfo.FrameStyle:= slsWaved;
  ErrorMarkUp.MarkupInfo.FrameEdges := sfeBottom;
  ErrorMarkUp.MarkupInfo.FrameColor := clRED;



 // ErrorMarkUp.MarkupInfo.Foreground := clWhite;
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

procedure TShaderToyConverterForm.ShaderCompileFs(FragmentShader: string);
var
  ShProcess: TProcess;
  Output: TStringList;
  i, P, P2: Integer;
  S, ErrorToken: string;
  HasError: Boolean;
begin
  IDEMessagesWindow.Clear;
  ShProcess := TProcess.Create(nil);
  Output := TStringList.Create;
  HasError := False;
  ErrorToken := '';

  {$IFDEF LINUX}
    {$IFDEF CPU64}
       ShProcess.Executable := GetRay4lazDir + PathDelim + 'tool' + PathDelim + 'shader_compiler_linux64';
    {$ELSE}
       ShProcess.Executable := GetRay4lazDir + PathDelim + 'tool' + PathDelim + 'shader_compiler_linux32';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$IFDEF CPU64}
       ShProcess.Executable := GetRay4lazDir + PathDelim + 'tool' + PathDelim + 'shader_compiler_windows64.exe';
    {$ELSE}
       ShProcess.Executable := GetRay4lazDir + PathDelim + 'tool' + PathDelim + 'shader_compiler_windows32.exe';
    {$ENDIF}
  {$ENDIF}

  ShProcess.Parameters.Add('-fs');
  ShProcess.Parameters.Add(FragmentShader);
  ShProcess.Options := [poWaitOnExit, poUsePipes];
  ShProcess.Execute;
  Output.LoadFromStream(ShProcess.Output);

  // Вывод сообщений в IDE и поиск ошибок
  for i := 0 to Output.Count - 1 do
  begin
    S := Output.Strings[i];

    // Пропускаем стандартные сообщения raylib
    if Pos('SHADER: [ID 1]', S) > 0 then Continue;
    if Pos('SHADER: [ID 2]', S) > 0 then Continue;
    if Pos('SHADER: [ID 3]', S) > 0 then Continue;

    // Обрабатываем сообщения
    if (Pos('Failed to compile', S) > 0) or (Pos('error C', S) > 0) then
    begin
      IDEMessagesWindow.AddCustomMessage(mluError, S);
      HasError := True;

      // Ищем текст в кавычках для подсветки
      P := Pos('"', S);
      if P > 0 then
      begin
        P2 := PosEx('"', S, P + 1);
        if P2 > 0 then
          ErrorToken := Copy(S, P + 1, P2 - P - 1);
      end;
    end
    else if Pos('WARNING:', S) > 0 then
      IDEMessagesWindow.AddCustomMessage(mluWarning, S)
    else if Pos('SHADER:', S) > 0 then
      IDEMessagesWindow.AddCustomMessage(mluHint, S);
  end;

  // Подсветка ошибки в редакторе
  if HasError and Assigned(ErrorMarkUp) then
  begin
    if ErrorToken <> '' then
      ErrorMarkUp.SearchString := ErrorToken
    else
      ErrorMarkUp.SearchString := ''; // Сброс, если не нашли слово в кавычках
  end;

  // Сообщение об успешной компиляции
  if (Output.Count > 0) and (Pos('Shader loaded successfully!', Output.Strings[Output.Count-1]) > 0) then
  IDEMessagesWindow.AddCustomMessage(mluProgress, Output.Strings[Output.Count-1]);

  ShProcess.Free;
  Output.Free;
end;

procedure TShaderToyConverterForm.actFileOpenAccept(Sender: TObject);
begin
 InputEditor.Lines.LoadFromFile(actFileOpen.Dialog.FileName);
end;

procedure TShaderToyConverterForm.actCutExecute(Sender: TObject);
begin
 if (InputEditor.SelText <> '') and (InputEditor.Focused) then
 InputEditor.CutToClipboard else
 if (ResultEditor.SelText <> '') and (ResultEditor.Focused) then
   ResultEditor.CutToClipboard;
end;

procedure TShaderToyConverterForm.actSelAllExecute(Sender: TObject);
begin
 if (InputEditor.Focused) then
 InputEditor.SelectAll
 else
 if ResultEditor.Focused then
  begin
    ResultEditor.SelectAll;
    //ResultEditor.ExecuteCommand(ecStickySelectionLine,'',nil);
    ///ErrorMarkUp.MarkupInfo.SetFrameBoundsLog(1,5);

  //  ResultEditor.SelectWord;
   // ErrorMarkUp.SearchString:='version 330';

  end;
end;


procedure TShaderToyConverterForm.actNewExecute(Sender: TObject);
begin
  InputEditor.Lines.Clear;
  ResultEditor.Lines.Clear;
end;

procedure TShaderToyConverterForm.actPasteExecute(Sender: TObject);
begin
  if InputEditor.Focused then
    InputEditor.PasteFromClipboard()
  else
  if ResultEditor.Focused then
    ResultEditor.PasteFromClipboard();
end;

procedure TShaderToyConverterForm.ActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
 actPaste.Enabled := (Clipboard.HasFormat(CF_TEXT) and InputEditor.Focused) or
 (Clipboard.HasFormat(CF_TEXT) and ResultEditor.Focused) ;

 actFileSaveAs.Enabled := ResultEditor.Lines.Count > 1;
 actConvert.Enabled := (InputEditor.Lines.Count>1);


 actCopy.Enabled := (InputEditor.SelText <> '') and (InputEditor.Focused) or
                   (ResultEditor.SelText <> '') and (ResultEditor.Focused);

 actCut.Enabled := (InputEditor.SelText <> '') and (InputEditor.Focused) or
                   (ResultEditor.SelText <> '') and (ResultEditor.Focused);

 actSelAll.Enabled := (InputEditor.Lines.Count>0) and (InputEditor.Focused) or
                      (ResultEditor.Lines.Count>0) and (ResultEditor.Focused);


end;

procedure TShaderToyConverterForm.actConvertExecute(Sender: TObject);
begin
 ResultEditor.ClearAll;
 ResultEditor.Text := ConvertShaderToyToRaylib(InputEditor.Text);
end;

procedure TShaderToyConverterForm.actCompileFsUpdate(Sender: TObject);
begin //  (InputEditor.Lines.Count>0)
  actCompileFs.Enabled := IsSystemSupport() and (InputEditor.Lines.Count>0);
  actCompileVs.Enabled := IsSystemSupport() and (InputEditor.Lines.Count>0);
end;

procedure TShaderToyConverterForm.actCompileFsExecute(Sender: TObject);
begin
  ShaderCompileFs(resultEditor.Text);

end;

procedure TShaderToyConverterForm.actCompileVsExecute(Sender: TObject);
begin

end;

procedure TShaderToyConverterForm.actCopyExecute(Sender: TObject);
begin
 if (InputEditor.Focused) then InputEditor.CopyToClipboard else
 if (ResultEditor.Focused) then ResultEditor.CopyToClipboard;
end;

procedure TShaderToyConverterForm.actFileSaveAsAccept(Sender: TObject);
begin
 ResultEditor.Lines.SaveToFile(actFileSaveAs.Dialog.FileName);
end;

procedure TShaderToyConverterForm.actInsertAsUnitExecute(Sender: TObject);
var
  NewUnitText: String;
  LUnitName: String;
  Editor: TSourceEditorInterface;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorUnit,'','',[nfIsPartOfProject,nfOpenInEditor]);
  Editor:=SourceEditorManagerIntf.ActiveEditor;

  LUnitName :=  Editor.PageCaption;
  NewUnitText := GenerateConstUnit(LUnitName, 'SHADER', ResultEditor.Lines);
  Editor.InsertLine(0,NewUnitText);
end;

function TShaderToyConverterForm.ConvertShaderToyToRaylib(const ShaderCode: string): string;
var
  Lines: TStringList;
  i: Integer;
  MainImageFound: Boolean;
  UsesFragCoord: Boolean;
  Uniforms: TStringList;
  RE: TRegExpr;
begin
  Lines := TStringList.Create;
  Uniforms := TStringList.Create;
  Uniforms.Sorted := True;
  Uniforms.Duplicates := dupIgnore;
  RE := TRegExpr.Create;
  try
    Lines.Text := ShaderCode;

    // Ищем все uniform-переменные в коде
    RE.Expression := 'uniform\s+(bool|int|float|vec2|vec3|vec4|mat2|mat3|mat4)\s+(\w+)\s*;';
    if RE.Exec(Lines.Text) then
    begin
      repeat
        Uniforms.Add(Format('uniform %s %s;', [RE.Match[1], RE.Match[2]]));
      until not RE.ExecNext;
    end;

    // Добавляем стандартные uniform-переменные ShaderToy, если они используются в коде
    if Pos('iResolution', Lines.Text) > 0 then
      Uniforms.Add('uniform vec3 iResolution;   // viewport resolution (pixels)');
    if Pos('iTime', Lines.Text) > 0 then
      Uniforms.Add('uniform float iTime;        // shader playback time (s)');
    if Pos('iTimeDelta', Lines.Text) > 0 then
      Uniforms.Add('uniform float iTimeDelta;   // render time (s)');
    if Pos('iFrame', Lines.Text) > 0 then
      Uniforms.Add('uniform int iFrame;         // shader playback frame');
    if Pos('iMouse', Lines.Text) > 0 then
      Uniforms.Add('uniform vec4 iMouse;        // mouse pixel coords');
    if Pos('iDate', Lines.Text) > 0 then
      Uniforms.Add('uniform vec4 iDate;         // (year, month, day, time in s)');
    if Pos('iSampleRate', Lines.Text) > 0 then
      Uniforms.Add('uniform float iSampleRate;  // sound sample rate (Hz)');
    if Pos('iChannel', Lines.Text) > 0 then
    begin
      // Ищем максимальный номер канала (0..3)
      RE.Expression := 'iChannel(\d)';
      if RE.Exec(Lines.Text) then
      begin
        repeat
          Uniforms.Add(Format('uniform sampler2D iChannel%s;', [RE.Match[1]]));
        until not RE.ExecNext;
      end
      else
      begin
        // Если номера нет, добавляем хотя бы один канал
        Uniforms.Add('uniform sampler2D iChannel0;');
      end;
    end;

    // Добавляем заголовок для raylib (GLSL 330)
    Lines.Insert(0, '#version 330');
    Lines.Insert(1, '');
    Lines.Insert(2, '// ' +  rsAboutConvert);// 'Автоматически сконвертировано из ShaderToy в raylib формат');
    Lines.Insert(3, '');
    Lines.Insert(4, '// ' + rsInputVar);//'Входные переменные от raylib');
    Lines.Insert(5, 'in vec2 fragCoord;');
    Lines.Insert(6, 'out vec4 fragColor;');
    Lines.Insert(7, '');

    // Добавляем найденные uniform-переменные
    if Uniforms.Count > 0 then
    begin
      Lines.Insert(8, '// ' + rsUniformVar);//'Uniform-переменные ShaderToy');
      for i := 0 to Uniforms.Count - 1 do
        Lines.Insert(9 + i, Uniforms[i]);
      Lines.Insert(9 + Uniforms.Count, '');
    end;

    MainImageFound := False;
    UsesFragCoord := False;

    // Анализируем код для поиска использования gl_FragCoord
    for i := 0 to Lines.Count - 1 do
    begin
      if Pos('mainImage', Lines[i]) > 0 then
        MainImageFound := True;

      if Pos('gl_FragCoord', Lines[i]) > 0 then
        UsesFragCoord := True;
    end;

    // Заменяем iResolution.xy на iResolution.xy, если используется vec3 iResolution
    for i := 0 to Lines.Count - 1 do
    begin
      if Pos('iResolution.xy', Lines[i]) > 0 then
        Lines[i] := StringReplace(Lines[i], 'iResolution.xy', 'iResolution.xy', [rfReplaceAll]);
    end;

    // Добавляем функцию main() если её нет
    if MainImageFound then
    begin
      Lines.Add('');
      Lines.Add('// ' + rsMainFunction);//'Главная функция, которую вызывает raylib');
      Lines.Add('void main()');
      Lines.Add('{');
      if UsesFragCoord then
        Lines.Add('    // '+ rsCallOrig) //'// Вызываем оригинальную функцию из ShaderToy')
      else
        Lines.Add('    // '+ rsCallOrigInNormal);//'// Вызываем оригинальную функцию из ShaderToy с нормализованными координатами');

      if UsesFragCoord then
        Lines.Add('    mainImage(fragColor, fragCoord);')
      else
        Lines.Add('    mainImage(fragColor, gl_FragCoord.xy);');
      Lines.Add('}');
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
    Uniforms.Free;
    RE.Free;
  end;
end;

function TShaderToyConverterForm.StringListToConstDeclaration(sl: TStrings;
  const ConstName: string): string;
var
  i: Integer;
begin
  Result :=
  'const' + LineEnding +
  '  ' + ConstName + ' =' + LineEnding;

  for i := 0 to sl.Count - 1 do
  begin
    Result := Result + '  ''' + sl[i] + '''';

    if i < sl.Count - 1 then
      Result := Result + '+#10+';

    Result := Result + LineEnding;
  end;
  Result := Result + ';';
end;

function TShaderToyConverterForm.GenerateConstUnit(const UnitName_,
  ConstName: string; sl: TStrings; const UsesClause: string): string;
var
  i: Integer;
begin
  Result := 'unit ' + UnitName_ + ';' + LineEnding + LineEnding +
            '{$mode objfpc}{$H+}' + LineEnding + LineEnding +
            'interface' + LineEnding + LineEnding;

  if UsesClause <> '' then
    Result := Result + 'uses' + LineEnding +
              '  ' + UsesClause + ';' + LineEnding + LineEnding;

  Result := Result + 'const' + LineEnding +
            '  ' + ConstName + ' =' + LineEnding;

  for i := 0 to sl.Count - 1 do
  begin
    Result := Result + '  ''' + StringReplace(sl[i], '''', '''''', [rfReplaceAll]) + '''';
    if i < sl.Count - 1 then
      Result := Result + ' + #10 +';
    Result := Result + LineEnding;
  end;

  Result := Result + '  ;' + LineEnding + LineEnding +
            'implementation' + LineEnding + LineEnding +
            'end.' + LineEnding;
end;

function TShaderToyConverterForm.IsSystemSupport: Boolean;
begin
  Result := False;
  {$IFDEF MSWINDOWS}
    // Проверяем, что это не ARM (AARCH64 или ARMHF)
    {$IFNDEF CPUARM}
      {$IFNDEF CPUAARCH64}
        // Если это x86 или x64, то поддерживается
        Result := True;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    // Аналогично для Linux
    {$IFNDEF CPUARM}
      {$IFNDEF CPUAARCH64}
        Result := True;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;



end.

