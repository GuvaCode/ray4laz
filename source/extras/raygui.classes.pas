{********************************************************************************************
*                                                                                           *
*    RAYGUI OBJECT-ORIENTED WRAPPER                                                         *
*    Based on raygui v4.5-dev                                                               *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   Object-oriented wrapper for raygui library with classes for each GUI control            *
*                                                                                           *
*   pascal wrapper 2025 by Vadim Gunko                                                      *
*                                                                                           *
*********************************************************************************************}

unit RayGUI.Classes;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, raylib, raygui;

type
  { Forward declarations }
  TGUIBaseControl = class;
  TGUIWindowBox = class;
  TGUIGroupBox = class;
  TGUILine = class;
  TGUIPanel = class;
  TGUITabBar = class;
  TGUIButton = class;
  TGUILabel = class;
  TGUIToggle = class;
  TGUICheckBox = class;
  TGUIComboBox = class;
  TGUIDropdownBox = class;
  TGUISpinner = class;
  TGUIValueBox = class;
  TGUIValueBoxFloat = class;
  TGUITextBox = class;
  TGUISlider = class;
  TGUISliderBar = class;
  TGUIProgressBar = class;
  TGUIListView = class;
  TGUIMessageBox = class;
  TGUIColorPicker = class;

  { Base control class }
  TGUIBaseControl = class
  private
    FBounds: TRectangle;
    FText: string;
    FVisible: Boolean;
    FEnabled: Boolean;
  protected
    procedure SetBounds(const AValue: TRectangle);
    procedure SetText(const AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');

    property Bounds: TRectangle read FBounds write SetBounds;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  { TGUIControlManager - Manages GUI state and controls }
  TGUIControlManager = class
  private
    FControls: TList;
    FAlpha: Single;
    FLocked: Boolean;
    FCustomFont: TFont;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enable;
    procedure Disable;
    procedure Lock;
    procedure Unlock;
    function IsLocked: Boolean;

    procedure SetAlpha(AAlpha: Single);
    procedure SetFont(AFont: TFont);
    function GetFont: TFont;

    procedure LoadStyle(const AFileName: string);
    procedure LoadStyleDefault;

    procedure EnableTooltip;
    procedure DisableTooltip;
    procedure SetTooltip(const ATooltip: string);

    procedure AddControl(AControl: TGUIBaseControl);
    procedure RemoveControl(AControl: TGUIBaseControl);
    procedure ClearControls;

    property Alpha: Single read FAlpha write SetAlpha;
    property Locked: Boolean read FLocked;
    property CustomFont: TFont read FCustomFont write SetFont;
  end;

  { Container controls }

  TGUIWindowBox = class(TGUIBaseControl)
  private
    FCloseButtonClicked: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const ATitle: string = '');
    function Draw: Boolean;
    property CloseButtonClicked: Boolean read FCloseButtonClicked;
  end;

  TGUIGroupBox = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  TGUILine = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  TGUIPanel = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  TGUITabBar = class(TGUIBaseControl)
  private
    FTabs: TStringList;
    FActiveTab: Integer;
  public
    constructor Create(const ABounds: TRectangle; const ATabs: array of string);
    destructor Destroy; override;
    function Draw: Integer;
    property ActiveTab: Integer read FActiveTab write FActiveTab;
    property Tabs: TStringList read FTabs;
  end;

  { Basic controls }

  TGUIButton = class(TGUIBaseControl)
  private
    FClicked: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    function Draw: Boolean;
    property Clicked: Boolean read FClicked;
  end;

  TGUILabelButton = class(TGUIButton)
  public
    function Draw: Boolean;
  end;

  TGUILabel = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  TGUIToggle = class(TGUIBaseControl)
  private
    FActive: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = ''; AActive: Boolean = False);
    function Draw: Boolean;
    property Active: Boolean read FActive write FActive;
  end;

  TGUIToggleGroup = class(TGUIBaseControl)
  private
    FActiveIndex: Integer;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = ''; AActiveIndex: Integer = 0);
    function Draw: Integer;
    property ActiveIndex: Integer read FActiveIndex write FActiveIndex;
  end;

  TGUIToggleSlider = class(TGUIToggleGroup)
  public
    function Draw: Integer;
  end;

  TGUICheckBox = class(TGUIBaseControl)
  private
    FChecked: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = ''; AChecked: Boolean = False);
    function Draw: Boolean;
    property Checked: Boolean read FChecked write FChecked;
  end;

  TGUIComboBox = class(TGUIBaseControl)
  private
    FActiveIndex: Integer;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = ''; AActiveIndex: Integer = 0);
    function Draw: Integer;
    property ActiveIndex: Integer read FActiveIndex write FActiveIndex;
  end;

  TGUIDropdownBox = class(TGUIBaseControl)
  private
    FActiveIndex: Integer;
    FEditMode: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AActiveIndex: Integer = 0; AEditMode: Boolean = False);
    function Draw: Integer;
    property ActiveIndex: Integer read FActiveIndex write FActiveIndex;
    property EditMode: Boolean read FEditMode write FEditMode;
  end;

  TGUISpinner = class(TGUIBaseControl)
  private
    FValue: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    FEditMode: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AValue: Integer = 0; AMinValue: Integer = 0; AMaxValue: Integer = 100;
      AEditMode: Boolean = False);
    function Draw: Integer;
    property Value: Integer read FValue write FValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property EditMode: Boolean read FEditMode write FEditMode;
  end;

  TGUIValueBox = class(TGUIBaseControl)
  private
    FValue: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    FEditMode: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AValue: Integer = 0; AMinValue: Integer = 0; AMaxValue: Integer = 100;
      AEditMode: Boolean = False);
    function Draw: Integer;
    property Value: Integer read FValue write FValue;
    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property EditMode: Boolean read FEditMode write FEditMode;
  end;

  TGUIValueBoxFloat = class(TGUIBaseControl)
  private
    FValue: Single;
    FTextValue: string;
    FEditMode: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      const ATextValue: string = '0.0'; AValue: Single = 0.0;
      AEditMode: Boolean = False);
    function Draw: Single;
    property Value: Single read FValue write FValue;
    property TextValue: string read FTextValue write FTextValue;
    property EditMode: Boolean read FEditMode write FEditMode;
  end;

  { TGUITextBox }

  TGUITextBox = class(TGUIBaseControl)
  private
    FEditMode: Boolean;
    FTextBuffer: string;
    FMaxLength: Integer;
    FBuffer: PChar;
    function GetText: string;
    procedure SetText(const AValue: string);
    procedure SetEditMode(AValue: Boolean);  // Добавьте эту строку
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AMaxLength: Integer = 256; AEditMode: Boolean = False);
    destructor Destroy; override;
    function Draw: Boolean;
    function IsEditing: Boolean;
    property EditMode: Boolean read FEditMode write SetEditMode;
    property TextBuffer: string read FTextBuffer write FTextBuffer;
    property MaxLength: Integer read FMaxLength write FMaxLength;
    property Text: string read GetText write SetText;
  end;

  TGUISlider = class(TGUIBaseControl)
  private
    FValue: Single;
    FMinValue: Single;
    FMaxValue: Single;
    FTextLeft: string;
    FTextRight: string;
  public
    constructor Create(const ABounds: TRectangle;
      const ATextLeft: string = ''; const ATextRight: string = '';
      AValue: Single = 0.0; AMinValue: Single = 0.0; AMaxValue: Single = 100.0);
    function Draw: Single;
    property Value: Single read FValue write FValue;
    property MinValue: Single read FMinValue write FMinValue;
    property MaxValue: Single read FMaxValue write FMaxValue;
    property TextLeft: string read FTextLeft write FTextLeft;
    property TextRight: string read FTextRight write FTextRight;
  end;

  TGUISliderBar = class(TGUISlider)
  public
    function Draw: Single;
  end;

  TGUIProgressBar = class(TGUISlider)
  public
    function Draw: Single;
  end;

  TGUIStatusBar = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  TGUIDummyRec = class(TGUIBaseControl)
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '');
    procedure Draw;
  end;

  { Advanced controls }

  TGUIListView = class(TGUIBaseControl)
  private
    FScrollIndex: Integer;
    FActiveIndex: Integer;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AScrollIndex: Integer = 0; AActiveIndex: Integer = -1);
    function Draw: Integer;
    property ScrollIndex: Integer read FScrollIndex write FScrollIndex;
    property ActiveIndex: Integer read FActiveIndex write FActiveIndex;
  end;

  TGUIListViewEx = class(TGUIBaseControl)
  private
    FItems: TStringList;
    FScrollIndex: Integer;
    FActiveIndex: Integer;
    FFocusIndex: Integer;
  public
    constructor Create(const ABounds: TRectangle; AItems: TStringList;
      AScrollIndex: Integer = 0; AActiveIndex: Integer = -1;
      AFocusIndex: Integer = -1);
    destructor Destroy; override;
    function Draw: Integer;
    property ScrollIndex: Integer read FScrollIndex write FScrollIndex;
    property ActiveIndex: Integer read FActiveIndex write FActiveIndex;
    property FocusIndex: Integer read FFocusIndex write FFocusIndex;
    property Items: TStringList read FItems;
  end;

  TGUIMessageBox = class(TGUIBaseControl)
  private
    FTitle: string;
    FMessage: string;
    FButtons: string;

    // Переменные для перетаскивания окна
    FDragMode: Boolean;
    FSupportDrag: Boolean;
    FPanOffset: TVector2;
    FOriginalBounds: TRectangle;

  public
    constructor Create(const ABounds: TRectangle; const ATitle, AMessage, AButtons: string);
    function Draw: Integer;

    // Свойства для управления перетаскиванием
    property SupportDrag: Boolean read FSupportDrag write FSupportDrag;

    // Методы управления позицией
    procedure ResetPosition;
    procedure CenterOnScreen;

    property Title: string read FTitle write FTitle;
    property Message: string read FMessage write FMessage;
    property Buttons: string read FButtons write FButtons;
  end;

  TGUIInputBox = class(TGUIBaseControl)
  private
    FTitle: string;
    FMessage: string;
    FButtons: string;
    FInputText: string;
    FMaxSize: Integer;
    FSecretViewActive: Boolean;
  public
    constructor Create(const ABounds: TRectangle; const ATitle, AMessage, AButtons: string;
      const AText: string = ''; AMaxSize: Integer = 256; ASecretViewActive: Boolean = False);
    function Draw: Integer;
    property Title: string read FTitle write FTitle;
    property Message: string read FMessage write FMessage;
    property Buttons: string read FButtons write FButtons;
    property InputText: string read FInputText write FInputText;
    property MaxSize: Integer read FMaxSize write FMaxSize;
    property SecretViewActive: Boolean read FSecretViewActive write FSecretViewActive;
  end;

  TGUIColorPicker = class(TGUIBaseControl)
  private
    FColor: TColor;
  public
    constructor Create(const ABounds: TRectangle; const AText: string); overload;
    constructor Create(const ABounds: TRectangle; const AText: string; AColor: TColor); overload;
    function Draw: TColor;
    property Color: TColor read FColor write FColor;
  end;

  TGUIColorPanel = class(TGUIColorPicker)
  public
    function Draw: TColor;
  end;

  TGUIColorBarAlpha = class(TGUIBaseControl)
  private
    FAlpha: Single;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AAlpha: Single = 1.0);
    function Draw: Single;
    property Alpha: Single read FAlpha write FAlpha;
  end;

  TGUIColorBarHue = class(TGUIBaseControl)
  private
    FValue: Single;
  public
    constructor Create(const ABounds: TRectangle; const AText: string = '';
      AValue: Single = 0.0);
    function Draw: Single;
    property Value: Single read FValue write FValue;
  end;

  { TGUIColorPickerHSV }

  TGUIColorPickerHSV = class(TGUIBaseControl)
  private
    FColorHSV: TVector3;
  public
    constructor Create(const ABounds: TRectangle; const AText: string); overload;
    constructor Create(const ABounds: TRectangle; const AText: string; AColorHSV: TVector3); overload;
    function Draw: TVector3;
    property ColorHSV: TVector3 read FColorHSV write FColorHSV;
  end;

  TGUIColorPanelHSV = class(TGUIColorPickerHSV)
  public
    function Draw: TVector3;
  end;

  { Utility classes }

  TGUIPropertyHelper = class
  public
    class procedure SetControlStyle(AControl: Integer; AProperty: Integer; AValue: Integer);
    class function GetControlStyle(AControl: Integer; AProperty: Integer): Integer;
    class function GetTextWidth(const AText: string): Integer;
    class function IconText(AIconId: Integer; const AText: string): string;
  end;

  TGUIStyleProperty = class
  private
    FControlId: Word;
    FPropertyId: Word;
    FPropertyValue: Integer;
  public
    constructor Create(AControlId, APropertyId: Word; APropertyValue: Integer);
    property ControlId: Word read FControlId write FControlId;
    property PropertyId: Word read FPropertyId write FPropertyId;
    property PropertyValue: Integer read FPropertyValue write FPropertyValue;
  end;

implementation

{ TGUIBaseControl }

constructor TGUIBaseControl.Create(const ABounds: TRectangle; const AText: string);
begin
  FBounds := ABounds;
  FText := AText;
  FVisible := True;
  FEnabled := True;
end;

procedure TGUIBaseControl.SetBounds(const AValue: TRectangle);
begin
  FBounds := AValue;
end;

procedure TGUIBaseControl.SetText(const AValue: string);
begin
  FText := AValue;
end;

procedure TGUIBaseControl.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

procedure TGUIBaseControl.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
  if not AValue then
    GuiSetState(STATE_DISABLED)
  else
    GuiSetState(STATE_NORMAL);
end;

{ TGUIControlManager }

constructor TGUIControlManager.Create;
begin
  FControls := TList.Create;
  FAlpha := 1.0;
  FLocked := False;
  FCustomFont := GetFontDefault;
end;

destructor TGUIControlManager.Destroy;
begin
  ClearControls;
  FControls.Free;
  inherited Destroy;
end;

procedure TGUIControlManager.Enable;
begin
  GuiEnable;
end;

procedure TGUIControlManager.Disable;
begin
  GuiDisable;
end;

procedure TGUIControlManager.Lock;
begin
  GuiLock;
  FLocked := True;
end;

procedure TGUIControlManager.Unlock;
begin
  GuiUnlock;
  FLocked := False;
end;

function TGUIControlManager.IsLocked: Boolean;
begin
  Result := GuiIsLocked;
  FLocked := Result;
end;

procedure TGUIControlManager.SetAlpha(AAlpha: Single);
begin
  FAlpha := AAlpha;
  GuiSetAlpha(AAlpha);
end;

procedure TGUIControlManager.SetFont(AFont: TFont);
begin
  FCustomFont := AFont;
  GuiSetFont(AFont);
end;

function TGUIControlManager.GetFont: TFont;
begin
  Result := GuiGetFont;
end;

procedure TGUIControlManager.LoadStyle(const AFileName: string);
begin
  GuiLoadStyle(PChar(AFileName));
end;

procedure TGUIControlManager.LoadStyleDefault;
begin
  GuiLoadStyleDefault;
end;

procedure TGUIControlManager.EnableTooltip;
begin
  GuiEnableTooltip;
end;

procedure TGUIControlManager.DisableTooltip;
begin
  GuiDisableTooltip;
end;

procedure TGUIControlManager.SetTooltip(const ATooltip: string);
begin
  GuiSetTooltip(PChar(ATooltip));
end;

procedure TGUIControlManager.AddControl(AControl: TGUIBaseControl);
begin
  FControls.Add(AControl);
end;

procedure TGUIControlManager.RemoveControl(AControl: TGUIBaseControl);
begin
  FControls.Remove(AControl);
end;

procedure TGUIControlManager.ClearControls;
var
  I: Integer;
begin
  for I := 0 to FControls.Count - 1 do
    TGUIBaseControl(FControls[I]).Free;
  FControls.Clear;
end;

{ TGUIWindowBox }

constructor TGUIWindowBox.Create(const ABounds: TRectangle; const ATitle: string);
begin
  inherited Create(ABounds, ATitle);
  FCloseButtonClicked := False;
end;

function TGUIWindowBox.Draw: Boolean;
begin
  if not FVisible then Exit(False);
  FCloseButtonClicked := GuiWindowBox(FBounds, PChar(FText)) <> 0;
  Result := FCloseButtonClicked;
end;

{ TGUIGroupBox }

constructor TGUIGroupBox.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUIGroupBox.Draw;
begin
  if not FVisible then Exit;
  GuiGroupBox(FBounds, PChar(FText));
end;

{ TGUILine }

constructor TGUILine.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUILine.Draw;
begin
  if not FVisible then Exit;
  GuiLine(FBounds, PChar(FText));
end;

{ TGUIPanel }

constructor TGUIPanel.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUIPanel.Draw;
begin
  if not FVisible then Exit;
  GuiPanel(FBounds, PChar(FText));
end;

{ TGUITabBar }

constructor TGUITabBar.Create(const ABounds: TRectangle; const ATabs: array of string);
var
  I: Integer;
begin
  inherited Create(ABounds, '');
  FTabs := TStringList.Create;
  for I := Low(ATabs) to High(ATabs) do
    FTabs.Add(ATabs[I]);
  FActiveTab := 0;
end;

destructor TGUITabBar.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

function TGUITabBar.Draw: Integer;
var
  TabArray: PPChar;
  I: Integer;
begin
  if not FVisible then Exit(FActiveTab);

  TabArray := GetMem(FTabs.Count * SizeOf(PChar));
  try
    for I := 0 to FTabs.Count - 1 do
      TabArray[I] := PChar(FTabs[I]);

    GuiTabBar(FBounds, TabArray, FTabs.Count, @FActiveTab);
  finally
    FreeMem(TabArray);
  end;

  Result := FActiveTab;
end;

{ TGUIButton }

constructor TGUIButton.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
  FClicked := False;
end;

function TGUIButton.Draw: Boolean;
begin
  if not FVisible then Exit(False);
  FClicked := GuiButton(FBounds, PChar(FText)) <> 0;
  Result := FClicked;
end;

{ TGUILabelButton }

function TGUILabelButton.Draw: Boolean;
begin
  if not FVisible then Exit(False);
  FClicked := GuiLabelButton(FBounds, PChar(FText)) <> 0;
  Result := FClicked;
end;

{ TGUILabel }

constructor TGUILabel.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUILabel.Draw;
begin
  if not FVisible then Exit;
  GuiLabel(FBounds, PChar(FText));
end;

{ TGUIToggle }

constructor TGUIToggle.Create(const ABounds: TRectangle; const AText: string;
  AActive: Boolean);
begin
  inherited Create(ABounds, AText);
  FActive := AActive;
end;

function TGUIToggle.Draw: Boolean;
begin
  if not FVisible then Exit(FActive);
  GuiToggle(FBounds, PChar(FText), @FActive);
  Result := FActive;
end;

{ TGUIToggleGroup }

constructor TGUIToggleGroup.Create(const ABounds: TRectangle; const AText: string;
  AActiveIndex: Integer);
begin
  inherited Create(ABounds, AText);
  FActiveIndex := AActiveIndex;
end;

function TGUIToggleGroup.Draw: Integer;
begin
  if not FVisible then Exit(FActiveIndex);
  GuiToggleGroup(FBounds, PChar(FText), @FActiveIndex);
  Result := FActiveIndex;
end;

{ TGUIToggleSlider }

function TGUIToggleSlider.Draw: Integer;
begin
  if not FVisible then Exit(FActiveIndex);
  GuiToggleSlider(FBounds, PChar(FText), @FActiveIndex);
  Result := FActiveIndex;
end;

{ TGUICheckBox }

constructor TGUICheckBox.Create(const ABounds: TRectangle; const AText: string;
  AChecked: Boolean);
begin
  inherited Create(ABounds, AText);
  FChecked := AChecked;
end;

function TGUICheckBox.Draw: Boolean;
begin
  if not FVisible then Exit(FChecked);
  GuiCheckBox(FBounds, PChar(FText), @FChecked);// <> 0 then
  Result := FChecked;
end;

{ TGUIComboBox }

constructor TGUIComboBox.Create(const ABounds: TRectangle; const AText: string;
  AActiveIndex: Integer);
begin
  inherited Create(ABounds, AText);
  FActiveIndex := AActiveIndex;
end;

function TGUIComboBox.Draw: Integer;
begin
  if not FVisible then Exit(FActiveIndex);
  GuiComboBox(FBounds, PChar(FText), @FActiveIndex);
  Result := FActiveIndex;
end;

{ TGUIDropdownBox }

constructor TGUIDropdownBox.Create(const ABounds: TRectangle; const AText: string;
  AActiveIndex: Integer; AEditMode: Boolean);
begin
  inherited Create(ABounds, AText);
  FActiveIndex := AActiveIndex;
  FEditMode := AEditMode;
end;

function TGUIDropdownBox.Draw: Integer;
var
  mousePosition: TVector2;
begin
  if not FVisible then Exit(FActiveIndex);
  mousePosition := GetMousePosition;
  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
      begin
        if not CheckCollisionPointRec(mousePosition, FBounds) and FEditMode then
        FEditMode := False;
        // Кликнули по полю - активируем
        if CheckCollisionPointRec(mousePosition, FBounds) and FEditMode then
        FEditMode := False
        else
        if CheckCollisionPointRec(mousePosition, FBounds) and not FEditMode then
        FEditMode := True;
      end;
  GuiDropdownBox(FBounds, PChar(FText), @FActiveIndex, FEditMode);
  Result := FActiveIndex;
end;

{ TGUISpinner }

constructor TGUISpinner.Create(const ABounds: TRectangle; const AText: string;
  AValue: Integer; AMinValue: Integer; AMaxValue: Integer; AEditMode: Boolean);
begin
  inherited Create(ABounds, AText);
  FValue := AValue;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FEditMode := AEditMode;
end;

function TGUISpinner.Draw: Integer;
begin
  if not FVisible then Exit(FValue);
  GuiSpinner(FBounds, PChar(FText), @FValue, FMinValue, FMaxValue, FEditMode);
  Result := FValue;
end;

{ TGUIValueBox }

constructor TGUIValueBox.Create(const ABounds: TRectangle; const AText: string;
  AValue: Integer; AMinValue: Integer; AMaxValue: Integer; AEditMode: Boolean);
begin
  inherited Create(ABounds, AText);
  FValue := AValue;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FEditMode := AEditMode;
end;

function TGUIValueBox.Draw: Integer;
begin
  if not FVisible then Exit(FValue);
  GuiValueBox(FBounds, PChar(FText), @FValue, FMinValue, FMaxValue, FEditMode);
  Result := FValue;
end;

{ TGUIValueBoxFloat }

constructor TGUIValueBoxFloat.Create(const ABounds: TRectangle; const AText: string;
  const ATextValue: string; AValue: Single; AEditMode: Boolean);
begin
  inherited Create(ABounds, AText);
  FTextValue := ATextValue;
  FValue := AValue;
  FEditMode := AEditMode;
end;

function TGUIValueBoxFloat.Draw: Single;
var
  TempValue: Single;
begin
  if not FVisible then Exit(FValue);
  TempValue := FValue;
  GuiValueBoxFloat(FBounds, PChar(FText), PChar(FTextValue), @TempValue, FEditMode);
  FValue := TempValue;
  Result := FValue;
end;

{ TGUITextBox }

constructor TGUITextBox.Create(const ABounds: TRectangle; const AText: string;
  AMaxLength: Integer; AEditMode: Boolean);
begin
  inherited Create(ABounds, AText);
  FMaxLength := AMaxLength;
  FEditMode := AEditMode;
  FTextBuffer := AText;

  // Выделяем память для буфера
  GetMem(FBuffer, FMaxLength + 1);
  FillChar(FBuffer^, FMaxLength + 1, 0);

  // Копируем текст в буфер
  if Length(FTextBuffer) > 0 then
    StrLCopy(FBuffer, PChar(FTextBuffer), FMaxLength);
end;

destructor TGUITextBox.Destroy;
begin
  FreeMem(FBuffer);
  inherited Destroy;
end;

function TGUITextBox.Draw: Boolean;
var
  mousePosition: TVector2;
begin
  if not FVisible then
  begin
    Result := FEditMode;
    Exit;
  end;

  // Проверяем клик по полю
  mousePosition := GetMousePosition;
  //if CheckCollisionPointRec(mousePosition, FBounds) and

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    // Кликнули по полю - активируем
    if CheckCollisionPointRec(mousePosition, FBounds) then
    FEditMode := True else
    FEditMode := False;
  end;

  // Обрабатываем  для выхода
  if FEditMode and IsKeyPressed(KEY_ENTER) then
  begin
    FEditMode := False;
    Result := False;
    Exit;
  end;
  // Вызываем GuiTextBox

  Result := GuiTextBox(FBounds, FBuffer, FMaxLength, FEditMode) <> 0;

  // Обновляем текстовый буфер
  FTextBuffer := string(FBuffer);

  // НЕ сбрасываем FEditMode автоматически!

end;

function TGUITextBox.IsEditing: Boolean;
begin
  Result := FEditMode;
end;

function TGUITextBox.GetText: string;
begin
  Result := FTextBuffer;
end;

procedure TGUITextBox.SetText(const AValue: string);
begin
  FTextBuffer := AValue;
  if Length(FTextBuffer) > FMaxLength then
    SetLength(FTextBuffer, FMaxLength);

  if Assigned(FBuffer) then
    StrLCopy(FBuffer, PChar(FTextBuffer), FMaxLength);
end;

procedure TGUITextBox.SetEditMode(AValue: Boolean);
begin
  if FEditMode <> AValue then
  begin
    FEditMode := AValue;
    // При активации копируем текст в буфер
    if FEditMode then
    begin
      // Копируем текущий текст в буфер для редактирования
      if Length(FTextBuffer) > 0 then
        StrLCopy(FBuffer, PChar(FTextBuffer), FMaxLength)
      else
        FBuffer[0] := #0;
    end;
  end;
end;

{ TGUISlider }

constructor TGUISlider.Create(const ABounds: TRectangle; const ATextLeft: string;
  const ATextRight: string; AValue: Single; AMinValue: Single; AMaxValue: Single);
begin
  inherited Create(ABounds, '');
  FTextLeft := ATextLeft;
  FTextRight := ATextRight;
  FValue := AValue;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
end;

function TGUISlider.Draw: Single;
begin
  if not FVisible then Exit(FValue);
  GuiSlider(FBounds, PChar(FTextLeft), PChar(FTextRight), @FValue, FMinValue, FMaxValue);
  Result := FValue;
end;

{ TGUISliderBar }

function TGUISliderBar.Draw: Single;
begin
  if not FVisible then Exit(FValue);
  GuiSliderBar(FBounds, PChar(FTextLeft), PChar(FTextRight), @FValue, FMinValue, FMaxValue);
  Result := FValue;
end;

{ TGUIProgressBar }

function TGUIProgressBar.Draw: Single;
begin
  if not FVisible then Exit(FValue);
  GuiProgressBar(FBounds, PChar(FTextLeft), PChar(FTextRight), @FValue, FMinValue, FMaxValue);
  Result := FValue;
end;

{ TGUIStatusBar }

constructor TGUIStatusBar.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUIStatusBar.Draw;
begin
  if not FVisible then Exit;
  GuiStatusBar(FBounds, PChar(FText));
end;

{ TGUIDummyRec }

constructor TGUIDummyRec.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
end;

procedure TGUIDummyRec.Draw;
begin
  if not FVisible then Exit;
  GuiDummyRec(FBounds, PChar(FText));
end;

{ TGUIListView }

constructor TGUIListView.Create(const ABounds: TRectangle; const AText: string;
  AScrollIndex: Integer; AActiveIndex: Integer);
begin
  inherited Create(ABounds, AText);
  FScrollIndex := AScrollIndex;
  FActiveIndex := AActiveIndex;
end;

function TGUIListView.Draw: Integer;
begin
  if not FVisible then Exit(FActiveIndex);
  GuiListView(FBounds, PChar(FText), @FScrollIndex, @FActiveIndex);
  Result := FActiveIndex;
end;

{ TGUIListViewEx }

constructor TGUIListViewEx.Create(const ABounds: TRectangle; AItems: TStringList;
  AScrollIndex: Integer; AActiveIndex: Integer; AFocusIndex: Integer);
begin
  inherited Create(ABounds, '');
  FItems := TStringList.Create;
  FItems.Assign(AItems);
  FScrollIndex := AScrollIndex;
  FActiveIndex := AActiveIndex;
  FFocusIndex := AFocusIndex;
end;

destructor TGUIListViewEx.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TGUIListViewEx.Draw: Integer;
var
  ItemsArray: PPChar;
  I: Integer;
begin
  if not FVisible then Exit(FActiveIndex);

  ItemsArray := GetMem(FItems.Count * SizeOf(PChar));
  try
    for I := 0 to FItems.Count - 1 do
      ItemsArray[I] := PChar(FItems[I]);

    GuiListViewEx(FBounds, ItemsArray, FItems.Count, @FScrollIndex, @FActiveIndex, @FFocusIndex);
  finally
    FreeMem(ItemsArray);
  end;

  Result := FActiveIndex;
end;

{ TGUIMessageBox }

constructor TGUIMessageBox.Create(const ABounds: TRectangle; const ATitle,
  AMessage, AButtons: string);
begin
  inherited Create(ABounds, '');
  FTitle := ATitle;
  FMessage := AMessage;
  FButtons := AButtons;

  // Инициализация переменных для перетаскивания
  FDragMode := False;
  FSupportDrag := True;
  FPanOffset := Vector2Create(0, 0);
  FOriginalBounds := ABounds; // Сохраняем исходные координаты
end;

function TGUIMessageBox.Draw: Integer;
var
  mousePosition: TVector2;
  titleBarRect: TRectangle;
begin
  if not FVisible then Exit(-1);

  // Обновление перетаскивания окна
  if FSupportDrag then
  begin
    mousePosition := GetMousePosition;

    titleBarRect := RectangleCreate(
      FBounds.x,
      FBounds.y,
      FBounds.width,
      RAYGUI_WINDOWBOX_STATUSBAR_HEIGHT );

    // Начинаем перетаскивание при нажатии на заголовок
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and
       CheckCollisionPointRec(mousePosition, titleBarRect) then
    begin
      FDragMode := True;
      FPanOffset.x := mousePosition.x - FBounds.x;
      FPanOffset.y := mousePosition.y - FBounds.y;
    end;

    // Обновление позиции окна при перетаскивании
    if FDragMode then
    begin
      FBounds.x := mousePosition.x - FPanOffset.x;
      FBounds.y := mousePosition.y - FPanOffset.y;

      // Проверка границ экрана
      if FBounds.x < 0 then
        FBounds.x := 0
      else if FBounds.x > (GetScreenWidth - FBounds.width) then
        FBounds.x := GetScreenWidth - FBounds.width;

      if FBounds.y < 0 then
        FBounds.y := 0
      else if FBounds.y > (GetScreenHeight - FBounds.height) then
        FBounds.y := GetScreenHeight - FBounds.height;

      // Завершение перетаскивания
      if IsMouseButtonReleased(MOUSE_LEFT_BUTTON) then
        FDragMode := False;
    end;
  end;

  // Отображение MessageBox
  Result := GuiMessageBox(FBounds, PChar(FTitle), PChar(FMessage), PChar(FButtons));
end;

procedure TGUIMessageBox.ResetPosition;
begin
  FBounds := FOriginalBounds;
end;

procedure TGUIMessageBox.CenterOnScreen;
begin
  FBounds.x := GetScreenWidth div 2 - FBounds.width / 2;
  FBounds.y := GetScreenHeight div 2 - FBounds.height / 2;
  FOriginalBounds := FBounds; // Обновляем исходные координаты
end;

{ TGUIInputBox }

constructor TGUIInputBox.Create(const ABounds: TRectangle; const ATitle,
  AMessage, AButtons: string; const AText: string; AMaxSize: Integer;
  ASecretViewActive: Boolean);
begin
  inherited Create(ABounds, '');
  FTitle := ATitle;
  FMessage := AMessage;
  FButtons := AButtons;
  FInputText := AText;
  FMaxSize := AMaxSize;
  FSecretViewActive := ASecretViewActive;
end;

function TGUIInputBox.Draw: Integer;
var
  Buffer: PChar;
begin
  if not FVisible then Exit(-1);

  Buffer := StrAlloc(FMaxSize + 1);
  try
    StrPCopy(Buffer, FInputText);
    Result := GuiTextInputBox(FBounds, PChar(FTitle), PChar(FMessage),
      PChar(FButtons), Buffer, FMaxSize, @FSecretViewActive);
    FText := StrPas(Buffer);
  finally
    StrDispose(Buffer);
  end;
end;

{ TGUIColorPicker }

constructor TGUIColorPicker.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
  FColor := WHITE;
end;

constructor TGUIColorPicker.Create(const ABounds: TRectangle; const AText: string;
  AColor: TColor);
begin
  inherited Create(ABounds, AText);
  FColor := AColor;
end;

function TGUIColorPicker.Draw: TColor;
begin
  if not FVisible then Exit(FColor);
  GuiColorPicker(FBounds, PChar(FText), @FColor);
  Result := FColor;
end;

{ TGUIColorPanel }

function TGUIColorPanel.Draw: TColor;
begin
  if not FVisible then Exit(FColor);
  GuiColorPanel(FBounds, PChar(FText), @FColor);
  Result := FColor;
end;

{ TGUIColorBarAlpha }

constructor TGUIColorBarAlpha.Create(const ABounds: TRectangle; const AText: string;
  AAlpha: Single);
begin
  inherited Create(ABounds, AText);
  FAlpha := AAlpha;
end;

function TGUIColorBarAlpha.Draw: Single;
begin
  if not FVisible then Exit(FAlpha);
  GuiColorBarAlpha(FBounds, PChar(FText), @FAlpha);
  Result := FAlpha;
end;

{ TGUIColorBarHue }

constructor TGUIColorBarHue.Create(const ABounds: TRectangle; const AText: string;
  AValue: Single);
begin
  inherited Create(ABounds, AText);
  FValue := AValue;
end;

function TGUIColorBarHue.Draw: Single;
begin
  if not FVisible then Exit(FValue);
  GuiColorBarHue(FBounds, PChar(FText), @FValue);
  Result := FValue;
end;

{ TGUIColorPickerHSV }

constructor TGUIColorPickerHSV.Create(const ABounds: TRectangle; const AText: string);
begin
  inherited Create(ABounds, AText);
  FColorHSV := Vector3Create(0, 0, 0);
end;

constructor TGUIColorPickerHSV.Create(const ABounds: TRectangle; const AText: string;
  AColorHSV: TVector3);
begin
  inherited Create(ABounds, AText);
  FColorHSV := AColorHSV;
end;

function TGUIColorPickerHSV.Draw: TVector3;
begin
  if not FVisible then Exit(FColorHSV);
  GuiColorPickerHSV(FBounds, PChar(FText), @FColorHSV);
  Result := FColorHSV;
end;

{ TGUIColorPanelHSV }

function TGUIColorPanelHSV.Draw: TVector3;
begin
  if not FVisible then Exit(FColorHSV);
  GuiColorPanelHSV(FBounds, PChar(FText), @FColorHSV);
  Result := FColorHSV;
end;

{ TGUIPropertyHelper }

class procedure TGUIPropertyHelper.SetControlStyle(AControl: Integer;
  AProperty: Integer; AValue: Integer);
begin
  GuiSetStyle(AControl, AProperty, AValue);
end;

class function TGUIPropertyHelper.GetControlStyle(AControl: Integer;
  AProperty: Integer): Integer;
begin
  Result := GuiGetStyle(AControl, AProperty);
end;

class function TGUIPropertyHelper.GetTextWidth(const AText: string): Integer;
begin
  Result := GuiGetTextWidth(PChar(AText));
end;

class function TGUIPropertyHelper.IconText(AIconId: Integer; const AText: string): string;
begin
  Result := GuiIconText(AIconId, PChar(AText));
end;

{ TGUIStyleProperty }

constructor TGUIStyleProperty.Create(AControlId, APropertyId: Word;
  APropertyValue: Integer);
begin
  FControlId := AControlId;
  FPropertyId := APropertyId;
  FPropertyValue := APropertyValue;
end;

end.
