unit ray_gui;

{$mode objfpc}{$H+}

interface

uses ray_header;

{---------------------------------------------------------------------------------- }
{ Defines and Macros }
{---------------------------------------------------------------------------------- }
  (* Style property *)
  type
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
      controlId : word;
      propertyId : word;
      propertyValue : longint;
    end;

  (* Gui control state *)
  type
    PGuiControlState = ^TGuiControlState;
    TGuiControlState =  Longint;
    Const
      GUI_STATE_NORMAL = 0;
      GUI_STATE_FOCUSED = 1;
      GUI_STATE_PRESSED = 2;
      GUI_STATE_DISABLED = 3;

  (* Gui control text alignment *)
  type
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment =  Longint;
  Const
    GUI_TEXT_ALIGN_LEFT = 0;
    GUI_TEXT_ALIGN_CENTER = 1;
    GUI_TEXT_ALIGN_RIGHT = 2;

  (* Gui controls *)
  type
  PGuiControl = ^TGuiControl;
  TGuiControl =  Longint;
  Const
    DEFAULT = 0;
    _LABEL = 1;
    BUTTON = 2;
    TOGGLE = 3;
    SLIDER = 4;
    PROGRESSBAR = 5;
    CHECKBOX = 6;
    COMBOBOX = 7;
    DROPDOWNBOX = 8;
    TEXTBOX = 9;
    VALUEBOX = 10;
    SPINNER = 11;
    LISTVIEW = 12;
    COLORPICKER = 13;
    SCROLLBAR = 14;
    STATUSBAR = 15;

  (* Gui base properties for every control *)
  type
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty =  Longint;
  Const
    BORDER_COLOR_NORMAL = 0;
    BASE_COLOR_NORMAL = 1;
    TEXT_COLOR_NORMAL = 2;
    BORDER_COLOR_FOCUSED = 3;
    BASE_COLOR_FOCUSED = 4;
    TEXT_COLOR_FOCUSED = 5;
    BORDER_COLOR_PRESSED = 6;
    BASE_COLOR_PRESSED = 7;
    TEXT_COLOR_PRESSED = 8;
    BORDER_COLOR_DISABLED = 9;
    BASE_COLOR_DISABLED = 10;
    TEXT_COLOR_DISABLED = 11;
    BORDER_WIDTH = 12;
    TEXT_PADDING = 13;
    TEXT_ALIGNMENT = 14;
    RESERVED = 15;

  (* Gui extended properties depend on control *)
  type
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty =  Longint;
  Const
    TEXT_SIZE = 16;
    TEXT_SPACING = 17;
    LINE_COLOR = 18;
    BACKGROUND_COLOR = 19;

  (* Toggle / ToggleGroup *)
  type
   PGuiToggleProperty = ^TGuiToggleProperty;
   TGuiToggleProperty =  Longint;
  Const
    GROUP_PADDING = 16;

  (* Slider / SliderBar *)
  type
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty =  Longint;
  Const
    SLIDER_WIDTH = 16;
    SLIDER_PADDING = 17;

  (* ProgressBar *)
  type
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty =  Longint;
  Const
    PROGRESS_PADDING = 16;

  (* CheckBox *)
  type
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty =  Longint;
  Const
    CHECK_PADDING = 16;

  (* ComboBox *)
  type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty =  Longint;
  Const
    COMBO_BUTTON_WIDTH = 16;
    COMBO_BUTTON_PADDING = 17;

  (* DropdownBox *)
  type
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty =  Longint;
  Const
    ARROW_PADDING = 16;
    DROPDOWN_ITEMS_PADDING = 17;

  (* TextBox / TextBoxMulti / ValueBox / Spinner *)
  type
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty =  Longint;
  Const
    TEXT_INNER_PADDING = 16;
    TEXT_LINES_PADDING = 17;
    COLOR_SELECTED_FG = 18;
    COLOR_SELECTED_BG = 19;

  (* Spinner *)
  type
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty =  Longint;
  Const
    SPIN_BUTTON_WIDTH = 16;
    SPIN_BUTTON_PADDING = 17;

  (* ScrollBar *)
  type
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty =  Longint;
  Const
    ARROWS_SIZE = 16;
    ARROWS_VISIBLE = 17;
    SCROLL_SLIDER_PADDING = 18;
    SCROLL_SLIDER_SIZE = 19;
    SCROLL_PADDING = 20;
    SCROLL_SPEED = 21;

  (* ScrollBar side *)
  type
  PGuiScrollBarSide = ^TGuiScrollBarSide;
  TGuiScrollBarSide =  Longint;
  Const
    SCROLLBAR_LEFT_SIDE = 0;
    SCROLLBAR_RIGHT_SIDE = 1;

  (* ListView *)
  type
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty =  Longint;
  Const
    LIST_ITEMS_HEIGHT = 16;
    LIST_ITEMS_PADDING = 17;
    SCROLLBAR_WIDTH = 18;
    SCROLLBAR_SIDE = 19;

  (* ColorPicker *)
  type
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty =  Longint;
  Const
    COLOR_SELECTOR_SIZE = 16;
    HUEBAR_WIDTH = 17;
    HUEBAR_PADDING = 18;
    HUEBAR_SELECTOR_HEIGHT = 19;
    HUEBAR_SELECTOR_OVERFLOW = 20;

{---------------------------------------------------------------------------------- }
{ Module Functions Declaration }
{---------------------------------------------------------------------------------- }

(* State modification functions *)
procedure GuiEnable;cdecl;external cDllName; // Enable gui controls (global state)
procedure GuiDisable;cdecl;external cDllName; // Disable gui controls (global state)
procedure GuiLock;cdecl;external cDllName; // Lock gui controls (global state)
procedure GuiUnlock;cdecl;external cDllName; // Unlock gui controls (global state)
procedure GuiFade(alpha:single);cdecl;external cDllName; // Set gui controls alpha (global state), alpha goes from 0.0 to 1.0
procedure GuiSetState(state:longint);cdecl;external cDllName; // Set gui state (global state)
function GuiGetState:longint;cdecl;external cDllName; // Get gui state (global state)

(* Font set/get functions *)
procedure GuiSetFont(font:TFont);cdecl;external cDllName; // Set gui custom font (global state)
function GuiGetFont:TFont;cdecl;external cDllName; // Get gui custom font (global state)

(* Style set/get functions *)
procedure GuiSetStyle(control:longint; _property:longint; value:longint);cdecl;external cDllName; // Set one style property
function GuiGetStyle(control:longint; _property:longint):longint;cdecl;external cDllName; //Get one style property

(* Tooltips set functions *)
procedure GuiEnableTooltip;cdecl;external cDllName; // Enable gui tooltips
procedure GuiDisableTooltip;cdecl;external cDllName; // Disable gui tooltips
procedure GuiSetTooltip(tooltip:Pchar);cdecl;external cDllName; // Set current tooltip for display
procedure GuiClearTooltip;cdecl;external cDllName; // Clear any tooltip registered }

(* Container/separator controls, useful for controls organization *)
function GuiWindowBox(bounds:TRectangle; title:Pchar):boolean;cdecl;external cDllName; // Window Box control, shows a window that can be closed
procedure GuiGroupBox(bounds:TRectangle; text:Pchar);cdecl;external cDllName; // Group Box control with text name
procedure GuiLine(bounds:TRectangle; text:Pchar);cdecl;external cDllName; // Line separator control, could contain text
procedure GuiPanel(bounds:TRectangle);cdecl;external cDllName; // Panel control, useful to group controls
function GuiScrollPanel(bounds:TRectangle; content:TRectangle; scroll:PVector2):TRectangle;cdecl;external cDllName; //Scroll Panel control

(* Basic controls set *)
procedure GuiLabel(bounds:TRectangle; text:Pchar);cdecl;external cDllName; // Label control, shows text
function GuiButton(bounds:TRectangle; text:Pchar):boolean;cdecl;external cDllName; // Button control, returns true when clicked
function GuiLabelButton(bounds:TRectangle; text:Pchar):boolean;cdecl;external cDllName; // Label button control, show true when clicked
function GuiImageButton(bounds:TRectangle; text:Pchar; texture:TTexture2D):boolean;cdecl;external cDllName; // Image button control, returns true when clicked
function GuiImageButtonEx(bounds:TRectangle; text:Pchar; texture:TTexture2D; texSource:TRectangle):boolean;cdecl;external cDllName; // Image button extended control, returns true when clicked
function GuiToggle(bounds:TRectangle; text:Pchar; active:boolean):boolean;cdecl;external cDllName; // Toggle Button control, returns true when active
function GuiToggleGroup(bounds:TRectangle; text:Pchar; active:longint):longint;cdecl;external cDllName; // Toggle Group control, returns active toggle index
function GuiCheckBox(bounds:TRectangle; text:Pchar; checked:boolean):boolean;cdecl;external cDllName; // Check Box control, returns true when active
function GuiComboBox(bounds:TRectangle; text:Pchar; active:longint):longint;cdecl;external cDllName; // Combo Box control, returns selected item index
function GuiDropdownBox(bounds:TRectangle; text:Pchar; active:Plongint; editMode:boolean):boolean;cdecl;external cDllName; // Dropdown Box control, returns selected item
function GuiSpinner(bounds:TRectangle; text:Pchar; value:Plongint; minValue:longint; maxValue:longint;editMode:boolean):boolean;cdecl;external cDllName; // Spinner control, returns selected value
function GuiValueBox(bounds:TRectangle; text:Pchar; value:Plongint; minValue:longint; maxValue:longint;editMode:boolean):boolean;cdecl;external cDllName; // Value Box control, updates input text with numbers
function GuiTextBox(bounds:TRectangle; text:Pchar; textSize:longint; editMode:boolean):boolean;cdecl;external cDllName; // Text Box control, updates input text
function GuiTextBoxMulti(bounds:TRectangle; text:Pchar; textSize:longint; editMode:boolean):boolean;cdecl;external cDllName; // Text Box control with multiple lines
function GuiSlider(bounds:TRectangle; textLeft:Pchar; textRight:Pchar; value:single; minValue:single;maxValue:single):single;cdecl;external cDllName; // Slider control, returns selected value
function GuiSliderBar(bounds:TRectangle; textLeft:Pchar; textRight:Pchar; value:single; minValue:single;maxValue:single):single;cdecl;external cDllName; // Slider Bar control, returns selected value
function GuiProgressBar(bounds:TRectangle; textLeft:Pchar; textRight:Pchar; value:single; minValue:single;maxValue:single):single;cdecl;external cDllName; // Progress Bar control, shows current progress value
procedure GuiStatusBar(bounds:TRectangle; text:Pchar);cdecl;external cDllName; // Status Bar control, shows info text
procedure GuiDummyRec(bounds:TRectangle; text:Pchar);cdecl;external cDllName; // Dummy control for placeholders
function GuiScrollBar(bounds:TRectangle; value:longint; minValue:longint; maxValue:longint):longint;cdecl;external cDllName; // Scroll Bar control
function GuiGrid(bounds:TRectangle; spacing:single; subdivs:longint):TVector2;cdecl;external cDllName; // Grid control

(* Advance controls set *)
function GuiListView(bounds:TRectangle; text:Pchar; scrollIndex:Plongint; active:longint):longint;cdecl;external cDllName; // List View control, returns selected list item index
function GuiListViewEx(bounds:TRectangle; text:PPchar; count:longint; focus:Plongint; scrollIndex:Plongint; active:longint):longint;cdecl;external cDllName; // List View with extended parameters
function GuiMessageBox(bounds:TRectangle; title:Pchar; message:Pchar; buttons:Pchar):longint;cdecl;external cDllName; // Message Box control, displays a message }
function GuiTextInputBox(bounds:TRectangle; title:Pchar; message:Pchar; buttons:Pchar; text:Pchar):longint;cdecl;external cDllName; // Text Input Box control, ask for text
function GuiColorPicker(bounds:TRectangle; color:TColor):TColor;cdecl;external cDllName; // Color Picker control (multiple color controls)
function GuiColorPanel(bounds:TRectangle; color:TColor):TColor;cdecl;external cDllName; // Color Panel control
function GuiColorBarAlpha(bounds:TRectangle; alpha:single):single;cdecl;external cDllName; // Color Bar Alpha control
function GuiColorBarHue(bounds:TRectangle; value:single):single;cdecl;external cDllName; // Color Bar Hue control

(* Styles loading functions *)
procedure GuiLoadStyle(fileName:Pchar);cdecl;external cDllName; // Load style file (.rgs)
procedure GuiLoadStyleDefault;cdecl;external cDllName; // Load style default over global style
procedure GuiDrawIcon(iconId:longint; position:TVector2; pixelSize:longint; color:TColor);cdecl;external cDllName;
function GuiGetIcons:Pdword;cdecl;external cDllName; // Get full icons data pointer
function GuiGetIconData(iconId:longint):Pdword;cdecl;external cdllName; // Get icon bit data
procedure GuiSetIconData(iconId:longint; data:Pdword);cdecl;external cDllName; // Set icon bit data
procedure GuiSetIconPixel(iconId:longint; x:longint; y:longint);cdecl;external cDllName; // Set icon pixel value
procedure GuiClearIconPixel(iconId:longint; x:longint; y:longint);cdecl;external cDllName; // Clear icon pixel value
function GuiCheckIconPixel(iconId:longint; x:longint; y:longint):boolean;cdecl;external cDllName; // Check icon pixel value


implementation

end.

