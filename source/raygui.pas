{********************************************************************************************
*                                                                                           *
*   raygui v3.0 - A simple and easy-to-use immediate-mode gui library                       *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also       *
*   available as a standalone library, as long as input and drawing functions are provided. *
*********************************************************************************************}

unit raygui;

{$mode ObjFPC}{$H+}

interface

uses
 raylib;

type
// GlyphInfo, font characters glyphs info
  PGlyphInfo = ^TGlyphInfo;
  TGlyphInfo = record
    value    : longint;// Character value (Unicode)
    offsetX  : longint;// Character offset X when drawing
    offsetY  : longint;// Character offset Y when drawing
    advanceX : longint;// Character advance position X
    image    : TImage; // Character image data
  end;

  // Style property
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
    controlId     : Word;
    propertyId    : Word;
    propertyValue : longint;
  end;

  // Gui control state
  PGuiControlState = ^TGuiControlState;
  TGuiControlState = Longint;
    const
      GUI_STATE_NORMAL   = 0;
      GUI_STATE_FOCUSED  = 1;
      GUI_STATE_PRESSED  = 2;
      GUI_STATE_DISABLED = 3;

type
  // Gui control text alignment
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment = Longint;
    const
      GUI_TEXT_ALIGN_LEFT   = 0;
      GUI_TEXT_ALIGN_CENTER = 1;
      GUI_TEXT_ALIGN_RIGHT  = 2;

type
  // Gui controls
  PGuiControl = ^TGuiControl;
  TGuiControl = Longint;
    const
      DEFAULT       = 0; // Generic control -> populates to all controls when set
      LABELs        = 1; // Used also for: LABELBUTTON
      BUTTON        = 2;
      TOGGLE        = 3; // Used also for: TOGGLEGROUP
      SLIDER        = 4; // Used also for: SLIDERBAR
      PROGRESSBAR   = 5;
      CHECKBOX      = 6;
      COMBOBOX      = 7;
      DROPDOWNBOX   = 8;
      TEXTBOX       = 9;// Used also for: TEXTBOXMULTI
      VALUEBOX      = 10;
      SPINNER       = 11;
      LISTVIEW      = 12;
      COLORPICKER   = 13;
      SCROLLBAR     = 14;
      STATUSBAR     = 15;

type
  // Gui base properties for every control
  // NOTE: RAYGUI_MAX_PROPS_BASE properties (by default 16 properties)
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty = Longint;
    const
      BORDER_COLOR_NORMAL     = 0;
      BASE_COLOR_NORMAL       = 1;
      TEXT_COLOR_NORMAL       = 2;
      BORDER_COLOR_FOCUSED    = 3;
      BASE_COLOR_FOCUSED      = 4;
      TEXT_COLOR_FOCUSED      = 5;
      BORDER_COLOR_PRESSED    = 6;
      BASE_COLOR_PRESSED      = 7;
      TEXT_COLOR_PRESSED      = 8;
      BORDER_COLOR_DISABLED   = 9;
      BASE_COLOR_DISABLED     = 10;
      TEXT_COLOR_DISABLED     = 11;
      BORDER_WIDTH            = 12;
      TEXT_PADDING            = 13;
      TEXT_ALIGNMENT          = 14;
      RESERVED                = 16;

type
  // DEFAULT extended properties
  // NOTE: Those properties are actually common to all controls
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty = Longint;
    const
      TEXT_SIZE        = 16;
      TEXT_SPACING     = 17;
      LINE_COLOR       = 18;
      BACKGROUND_COLOR = 19;

type
  // Toggle/ToggleGroup
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Longint;
    const
      GROUP_PADDING = 16;

type
  // Slider/SliderBar
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Longint;
    const
      SLIDER_WIDTH   = 16;
      SLIDER_PADDING = 17;

type
  // ProgressBar
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Longint;
    const
      PROGRESS_PADDING = 16;

type
  // CheckBox
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Longint;
    const
      CHECK_PADDING = 16;

type
  // DropdownBox
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Longint;
    const
      ARROW_PADDING = 16;
      DROPDOWN_ITEMS_PADDING = 17;

type
  // TextBox/TextBoxMulti/ValueBox/Spinner
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Longint;
    const
      TEXT_INNER_PADDING = 16;
      TEXT_LINES_PADDING = 17;
      COLOR_SELECTED_FG  = 18;
      COLOR_SELECTED_BG  = 19;

type
  // Spinner
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty = Longint;
    const
      SPIN_BUTTON_WIDTH   = 16;
      SPIN_BUTTON_PADDING = 17;

type
  // ScrollBar
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Longint;
    const
      ARROWS_SIZE           = 16;
      ARROWS_VISIBLE        = 17;
      SCROLL_SLIDER_PADDING = 18;
      SCROLL_SLIDER_SIZE    = 19;
      SCROLL_PADDING        = 20;
      SCROLL_SPEED          = 21;

type
  // ScrollBar side
  PGuiScrollBarSide = ^TGuiScrollBarSide;
  TGuiScrollBarSide = Longint;
    const
      SCROLLBAR_LEFT_SIDE   = 0;
      SCROLLBAR_RIGHT_SIDE  = 1;

type
  // ListView
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Longint;
    const
      LIST_ITEMS_HEIGHT   = 16;
      LIST_ITEMS_PADDING  = 17;
      SCROLLBAR_WIDTH     = 18;
      SCROLLBAR_SIDE      = 19;

type
  // ColorPicker
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Longint;
    const
      COLOR_SELECTOR_SIZE      = 16;
      HUEBAR_WIDTH             = 17; // Right hue bar width
      HUEBAR_PADDING           = 18; // Right hue bar separation from panel
      HUEBAR_SELECTOR_HEIGHT   = 19; // Right hue bar selector height
      HUEBAR_SELECTOR_OVERFLOW = 20; // Right hue bar selector overflow

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

// Global gui state control functions
procedure GuiEnable; cdecl;external cDllName;// Enable gui controls (global state)
procedure GuiDisable; cdecl;external cDllName;// Disable gui controls (global state)
procedure GuiLock; cdecl;external cDllName;// Lock gui controls (global state)
procedure GuiUnlock; cdecl;external cDllName;// Unlock gui controls (global state)
function  GuiIsLocked:boolean; cdecl;external cDllName;// Check if gui is locked (global state)
procedure GuiFade(alpha: single); cdecl;external cDllName;// Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f
procedure GuiSetState(state: longint); cdecl;external cDllName;// Set gui state (global state)
function GuiGetState:longint; cdecl;external cDllName;// Get gui state (global state)

// Font set/get functions
procedure GuiSetFont(font:TFont); cdecl;external cDllName;// Set gui custom font (global state)
function GuiGetFont:TFont; cdecl;external cDllName;// Get gui custom font (global state)

// Style set/get functions
procedure GuiSetStyle(control: longint; property_: longint; value: longint); cdecl;external cDllName;// Set one style property
function GuiGetStyle(control: longint; property_: longint): longint; cdecl;external cDllName;// Get one style property

// Container/separator controls, useful for controls organization
function GuiWindowBox(bounds: TRectangle; const title: pchar): boolean; cdecl;external cDllName;// Window Box control, shows a window that can be closed
procedure GuiGroupBox(bounds: TRectangle; const text: pchar); cdecl;external cDllName;// Group Box control with text name
procedure GuiLine(bounds: TRectangle; const text: pchar); cdecl;external cDllName;// Line separator control, could contain text
procedure GuiPanel(bounds: TRectangle); cdecl;external cDllName;// Panel control, useful to group controls
function GuiScrollPanel(bounds: TRectangle; content: TRectangle; scroll: PVector2): TRectangle; cdecl;external cDllName;// Scroll Panel control

// Basic controls set
procedure GuiLabel(bounds: TRectangle; const text: pchar); cdecl;external cDllName;// Label control, shows text
function GuiButton(bounds: TRectangle; const text: pchar): boolean; cdecl;external cDllName;// Button control, returns true when clicked
function GuiLabelButton(bounds: TRectangle; const text: pchar): boolean; cdecl;external cDllName;// Label button control, show true when clicked
function GuiToggle(bounds: TRectangle; const text: pchar; active: boolean):boolean; cdecl;external cDllName;// Toggle Button control, returns true when active
function GuiToggleGroup(bounds: TRectangle; const text: pchar; active: longint): longint; cdecl;external cDllName;// Toggle Group control, returns active toggle index
function GuiCheckBox(bounds: TRectangle; const text: pchar; checked: boolean): boolean; cdecl;external cDllName;// Check Box control, returns true when active
function GuiComboBox(bounds: TRectangle; const text: pchar; active: longint): longint; cdecl;external cDllName;// Combo Box control, returns selected item index
function GuiDropdownBox(bounds: TRectangle; const text: pchar; active: plongint; editMode: boolean): boolean; cdecl;external cDllName;// Dropdown Box control, returns selected item
function GuiSpinner(bounds: TRectangle; const text: pchar; value: plongint; minValue: longint; maxValue: longint; editMode: boolean): boolean; cdecl;external cDllName;// Spinner control, returns selected value
function GuiValueBox(bounds: TRectangle; const text: pchar; value: plongint; minValue: longint; maxValue: longint; editMode: boolean): boolean; cdecl;external cDllName;// Value Box control, updates input text with numbers
function GuiTextBox(bounds: TRectangle; text: pchar; textSize: longint; editMode: boolean): boolean; cdecl;external cDllName;// Text Box control, updates input text
function GuiTextBoxMulti(bounds: TRectangle; text: pchar; textSize: longint; editMode: boolean): boolean; cdecl;external cDllName;// Text Box control with multiple lines
function GuiSlider(bounds: TRectangle; const textLeft: pchar; const textRight: pchar; value: single; minValue: single; maxValue: single): single; cdecl;external cDllName;// Slider control, returns selected value
function GuiSliderBar(bounds: TRectangle; const textLeft: pchar; const textRight: pchar; value: single; minValue: single; maxValue: single): single; cdecl;external cDllName;// Slider Bar control, returns selected value
function GuiProgressBar(bounds: TRectangle; const textLeft: pchar; const textRight: pchar; value: single; minValue: single; maxValue: single): single; cdecl;external cDllName;// Progress Bar control, shows current progress value
procedure GuiStatusBar(bounds: TRectangle; const text: pchar);cdecl;external cDllName;// Status Bar control, shows info text
procedure GuiDummyRec(bounds: TRectangle; const text: pchar);cdecl;external cDllName;// Dummy control for placeholders
function GuiScrollBar(bounds: TRectangle; value: longint; minValue: longint; maxValue: longint): longint;cdecl;external cDllName;// Scroll Bar control
function GuiGrid(bounds: TRectangle; spacing: single; subdivs: longint): TVector2; cdecl;external cDllName;// Grid control

// Advance controls set
function GuiListView(bounds: TRectangle; const text: pchar; scrollIndex: plongint; active: longint): longint; cdecl;external cDllName;// List View control, returns selected list item index
function GuiListViewEx(bounds: TRectangle; const text: ppchar; count: longint; focus: plongint; scrollIndex: plongint; active: longint): longint; cdecl;external cDllName;// List View with extended parameters
function GuiMessageBox(bounds: TRectangle; const title: pchar; const message: pchar; const buttons: pchar): longint; cdecl;external cDllName;// Message Box control, displays a message
function GuiTextInputBox(bounds: TRectangle; const title: pchar; const message: pchar; const buttons: pchar; text: pchar): longint; cdecl;external cDllName;// Text Input Box control, ask for text
function GuiColorPicker(bounds: TRectangle; color: TColor): TColor ;cdecl;external cDllName;// Color Picker control (multiple color controls)
function GuiColorPanel(bounds: TRectangle; color: TColor): TColor ;cdecl;external cDllName;// Color Panel control
function GuiColorBarAlpha(bounds: TRectangle; alpha: single): single ;cdecl;external cDllName;// Color Bar Alpha control
function GuiColorBarHue(bounds:TRectangle; value: single): single ;cdecl;external cDllName;// Color Bar Hue control

// Styles loading functions
RAYGUIAPI void GuiLoadStyle(const char *fileName);              // Load style file over global style variable (.rgs)
RAYGUIAPI void GuiLoadStyleDefault(void);                       // Load style default over global style


implementation

end.

