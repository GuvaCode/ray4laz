{********************************************************************************************
*                                                                                           *
*    5.0 (2026)                                                                             *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   raygui is a tools-dev-
ed immediate-mode-gui library based on raylib but also       *
*   available as a standalone library, as long as input and drawing functions are provided. *
*                                                                                           *
*   Pascal header 2021 - 2026 by Gunko Vadim                                                *
*                                                                                           *
*********************************************************************************************}

unit raygui;

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I ../raylib.inc}
{.$DEFINE RAYGUI_NO_ICONS}
//   Avoid including embedded icons data (256 icons, 16x16 pixels, 1-bit per pixel, 2KB)

interface

uses
  raylib;

const
  RAYGUI_VERSION_MAJOR = 5;
  RAYGUI_VERSION_MINOR = 0;
  RAYGUI_VERSION_PATCH = 0;
  RAYGUI_VERSION = '5.0';

//----------------------------------------------------------------------------------
// Defines and Macros (translated for Pascal)
//----------------------------------------------------------------------------------
const
  SCROLLBAR_LEFT_SIDE = 0;
  SCROLLBAR_RIGHT_SIDE = 1;
  RAYGUI_WINDOWBOX_STATUSBAR_HEIGHT = 24;
  RAYGUI_WINDOWBOX_CLOSEBUTTON_HEIGHT = 18;
  RAYGUI_GROUPBOX_LINE_THICK = 1;
  RAYGUI_LINE_MARGIN_TEXT = 12;
  RAYGUI_LINE_TEXT_PADDING = 4;
  RAYGUI_PANEL_BORDER_WIDTH = 1;
  RAYGUI_TABBAR_ITEM_WIDTH = 148;
  RAYGUI_MIN_SCROLLBAR_WIDTH = 40;
  RAYGUI_MIN_SCROLLBAR_HEIGHT = 40;
  RAYGUI_MIN_MOUSE_WHEEL_SPEED = 20;
  RAYGUI_TEXTBOX_AUTO_CURSOR_COOLDOWN = 20;
  RAYGUI_TEXTBOX_AUTO_CURSOR_DELAY = 1;
  RAYGUI_VALUEBOX_MAX_CHARS = 32;
  RAYGUI_COLORBARALPHA_CHECKED_SIZE = 10;
  RAYGUI_MESSAGEBOX_BUTTON_HEIGHT = 24;
  RAYGUI_MESSAGEBOX_BUTTON_PADDING = 12;
  RAYGUI_TEXTINPUTBOX_BUTTON_HEIGHT = 24;
  RAYGUI_TEXTINPUTBOX_BUTTON_PADDING = 12;
  RAYGUI_TEXTINPUTBOX_HEIGHT = 26;
  RAYGUI_GRID_ALPHA = 0.15;
  ICON_TEXT_PADDING = 4;
  RAYGUI_MAX_TEXT_LINES = 128;
  RAYGUI_TEXTSPLIT_MAX_ITEMS = 128;
  RAYGUI_TEXTSPLIT_MAX_TEXT_SIZE = 1024;
  RAYGUI_TEXTFORMAT_MAX_SIZE = 256;

//----------------------------------------------------------------------------------
// Icons enumeration
//----------------------------------------------------------------------------------
type
  PGuiIconName = ^TGuiIconName;
  TGuiIconName = Integer;

const
  ICON_NONE = 0;
  ICON_FOLDER_FILE_OPEN = 1;
  ICON_FILE_SAVE_CLASSIC = 2;
  ICON_FOLDER_OPEN = 3;
  ICON_FOLDER_SAVE = 4;
  ICON_FILE_OPEN = 5;
  ICON_FILE_SAVE = 6;
  ICON_FILE_EXPORT = 7;
  ICON_FILE_ADD = 8;
  ICON_FILE_DELETE = 9;
  ICON_FILETYPE_TEXT = 10;
  ICON_FILETYPE_AUDIO = 11;
  ICON_FILETYPE_IMAGE = 12;
  ICON_FILETYPE_PLAY = 13;
  ICON_FILETYPE_VIDEO = 14;
  ICON_FILETYPE_INFO = 15;
  ICON_FILE_COPY = 16;
  ICON_FILE_CUT = 17;
  ICON_FILE_PASTE = 18;
  ICON_CURSOR_HAND = 19;
  ICON_CURSOR_POINTER = 20;
  ICON_CURSOR_CLASSIC = 21;
  ICON_PENCIL = 22;
  ICON_PENCIL_BIG = 23;
  ICON_BRUSH_CLASSIC = 24;
  ICON_BRUSH_PAINTER = 25;
  ICON_WATER_DROP = 26;
  ICON_COLOR_PICKER = 27;
  ICON_RUBBER = 28;
  ICON_COLOR_BUCKET = 29;
  ICON_TEXT_T = 30;
  ICON_TEXT_A = 31;
  ICON_SCALE = 32;
  ICON_RESIZE = 33;
  ICON_FILTER_POINT = 34;
  ICON_FILTER_BILINEAR = 35;
  ICON_CROP = 36;
  ICON_CROP_ALPHA = 37;
  ICON_SQUARE_TOGGLE = 38;
  ICON_SYMMETRY = 39;
  ICON_SYMMETRY_HORIZONTAL = 40;
  ICON_SYMMETRY_VERTICAL = 41;
  ICON_LENS = 42;
  ICON_LENS_BIG = 43;
  ICON_EYE_ON = 44;
  ICON_EYE_OFF = 45;
  ICON_FILTER_TOP = 46;
  ICON_FILTER = 47;
  ICON_TARGET_POINT = 48;
  ICON_TARGET_SMALL = 49;
  ICON_TARGET_BIG = 50;
  ICON_TARGET_MOVE = 51;
  ICON_CURSOR_MOVE = 52;
  ICON_CURSOR_SCALE = 53;
  ICON_CURSOR_SCALE_RIGHT = 54;
  ICON_CURSOR_SCALE_LEFT = 55;
  ICON_UNDO = 56;
  ICON_REDO = 57;
  ICON_REREDO = 58;
  ICON_MUTATE = 59;
  ICON_ROTATE = 60;
  ICON_REPEAT = 61;
  ICON_SHUFFLE = 62;
  ICON_EMPTYBOX = 63;
  ICON_TARGET = 64;
  ICON_TARGET_SMALL_FILL = 65;
  ICON_TARGET_BIG_FILL = 66;
  ICON_TARGET_MOVE_FILL = 67;
  ICON_CURSOR_MOVE_FILL = 68;
  ICON_CURSOR_SCALE_FILL = 69;
  ICON_CURSOR_SCALE_RIGHT_FILL = 70;
  ICON_CURSOR_SCALE_LEFT_FILL = 71;
  ICON_UNDO_FILL = 72;
  ICON_REDO_FILL = 73;
  ICON_REREDO_FILL = 74;
  ICON_MUTATE_FILL = 75;
  ICON_ROTATE_FILL = 76;
  ICON_REPEAT_FILL = 77;
  ICON_SHUFFLE_FILL = 78;
  ICON_EMPTYBOX_SMALL = 79;
  ICON_BOX = 80;
  ICON_BOX_TOP = 81;
  ICON_BOX_TOP_RIGHT = 82;
  ICON_BOX_RIGHT = 83;
  ICON_BOX_BOTTOM_RIGHT = 84;
  ICON_BOX_BOTTOM = 85;
  ICON_BOX_BOTTOM_LEFT = 86;
  ICON_BOX_LEFT = 87;
  ICON_BOX_TOP_LEFT = 88;
  ICON_BOX_CENTER = 89;
  ICON_BOX_CIRCLE_MASK = 90;
  ICON_POT = 91;
  ICON_ALPHA_MULTIPLY = 92;
  ICON_ALPHA_CLEAR = 93;
  ICON_DITHERING = 94;
  ICON_MIPMAPS = 95;
  ICON_BOX_GRID = 96;
  ICON_GRID = 97;
  ICON_BOX_CORNERS_SMALL = 98;
  ICON_BOX_CORNERS_BIG = 99;
  ICON_FOUR_BOXES = 100;
  ICON_GRID_FILL = 101;
  ICON_BOX_MULTISIZE = 102;
  ICON_ZOOM_SMALL = 103;
  ICON_ZOOM_MEDIUM = 104;
  ICON_ZOOM_BIG = 105;
  ICON_ZOOM_ALL = 106;
  ICON_ZOOM_CENTER = 107;
  ICON_BOX_DOTS_SMALL = 108;
  ICON_BOX_DOTS_BIG = 109;
  ICON_BOX_CONCENTRIC = 110;
  ICON_BOX_GRID_BIG = 111;
  ICON_OK_TICK = 112;
  ICON_CROSS = 113;
  ICON_ARROW_LEFT = 114;
  ICON_ARROW_RIGHT = 115;
  ICON_ARROW_DOWN = 116;
  ICON_ARROW_UP = 117;
  ICON_ARROW_LEFT_FILL = 118;
  ICON_ARROW_RIGHT_FILL = 119;
  ICON_ARROW_DOWN_FILL = 120;
  ICON_ARROW_UP_FILL = 121;
  ICON_AUDIO = 122;
  ICON_FX = 123;
  ICON_WAVE = 124;
  ICON_WAVE_SINUS = 125;
  ICON_WAVE_SQUARE = 126;
  ICON_WAVE_TRIANGULAR = 127;
  ICON_CROSS_SMALL = 128;
  ICON_PLAYER_PREVIOUS = 129;
  ICON_PLAYER_PLAY_BACK = 130;
  ICON_PLAYER_PLAY = 131;
  ICON_PLAYER_PAUSE = 132;
  ICON_PLAYER_STOP = 133;
  ICON_PLAYER_NEXT = 134;
  ICON_PLAYER_RECORD = 135;
  ICON_MAGNET = 136;
  ICON_LOCK_CLOSE = 137;
  ICON_LOCK_OPEN = 138;
  ICON_CLOCK = 139;
  ICON_TOOLS = 140;
  ICON_GEAR = 141;
  ICON_GEAR_BIG = 142;
  ICON_BIN = 143;
  ICON_HAND_POINTER = 144;
  ICON_LASER = 145;
  ICON_COIN = 146;
  ICON_EXPLOSION = 147;
  ICON_1UP = 148;
  ICON_PLAYER = 149;
  ICON_PLAYER_JUMP = 150;
  ICON_KEY = 151;
  ICON_DEMON = 152;
  ICON_TEXT_POPUP = 153;
  ICON_GEAR_EX = 154;
  ICON_CRACK = 155;
  ICON_CRACK_POINTS = 156;
  ICON_STAR = 157;
  ICON_DOOR = 158;
  ICON_EXIT = 159;
  ICON_MODE_2D = 160;
  ICON_MODE_3D = 161;
  ICON_CUBE = 162;
  ICON_CUBE_FACE_TOP = 163;
  ICON_CUBE_FACE_LEFT = 164;
  ICON_CUBE_FACE_FRONT = 165;
  ICON_CUBE_FACE_BOTTOM = 166;
  ICON_CUBE_FACE_RIGHT = 167;
  ICON_CUBE_FACE_BACK = 168;
  ICON_CAMERA = 169;
  ICON_SPECIAL = 170;
  ICON_LINK_NET = 171;
  ICON_LINK_BOXES = 172;
  ICON_LINK_MULTI = 173;
  ICON_LINK = 174;
  ICON_LINK_BROKE = 175;
  ICON_TEXT_NOTES = 176;
  ICON_NOTEBOOK = 177;
  ICON_SUITCASE = 178;
  ICON_SUITCASE_ZIP = 179;
  ICON_MAILBOX = 180;
  ICON_MONITOR = 181;
  ICON_PRINTER = 182;
  ICON_PHOTO_CAMERA = 183;
  ICON_PHOTO_CAMERA_FLASH = 184;
  ICON_HOUSE = 185;
  ICON_HEART = 186;
  ICON_CORNER = 187;
  ICON_VERTICAL_BARS = 188;
  ICON_VERTICAL_BARS_FILL = 189;
  ICON_LIFE_BARS = 190;
  ICON_INFO = 191;
  ICON_CROSSLINE = 192;
  ICON_HELP = 193;
  ICON_FILETYPE_ALPHA = 194;
  ICON_FILETYPE_HOME = 195;
  ICON_LAYERS_VISIBLE = 196;
  ICON_LAYERS = 197;
  ICON_WINDOW = 198;
  ICON_HIDPI = 199;
  ICON_FILETYPE_BINARY = 200;
  ICON_HEX = 201;
  ICON_SHIELD = 202;
  ICON_FILE_NEW = 203;
  ICON_FOLDER_ADD = 204;
  ICON_ALARM = 205;
  ICON_CPU = 206;
  ICON_ROM = 207;
  ICON_STEP_OVER = 208;
  ICON_STEP_INTO = 209;
  ICON_STEP_OUT = 210;
  ICON_RESTART = 211;
  ICON_BREAKPOINT_ON = 212;
  ICON_BREAKPOINT_OFF = 213;
  ICON_BURGER_MENU = 214;
  ICON_CASE_SENSITIVE = 215;
  ICON_REG_EXP = 216;
  ICON_FOLDER = 217;
  ICON_FILE = 218;
  ICON_SAND_TIMER = 219;
  ICON_WARNING = 220;
  ICON_HELP_BOX = 221;
  ICON_INFO_BOX = 222;
  ICON_PRIORITY = 223;
  ICON_LAYERS_ISO = 224;
  ICON_LAYERS2 = 225;
  ICON_MLAYERS = 226;
  ICON_MAPS = 227;
  ICON_HOT = 228;
  ICON_LABEL = 229;
  ICON_NAME_ID = 230;
  ICON_SLICING = 231;
  ICON_MANUAL_CONTROL = 232;
  ICON_COLLISION = 233;
  ICON_CIRCLE_ADD = 234;
  ICON_CIRCLE_ADD_FILL = 235;
  ICON_CIRCLE_WARNING = 236;
  ICON_CIRCLE_WARNING_FILL = 237;
  ICON_BOX_MORE = 238;
  ICON_BOX_MORE_FILL = 239;
  ICON_BOX_MINUS = 240;
  ICON_BOX_MINUS_FILL = 241;
  ICON_UNION = 242;
  ICON_INTERSECTION = 243;
  ICON_DIFFERENCE = 244;
  ICON_SPHERE = 245;
  ICON_CYLINDER = 246;
  ICON_CONE = 247;
  ICON_ELLIPSOID = 248;
  ICON_CAPSULE = 249;
  ICON_250 = 250;
  ICON_251 = 251;
  ICON_252 = 252;
  ICON_253 = 253;
  ICON_254 = 254;
  ICON_255 = 255;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------

type
  PGuiState = ^TGuiState;
  TGuiState = Integer;

const
  STATE_NORMAL = 0;
  STATE_FOCUSED = 1;
  STATE_PRESSED = 2;
  STATE_DISABLED = 3;

type
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment = Integer;

const
  TEXT_ALIGN_LEFT = 0;
  TEXT_ALIGN_CENTER = 1;
  TEXT_ALIGN_RIGHT = 2;

type
  PGuiTextAlignmentVertical = ^TGuiTextAlignmentVertical;
  TGuiTextAlignmentVertical = Integer;

const
  TEXT_ALIGN_TOP = 0;
  TEXT_ALIGN_MIDDLE = 1;
  TEXT_ALIGN_BOTTOM = 2;

type
  PGuiTextWrapMode = ^TGuiTextWrapMode;
  TGuiTextWrapMode = Integer;

const
  TEXT_WRAP_NONE = 0;
  TEXT_WRAP_CHAR = 1;
  TEXT_WRAP_WORD = 2;

// Gui controls (updated to match v5.0-dev)
type
  PGuiControl = ^TGuiControl;
  TGuiControl = Integer;

const
  // Default -> populates to all controls when set
  DEFAULT = 0;
  // Basic controls
  UILABEL = 1;          // Used also for: LABELBUTTON
  BUTTON = 2;
  TOGGLE = 3;         // Used also for: TOGGLEGROUP
  SLIDER = 4;         // Used also for: SLIDERBAR, TOGGLESLIDER
  PROGRESSBAR = 5;
  CHECKBOX = 6;
  COMBOBOX = 7;
  DROPDOWNBOX = 8;
  TEXTBOX = 9;        // Used also for: TEXTBOXMULTI
  VALUEBOX = 10;
  CONTROL11 = 11;
  LISTVIEW = 12;
  COLORPICKER = 13;
  SCROLLBAR = 14;
  STATUSBAR = 15;

// Gui base properties for every control
type
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty = Integer;

const
  BORDER_COLOR_NORMAL = 0;    // Control border color in STATE_NORMAL
  BASE_COLOR_NORMAL = 1;      // Control base color in STATE_NORMAL
  TEXT_COLOR_NORMAL = 2;      // Control text color in STATE_NORMAL
  BORDER_COLOR_FOCUSED = 3;   // Control border color in STATE_FOCUSED
  BASE_COLOR_FOCUSED = 4;     // Control base color in STATE_FOCUSED
  TEXT_COLOR_FOCUSED = 5;     // Control text color in STATE_FOCUSED
  BORDER_COLOR_PRESSED = 6;   // Control border color in STATE_PRESSED
  BASE_COLOR_PRESSED = 7;     // Control base color in STATE_PRESSED
  TEXT_COLOR_PRESSED = 8;     // Control text color in STATE_PRESSED
  BORDER_COLOR_DISABLED = 9;  // Control border color in STATE_DISABLED
  BASE_COLOR_DISABLED = 10;   // Control base color in STATE_DISABLED
  TEXT_COLOR_DISABLED = 11;   // Control text color in STATE_DISABLED
  BORDER_WIDTH = 12;          // Control border size, 0 for no border
  TEXT_PADDING = 13;          // Control text padding, not considering border
  TEXT_ALIGNMENT = 14;        // Control text horizontal alignment inside control text bound (after border and padding)

// Gui extended properties (depend on control)
// DEFAULT extended properties (common to all controls or global)
type
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty = Integer;

const
  TEXT_SIZE = 16;                 // Text size (glyphs max height)
  TEXT_SPACING = 17;              // Text spacing between glyphs
  LINE_COLOR = 18;                // Line control color
  BACKGROUND_COLOR = 19;          // Background color
  TEXT_LINE_SPACING = 20;         // Text spacing between lines
  TEXT_ALIGNMENT_VERTICAL = 21;   // Text vertical alignment inside text bounds (after border and padding)
  TEXT_WRAP_MODE = 22;            // Text wrap-mode inside text bounds

// Toggle/ToggleGroup
type
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Integer;

const
  GROUP_PADDING = 16;              // ToggleGroup separation between toggles

// Slider/SliderBar
type
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Integer;

const
  SLIDER_WIDTH = 16;               // Slider size of internal bar
  SLIDER_PADDING = 17;             // Slider/SliderBar internal bar padding

// ProgressBar
type
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Integer;

const
  PROGRESS_PADDING = 16;           // ProgressBar internal padding
  PROGRESS_SIDE = 17;              // ProgressBar increment side: 0-left->right, 1-right-left

// ScrollBar
type
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Integer;

const
  ARROWS_SIZE = 16;                // ScrollBar arrows size
  ARROWS_VISIBLE = 17;             // ScrollBar arrows visible
  SCROLL_SLIDER_PADDING = 18;      // ScrollBar slider internal padding
  SCROLL_SLIDER_SIZE = 19;         // ScrollBar slider size
  SCROLL_PADDING = 20;             // ScrollBar scroll padding from arrows
  SCROLL_SPEED = 21;               // ScrollBar scrolling speed

// CheckBox
type
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Integer;

const
  CHECK_PADDING = 16;              // CheckBox internal check padding

// ComboBox
type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty = Integer;

const
  COMBO_BUTTON_WIDTH = 16;         // ComboBox right button width
  COMBO_BUTTON_SPACING = 17;       // ComboBox button separation

// DropdownBox
type
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Integer;

const
  ARROW_PADDING = 16;              // DropdownBox arrow separation from border and items
  DROPDOWN_ITEMS_SPACING = 17;     // DropdownBox items separation
  DROPDOWN_ARROW_HIDDEN = 18;      // DropdownBox arrow hidden
  DROPDOWN_ROLL_UP = 19;           // DropdownBox roll up flag (default rolls down)

// TextBox/TextBoxMulti/ValueBox/Spinner
type
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Integer;

const
  TEXT_READONLY = 16;              // TextBox in read-only mode: 0-text editable, 1-text no-editable

// ValueBox/Spinner
type
  PGuiValueBoxProperty = ^TGuiValueBoxProperty;
  TGuiValueBoxProperty = Integer;

const
  SPINNER_BUTTON_WIDTH = 16;       // Spinner left/right buttons width
  SPINNER_BUTTON_SPACING = 17;     // Spinner buttons separation

// ListView
type
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Integer;

const
  LIST_ITEMS_HEIGHT = 16;          // ListView items height
  LIST_ITEMS_SPACING = 17;         // ListView items separation
  SCROLLBAR_WIDTH = 18;            // ListView scrollbar size (usually width)
  SCROLLBAR_SIDE = 19;             // ListView scrollbar side (0-SCROLLBAR_LEFT_SIDE, 1-SCROLLBAR_RIGHT_SIDE)
  LIST_ITEMS_BORDER_NORMAL = 20;   // ListView items border enabled in normal state
  LIST_ITEMS_BORDER_WIDTH = 21;    // ListView items border width

// ColorPicker
type
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Integer;

const
  COLOR_SELECTOR_SIZE = 16;
  HUEBAR_WIDTH = 17;                // ColorPicker right hue bar width
  HUEBAR_PADDING = 18;              // ColorPicker right hue bar separation from panel
  HUEBAR_SELECTOR_HEIGHT = 19;      // ColorPicker right hue bar selector height
  HUEBAR_SELECTOR_OVERFLOW = 20;    // ColorPicker right hue bar selector overflow

// Style property
type
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
    controlId: Word;      // Control identifier
    propertyId: Word;     // Property identifier
    propertyValue: Integer; // Property value
  end;

//----------------------------------------------------------------------------------
// Global Functions Declaration
//----------------------------------------------------------------------------------

// Global gui state control functions
procedure GuiEnable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnable';
procedure GuiDisable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisable';
procedure GuiLock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLock';
procedure GuiUnlock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiUnlock';
function GuiIsLocked: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIsLocked';
procedure GuiSetAlpha(alpha: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetAlpha';
procedure GuiSetState(state: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetState';
function GuiGetState: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetState';

// Font set/get functions
procedure GuiSetFont(font: TFont); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetFont';
function GuiGetFont: TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetFont';

// Style set/get functions
procedure GuiSetStyle(control: Integer; property_: Integer; value: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetStyle';
function GuiGetStyle(control: Integer; property_: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetStyle';

// Styles loading functions
procedure GuiLoadStyle(const fileName: PAnsiChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyle';
procedure GuiLoadStyleDefault; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyleDefault';

// Tooltips management functions
procedure GuiEnableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnableTooltip';
procedure GuiDisableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisableTooltip';
procedure GuiSetTooltip(const tooltip: PAnsiChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetTooltip';

// Icons functionality
function GuiIconText(iconId: Integer; const text: PAnsiChar): PAnsiChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIconText';

{$IFNDEF RAYGUI_NO_ICONS}
procedure GuiSetIconScale(scale: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetIconScale';
function GuiGetIcons: Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetIcons';
function GuiLoadIcons(const fileName: PAnsiChar; loadIconsName: Boolean): PPAnsiChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadIcons';
function GuiLoadIconsFromMemory(const fileData: PByte; dataSize: Integer; loadIconsName: Boolean): PPAnsiChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadIconsFromMemory';
procedure GuiDrawIcon(iconId, posX, posY, pixelSize: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDrawIcon';
{$ENDIF}

// Utility functions
function GuiGetTextWidth(const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetTextWidth';

// Controls
//----------------------------------------------------------------------------------------------------------
// Container/separator controls, useful for controls organization
function GuiWindowBox(bounds: TRectangle; const title: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiWindowBox';
function GuiGroupBox(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGroupBox';
function GuiLine(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLine';
function GuiPanel(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiPanel';
function GuiTabBar(bounds: TRectangle; text: PPAnsiChar; count: Integer; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTabBar';
function GuiScrollPanel(bounds: TRectangle; const text: PAnsiChar; content: TRectangle; scroll: PVector2; view: PRectangle): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiScrollPanel';

// Basic controls set
function GuiLabel(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabel';
function GuiButton(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiButton';
function GuiLabelButton(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabelButton';
function GuiToggle(bounds: TRectangle; const text: PAnsiChar; active: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggle';
function GuiToggleGroup(bounds: TRectangle; const text: PAnsiChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleGroup';
function GuiToggleSlider(bounds: TRectangle; const text: PAnsiChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleSlider';
function GuiCheckBox(bounds: TRectangle; const text: PAnsiChar; checked: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiCheckBox';
function GuiComboBox(bounds: TRectangle; const text: PAnsiChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiComboBox';
function GuiDropdownBox(bounds: TRectangle; const text: PAnsiChar; active: PInteger; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDropdownBox';
function GuiSpinner(bounds: TRectangle; const text: PAnsiChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSpinner';
function GuiValueBox(bounds: TRectangle; const text: PAnsiChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBox';
function GuiValueBoxFloat(bounds: TRectangle; const text: PAnsiChar; textValue: PAnsiChar; value: PSingle; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBoxFloat';
function GuiTextBox(bounds: TRectangle; text: PAnsiChar; textSize: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextBox';
function GuiSlider(bounds: TRectangle; const textLeft: PAnsiChar; const textRight: PAnsiChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSlider';
function GuiSliderBar(bounds: TRectangle; const textLeft: PAnsiChar; const textRight: PAnsiChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSliderBar';
function GuiProgressBar(bounds: TRectangle; const textLeft: PAnsiChar; const textRight: PAnsiChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiProgressBar';
function GuiStatusBar(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiStatusBar';
function GuiDummyRec(bounds: TRectangle; const text: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDummyRec';
function GuiGrid(bounds: TRectangle; const text: PAnsiChar; spacing: Single; subdivs: Integer; mouseCell: PVector2): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGrid';

// Advance controls set
function GuiListView(bounds: TRectangle; const text: PChar; scrollIndex: PInteger; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListView';
function GuiListViewEx(bounds: TRectangle; text: PPChar; count: Integer; scrollIndex: PInteger; active: PInteger; focus: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListViewEx';
function GuiMessageBox(bounds: TRectangle; const title, message, buttons: PAnsiChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiMessageBox';
function GuiTextInputBox(bounds: TRectangle; const title, message, buttons, text: PAnsiChar; textMaxSize: Integer; secretViewActive: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextInputBox';
function GuiColorPicker(bounds: TRectangle; const text: PAnsiChar; color: PColorB): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPicker';
function GuiColorPanel(bounds: TRectangle; const text: PAnsiChar; color: PColorB): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanel';
function GuiColorBarAlpha(bounds: TRectangle; const text: PAnsiChar; alpha: PSingle): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarAlpha';
function GuiColorBarHue(bounds: TRectangle; const text: PAnsiChar; value: PSingle): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarHue';
function GuiColorPickerHSV(bounds: TRectangle; const text: PAnsiChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPickerHSV';
function GuiColorPanelHSV(bounds: TRectangle; const text: PAnsiChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanelHSV';

implementation

end.
