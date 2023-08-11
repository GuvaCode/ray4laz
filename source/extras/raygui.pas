{********************************************************************************************
*                                                                                           *
*   raygui v4.0-dev - A simple and easy-to-use immediate-mode gui library                   *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also       *
*   available as a standalone library, as long as input and drawing functions are provided. *
*                                                                                           *
*   pascal header 2021 - 2023 by Gunko Vadim                                                *
*                                                                                           *
*********************************************************************************************}

unit raygui;

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I ../raylib.inc}

interface

uses
 raylib;

const
  RAYGUI_VERSION_MAJOR = 4;
  RAYGUI_VERSION_MINOR = 0;
  RAYGUI_VERSION_PATCH = 0;
  RAYGUI_VERSION = '4.0-dev';
  SCROLLBAR_LEFT_SIDE = 0;
  SCROLLBAR_RIGHT_SIDE = 1;

//----------------------------------------------------------------------------------
// Icons enumeration
//----------------------------------------------------------------------------------
type
  GuiIconName = Integer;
  PGuiIconName = ^GuiIconName;

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
  ICON_220 = 220;
  ICON_221 = 221;
  ICON_222 = 222;
  ICON_223 = 223;
  ICON_224 = 224;
  ICON_225 = 225;
  ICON_226 = 226;
  ICON_227 = 227;
  ICON_228 = 228;
  ICON_229 = 229;
  ICON_230 = 230;
  ICON_231 = 231;
  ICON_232 = 232;
  ICON_233 = 233;
  ICON_234 = 234;
  ICON_235 = 235;
  ICON_236 = 236;
  ICON_237 = 237;
  ICON_238 = 238;
  ICON_239 = 239;
  ICON_240 = 240;
  ICON_241 = 241;
  ICON_242 = 242;
  ICON_243 = 243;
  ICON_244 = 244;
  ICON_245 = 245;
  ICON_246 = 246;
  ICON_247 = 247;
  ICON_248 = 248;
  ICON_249 = 249;
  ICON_250 = 250;
  ICON_251 = 251;
  ICON_252 = 252;
  ICON_253 = 253;
  ICON_254 = 254;
  ICON_255 = 255;


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
  PGuiControl = ^TGuiControl;
  TGuiControl = Integer;

const
  DEFAULT = 0;
  LABELS = 1;
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

type
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty = Integer;

const
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

type
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty = Integer;

const
  TEXT_SIZE = 16;
  TEXT_SPACING = 17;
  LINE_COLOR = 18;
  BACKGROUND_COLOR = 19;
  TEXT_LINE_SPACING = 20;

type
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Integer;

const
  GROUP_PADDING = 16;

type
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Integer;

const
  SLIDER_WIDTH = 16;
  SLIDER_PADDING = 17;

type
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Integer;

const
  PROGRESS_PADDING = 16;

type
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Integer;

const
  ARROWS_SIZE = 16;
  ARROWS_VISIBLE = 17;
  SCROLL_SLIDER_PADDING = 18;
  SCROLL_SLIDER_SIZE = 19;
  SCROLL_PADDING = 20;
  SCROLL_SPEED = 21;


type
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Integer;

const
  CHECK_PADDING = 16;

type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty = Integer;

const
  COMBO_BUTTON_WIDTH = 16;
  COMBO_BUTTON_SPACING = 17;

type
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Integer;

const
  ARROW_PADDING = 16;
  DROPDOWN_ITEMS_SPACING = 17;

// TextBox/TextBoxMulti/ValueBox/Spinner
type
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Integer;

const
  TEXT_INNER_PADDING = 16;       // TextBox/TextBoxMulti/ValueBox/Spinner inner text padding
  TEXT_LINES_SPACING = 17;       // TextBoxMulti lines separation
  TEXT_ALIGNMENT_VERTICAL = 18;  // TextBoxMulti vertical alignment: 0-CENTERED, 1-UP, 2-DOWN
  TEXT_MULTILINE = 19;           // TextBox supports multiple lines
  TEXT_WRAP_MODE = 20;           // TextBox wrap mode for multiline: 0-NO_WRAP, 1-CHAR_WRAP, 2-WORD_WRAP
  TEXT_READONLY = 21;            // TextBox is readonly, no editable

// Spinner
type
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty = Integer;

const
  SPIN_BUTTON_WIDTH = 16;    // Spinner left/right buttons width
  SPIN_BUTTON_SPACING = 17;  // Spinner buttons separation

// ListView
type
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Integer;

const
  LIST_ITEMS_HEIGHT = 16;   // ListView items height
  LIST_ITEMS_SPACING = 17;
  SCROLLBAR_WIDTH = 18;
  SCROLLBAR_SIDE = 19;

// ColorPicker
type
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Integer;

const
  COLOR_SELECTOR_SIZE = 16;
  HUEBAR_WIDTH = 17;
  HUEBAR_PADDING = 18;
  HUEBAR_SELECTOR_HEIGHT = 19;
  HUEBAR_SELECTOR_OVERFLOW = 20;

// Style property
type
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
    controlId: Word;
    propertyId: Word;
    propertyValue: Cardinal;
  end;

// Global gui state control functions

procedure GuiEnable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnable';  // Enable gui controls (global state)
procedure GuiDisable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisable';  // Disable gui controls (global state)
procedure GuiLock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLock';  // Lock gui controls (global state)
procedure GuiUnlock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiUnlock';  // Unlock gui controls (global state)
function  GuiIsLocked: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIsLocked'; // Check if gui is locked (global state)
procedure GuiSetAlpha(alpha: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetAlpha';  // Set gui controls alpha (global state), alpha goes from 0.0f to 1.0
procedure GuiSetState(state: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetState';  // Set gui state (global state)
function GuiGetState: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetState';  // Get gui state (global state)

// Font set/get functions
procedure GuiSetFont(font: TFont); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetFont';  // Set gui custom font (global state)
function GuiGetFont:TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetFont';  // Get gui custom font (global state)

// Style set/get functions
procedure GuiSetStyle(control: Integer ; property_: Integer; value: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetStyle';  // Set one style property
function GuiGetStyle(control: Integer; property_: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetStyle'; // Get one style property

// Styles loading functions
procedure GuiLoadStyle(const fileName: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyle';  // Load style file over global style variable (.rgs)
procedure GuiLoadStyleDefault; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyleDefault';  // Load style default over global style

// Tooltips management functions
procedure GuiEnableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnableTooltip';  // Enable gui tooltips (global state)
procedure GuiDisableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisableTooltip';  // Disable gui tooltips (global state)
procedure GuiSetTooltip(const tooltip: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetTooltip'; // Set tooltip string

// Icons functionality
function GuiIconText(iconId: Integer; const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIconText';  // Get text with icon id prepended (if supported)
{$IFDEF RAYGUI_NO_RICONS}
procedure GuiSetIconScale(scale: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetIcons';  // Set default icon drawing size
function GuiGetIcons: Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetIcons';  // Get raygui icons data pointer
function GuiLoadIcons(const fileName: PChar; loadIconsName: Boolean): PPChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadIcons';  // Load raygui icons file (.rgi) into internal icons data
procedure GuiDrawIcon(iconId, posX, posY, pixelSize: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDrawIcon';  // Draw icon using pixel size at specified position
{$ENDIF}

// Controls
//------------------------------------------------------------------------------
// Container/separator controls, useful for controls organization
function GuiWindowBox(bounds: TRectangle; const title: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiWindowBox';  // Window Box control, shows a window that can be closed
function GuiGroupBox(bounds: TRectangle; const text:PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGroupBox';  // Group Box control with text name
function GuiLine(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLine';  // Line separator control, could contain text
function GuiPanel(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiPanel';  // Panel control, useful to group controls
function GuiTabBar(bounds: TRectangle; const text: PPChar; count: Integer; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTabBar';  // Tab Bar control, returns TAB to be closed or -1
function GuiScrollPanel(bounds: TRectangle; const text: PChar; content: TRectangle; scroll: PVector2; view: PRectangle): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiScrollPanel';  // Scroll Panel control

// Basic controls set
function GuiLabel(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabel';  // Label control, shows text
function GuiButton(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiButton';  // Button control, returns true when clicked
function GuiLabelButton(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabelButton';  // Label button control, show true when clicked
function GuiToggle(bounds: TRectangle; const text: PChar; active: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggle';  // Toggle Button control, returns true when active
function GuiToggleGroup(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleGroup';  // Toggle Group control, returns active toggle index
function GuiToggleSlider(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleSlider'; // Toggle Slider control, returns true when clicked
function GuiCheckBox(bounds: TRectangle; const text: PChar; checked: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiCheckBox';  // Check Box control, returns true when active
function GuiComboBox(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiComboBox'; // Combo Box control, returns selected item index
function GuiDropdownBox(bounds: TRectangle; const text: PChar; active: PInteger; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDropdownBox';  // Dropdown Box control, returns selected item
function GuiSpinner(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSpinner';  // Spinner control, returns selected value
function GuiValueBox(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBox';  // Value Box control, updates input text with numbers
function GuiTextBox(bounds: TRectangle; text: PChar; textSize: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextBox';  // Text Box control, updates input text
function GuiSlider(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSlider';  // Slider control, returns selected value
function GuiSliderBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSliderBar';  // Slider Bar control, returns selected value
function GuiProgressBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiProgressBar';  // Progress Bar control, shows current progress value
function GuiStatusBar(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiStatusBar';  // Status Bar control, shows info text
function GuiDummyRec(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDummyRec';  // Dummy control for placeholders
function GuiGrid(bounds: TRectangle; const text: PChar; spacing: Single; subdivs: Integer; mouseCell: PVector2): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGrid';  // Grid control, returns mouse cell position

// Advance controls set
function GuiListView(bounds: TRectangle; const text: PChar; scrollIndex: PInteger; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListView';  // List View control, returns selected list item index
function GuiListViewEx(bounds: TRectangle; const text: PPChar; count: Integer; scrollIndex: PInteger; active, focus: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListViewEx';  // List View with extended parameters
function GuiMessageBox(bounds: TRectangle; const title, message, buttons: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiMessageBox';  // Message Box control, displays a message
function GuiTextInputBox(bounds: TRectangle; const title, message, buttons, text: PChar; textMaxSize: Integer; secretViewActive: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextInputBox';  // Text Input Box control, ask for text, supports secret
function GuiColorPicker(bounds: TRectangle; const text: PChar; color: PColorB): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPicker';   // Color Picker control (multiple color controls)
function GuiColorPanel(bounds: TRectangle; const text: PChar; color: PColorB): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanel';  // Color Panel control
function GuiColorBarAlpha(bounds: TRectangle; const text: PChar; alpha: PSingle): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarAlpha';  // Color Bar Alpha control
function GuiColorBarHue(bounds: TRectangle; const text: PChar; value: PSingle): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarHue';   // Color Bar Hue control
function GuiColorPickerHSV(bounds: TRectangle; const text: PChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPickerHSV';  // Color Picker control that avoids conversion to RGB on each call (multiple color controls)
function GuiColorPanelHSV(bounds: TRectangle; const text: PChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanelHSV';  // Color Panel control that returns HSV color value, used by GuiColorPickerHSV()


implementation

end.

