{********************************************************************************************
*                                                                                           *
*    4.5-dev (2025)    Current dev version...                                               *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also       *
*   available as a standalone library, as long as input and drawing functions are provided. *
*                                                                                           *
*   pascal header 2021 - 2025 by Gunko Vadim                                                *
*                                                                                           *
*********************************************************************************************}

unit raygui;

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I ../raylib.inc}
{.$DEFINE RAYGUI_NO_RICONS}
//   Avoid including embedded ricons data (256 icons, 16x16 pixels, 1-bit per pixel, 2KB)

interface

uses
 raylib;

const
  RAYGUI_VERSION_MAJOR = 4;
  RAYGUI_VERSION_MINOR = 5;
  RAYGUI_VERSION_PATCH = 0;
  RAYGUI_VERSION = '4.5-dev';
  SCROLLBAR_LEFT_SIDE = 0;
  SCROLLBAR_RIGHT_SIDE = 1;
  RAYGUI_WINDOWBOX_STATUSBAR_HEIGHT =  24;
  RAYGUI_WINDOWBOX_CLOSEBUTTON_HEIGHT = 18;
//----------------------------------------------------------------------------------
// Icons enumeration
//----------------------------------------------------------------------------------
type
  TGuiIconName = Integer;
  PGuiIconName = ^TGuiIconName;
const
  ICON_NONE = TGuiIconName(0);
  ICON_FOLDER_FILE_OPEN = TGuiIconName(1);
  ICON_FILE_SAVE_CLASSIC = TGuiIconName(2);
  ICON_FOLDER_OPEN = TGuiIconName(3);
  ICON_FOLDER_SAVE = TGuiIconName(4);
  ICON_FILE_OPEN = TGuiIconName(5);
  ICON_FILE_SAVE = TGuiIconName(6);
  ICON_FILE_EXPORT = TGuiIconName(7);
  ICON_FILE_ADD = TGuiIconName(8);
  ICON_FILE_DELETE = TGuiIconName(9);
  ICON_FILETYPE_TEXT = TGuiIconName(10);
  ICON_FILETYPE_AUDIO = TGuiIconName(11);
  ICON_FILETYPE_IMAGE = TGuiIconName(12);
  ICON_FILETYPE_PLAY = TGuiIconName(13);
  ICON_FILETYPE_VIDEO = TGuiIconName(14);
  ICON_FILETYPE_INFO = TGuiIconName(15);
  ICON_FILE_COPY = TGuiIconName(16);
  ICON_FILE_CUT = TGuiIconName(17);
  ICON_FILE_PASTE = TGuiIconName(18);
  ICON_CURSOR_HAND = TGuiIconName(19);
  ICON_CURSOR_POINTER = TGuiIconName(20);
  ICON_CURSOR_CLASSIC = TGuiIconName(21);
  ICON_PENCIL = TGuiIconName(22);
  ICON_PENCIL_BIG = TGuiIconName(23);
  ICON_BRUSH_CLASSIC = TGuiIconName(24);
  ICON_BRUSH_PAINTER = TGuiIconName(25);
  ICON_WATER_DROP = TGuiIconName(26);
  ICON_COLOR_PICKER = TGuiIconName(27);
  ICON_RUBBER = TGuiIconName(28);
  ICON_COLOR_BUCKET = TGuiIconName(29);
  ICON_TEXT_T = TGuiIconName(30);
  ICON_TEXT_A = TGuiIconName(31);
  ICON_SCALE = TGuiIconName(32);
  ICON_RESIZE = TGuiIconName(33);
  ICON_FILTER_POINT = TGuiIconName(34);
  ICON_FILTER_BILINEAR = TGuiIconName(35);
  ICON_CROP = TGuiIconName(36);
  ICON_CROP_ALPHA = TGuiIconName(37);
  ICON_SQUARE_TOGGLE = TGuiIconName(38);
  ICON_SYMMETRY = TGuiIconName(39);
  ICON_SYMMETRY_HORIZONTAL = TGuiIconName(40);
  ICON_SYMMETRY_VERTICAL = TGuiIconName(41);
  ICON_LENS = TGuiIconName(42);
  ICON_LENS_BIG = TGuiIconName(43);
  ICON_EYE_ON = TGuiIconName(44);
  ICON_EYE_OFF = TGuiIconName(45);
  ICON_FILTER_TOP = TGuiIconName(46);
  ICON_FILTER = TGuiIconName(47);
  ICON_TARGET_POINT = TGuiIconName(48);
  ICON_TARGET_SMALL = TGuiIconName(49);
  ICON_TARGET_BIG = TGuiIconName(50);
  ICON_TARGET_MOVE = TGuiIconName(51);
  ICON_CURSOR_MOVE = TGuiIconName(52);
  ICON_CURSOR_SCALE = TGuiIconName(53);
  ICON_CURSOR_SCALE_RIGHT = TGuiIconName(54);
  ICON_CURSOR_SCALE_LEFT = TGuiIconName(55);
  ICON_UNDO = TGuiIconName(56);
  ICON_REDO = TGuiIconName(57);
  ICON_REREDO = TGuiIconName(58);
  ICON_MUTATE = TGuiIconName(59);
  ICON_ROTATE = TGuiIconName(60);
  ICON_REPEAT = TGuiIconName(61);
  ICON_SHUFFLE = TGuiIconName(62);
  ICON_EMPTYBOX = TGuiIconName(63);
  ICON_TARGET = TGuiIconName(64);
  ICON_TARGET_SMALL_FILL = TGuiIconName(65);
  ICON_TARGET_BIG_FILL = TGuiIconName(66);
  ICON_TARGET_MOVE_FILL = TGuiIconName(67);
  ICON_CURSOR_MOVE_FILL = TGuiIconName(68);
  ICON_CURSOR_SCALE_FILL = TGuiIconName(69);
  ICON_CURSOR_SCALE_RIGHT_FILL = TGuiIconName(70);
  ICON_CURSOR_SCALE_LEFT_FILL = TGuiIconName(71);
  ICON_UNDO_FILL = TGuiIconName(72);
  ICON_REDO_FILL = TGuiIconName(73);
  ICON_REREDO_FILL = TGuiIconName(74);
  ICON_MUTATE_FILL = TGuiIconName(75);
  ICON_ROTATE_FILL = TGuiIconName(76);
  ICON_REPEAT_FILL = TGuiIconName(77);
  ICON_SHUFFLE_FILL = TGuiIconName(78);
  ICON_EMPTYBOX_SMALL = TGuiIconName(79);
  ICON_BOX = TGuiIconName(80);
  ICON_BOX_TOP = TGuiIconName(81);
  ICON_BOX_TOP_RIGHT = TGuiIconName(82);
  ICON_BOX_RIGHT = TGuiIconName(83);
  ICON_BOX_BOTTOM_RIGHT = TGuiIconName(84);
  ICON_BOX_BOTTOM = TGuiIconName(85);
  ICON_BOX_BOTTOM_LEFT = TGuiIconName(86);
  ICON_BOX_LEFT = TGuiIconName(87);
  ICON_BOX_TOP_LEFT = TGuiIconName(88);
  ICON_BOX_CENTER = TGuiIconName(89);
  ICON_BOX_CIRCLE_MASK = TGuiIconName(90);
  ICON_POT = TGuiIconName(91);
  ICON_ALPHA_MULTIPLY = TGuiIconName(92);
  ICON_ALPHA_CLEAR = TGuiIconName(93);
  ICON_DITHERING = TGuiIconName(94);
  ICON_MIPMAPS = TGuiIconName(95);
  ICON_BOX_GRID = TGuiIconName(96);
  ICON_GRID = TGuiIconName(97);
  ICON_BOX_CORNERS_SMALL = TGuiIconName(98);
  ICON_BOX_CORNERS_BIG = TGuiIconName(99);
  ICON_FOUR_BOXES = TGuiIconName(100);
  ICON_GRID_FILL = TGuiIconName(101);
  ICON_BOX_MULTISIZE = TGuiIconName(102);
  ICON_ZOOM_SMALL = TGuiIconName(103);
  ICON_ZOOM_MEDIUM = TGuiIconName(104);
  ICON_ZOOM_BIG = TGuiIconName(105);
  ICON_ZOOM_ALL = TGuiIconName(106);
  ICON_ZOOM_CENTER = TGuiIconName(107);
  ICON_BOX_DOTS_SMALL = TGuiIconName(108);
  ICON_BOX_DOTS_BIG = TGuiIconName(109);
  ICON_BOX_CONCENTRIC = TGuiIconName(110);
  ICON_BOX_GRID_BIG = TGuiIconName(111);
  ICON_OK_TICK = TGuiIconName(112);
  ICON_CROSS = TGuiIconName(113);
  ICON_ARROW_LEFT = TGuiIconName(114);
  ICON_ARROW_RIGHT = TGuiIconName(115);
  ICON_ARROW_DOWN = TGuiIconName(116);
  ICON_ARROW_UP = TGuiIconName(117);
  ICON_ARROW_LEFT_FILL = TGuiIconName(118);
  ICON_ARROW_RIGHT_FILL = TGuiIconName(119);
  ICON_ARROW_DOWN_FILL = TGuiIconName(120);
  ICON_ARROW_UP_FILL = TGuiIconName(121);
  ICON_AUDIO = TGuiIconName(122);
  ICON_FX = TGuiIconName(123);
  ICON_WAVE = TGuiIconName(124);
  ICON_WAVE_SINUS = TGuiIconName(125);
  ICON_WAVE_SQUARE = TGuiIconName(126);
  ICON_WAVE_TRIANGULAR = TGuiIconName(127);
  ICON_CROSS_SMALL = TGuiIconName(128);
  ICON_PLAYER_PREVIOUS = TGuiIconName(129);
  ICON_PLAYER_PLAY_BACK = TGuiIconName(130);
  ICON_PLAYER_PLAY = TGuiIconName(131);
  ICON_PLAYER_PAUSE = TGuiIconName(132);
  ICON_PLAYER_STOP = TGuiIconName(133);
  ICON_PLAYER_NEXT = TGuiIconName(134);
  ICON_PLAYER_RECORD = TGuiIconName(135);
  ICON_MAGNET = TGuiIconName(136);
  ICON_LOCK_CLOSE = TGuiIconName(137);
  ICON_LOCK_OPEN = TGuiIconName(138);
  ICON_CLOCK = TGuiIconName(139);
  ICON_TOOLS = TGuiIconName(140);
  ICON_GEAR = TGuiIconName(141);
  ICON_GEAR_BIG = TGuiIconName(142);
  ICON_BIN = TGuiIconName(143);
  ICON_HAND_POINTER = TGuiIconName(144);
  ICON_LASER = TGuiIconName(145);
  ICON_COIN = TGuiIconName(146);
  ICON_EXPLOSION = TGuiIconName(147);
  ICON_1UP = TGuiIconName(148);
  ICON_PLAYER = TGuiIconName(149);
  ICON_PLAYER_JUMP = TGuiIconName(150);
  ICON_KEY = TGuiIconName(151);
  ICON_DEMON = TGuiIconName(152);
  ICON_TEXT_POPUP = TGuiIconName(153);
  ICON_GEAR_EX = TGuiIconName(154);
  ICON_CRACK = TGuiIconName(155);
  ICON_CRACK_POINTS = TGuiIconName(156);
  ICON_STAR = TGuiIconName(157);
  ICON_DOOR = TGuiIconName(158);
  ICON_EXIT = TGuiIconName(159);
  ICON_MODE_2D = TGuiIconName(160);
  ICON_MODE_3D = TGuiIconName(161);
  ICON_CUBE = TGuiIconName(162);
  ICON_CUBE_FACE_TOP = TGuiIconName(163);
  ICON_CUBE_FACE_LEFT = TGuiIconName(164);
  ICON_CUBE_FACE_FRONT = TGuiIconName(165);
  ICON_CUBE_FACE_BOTTOM = TGuiIconName(166);
  ICON_CUBE_FACE_RIGHT = TGuiIconName(167);
  ICON_CUBE_FACE_BACK = TGuiIconName(168);
  ICON_CAMERA = TGuiIconName(169);
  ICON_SPECIAL = TGuiIconName(170);
  ICON_LINK_NET = TGuiIconName(171);
  ICON_LINK_BOXES = TGuiIconName(172);
  ICON_LINK_MULTI = TGuiIconName(173);
  ICON_LINK = TGuiIconName(174);
  ICON_LINK_BROKE = TGuiIconName(175);
  ICON_TEXT_NOTES = TGuiIconName(176);
  ICON_NOTEBOOK = TGuiIconName(177);
  ICON_SUITCASE = TGuiIconName(178);
  ICON_SUITCASE_ZIP = TGuiIconName(179);
  ICON_MAILBOX = TGuiIconName(180);
  ICON_MONITOR = TGuiIconName(181);
  ICON_PRINTER = TGuiIconName(182);
  ICON_PHOTO_CAMERA = TGuiIconName(183);
  ICON_PHOTO_CAMERA_FLASH = TGuiIconName(184);
  ICON_HOUSE = TGuiIconName(185);
  ICON_HEART = TGuiIconName(186);
  ICON_CORNER = TGuiIconName(187);
  ICON_VERTICAL_BARS = TGuiIconName(188);
  ICON_VERTICAL_BARS_FILL = TGuiIconName(189);
  ICON_LIFE_BARS = TGuiIconName(190);
  ICON_INFO = TGuiIconName(191);
  ICON_CROSSLINE = TGuiIconName(192);
  ICON_HELP = TGuiIconName(193);
  ICON_FILETYPE_ALPHA = TGuiIconName(194);
  ICON_FILETYPE_HOME = TGuiIconName(195);
  ICON_LAYERS_VISIBLE = TGuiIconName(196);
  ICON_LAYERS = TGuiIconName(197);
  ICON_WINDOW = TGuiIconName(198);
  ICON_HIDPI = TGuiIconName(199);
  ICON_FILETYPE_BINARY = TGuiIconName(200);
  ICON_HEX = TGuiIconName(201);
  ICON_SHIELD = TGuiIconName(202);
  ICON_FILE_NEW = TGuiIconName(203);
  ICON_FOLDER_ADD = TGuiIconName(204);
  ICON_ALARM = TGuiIconName(205);
  ICON_CPU = TGuiIconName(206);
  ICON_ROM = TGuiIconName(207);
  ICON_STEP_OVER = TGuiIconName(208);
  ICON_STEP_INTO = TGuiIconName(209);
  ICON_STEP_OUT = TGuiIconName(210);
  ICON_RESTART = TGuiIconName(211);
  ICON_BREAKPOINT_ON = TGuiIconName(212);
  ICON_BREAKPOINT_OFF = TGuiIconName(213);
  ICON_BURGER_MENU = TGuiIconName(214);
  ICON_CASE_SENSITIVE = TGuiIconName(215);
  ICON_REG_EXP = TGuiIconName(216);
  ICON_FOLDER = TGuiIconName(217);
  ICON_FILE = TGuiIconName(218);
  ICON_SAND_TIMER = TGuiIconName(219);
  ICON_WARNING = TGuiIconName(220);
  ICON_HELP_BOX = TGuiIconName(221);
  ICON_INFO_BOX = TGuiIconName(222);
  ICON_PRIORITY = TGuiIconName(223);
  ICON_LAYERS_ISO = TGuiIconName(224);
  ICON_LAYERS2 = TGuiIconName(225);
  ICON_MLAYERS = TGuiIconName(226);
  ICON_MAPS = TGuiIconName(227);
  ICON_HOT = TGuiIconName(228);
  ICON_LABEL = TGuiIconName(229);
  ICON_NAME_ID = TGuiIconName(230);
  ICON_SLICING = TGuiIconName(231);
  ICON_MANUAL_CONTROL = TGuiIconName(232);
  ICON_COLLISION = TGuiIconName(233);
  ICON_CIRCLE_ADD = TGuiIconName(234);
  ICON_CIRCLE_ADD_FILL = TGuiIconName(235);
  ICON_CIRCLE_WARNING = TGuiIconName(236);
  ICON_CIRCLE_WARNING_FILL = TGuiIconName(237);
  ICON_BOX_MORE = TGuiIconName(238);
  ICON_BOX_MORE_FILL = TGuiIconName(239);
  ICON_BOX_MINUS = TGuiIconName(240);
  ICON_BOX_MINUS_FILL = TGuiIconName(241);
  ICON_UNION = TGuiIconName(242);
  ICON_INTERSECTION = TGuiIconName(243);
  ICON_DIFFERENCE = TGuiIconName(244);
  ICON_SPHERE = TGuiIconName(245);
  ICON_CYLINDER = TGuiIconName(246);
  ICON_CONE = TGuiIconName(247);
  ICON_ELLIPSOID = TGuiIconName(248);
  ICON_CAPSULE = TGuiIconName(249);
  ICON_250 = TGuiIconName(250);
  ICON_251 = TGuiIconName(251);
  ICON_252 = TGuiIconName(252);
  ICON_253 = TGuiIconName(253);
  ICON_254 = TGuiIconName(254);
  ICON_255 = TGuiIconName(255);


type
  PGuiState = ^TGuiState;
  TGuiState = Integer;
const
  STATE_NORMAL = TGuiState(0);
  STATE_FOCUSED = TGuiState(1);
  STATE_PRESSED = TGuiState(2);
  STATE_DISABLED = TGuiState(3);


// Gui control text alignment vertical
// NOTE: Text vertical position inside the text bounds
type
  PGuiTextAlignmentVertical = ^TGuiTextAlignmentVertical;
  TGuiTextAlignmentVertical = Integer;
const
  TEXT_ALIGN_TOP = TGuiTextAlignmentVertical(0);
  TEXT_ALIGN_MIDDLE = TGuiTextAlignmentVertical(1);
  TEXT_ALIGN_BOTTOM = TGuiTextAlignmentVertical(2);

  // Gui control text wrap mode
  // NOTE: Useful for multiline text
type
  PGuiTextWrapMode = ^TGuiTextWrapMode;
  TGuiTextWrapMode = integer;
const
  TEXT_WRAP_NONE = TGuiTextWrapMode(0);
  TEXT_WRAP_CHAR = TGuiTextWrapMode(1);
  TEXT_WRAP_WORD = TGuiTextWrapMode(2);

type
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment =integer;
const
  TEXT_ALIGN_LEFT = TGuiTextAlignment(0);
  TEXT_ALIGN_CENTER = TGuiTextAlignment(1);
  TEXT_ALIGN_RIGHT = TGuiTextAlignment(2);

type
  PGuiControl = ^TGuiControl;
  TGuiControl = Integer;
const
  DEFAULT = TGuiControl(0);
  LABELS = TGuiControl(1);
  BUTTON = TGuiControl(2);
  TOGGLE = TGuiControl(3);
  SLIDER = TGuiControl(4);
  PROGRESSBAR = TGuiControl(5);
  CHECKBOX = TGuiControl(6);
  COMBOBOX = TGuiControl(7);
  DROPDOWNBOX = TGuiControl(8);
  TEXTBOX = TGuiControl(9);
  VALUEBOX = TGuiControl(10);
  CONTROL11 = TGuiControl(11);
  LISTVIEW = TGuiControl(12);
  COLORPICKER = TGuiControl(13);
  SCROLLBAR = TGuiControl(14);
  STATUSBAR = TGuiControl(15);

type
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty = Integer;
const
  BORDER_COLOR_NORMAL   = TGuiControlProperty(0);    // Control border color in STATE_NORMAL
  BASE_COLOR_NORMAL     = TGuiControlProperty(1);    // Control base color in STATE_NORMAL
  TEXT_COLOR_NORMAL     = TGuiControlProperty(2);    // Control text color in STATE_NORMAL
  BORDER_COLOR_FOCUSED  = TGuiControlProperty(3);    // Control border color in STATE_FOCUSED
  BASE_COLOR_FOCUSED    = TGuiControlProperty(4);    // Control base color in STATE_FOCUSED
  TEXT_COLOR_FOCUSED    = TGuiControlProperty(5);    // Control text color in STATE_FOCUSED
  BORDER_COLOR_PRESSED  = TGuiControlProperty(6);    // Control border color in STATE_PRESSED
  BASE_COLOR_PRESSED    = TGuiControlProperty(7);    // Control base color in STATE_PRESSED
  TEXT_COLOR_PRESSED    = TGuiControlProperty(8);    // Control text color in STATE_PRESSED
  BORDER_COLOR_DISABLED = TGuiControlProperty(9);    // Control border color in STATE_DISABLED
  BASE_COLOR_DISABLED   = TGuiControlProperty(10);   // Control base color in STATE_DISABLED
  TEXT_COLOR_DISABLED   = TGuiControlProperty(11);   // Control text color in STATE_DISABLED
  BORDER_WIDTH          = TGuiControlProperty(12);   // Control border size, 0 for no border
  TEXT_PADDING          = TGuiControlProperty(13);   // Control text padding, not considering border
  TEXT_ALIGNMENT        = TGuiControlProperty(14);   // Control text horizontal alignment inside control text bound (after border and padding)


type
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty = Integer;
const
  TEXT_SIZE = TGuiDefaultProperty(16);                  // Text size (glyphs max height
  TEXT_SPACING = TGuiDefaultProperty(17);               // Text spacing between glyphs
  LINE_COLOR = TGuiDefaultProperty(18);                 // Line control color
  BACKGROUND_COLOR = TGuiDefaultProperty(19);           // Background color
  TEXT_LINE_SPACING = TGuiDefaultProperty(20);          // Text spacing between lines
  TEXT_ALIGNMENT_VERTICAL = TGuiDefaultProperty(21);    // Text vertical alignment inside text bounds (after border and padding)
  TEXT_WRAP_MODE = TGuiDefaultProperty(22);             // Text wrap-mode inside text bounds

type
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Integer;
const
  GROUP_PADDING = TGuiToggleProperty(16);

type
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Integer;
const
  SLIDER_WIDTH = TGuiSliderProperty(16);
  SLIDER_PADDING = TGuiSliderProperty(17);

type
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Integer;
const
  PROGRESS_PADDING = TGuiProgressBarProperty(16);

type
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Integer;
const
  ARROWS_SIZE = TGuiScrollBarProperty(16);
  ARROWS_VISIBLE = TGuiScrollBarProperty(17);
  SCROLL_SLIDER_PADDING = TGuiScrollBarProperty(18);
  SCROLL_SLIDER_SIZE = TGuiScrollBarProperty(19);
  SCROLL_PADDING = TGuiScrollBarProperty(20);
  SCROLL_SPEED = TGuiScrollBarProperty(21);

type
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Integer;
const
  CHECK_PADDING = TGuiCheckBoxProperty(16);

type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty = Integer;
const
  COMBO_BUTTON_WIDTH = TGuiComboBoxProperty(16);
  COMBO_BUTTON_SPACING = TGuiComboBoxProperty(17);

type
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Integer;
const
  ARROW_PADDING = TGuiDropdownBoxProperty(16);
  DROPDOWN_ITEMS_SPACING = TGuiDropdownBoxProperty(17);
  DROPDOWN_ARROW_HIDDEN = TGuiDropdownBoxProperty(18);      // DropdownBox arrow hidden
  DROPDOWN_ROLL_UP = TGuiDropdownBoxProperty(19);           // DropdownBox roll up flag (default rolls down)
// TextBox/TextBoxMulti/ValueBox/Spinner

type
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Integer;
const
  TEXT_READONLY = TGuiTextBoxProperty(16);         // TextBox in read-only mode: 0-text editable, 1-text no-editable

// Spinner
type
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty = Integer;
const
  SPIN_BUTTON_WIDTH = TGuiSpinnerProperty(16);    // Spinner left/right buttons width
  SPIN_BUTTON_SPACING = TGuiSpinnerProperty(17);  // Spinner buttons separation

// ListView
type
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Integer;
const
  LIST_ITEMS_HEIGHT = TGuiListViewProperty(16);   // ListView items height
  LIST_ITEMS_SPACING = TGuiListViewProperty(17);
  SCROLLBAR_WIDTH = TGuiListViewProperty(18);
  LIST_ITEMS_BORDER_NORMAL = TGuiListViewProperty(19);
  SCROLLBAR_SIDE = TGuiListViewProperty(20);
  LIST_ITEMS_BORDER_WIDTH = TGuiListViewProperty(21);    // ListView items border width

  // ColorPicker
type
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Integer;
const
  COLOR_SELECTOR_SIZE = TGuiColorPickerProperty(16);
  HUEBAR_WIDTH = TGuiColorPickerProperty(17);
  HUEBAR_PADDING = TGuiColorPickerProperty(18);
  HUEBAR_SELECTOR_HEIGHT = TGuiColorPickerProperty(19);
  HUEBAR_SELECTOR_OVERFLOW = TGuiColorPickerProperty(20);

// Style property
type
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
    controlId: Word;  // Control identifier
    propertyId: Word; // Property identifier
    propertyValue: Integer; // Property value
  end;

// Controls text style -NOT USED-
// NOTE: Text style is defined by control
type
  TGuiTextStyle = record
    size: Cardinal;
    charSpacing: Integer;
    lineSpacing: Integer;
    alignmentH: Integer;
    alignmentV: Integer;
    padding: Integer;
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

// Utility functions
function GuiGetTextWidth(const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetTextWidth'; // Get text width considering gui style and icon size (if required)

// Controls
//------------------------------------------------------------------------------

(* Container/separator controls, useful for controls organization *)

{Window Box control, shows a window that can be closed}
function GuiWindowBox(bounds: TRectangle; const title: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiWindowBox';
{Group Box control with text name}
function GuiGroupBox(bounds: TRectangle; const text:PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGroupBox';
{Line separator control, could contain text}
function GuiLine(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLine';
{Panel control, useful to group controls}
function GuiPanel(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiPanel';
{Tab Bar control, returns TAB to be closed or -1}
function GuiTabBar(bounds: TRectangle; const text: PPChar; count: Integer; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTabBar';
{Scroll Panel control}
function GuiScrollPanel(bounds: TRectangle; const text: PChar; content: TRectangle; scroll: PVector2; view: PRectangle): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiScrollPanel';

(* Basic controls set *)

{Label control}
function GuiLabel(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabel';
{Button control, returns true when clicked}
function GuiButton(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiButton';
{Label button control, returns true when clicked}
function GuiLabelButton(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabelButton';
{Toggle Button control}
function GuiToggle(bounds: TRectangle; const text: PChar; active: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggle';
{Toggle Group control}
function GuiToggleGroup(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleGroup';
{Toggle Slider control}
function GuiToggleSlider(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleSlider';
{Check Box control, returns true when active}
function GuiCheckBox(bounds: TRectangle; const text: PChar; checked: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiCheckBox';
{Combo Box control}
function GuiComboBox(bounds: TRectangle; const text: PChar; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiComboBox';
{Dropdown Box control}
function GuiDropdownBox(bounds: TRectangle; const text: PChar; active: PInteger; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDropdownBox';
{Spinner control}
function GuiSpinner(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSpinner';
{Value Box control, updates input text with numbers}
function GuiValueBox(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBox';
{Value box control for float values}
function GuiValueBoxFloat( bounds: TRectangle; const text, textValue: PChar; value: PSingle; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBoxFloat';
{Text Box control, updates input text}
function GuiTextBox(bounds: TRectangle; text: PChar; textSize: Integer; editMode: Boolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextBox';
{Slider control}
function GuiSlider(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSlider';
{Slider Bar control}
function GuiSliderBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSliderBar';
{Progress Bar control}
function GuiProgressBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value: PSingle; minValue, maxValue: Single): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiProgressBar';
{Status Bar control, shows info text}
function GuiStatusBar(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiStatusBar';
{Dummy control for placeholders}
function GuiDummyRec(bounds: TRectangle; const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDummyRec';
{Grid control}
function GuiGrid(bounds: TRectangle; const text: PChar; spacing: Single; subdivs: Integer; mouseCell: PVector2): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGrid';

(* Advance controls set *)

{List View control}
function GuiListView(bounds: TRectangle; const text: PChar; scrollIndex: PInteger; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListView';
{List View with extended parameters}
function GuiListViewEx(bounds: TRectangle; const text: PPChar; count: Integer; scrollIndex: PInteger; active, focus: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListViewEx';
{Message Box control, displays a message}
function GuiMessageBox(bounds: TRectangle; const title, message, buttons: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiMessageBox';
{Text Input Box control, ask for text, supports secret}
function GuiTextInputBox(bounds: TRectangle; const title, message, buttons, text: PChar; textMaxSize: Integer; secretViewActive: PBoolean): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextInputBox';
{Color Picker control (multiple color controls)}
function GuiColorPicker(bounds: TRectangle; const text: PChar; color: PColorB): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPicker';
{Color Panel control}
function GuiColorPanel(bounds: TRectangle; const text: PChar; color: PColorB): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanel';
{Color Bar Alpha control}
function GuiColorBarAlpha(bounds: TRectangle; const text: PChar; alpha: PSingle): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarAlpha';
{Color Bar Hue control}
function GuiColorBarHue(bounds: TRectangle; const text: PChar; value: PSingle): Integer ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarHue';
{Color Picker control that avoids conversion to RGB on each call (multiple color controls)}
function GuiColorPickerHSV(bounds: TRectangle; const text: PChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPickerHSV';
{Color Panel control that updates Hue-Saturation-Value color value, used by GuiColorPickerHSV()}
function GuiColorPanelHSV(bounds: TRectangle; const text: PChar; colorHsv: PVector3): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanelHSV';


implementation

end.

