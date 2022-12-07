{********************************************************************************************
*                                                                                           *
*   raygui v3.5 dev - A simple and easy-to-use immediate-mode gui library                   *
*                                                                                           *
*   DESCRIPTION:                                                                            *
*                                                                                           *
*   raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also       *
*   available as a standalone library, as long as input and drawing functions are provided. *
*                                                                                           *
*   pascal header by Gunko Vadim                                                            *
*                                                                                           *
*********************************************************************************************}

unit raygui;

{$mode ObjFPC}{$H+}

interface

uses
 raylib;

{$DEFINE RAYGUI_NO_RICONS}

//----------------------------------------------------------------------------------
// Icons enumeration
//----------------------------------------------------------------------------------
type
  PguiIconName = ^TguiIconName;
  TGuiIconName = Integer;
  const
    RICON_NONE                     = 0;
    RICON_FOLDER_FILE_OPEN         = 1;
    RICON_FILE_SAVE_CLASSIC        = 2;
    RICON_FOLDER_OPEN              = 3;
    RICON_FOLDER_SAVE              = 4;
    RICON_FILE_OPEN                = 5;
    RICON_FILE_SAVE                = 6;
    RICON_FILE_EXPORT              = 7;
    RICON_FILE_NEW                 = 8;
    RICON_FILE_DELETE              = 9;
    RICON_FILETYPE_TEXT            = 10;
    RICON_FILETYPE_AUDIO           = 11;
    RICON_FILETYPE_IMAGE           = 12;
    RICON_FILETYPE_PLAY            = 13;
    RICON_FILETYPE_VIDEO           = 14;
    RICON_FILETYPE_INFO            = 15;
    RICON_FILE_COPY                = 16;
    RICON_FILE_CUT                 = 17;
    RICON_FILE_PASTE               = 18;
    RICON_CURSOR_HAND              = 19;
    RICON_CURSOR_POINTER           = 20;
    RICON_CURSOR_CLASSIC           = 21;
    RICON_PENCIL                   = 22;
    RICON_PENCIL_BIG               = 23;
    RICON_BRUSH_CLASSIC            = 24;
    RICON_BRUSH_PAINTER            = 25;
    RICON_WATER_DROP               = 26;
    RICON_COLOR_PICKER             = 27;
    RICON_RUBBER                   = 28;
    RICON_COLOR_BUCKET             = 29;
    RICON_TEXT_T                   = 30;
    RICON_TEXT_A                   = 31;
    RICON_SCALE                    = 32;
    RICON_RESIZE                   = 33;
    RICON_FILTER_POINT             = 34;
    RICON_FILTER_BILINEAR          = 35;
    RICON_CROP                     = 36;
    RICON_CROP_ALPHA               = 37;
    RICON_SQUARE_TOGGLE            = 38;
    RICON_SYMMETRY                 = 39;
    RICON_SYMMETRY_HORIZONTAL      = 40;
    RICON_SYMMETRY_VERTICAL        = 41;
    RICON_LENS                     = 42;
    RICON_LENS_BIG                 = 43;
    RICON_EYE_ON                   = 44;
    RICON_EYE_OFF                  = 45;
    RICON_FILTER_TOP               = 46;
    RICON_FILTER                   = 47;
    RICON_TARGET_POINT             = 48;
    RICON_TARGET_SMALL             = 49;
    RICON_TARGET_BIG               = 50;
    RICON_TARGET_MOVE              = 51;
    RICON_CURSOR_MOVE              = 52;
    RICON_CURSOR_SCALE             = 53;
    RICON_CURSOR_SCALE_RIGHT       = 54;
    RICON_CURSOR_SCALE_LEFT        = 55;
    RICON_UNDO                     = 56;
    RICON_REDO                     = 57;
    RICON_REREDO                   = 58;
    RICON_MUTATE                   = 59;
    RICON_ROTATE                   = 60;
    RICON_REPEAT                   = 61;
    RICON_SHUFFLE                  = 62;
    RICON_EMPTYBOX                 = 63;
    RICON_TARGET                   = 64;
    RICON_TARGET_SMALL_FILL        = 65;
    RICON_TARGET_BIG_FILL          = 66;
    RICON_TARGET_MOVE_FILL         = 67;
    RICON_CURSOR_MOVE_FILL         = 68;
    RICON_CURSOR_SCALE_FILL        = 69;
    RICON_CURSOR_SCALE_RIGHT_FILL  = 70;
    RICON_CURSOR_SCALE_LEFT_FILL   = 71;
    RICON_UNDO_FILL                = 72;
    RICON_REDO_FILL                = 73;
    RICON_REREDO_FILL              = 74;
    RICON_MUTATE_FILL              = 75;
    RICON_ROTATE_FILL              = 76;
    RICON_REPEAT_FILL              = 77;
    RICON_SHUFFLE_FILL             = 78;
    RICON_EMPTYBOX_SMALL           = 79;
    RICON_BOX                      = 80;
    RICON_BOX_TOP                  = 81;
    RICON_BOX_TOP_RIGHT            = 82;
    RICON_BOX_RIGHT                = 83;
    RICON_BOX_BOTTOM_RIGHT         = 84;
    RICON_BOX_BOTTOM               = 85;
    RICON_BOX_BOTTOM_LEFT          = 86;
    RICON_BOX_LEFT                 = 87;
    RICON_BOX_TOP_LEFT             = 88;
    RICON_BOX_CENTER               = 89;
    RICON_BOX_CIRCLE_MASK          = 90;
    RICON_POT                      = 91;
    RICON_ALPHA_MULTIPLY           = 92;
    RICON_ALPHA_CLEAR              = 93;
    RICON_DITHERING                = 94;
    RICON_MIPMAPS                  = 95;
    RICON_BOX_GRID                 = 96;
    RICON_GRID                     = 97;
    RICON_BOX_CORNERS_SMALL        = 98;
    RICON_BOX_CORNERS_BIG          = 99;
    RICON_FOUR_BOXES               = 100;
    RICON_GRID_FILL                = 101;
    RICON_BOX_MULTISIZE            = 102;
    RICON_ZOOM_SMALL               = 103;
    RICON_ZOOM_MEDIUM              = 104;
    RICON_ZOOM_BIG                 = 105;
    RICON_ZOOM_ALL                 = 106;
    RICON_ZOOM_CENTER              = 107;
    RICON_BOX_DOTS_SMALL           = 108;
    RICON_BOX_DOTS_BIG             = 109;
    RICON_BOX_CONCENTRIC           = 110;
    RICON_BOX_GRID_BIG             = 111;
    RICON_OK_TICK                  = 112;
    RICON_CROSS                    = 113;
    RICON_ARROW_LEFT               = 114;
    RICON_ARROW_RIGHT              = 115;
    RICON_ARROW_DOWN               = 116;
    RICON_ARROW_UP                 = 117;
    RICON_ARROW_LEFT_FILL          = 118;
    RICON_ARROW_RIGHT_FILL         = 119;
    RICON_ARROW_DOWN_FILL          = 120;
    RICON_ARROW_UP_FILL            = 121;
    RICON_AUDIO                    = 122;
    RICON_FX                       = 123;
    RICON_WAVE                     = 124;
    RICON_WAVE_SINUS               = 125;
    RICON_WAVE_SQUARE              = 126;
    RICON_WAVE_TRIANGULAR          = 127;
    RICON_CROSS_SMALL              = 128;
    RICON_PLAYER_PREVIOUS          = 129;
    RICON_PLAYER_PLAY_BACK         = 130;
    RICON_PLAYER_PLAY              = 131;
    RICON_PLAYER_PAUSE             = 132;
    RICON_PLAYER_STOP              = 133;
    RICON_PLAYER_NEXT              = 134;
    RICON_PLAYER_RECORD            = 135;
    RICON_MAGNET                   = 136;
    RICON_LOCK_CLOSE               = 137;
    RICON_LOCK_OPEN                = 138;
    RICON_CLOCK                    = 139;
    RICON_TOOLS                    = 140;
    RICON_GEAR                     = 141;
    RICON_GEAR_BIG                 = 142;
    RICON_BIN                      = 143;
    RICON_HAND_POINTER             = 144;
    RICON_LASER                    = 145;
    RICON_COIN                     = 146;
    RICON_EXPLOSION                = 147;
    RICON_1UP                      = 148;
    RICON_PLAYER                   = 149;
    RICON_PLAYER_JUMP              = 150;
    RICON_KEY                      = 151;
    RICON_DEMON                    = 152;
    RICON_TEXT_POPUP               = 153;
    RICON_GEAR_EX                  = 154;
    RICON_CRACK                    = 155;
    RICON_CRACK_POINTS             = 156;
    RICON_STAR                     = 157;
    RICON_DOOR                     = 158;
    RICON_EXIT                     = 159;
    RICON_MODE_2D                  = 160;
    RICON_MODE_3D                  = 161;
    RICON_CUBE                     = 162;
    RICON_CUBE_FACE_TOP            = 163;
    RICON_CUBE_FACE_LEFT           = 164;
    RICON_CUBE_FACE_FRONT          = 165;
    RICON_CUBE_FACE_BOTTOM         = 166;
    RICON_CUBE_FACE_RIGHT          = 167;
    RICON_CUBE_FACE_BACK           = 168;
    RICON_CAMERA                   = 169;
    RICON_SPECIAL                  = 170;
    RICON_LINK_NET                 = 171;
    RICON_LINK_BOXES               = 172;
    RICON_LINK_MULTI               = 173;
    RICON_LINK                     = 174;
    RICON_LINK_BROKE               = 175;
    RICON_TEXT_NOTES               = 176;
    RICON_NOTEBOOK                 = 177;
    RICON_SUITCASE                 = 178;
    RICON_SUITCASE_ZIP             = 179;
    RICON_MAILBOX                  = 180;
    RICON_MONITOR                  = 181;
    RICON_PRINTER                  = 182;
    RICON_PHOTO_CAMERA             = 183;
    RICON_PHOTO_CAMERA_FLASH       = 184;
    RICON_HOUSE                    = 185;
    RICON_HEART                    = 186;
    RICON_CORNER                   = 187;
    RICON_VERTICAL_BARS            = 188;
    RICON_VERTICAL_BARS_FILL       = 189;
    RICON_LIFE_BARS                = 190;
    RICON_INFO                     = 191;
    RICON_CROSSLINE                = 192;
    RICON_HELP                     = 193;
    RICON_FILETYPE_ALPHA           = 194;
    RICON_FILETYPE_HOME            = 195;
    RICON_LAYERS_VISIBLE           = 196;
    RICON_LAYERS                   = 197;
    RICON_WINDOW                   = 198;
    RICON_HIDPI                    = 199;
    ICON_FILETYPE_BINARY           = 200;
    ICON_HEX                       = 201;
    ICON_SHIELD                    = 202;
    ICON_FILE_NEW                  = 203;
    ICON_FOLDER_ADD                = 204;
    ICON_ALARM                     = 205;
    ICON_CPU                       = 206;
    ICON_ROM                       = 207;
    ICON_STEP_OVER                 = 208;
    ICON_STEP_INTO                 = 209;
    ICON_STEP_OUT                  = 210;
    ICON_RESTART                   = 211;
    ICON_BREAKPOINT_ON             = 212;
    ICON_BREAKPOINT_OFF            = 213;
    ICON_BURGER_MENU               = 214;
    ICON_CASE_SENSITIVE            = 215;
    ICON_REG_EXP                   = 216;
    ICON_FOLDER                    = 217;
    ICON_FILE                      = 218;
    RICON_219                      = 219;
    RICON_220                      = 220;
    RICON_221                      = 221;
    RICON_222                      = 222;
    RICON_223                      = 223;
    RICON_224                      = 224;
    RICON_225                      = 225;
    RICON_226                      = 226;
    RICON_227                      = 227;
    RICON_228                      = 228;
    RICON_229                      = 229;
    RICON_230                      = 230;
    RICON_231                      = 231;
    RICON_232                      = 232;
    RICON_233                      = 233;
    RICON_234                      = 234;
    RICON_235                      = 235;
    RICON_236                      = 236;
    RICON_237                      = 237;
    RICON_238                      = 238;
    RICON_239                      = 239;
    RICON_240                      = 240;
    RICON_241                      = 241;
    RICON_242                      = 242;
    RICON_243                      = 243;
    RICON_244                      = 244;
    RICON_245                      = 245;
    RICON_246                      = 246;
    RICON_247                      = 247;
    RICON_248                      = 248;
    RICON_249                      = 249;
    RICON_250                      = 250;
    RICON_251                      = 251;
    RICON_252                      = 252;
    RICON_253                      = 253;
    RICON_254                      = 254;
    RICON_255                      = 255;

type
// GlyphInfo, font characters glyphs info
  PGlyphInfo = ^TGlyphInfo;
  TGlyphInfo = record
    value    : Integer;// Character value (Unicode)
    offsetX  : Integer;// Character offset X when drawing
    offsetY  : Integer;// Character offset Y when drawing
    advanceX : Integer;// Character advance position X
    image    : TImage; // Character image data
  end;

  // Style property
  PGuiStyleProp = ^TGuiStyleProp;
  TGuiStyleProp = record
    controlId     : Word;
    propertyId    : Word;
    propertyValue : Integer;
  end;

  // Gui control state
  PGuiControlState = ^TGuiControlState;
  TGuiControlState = Integer;
    const
      STATE_NORMAL   = 0;
      STATE_FOCUSED  = 1;
      STATE_PRESSED  = 2;
      STATE_DISABLED = 3;

type
  // Gui control text alignment
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment = Integer;
    const
      TEXT_ALIGN_LEFT   = 0;
      TEXT_ALIGN_CENTER = 1;
      TEXT_ALIGN_RIGHT  = 2;

type
  // Gui controls
  PGuiControl = ^TGuiControl;
  TGuiControl = Integer;
    const
      DEFAULT       = 0; // Generic control -> populates to all controls when set
      // Basic controls
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
  TGuiControlProperty = Integer;
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
  TGuiDefaultProperty = Integer;
    const
      TEXT_SIZE        = 16;
      TEXT_SPACING     = 17;
      LINE_COLOR       = 18;
      BACKGROUND_COLOR = 19;

type
  // Toggle/ToggleGroup
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Integer;
    const
      GROUP_PADDING = 16;

type
  // Slider/SliderBar
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Integer;
    const
      SLIDER_WIDTH   = 16;
      SLIDER_PADDING = 17;

type
  // ProgressBar
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Integer;
    const
      PROGRESS_PADDING = 16;

type
  // ScrollBar
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Integer;
    const
      ARROWS_SIZE           = 16;
      ARROWS_VISIBLE        = 17;
      SCROLL_SLIDER_PADDING = 18;
      SCROLL_SLIDER_SIZE    = 19;
      SCROLL_PADDING        = 20;
      SCROLL_SPEED          = 21;

type
  // CheckBox
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Integer;
    const
      CHECK_PADDING = 16;

type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty = Integer;
    const
      COMBO_BUTTON_WIDTH = 16;    // ComboBox right button width
      COMBO_BUTTON_SPACING = 17;       // ComboBox button separation

type
  // DropdownBox
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Integer;
    const
      ARROW_PADDING = 16;
      DROPDOWN_ITEMS_SPACING = 17;

type
  // TextBox/TextBoxMulti/ValueBox/Spinner
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Integer;
    const
      TEXT_INNER_PADDING = 16;
      TEXT_LINES_SPACING = 17;

type
  // Spinner
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty = Integer;
    const
      SPIN_BUTTON_WIDTH   = 16;
      SPIN_BUTTON_SPACING = 17;

type
  // ListView
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Integer;
    const
      LIST_ITEMS_HEIGHT   = 16;
      LIST_ITEMS_SPACING  = 17;
      SCROLLBAR_WIDTH     = 18;
      SCROLLBAR_SIDE      = 19;

type
  // ColorPicker
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Integer;
    const
      COLOR_SELECTOR_SIZE      = 16;
      HUEBAR_WIDTH             = 17; // Right hue bar width
      HUEBAR_PADDING           = 18; // Right hue bar separation from panel
      HUEBAR_SELECTOR_HEIGHT   = 19; // Right hue bar selector height
      HUEBAR_SELECTOR_OVERFLOW = 20; // Right hue bar selector overflow

    const
      SCROLLBAR_LEFT_SIDE   = 0;
      SCROLLBAR_RIGHT_SIDE  = 1;

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

(*Global gui state control functions*)

{Enable gui controls (global state)}
procedure GuiEnable; cdecl; external cDllName;
{Disable gui controls (global state)}
procedure GuiDisable; cdecl; external cDllName;
{Lock gui controls (global state)}
procedure GuiLock; cdecl; external cDllName;
{Unlock gui controls (global state)}
procedure GuiUnlock; cdecl; external cDllName;
{Check if gui is locked (global state)}
function  GuiIsLocked: Boolean; cdecl; external cDllName;
{Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f}
procedure GuiFade(alpha: Single); cdecl; external cDllName;
{Set gui state (global state)}
procedure GuiSetState(state: Integer); cdecl; external cDllName;
{Get gui state (global state)}
function GuiGetState: Integer; cdecl; external cDllName;

(*Font set/get functions*)

{Set gui custom font (global state)}
procedure GuiSetFont(font: TFont); cdecl; external cDllName;
{Get gui custom font (global state)}
function GuiGetFont:TFont; cdecl; external cDllName;

(*Style set/get functions*)

{Set one style property}
procedure GuiSetStyle(control, property_, value: Integer); cdecl; external cDllName;
{Get one style property}
function GuiGetStyle(control, property_: Integer): Integer; cdecl; external cDllName;

(*Container/separator controls, useful for controls organization*)

{Window Box control, shows a window that can be closed}
function GuiWindowBox(bounds: TRectangle; const title: PChar): Boolean; cdecl; external cDllName;
{Group Box control with text name}
procedure GuiGroupBox(bounds: TRectangle; const text:PChar); cdecl; external cDllName;
{Line separator control, could contain text}
procedure GuiLine(bounds: TRectangle; const text: PChar); cdecl; external cDllName;
{Panel control, useful to group controls}
procedure GuiPanel(bounds: TRectangle; const text: PChar); cdecl; external cDllName;
{Tab Bar control, returns TAB to be closed or -1}
function GuiTabBar(bounds: TRectangle; const text: PPChar; count: Integer; active: PInteger): Integer; cdecl; external cDllName;
{Scroll Panel control}
function GuiScrollPanel(bounds: TRectangle; const text: PChar; content: TRectangle; scroll: PVector2): TRectangle; cdecl; external cDllName;

(*Basic controls set*)

{Label control, shows text}
procedure GuiLabel(bounds: TRectangle; const text: PChar); cdecl; external cDllName;
{Button control, returns true when clicked}
function GuiButton(bounds: TRectangle; const text: PChar): Boolean; cdecl; external cDllName;
{Label button control, show true when clicked}
function GuiLabelButton(bounds: TRectangle; const text: PChar): Boolean; cdecl; external cDllName;
{Toggle Button control, returns true when active}
function GuiToggle(bounds: TRectangle; const text: PChar; active: Boolean): Boolean; cdecl; external cDllName;
{Toggle Group control, returns active toggle index}
function GuiToggleGroup(bounds: TRectangle; const text: PChar; active: Integer): Integer; cdecl; external cDllName;
{Check Box control, returns true when active}
function GuiCheckBox(bounds: TRectangle; const text: PChar; checked: Boolean): Boolean; cdecl; external cDllName;
{Combo Box control, returns selected item index}
function GuiComboBox(bounds: TRectangle; const text: PChar; active: Integer): Integer; cdecl; external cDllName;
{Dropdown Box control, returns selected item}
function GuiDropdownBox(bounds: TRectangle; const text: PChar; active: PInteger; editMode: Boolean): Boolean; cdecl; external cDllName;
{Spinner control, returns selected value}
function GuiSpinner(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Boolean; cdecl; external cDllName;
{Value Box control, updates input text with numbers}
function GuiValueBox(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Boolean; cdecl; external cDllName;
{Text Box control, updates input text}
function GuiTextBox(bounds: TRectangle; text: PChar; textSize: Integer; editMode: Boolean): Boolean; cdecl; external cDllName;
{Text Box control with multiple lines}
function GuiTextBoxMulti(bounds: TRectangle; text: PChar; textSize: Integer; editMode: Boolean): Boolean; cdecl; external cDllName;
{Slider control, returns selected value}
function GuiSlider(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external cDllName;
{Slider Bar control, returns selected value}
function GuiSliderBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external cDllName;
{Progress Bar control, shows current progress value}
function GuiProgressBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external cDllName;
{Status Bar control, shows info text}
procedure GuiStatusBar(bounds: TRectangle; const text: PChar); cdecl; external cDllName;
{Dummy control for placeholders}
procedure GuiDummyRec(bounds: TRectangle; const text: PChar); cdecl; external cDllName;
{Scroll Bar control}
function GuiScrollBar(bounds: TRectangle; value, minValue, maxValue: Integer): Integer; cdecl; external cDllName;
{Grid control}
function GuiGrid(bounds: TRectangle; const text: PChar; spacing:Single; subdivs: Integer): TVector2; cdecl; external cDllName;

(*Advance controls set*)

{List View control, returns selected list item index}
function GuiListView(bounds: TRectangle; const text: PChar; scrollIndex: PInteger; active: Integer): Integer; cdecl; external cDllName;
{List View with extended parameters}
function GuiListViewEx(bounds: TRectangle; const text: PPChar; count: Integer; focus: PInteger; scrollIndex: PInteger; active: Integer): Integer; cdecl; external cDllName;
{Message Box control, displays a message}
function GuiMessageBox(bounds: TRectangle; const title, message, buttons: PChar): Integer; cdecl; external cDllName;
{Text Input Box control, ask for text}
function GuiTextInputBox(bounds: TRectangle; const title, message, buttons, text: PChar; textMaxSize: Integer; secretViewActive: PInteger): Integer; cdecl; external cDllName;
{Color Picker control (multiple color controls)}
function GuiColorPicker(bounds: TRectangle; const text: PChar; color: TColorB): TColorB ; cdecl; external cDllName;
{Color Panel control}
function GuiColorPanel(bounds: TRectangle; const text: PChar; color: TColorB): TColorB ; cdecl; external cDllName;
{Color Bar Alpha control}
function GuiColorBarAlpha(bounds: TRectangle; const text: PChar; alpha: Single): Single ; cdecl; external cDllName;
{Color Bar Hue control}
function GuiColorBarHue(bounds:TRectangle; const text: PChar; value: Single): Single ; cdecl; external cDllName;

(* Styles loading functions *)
{Load style file over global style variable (.rgs)}
procedure GuiLoadStyle(const fileName: PChar); cdecl; external cDllName;
{Load style default over global style }
procedure GuiLoadStyleDefault; cdecl; external cDllName;

(* Tooltips management functions *)
{Enable gui tooltips (global state)}
procedure GuiEnableTooltip; cdecl; external cDllName;
{Disable gui tooltips (global state)}
procedure GuiDisableTooltip; cdecl; external cDllName;
{Set tooltip string}
procedure GuiSetTooltip(const tooltip: PChar); cdecl; external cDllName;

(* Icons functionality *)
{Get text with icon id prepended (if supported)}
function GuiIconText(iconId: longint; const text: PChar): PChar; cdecl; external cDllName;

{$IFDEF RAYGUI_NO_RICONS}
{Get full icons data pointer}
function GuiGetIcons: Pointer; cdecl; external cDllName;
{Load raygui icons file (.rgi) into internal icons data}
function GuiLoadIcons(const fileName: PChar; loadIconsName: Boolean): PPChar; cdecl; external cDllName;
{Gui icons functionality}
procedure GuiDrawIcon(iconId, posX, posY, pixelSize: Integer; color: TColorB); cdecl; external cDllName;
{$ENDIF}

implementation

end.

