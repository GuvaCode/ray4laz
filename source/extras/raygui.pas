{********************************************************************************************
*                                                                                           *
*   raygui v3.0 - A simple and easy-to-use immediate-mode gui library                       *
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
  TGuiIconName = Longint;
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
    RICON_200                      = 200;
    RICON_201                      = 201;
    RICON_202                      = 202;
    RICON_203                      = 203;
    RICON_204                      = 204;
    RICON_205                      = 205;
    RICON_206                      = 206;
    RICON_207                      = 207;
    RICON_208                      = 208;
    RICON_209                      = 209;
    RICON_210                      = 210;
    RICON_211                      = 211;
    RICON_212                      = 212;
    RICON_213                      = 213;
    RICON_214                      = 214;
    RICON_215                      = 215;
    RICON_216                      = 216;
    RICON_217                      = 217;
    RICON_218                      = 218;
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
procedure GuiLoadStyle(const fileName: pchar); cdecl;external cDllName;// Load style file over global style variable (.rgs)
procedure GuiLoadStyleDefault; cdecl;external cDllName;// Load style default over global style

function GuiIconText(iconId: longint; const text: pchar): pchar; cdecl;external cDllName; // Get text with icon id prepended (if supported)

{$IFDEF RAYGUI_NO_RICONS}
// Gui icons functionality
procedure GuiDrawIcon(iconId: longint; posX: longint; posY: longint; pixelSize: longint; color: TColor); cdecl;external cDllName;

function GuiGetIcons: pdword; cdecl;external cDllName;// Get full icons data pointer
function GuiGetIconData(iconId: longint): pdword; cdecl;external cDllName;// Get icon bit data
procedure GuiSetIconData(iconId: longint; data: pdword); cdecl;external cDllName;// Set icon bit data
procedure GuiSetIconPixel(iconId: longint; x: longint; y: longint); cdecl;external cDllName;// Set icon pixel value
procedure GuiClearIconPixel(iconId: longint; x: longint; y: longint); cdecl;external cDllName;// Clear icon pixel value
function GuiCheckIconPixel(iconId: longint; x: longint; y: longint): boolean; cdecl;external cDllName;// Check icon pixel value
{$ENDIF}

implementation

end.

