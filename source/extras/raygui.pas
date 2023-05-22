{********************************************************************************************
*                                                                                           *
*   raygui v3.6 - A simple and easy-to-use immediate-mode gui library                   *
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

{$DEFINE RAYGUI_NO_RICONS}

//----------------------------------------------------------------------------------
// Icons enumeration
//----------------------------------------------------------------------------------
type
  PguiIconName = ^TguiIconName;
  TGuiIconName = Integer;
  const
    RICON_NONE                     = TGuiIconName(0);
    RICON_FOLDER_FILE_OPEN         = TGuiIconName(1);
    RICON_FILE_SAVE_CLASSIC        = TGuiIconName(2);
    RICON_FOLDER_OPEN              = TGuiIconName(3);
    RICON_FOLDER_SAVE              = TGuiIconName(4);
    RICON_FILE_OPEN                = TGuiIconName(5);
    RICON_FILE_SAVE                = TGuiIconName(6);
    RICON_FILE_EXPORT              = TGuiIconName(7);
    RICON_FILE_NEW                 = TGuiIconName(8);
    RICON_FILE_DELETE              = TGuiIconName(9);
    RICON_FILETYPE_TEXT            = TGuiIconName(10);
    RICON_FILETYPE_AUDIO           = TGuiIconName(11);
    RICON_FILETYPE_IMAGE           = TGuiIconName(12);
    RICON_FILETYPE_PLAY            = TGuiIconName(13);
    RICON_FILETYPE_VIDEO           = TGuiIconName(14);
    RICON_FILETYPE_INFO            = TGuiIconName(15);
    RICON_FILE_COPY                = TGuiIconName(16);
    RICON_FILE_CUT                 = TGuiIconName(17);
    RICON_FILE_PASTE               = TGuiIconName(18);
    RICON_CURSOR_HAND              = TGuiIconName(19);
    RICON_CURSOR_POINTER           = TGuiIconName(20);
    RICON_CURSOR_CLASSIC           = TGuiIconName(21);
    RICON_PENCIL                   = TGuiIconName(22);
    RICON_PENCIL_BIG               = TGuiIconName(23);
    RICON_BRUSH_CLASSIC            = TGuiIconName(24);
    RICON_BRUSH_PAINTER            = TGuiIconName(25);
    RICON_WATER_DROP               = TGuiIconName(26);
    RICON_COLOR_PICKER             = TGuiIconName(27);
    RICON_RUBBER                   = TGuiIconName(28);
    RICON_COLOR_BUCKET             = TGuiIconName(29);
    RICON_TEXT_T                   = TGuiIconName(30);
    RICON_TEXT_A                   = TGuiIconName(31);
    RICON_SCALE                    = TGuiIconName(32);
    RICON_RESIZE                   = TGuiIconName(33);
    RICON_FILTER_POINT             = TGuiIconName(34);
    RICON_FILTER_BILINEAR          = TGuiIconName(35);
    RICON_CROP                     = TGuiIconName(36);
    RICON_CROP_ALPHA               = TGuiIconName(37);
    RICON_SQUARE_TOGGLE            = TGuiIconName(38);
    RICON_SYMMETRY                 = TGuiIconName(39);
    RICON_SYMMETRY_HORIZONTAL      = TGuiIconName(40);
    RICON_SYMMETRY_VERTICAL        = TGuiIconName(41);
    RICON_LENS                     = TGuiIconName(42);
    RICON_LENS_BIG                 = TGuiIconName(43);
    RICON_EYE_ON                   = TGuiIconName(44);
    RICON_EYE_OFF                  = TGuiIconName(45);
    RICON_FILTER_TOP               = TGuiIconName(46);
    RICON_FILTER                   = TGuiIconName(47);
    RICON_TARGET_POINT             = TGuiIconName(48);
    RICON_TARGET_SMALL             = TGuiIconName(49);
    RICON_TARGET_BIG               = TGuiIconName(50);
    RICON_TARGET_MOVE              = TGuiIconName(51);
    RICON_CURSOR_MOVE              = TGuiIconName(52);
    RICON_CURSOR_SCALE             = TGuiIconName(53);
    RICON_CURSOR_SCALE_RIGHT       = TGuiIconName(54);
    RICON_CURSOR_SCALE_LEFT        = TGuiIconName(55);
    RICON_UNDO                     = TGuiIconName(56);
    RICON_REDO                     = TGuiIconName(57);
    RICON_REREDO                   = TGuiIconName(58);
    RICON_MUTATE                   = TGuiIconName(59);
    RICON_ROTATE                   = TGuiIconName(60);
    RICON_REPEAT                   = TGuiIconName(61);
    RICON_SHUFFLE                  = TGuiIconName(62);
    RICON_EMPTYBOX                 = TGuiIconName(63);
    RICON_TARGET                   = TGuiIconName(64);
    RICON_TARGET_SMALL_FILL        = TGuiIconName(65);
    RICON_TARGET_BIG_FILL          = TGuiIconName(66);
    RICON_TARGET_MOVE_FILL         = TGuiIconName(67);
    RICON_CURSOR_MOVE_FILL         = TGuiIconName(68);
    RICON_CURSOR_SCALE_FILL        = TGuiIconName(69);
    RICON_CURSOR_SCALE_RIGHT_FILL  = TGuiIconName(70);
    RICON_CURSOR_SCALE_LEFT_FILL   = TGuiIconName(71);
    RICON_UNDO_FILL                = TGuiIconName(72);
    RICON_REDO_FILL                = TGuiIconName(73);
    RICON_REREDO_FILL              = TGuiIconName(74);
    RICON_MUTATE_FILL              = TGuiIconName(75);
    RICON_ROTATE_FILL              = TGuiIconName(76);
    RICON_REPEAT_FILL              = TGuiIconName(77);
    RICON_SHUFFLE_FILL             = TGuiIconName(78);
    RICON_EMPTYBOX_SMALL           = TGuiIconName(79);
    RICON_BOX                      = TGuiIconName(80);
    RICON_BOX_TOP                  = TGuiIconName(81);
    RICON_BOX_TOP_RIGHT            = TGuiIconName(82);
    RICON_BOX_RIGHT                = TGuiIconName(83);
    RICON_BOX_BOTTOM_RIGHT         = TGuiIconName(84);
    RICON_BOX_BOTTOM               = TGuiIconName(85);
    RICON_BOX_BOTTOM_LEFT          = TGuiIconName(86);
    RICON_BOX_LEFT                 = TGuiIconName(87);
    RICON_BOX_TOP_LEFT             = TGuiIconName(88);
    RICON_BOX_CENTER               = TGuiIconName(89);
    RICON_BOX_CIRCLE_MASK          = TGuiIconName(90);
    RICON_POT                      = TGuiIconName(91);
    RICON_ALPHA_MULTIPLY           = TGuiIconName(92);
    RICON_ALPHA_CLEAR              = TGuiIconName(93);
    RICON_DITHERING                = TGuiIconName(94);
    RICON_MIPMAPS                  = TGuiIconName(95);
    RICON_BOX_GRID                 = TGuiIconName(96);
    RICON_GRID                     = TGuiIconName(97);
    RICON_BOX_CORNERS_SMALL        = TGuiIconName(98);
    RICON_BOX_CORNERS_BIG          = TGuiIconName(99);
    RICON_FOUR_BOXES               = TGuiIconName(100);
    RICON_GRID_FILL                = TGuiIconName(101);
    RICON_BOX_MULTISIZE            = TGuiIconName(102);
    RICON_ZOOM_SMALL               = TGuiIconName(103);
    RICON_ZOOM_MEDIUM              = TGuiIconName(104);
    RICON_ZOOM_BIG                 = TGuiIconName(105);
    RICON_ZOOM_ALL                 = TGuiIconName(106);
    RICON_ZOOM_CENTER              = TGuiIconName(107);
    RICON_BOX_DOTS_SMALL           = TGuiIconName(108);
    RICON_BOX_DOTS_BIG             = TGuiIconName(109);
    RICON_BOX_CONCENTRIC           = TGuiIconName(110);
    RICON_BOX_GRID_BIG             = TGuiIconName(111);
    RICON_OK_TICK                  = TGuiIconName(112);
    RICON_CROSS                    = TGuiIconName(113);
    RICON_ARROW_LEFT               = TGuiIconName(114);
    RICON_ARROW_RIGHT              = TGuiIconName(115);
    RICON_ARROW_DOWN               = TGuiIconName(116);
    RICON_ARROW_UP                 = TGuiIconName(117);
    RICON_ARROW_LEFT_FILL          = TGuiIconName(118);
    RICON_ARROW_RIGHT_FILL         = TGuiIconName(119);
    RICON_ARROW_DOWN_FILL          = TGuiIconName(120);
    RICON_ARROW_UP_FILL            = TGuiIconName(121);
    RICON_AUDIO                    = TGuiIconName(122);
    RICON_FX                       = TGuiIconName(123);
    RICON_WAVE                     = TGuiIconName(124);
    RICON_WAVE_SINUS               = TGuiIconName(125);
    RICON_WAVE_SQUARE              = TGuiIconName(126);
    RICON_WAVE_TRIANGULAR          = TGuiIconName(127);
    RICON_CROSS_SMALL              = TGuiIconName(128);
    RICON_PLAYER_PREVIOUS          = TGuiIconName(129);
    RICON_PLAYER_PLAY_BACK         = TGuiIconName(130);
    RICON_PLAYER_PLAY              = TGuiIconName(131);
    RICON_PLAYER_PAUSE             = TGuiIconName(132);
    RICON_PLAYER_STOP              = TGuiIconName(133);
    RICON_PLAYER_NEXT              = TGuiIconName(134);
    RICON_PLAYER_RECORD            = TGuiIconName(135);
    RICON_MAGNET                   = TGuiIconName(136);
    RICON_LOCK_CLOSE               = TGuiIconName(137);
    RICON_LOCK_OPEN                = TGuiIconName(138);
    RICON_CLOCK                    = TGuiIconName(139);
    RICON_TOOLS                    = TGuiIconName(140);
    RICON_GEAR                     = TGuiIconName(141);
    RICON_GEAR_BIG                 = TGuiIconName(142);
    RICON_BIN                      = TGuiIconName(143);
    RICON_HAND_POINTER             = TGuiIconName(144);
    RICON_LASER                    = TGuiIconName(145);
    RICON_COIN                     = TGuiIconName(146);
    RICON_EXPLOSION                = TGuiIconName(147);
    RICON_1UP                      = TGuiIconName(148);
    RICON_PLAYER                   = TGuiIconName(149);
    RICON_PLAYER_JUMP              = TGuiIconName(150);
    RICON_KEY                      = TGuiIconName(151);
    RICON_DEMON                    = TGuiIconName(152);
    RICON_TEXT_POPUP               = TGuiIconName(153);
    RICON_GEAR_EX                  = TGuiIconName(154);
    RICON_CRACK                    = TGuiIconName(155);
    RICON_CRACK_POINTS             = TGuiIconName(156);
    RICON_STAR                     = TGuiIconName(157);
    RICON_DOOR                     = TGuiIconName(158);
    RICON_EXIT                     = TGuiIconName(159);
    RICON_MODE_2D                  = TGuiIconName(160);
    RICON_MODE_3D                  = TGuiIconName(161);
    RICON_CUBE                     = TGuiIconName(162);
    RICON_CUBE_FACE_TOP            = TGuiIconName(163);
    RICON_CUBE_FACE_LEFT           = TGuiIconName(164);
    RICON_CUBE_FACE_FRONT          = TGuiIconName(165);
    RICON_CUBE_FACE_BOTTOM         = TGuiIconName(166);
    RICON_CUBE_FACE_RIGHT          = TGuiIconName(167);
    RICON_CUBE_FACE_BACK           = TGuiIconName(168);
    RICON_CAMERA                   = TGuiIconName(169);
    RICON_SPECIAL                  = TGuiIconName(170);
    RICON_LINK_NET                 = TGuiIconName(171);
    RICON_LINK_BOXES               = TGuiIconName(172);
    RICON_LINK_MULTI               = TGuiIconName(173);
    RICON_LINK                     = TGuiIconName(174);
    RICON_LINK_BROKE               = TGuiIconName(175);
    RICON_TEXT_NOTES               = TGuiIconName(176);
    RICON_NOTEBOOK                 = TGuiIconName(177);
    RICON_SUITCASE                 = TGuiIconName(178);
    RICON_SUITCASE_ZIP             = TGuiIconName(179);
    RICON_MAILBOX                  = TGuiIconName(180);
    RICON_MONITOR                  = TGuiIconName(181);
    RICON_PRINTER                  = TGuiIconName(182);
    RICON_PHOTO_CAMERA             = TGuiIconName(183);
    RICON_PHOTO_CAMERA_FLASH       = TGuiIconName(184);
    RICON_HOUSE                    = TGuiIconName(185);
    RICON_HEART                    = TGuiIconName(186);
    RICON_CORNER                   = TGuiIconName(187);
    RICON_VERTICAL_BARS            = TGuiIconName(188);
    RICON_VERTICAL_BARS_FILL       = TGuiIconName(189);
    RICON_LIFE_BARS                = TGuiIconName(190);
    RICON_INFO                     = TGuiIconName(191);
    RICON_CROSSLINE                = TGuiIconName(192);
    RICON_HELP                     = TGuiIconName(193);
    RICON_FILETYPE_ALPHA           = TGuiIconName(194);
    RICON_FILETYPE_HOME            = TGuiIconName(195);
    RICON_LAYERS_VISIBLE           = TGuiIconName(196);
    RICON_LAYERS                   = TGuiIconName(197);
    RICON_WINDOW                   = TGuiIconName(198);
    RICON_HIDPI                    = TGuiIconName(199);
    ICON_FILETYPE_BINARY           = TGuiIconName(200);
    ICON_HEX                       = TGuiIconName(201);
    ICON_SHIELD                    = TGuiIconName(202);
    ICON_FILE_NEW                  = TGuiIconName(203);
    ICON_FOLDER_ADD                = TGuiIconName(204);
    ICON_ALARM                     = TGuiIconName(205);
    ICON_CPU                       = TGuiIconName(206);
    ICON_ROM                       = TGuiIconName(207);
    ICON_STEP_OVER                 = TGuiIconName(208);
    ICON_STEP_INTO                 = TGuiIconName(209);
    ICON_STEP_OUT                  = TGuiIconName(210);
    ICON_RESTART                   = TGuiIconName(211);
    ICON_BREAKPOINT_ON             = TGuiIconName(212);
    ICON_BREAKPOINT_OFF            = TGuiIconName(213);
    ICON_BURGER_MENU               = TGuiIconName(214);
    ICON_CASE_SENSITIVE            = TGuiIconName(215);
    ICON_REG_EXP                   = TGuiIconName(216);
    ICON_FOLDER                    = TGuiIconName(217);
    ICON_FILE                      = TGuiIconName(218);
    ICON_SAND_TIMER                = TGuiIconName(219);
    RICON_220                      = TGuiIconName(220);
    RICON_221                      = TGuiIconName(221);
    RICON_222                      = TGuiIconName(222);
    RICON_223                      = TGuiIconName(223);
    RICON_224                      = TGuiIconName(224);
    RICON_225                      = TGuiIconName(225);
    RICON_226                      = TGuiIconName(226);
    RICON_227                      = TGuiIconName(227);
    RICON_228                      = TGuiIconName(228);
    RICON_229                      = TGuiIconName(229);
    RICON_230                      = TGuiIconName(230);
    RICON_231                      = TGuiIconName(231);
    RICON_232                      = TGuiIconName(232);
    RICON_233                      = TGuiIconName(233);
    RICON_234                      = TGuiIconName(234);
    RICON_235                      = TGuiIconName(235);
    RICON_236                      = TGuiIconName(236);
    RICON_237                      = TGuiIconName(237);
    RICON_238                      = TGuiIconName(238);
    RICON_239                      = TGuiIconName(239);
    RICON_240                      = TGuiIconName(240);
    RICON_241                      = TGuiIconName(241);
    RICON_242                      = TGuiIconName(242);
    RICON_243                      = TGuiIconName(243);
    RICON_244                      = TGuiIconName(244);
    RICON_245                      = TGuiIconName(245);
    RICON_246                      = TGuiIconName(246);
    RICON_247                      = TGuiIconName(247);
    RICON_248                      = TGuiIconName(248);
    RICON_249                      = TGuiIconName(249);
    RICON_250                      = TGuiIconName(250);
    RICON_251                      = TGuiIconName(251);
    RICON_252                      = TGuiIconName(252);
    RICON_253                      = TGuiIconName(253);
    RICON_254                      = TGuiIconName(254);
    RICON_255                      = TGuiIconName(255);

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
      STATE_NORMAL   = TGuiControlState(0);
      STATE_FOCUSED  = TGuiControlState(1);
      STATE_PRESSED  = TGuiControlState(2);
      STATE_DISABLED = TGuiControlState(3);

type
  // Gui control text alignment
  PGuiTextAlignment = ^TGuiTextAlignment;
  TGuiTextAlignment = Integer;
    const
      TEXT_ALIGN_LEFT   = TGuiTextAlignment(0);
      TEXT_ALIGN_CENTER = TGuiTextAlignment(1);
      TEXT_ALIGN_RIGHT  = TGuiTextAlignment(2);

type
  // Gui controls
  PGuiControl = ^TGuiControl;
  TGuiControl = Integer;
    const
      DEFAULT       = TGuiControl(0);  // Generic control -> populates to all controls when set
      // Basic controls
      LABELs        = TGuiControl(1);  // Used also for: LABELBUTTON
      BUTTON        = TGuiControl(2);
      TOGGLE        = TGuiControl(3);  // Used also for: TOGGLEGROUP
      SLIDER        = TGuiControl(4);  // Used also for: SLIDERBAR
      PROGRESSBAR   = TGuiControl(5);
      CHECKBOX      = TGuiControl(6);
      COMBOBOX      = TGuiControl(7);
      DROPDOWNBOX   = TGuiControl(8);
      TEXTBOX       = TGuiControl(9);  // Used also for: TEXTBOXMULTI
      VALUEBOX      = TGuiControl(10);
      SPINNER       = TGuiControl(11);
      LISTVIEW      = TGuiControl(12);
      COLORPICKER   = TGuiControl(13);
      SCROLLBAR     = TGuiControl(14);
      STATUSBAR     = TGuiControl(15);

type
  // Gui base properties for every control
  // NOTE: RAYGUI_MAX_PROPS_BASE properties (by default 16 properties)
  PGuiControlProperty = ^TGuiControlProperty;
  TGuiControlProperty = Integer;
    const
      BORDER_COLOR_NORMAL     = TGuiControlProperty(0);
      BASE_COLOR_NORMAL       = TGuiControlProperty(1);
      TEXT_COLOR_NORMAL       = TGuiControlProperty(2);
      BORDER_COLOR_FOCUSED    = TGuiControlProperty(3);
      BASE_COLOR_FOCUSED      = TGuiControlProperty(4);
      TEXT_COLOR_FOCUSED      = TGuiControlProperty(5);
      BORDER_COLOR_PRESSED    = TGuiControlProperty(6);
      BASE_COLOR_PRESSED      = TGuiControlProperty(7);
      TEXT_COLOR_PRESSED      = TGuiControlProperty(8);
      BORDER_COLOR_DISABLED   = TGuiControlProperty(9);
      BASE_COLOR_DISABLED     = TGuiControlProperty(10);
      TEXT_COLOR_DISABLED     = TGuiControlProperty(11);
      BORDER_WIDTH            = TGuiControlProperty(12);
      TEXT_PADDING            = TGuiControlProperty(13);
      TEXT_ALIGNMENT          = TGuiControlProperty(14);
      RESERVED                = TGuiControlProperty(16);

type
  // DEFAULT extended properties
  // NOTE: Those properties are actually common to all controls
  PGuiDefaultProperty = ^TGuiDefaultProperty;
  TGuiDefaultProperty = Integer;
    const
      TEXT_SIZE        = TGuiDefaultProperty(16);
      TEXT_SPACING     = TGuiDefaultProperty(17);
      LINE_COLOR       = TGuiDefaultProperty(18);
      BACKGROUND_COLOR = TGuiDefaultProperty(19);

type
  // Toggle/ToggleGroup
  PGuiToggleProperty = ^TGuiToggleProperty;
  TGuiToggleProperty = Integer;
    const
      GROUP_PADDING = TGuiToggleProperty(16);

type
  // Slider/SliderBar
  PGuiSliderProperty = ^TGuiSliderProperty;
  TGuiSliderProperty = Integer;
    const
      SLIDER_WIDTH   = TGuiSliderProperty(16);
      SLIDER_PADDING = TGuiSliderProperty(17);

type
  // ProgressBar
  PGuiProgressBarProperty = ^TGuiProgressBarProperty;
  TGuiProgressBarProperty = Integer;
    const
      PROGRESS_PADDING = TGuiProgressBarProperty(16);

type
  // ScrollBar
  PGuiScrollBarProperty = ^TGuiScrollBarProperty;
  TGuiScrollBarProperty = Integer;
    const
      ARROWS_SIZE           = TGuiScrollBarProperty(16);
      ARROWS_VISIBLE        = TGuiScrollBarProperty(17);
      SCROLL_SLIDER_PADDING = TGuiScrollBarProperty(18);
      SCROLL_SLIDER_SIZE    = TGuiScrollBarProperty(19);
      SCROLL_PADDING        = TGuiScrollBarProperty(20);
      SCROLL_SPEED          = TGuiScrollBarProperty(21);

type
  // CheckBox
  PGuiCheckBoxProperty = ^TGuiCheckBoxProperty;
  TGuiCheckBoxProperty = Integer;
    const
      CHECK_PADDING = TGuiCheckBoxProperty(16);

type
  PGuiComboBoxProperty = ^TGuiComboBoxProperty;
  TGuiComboBoxProperty = Integer;
    const
      COMBO_BUTTON_WIDTH   = TGuiComboBoxProperty(16); // ComboBox right button width
      COMBO_BUTTON_SPACING = TGuiComboBoxProperty(17); // ComboBox button separation

type
  // DropdownBox
  PGuiDropdownBoxProperty = ^TGuiDropdownBoxProperty;
  TGuiDropdownBoxProperty = Integer;
    const
      ARROW_PADDING          = TGuiDropdownBoxProperty(16);
      DROPDOWN_ITEMS_SPACING = TGuiDropdownBoxProperty(17);

type
  // TextBox/TextBoxMulti/ValueBox/Spinner
  PGuiTextBoxProperty = ^TGuiTextBoxProperty;
  TGuiTextBoxProperty = Integer;
    const
      TEXT_INNER_PADDING = TGuiTextBoxProperty(16);
      TEXT_LINES_SPACING = TGuiTextBoxProperty(17);
      TEXT_ALIGNMENT_VERTICAL = TGuiTextBoxProperty(18);    // TextBoxMulti vertical alignment: 0-CENTERED, 1-UP, 2-DOWN
      TEXT_MULTILINE = TGuiTextBoxProperty(19);              // TextBox supports multiple lines
      TEXT_WRAP_MODE = TGuiTextBoxProperty(20);             // TextBox wrap mode for multiline: 0-NO_WRAP, 1-CHAR_WRAP, 2-WORD_WRAP

type
  // Spinner
  PGuiSpinnerProperty = ^TGuiSpinnerProperty;
  TGuiSpinnerProperty = Integer;
    const
      SPIN_BUTTON_WIDTH   = TGuiSpinnerProperty(16);
      SPIN_BUTTON_SPACING = TGuiSpinnerProperty(17);

type
  // ListView
  PGuiListViewProperty = ^TGuiListViewProperty;
  TGuiListViewProperty = Integer;
    const
      LIST_ITEMS_HEIGHT   = TGuiListViewProperty(16);
      LIST_ITEMS_SPACING  = TGuiListViewProperty(17);
      SCROLLBAR_WIDTH     = TGuiListViewProperty(18);
      SCROLLBAR_SIDE      = TGuiListViewProperty(19);

type
  // ColorPicker
  PGuiColorPickerProperty = ^TGuiColorPickerProperty;
  TGuiColorPickerProperty = Integer;
    const
      COLOR_SELECTOR_SIZE      = TGuiColorPickerProperty(16);
      HUEBAR_WIDTH             = TGuiColorPickerProperty(17); // Right hue bar width
      HUEBAR_PADDING           = TGuiColorPickerProperty(18); // Right hue bar separation from panel
      HUEBAR_SELECTOR_HEIGHT   = TGuiColorPickerProperty(19); // Right hue bar selector height
      HUEBAR_SELECTOR_OVERFLOW = TGuiColorPickerProperty(20); // Right hue bar selector overflow

    const
      SCROLLBAR_LEFT_SIDE   = 0;
      SCROLLBAR_RIGHT_SIDE  = 1;

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

(*Global gui state control functions*)

{Enable gui controls (global state)}
procedure GuiEnable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnable';
{Disable gui controls (global state)}
procedure GuiDisable; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisable';
{Lock gui controls (global state)}
procedure GuiLock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLock';
{Unlock gui controls (global state)}
procedure GuiUnlock; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiUnlock';
{Check if gui is locked (global state)}
function  GuiIsLocked: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIsLocked';
{Set gui controls alpha (global state), alpha goes from 0.0f to 1.0f}
procedure GuiFade(alpha: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiFade';
{Set gui state (global state)}
procedure GuiSetState(state: TGuiControlState); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetState';
{Get gui state (global state)}
function GuiGetState: TGuiControlState; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetState';

(*Font set/get functions*)

{Set gui custom font (global state)}
procedure GuiSetFont(font: TFont); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetFont';
{Get gui custom font (global state)}
function GuiGetFont:TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetFont';

(*Style set/get functions*)

{Set one style property}
procedure GuiSetStyle(control: TGuiControl; property_: Integer; value: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetStyle';
{Get one style property}
function GuiGetStyle(control: TGuiControl; property_: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetStyle';

(*Container/separator controls, useful for controls organization*)

{Window Box control, shows a window that can be closed}
function GuiWindowBox(bounds: TRectangle; const title: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiWindowBox';
{Group Box control with text name}
procedure GuiGroupBox(bounds: TRectangle; const text:PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGroupBox';
{Line separator control, could contain text}
procedure GuiLine(bounds: TRectangle; const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLine';
{Panel control, useful to group controls}
procedure GuiPanel(bounds: TRectangle; const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiPanel';
{Tab Bar control, returns TAB to be closed or -1}
function GuiTabBar(bounds: TRectangle; const text: PPChar; count: Integer; active: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTabBar';
{Scroll Panel control}
function GuiScrollPanel(bounds: TRectangle; const text: PChar; content: TRectangle; scroll: PVector2): TRectangle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiScrollPanel';

(*Basic controls set*)

{Label control, shows text}
procedure GuiLabel(bounds: TRectangle; const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabel';
{Button control, returns true when clicked}
function GuiButton(bounds: TRectangle; const text: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiButton';
{Label button control, show true when clicked}
function GuiLabelButton(bounds: TRectangle; const text: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLabelButton';
{Toggle Button control, returns true when active}
function GuiToggle(bounds: TRectangle; const text: PChar; active: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggle';
{Toggle Group control, returns active toggle index}
function GuiToggleGroup(bounds: TRectangle; const text: PChar; active: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiToggleGroup';
{Check Box control, returns true when active}
function GuiCheckBox(bounds: TRectangle; const text: PChar; checked: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiCheckBox';
{Combo Box control, returns selected item index}
function GuiComboBox(bounds: TRectangle; const text: PChar; active: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiComboBox';
{Dropdown Box control, returns selected item}
function GuiDropdownBox(bounds: TRectangle; const text: PChar; active: PInteger; editMode: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDropdownBox';
{Spinner control, returns selected value}
function GuiSpinner(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSpinner';
{Value Box control, updates input text with numbers}
function GuiValueBox(bounds: TRectangle; const text: PChar; value: PInteger; minValue, maxValue: Integer; editMode: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiValueBox';
{Text Box control, updates input text}
function GuiTextBox(bounds: TRectangle; text: PChar; textSize: Integer; editMode: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextBox';
{Slider control, returns selected value}
function GuiSlider(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSlider';
{Slider Bar control, returns selected value}
function GuiSliderBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSliderBar';
{Progress Bar control, shows current progress value}
function GuiProgressBar(bounds: TRectangle; const textLeft: PChar; const textRight: PChar; value, minValue, maxValue: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiProgressBar';
{Status Bar control, shows info text}
procedure GuiStatusBar(bounds: TRectangle; const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiStatusBar';
{Dummy control for placeholders}
procedure GuiDummyRec(bounds: TRectangle; const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDummyRec';
{Scroll Bar control}
function GuiScrollBar(bounds: TRectangle; value, minValue, maxValue: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiScrollBar';
{Grid control}
function GuiGrid(bounds: TRectangle; const text: PChar; spacing:Single; subdivs: Integer): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGrid';

(*Advance controls set*)

{List View control, returns selected list item index}
function GuiListView(bounds: TRectangle; const text: PChar; scrollIndex: PInteger; active: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListView';
{List View with extended parameters}
function GuiListViewEx(bounds: TRectangle; const text: PPChar; count: Integer; focus: PInteger; scrollIndex: PInteger; active: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiListViewEx';
{Message Box control, displays a message}
function GuiMessageBox(bounds: TRectangle; const title, message, buttons: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiMessageBox';
{Text Input Box control, ask for text}
function GuiTextInputBox(bounds: TRectangle; const title, message, buttons, text: PChar; textMaxSize: Integer; secretViewActive: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiTextInputBox';
{Color Picker control (multiple color controls)}
function GuiColorPicker(bounds: TRectangle; const text: PChar; color: TColorB): TColorB ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPicker';
{Color Panel control}
function GuiColorPanel(bounds: TRectangle; const text: PChar; color: TColorB): TColorB ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorPanel';
{Color Bar Alpha control}
function GuiColorBarAlpha(bounds: TRectangle; const text: PChar; alpha: Single): Single ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarAlpha';
{Color Bar Hue control}
function GuiColorBarHue(bounds:TRectangle; const text: PChar; value: Single): Single ; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiColorBarHue';

(* Styles loading functions *)
{Load style file over global style variable (.rgs)}
procedure GuiLoadStyle(const fileName: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyle';
{Load style default over global style }
procedure GuiLoadStyleDefault; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadStyleDefault';

(* Tooltips management functions *)
{Enable gui tooltips (global state)}
procedure GuiEnableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiEnableTooltip';
{Disable gui tooltips (global state)}
procedure GuiDisableTooltip; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDisableTooltip';
{Set tooltip string}
procedure GuiSetTooltip(const tooltip: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiSetTooltip';

(* Icons functionality *)
{Get text with icon id prepended (if supported)}
function GuiIconText(iconId: TGuiIconName; const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiIconText';

{$IFDEF RAYGUI_NO_RICONS}
{Set icon drawing size}
procedure GuiSetIconScale(scale: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetIcons';
{Get full icons data pointer}
function GuiGetIcons: Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiGetIcons';
{Load raygui icons file (.rgi) into internal icons data}
function GuiLoadIcons(const fileName: PChar; loadIconsName: Boolean): PPChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiLoadIcons';
{Gui icons functionality}
procedure GuiDrawIcon(iconId, posX, posY, pixelSize: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GuiDrawIcon';


{$ENDIF}

implementation

end.

