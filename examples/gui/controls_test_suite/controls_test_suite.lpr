program raygui_controls_test;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  raylib, raygui;

const
  screenWidth  = 960;
  screenHeight = 580;

  STYLE_PATH = 'resources/gui_styles/';

var
  tabActive: Integer = 0;

  dropdownBox000Active: Integer = 0;
  dropDown000EditMode: Boolean = False;

  dropdownBox001Active: Integer = 0;
  dropDown001EditMode: Boolean = False;

  spinner001Value: Integer = 0;
  spinnerEditMode: Boolean = False;

  valueBox002Value: Integer = 0;
  valueBoxEditMode: Boolean = False;

  textBoxText: array[0..63] of Char = 'Text box';
  textBoxEditMode: Boolean = False;

  textBoxMultiText: array[0..1023] of Char =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ' +
    'tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, ' +
    'quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.' + #10#10 +
    'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu ' +
    'fugiat nulla pariatur.' + #10#10 +
    'Thisisastringlongerthanexpectedwithoutspacestotestcharbreaksforthosecases,checkingifworkingasexpected.' + #10#10 +
    'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt ' +
    'mollit anim id est laborum.';
  textBoxMultiEditMode: Boolean = False;

  listViewScrollIndex: Integer = 0;
  listViewActive: Integer = -1;

  listViewExScrollIndex: Integer = 0;
  listViewExActive: Integer = 2;
  listViewExFocus: Integer = -1;

  listViewExList: array[0..7] of PAnsiChar = (
    'This', 'is', 'a', 'list view', 'with', 'disable', 'elements', 'amazing!'
  );

  colorPickerValue: TColor;

  sliderValue: Single = 50.0;
  sliderBarValue: Single = 60.0;
  progressValue: Single = 0.1;

  forceSquaredChecked: Boolean = False;

  alphaValue: Single = 0.5;

  visualStyleActive: Integer = 0;
  prevVisualStyleActive: Integer = 0;

  toggleGroupActive: Integer = 0;
  toggleSliderActive: Integer = 0;

  viewScroll: TVector2;
  view: TRectangle;

  exitWindow: Boolean = False;
  showMessageBox: Boolean = False;

  textInput: array[0..255] of Char;
  textInputFileName: array[0..255] of Char;
  showTextInputBox: Boolean = False;

  alpha: Single = 1.0;

  frameCounter: Integer = 0;

  droppedFiles: TFilePathList;
  btnActive: Integer;
  mouseCell: TVector2;

// Вспомогательная функция: приводим Integer-результат raygui к Boolean
function GuiClicked(Result_: Integer): Boolean; inline;
begin
  GuiClicked := Result_ <> 0;
end;

// Вспомогательная функция: полный путь к .rgs-стилю
function StylePath(const FileName: string): PAnsiChar;
var
  full: string;
begin
  full := GetApplicationDirectory() + STYLE_PATH + FileName;
  StylePath := PAnsiChar(full);
end;

begin
  InitWindow(screenWidth, screenHeight, 'raygui - controls test suite');
  SetExitKey(0);

  colorPickerValue := RED;
  viewScroll.Create(0, 0);
  view.Create(0, 0, 0, 0);
  mouseCell.Create(0, 0);

  FillChar(textInput, SizeOf(textInput), 0);
  FillChar(textInputFileName, SizeOf(textInputFileName), 0);

  SetTargetFPS(60);

  // Main game loop
  while not exitWindow do
  begin
    // Update
    exitWindow := WindowShouldClose();

    if IsKeyPressed(KEY_ESCAPE) then
      showMessageBox := not showMessageBox;

    if IsKeyDown(KEY_LEFT_CONTROL) and IsKeyPressed(KEY_S) then
      showTextInputBox := True;

    if IsFileDropped() then
    begin
      droppedFiles := LoadDroppedFiles();

      if (droppedFiles.count > 0) and
         IsFileExtension(droppedFiles.paths[0], '.rgs') then
        GuiLoadStyle(droppedFiles.paths[0]);

      UnloadDroppedFiles(droppedFiles);
    end;

    if alpha < 0.0 then alpha := 0.0;
    if IsKeyPressed(KEY_SPACE) then alpha := 1.0;

    GuiSetAlpha(alpha);

    if IsKeyPressed(KEY_LEFT) then
      progressValue := progressValue - 0.1
    else if IsKeyPressed(KEY_RIGHT) then
      progressValue := progressValue + 0.1;
    if progressValue > 1.0 then progressValue := 1.0
    else if progressValue < 0.0 then progressValue := 0.0;

    if visualStyleActive <> prevVisualStyleActive then
    begin
      GuiLoadStyleDefault();

      case visualStyleActive of
        0:  ; // Default style
        1:  GuiLoadStyle(StylePath('style_jungle.rgs'));
        2:  GuiLoadStyle(StylePath('style_lavanda.rgs'));
        3:  GuiLoadStyle(StylePath('style_dark.rgs'));
        4:  GuiLoadStyle(StylePath('style_bluish.rgs'));
        5:  GuiLoadStyle(StylePath('style_cyber.rgs'));
        6:  GuiLoadStyle(StylePath('style_terminal.rgs'));
        7:  GuiLoadStyle(StylePath('style_candy.rgs'));
      end;

      GuiSetStyle(LABEL_, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
      prevVisualStyleActive := visualStyleActive;
    end;

    // Draw
    BeginDrawing();

    ClearBackground(GetColor(GuiGetStyle(DEFAULT, BACKGROUND_COLOR)));

    // Check all possible events that require GuiLock
    if dropDown000EditMode or dropDown001EditMode then GuiLock();
    if showTextInputBox then GuiLock();

    GuiSetStyle(TABBAR, TAB_ITEMS_WIDTH, 140);
    GuiTabBar(RectangleCreate(0, 8, GetScreenWidth(), 26),
      '#176#TAB_01;#177#TAB_02;#178#TAB_03;#179#TAB_04;#180#TAB_05;#181#TAB_06',
      nil, @tabActive);

    // First GUI column
    GuiCheckBox(RectangleCreate(25, 108 + 20, 15, 15), 'FORCE CHECK!', @forceSquaredChecked);

    GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);
    if GuiClicked(GuiSpinner(RectangleCreate(25, 135 + 20, 125, 30), nil, @spinner001Value, 0, 100, spinnerEditMode)) then
      spinnerEditMode := not spinnerEditMode;
    if GuiClicked(GuiValueBox(RectangleCreate(25, 175 + 20, 125, 30), nil, @valueBox002Value, 0, 100, valueBoxEditMode)) then
      valueBoxEditMode := not valueBoxEditMode;
    GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
    if GuiClicked(GuiTextBox(RectangleCreate(25, 215 + 20, 125, 30), textBoxText, 64, textBoxEditMode)) then
      textBoxEditMode := not textBoxEditMode;

    GuiSetStyle(BUTTON, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

    if GuiClicked(GuiButton(RectangleCreate(25, 255 + 20, 125, 30), GuiIconText(ICON_FILE_SAVE, 'Save File'))) then
      showTextInputBox := True;

    GuiGroupBox(RectangleCreate(25, 310 + 20, 125, 150), 'STATES');
    GuiSetState(STATE_NORMAL);   GuiButton(RectangleCreate(30, 320 + 20, 115, 30), 'NORMAL');
    GuiSetState(STATE_FOCUSED);  GuiButton(RectangleCreate(30, 355 + 20, 115, 30), 'FOCUSED');
    GuiSetState(STATE_PRESSED);  GuiButton(RectangleCreate(30, 390 + 20, 115, 30), '#15#PRESSED');
    GuiSetState(STATE_DISABLED); GuiButton(RectangleCreate(30, 425 + 20, 115, 30), 'DISABLED');
    GuiSetState(STATE_NORMAL);

    GuiSetStyle(COMBOBOX, COMBO_BUTTON_WIDTH, 40);
    GuiComboBox(RectangleCreate(25, 480 + 20, 125, 30),
      'default;Jungle;Lavanda;Dark;Bluish;Cyber;Terminal;Candy',
      @visualStyleActive);

    if dropDown000EditMode or dropDown001EditMode then GuiUnlock();
    if showTextInputBox then GuiLock();

    GuiSetStyle(DROPDOWNBOX, TEXT_PADDING, 4);
    GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
    if GuiClicked(GuiDropdownBox(RectangleCreate(25, 65 + 20, 125, 30),
      '#01#ONE;#02#TWO;#03#THREE;#04#FOUR',
      @dropdownBox001Active, dropDown001EditMode)) then
      dropDown001EditMode := not dropDown001EditMode;
    GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);
    GuiSetStyle(DROPDOWNBOX, TEXT_PADDING, 0);

    if GuiClicked(GuiDropdownBox(RectangleCreate(25, 25 + 20, 125, 30),
      '#27#ONE;#28#TWO;#29#THREE',
      @dropdownBox000Active, dropDown000EditMode)) then
      dropDown000EditMode := not dropDown000EditMode;

    // Second GUI column
    GuiListView(RectangleCreate(165, 25 + 20, 140, 124),
      'Charmander;Bulbasaur;#18#Squirtle;Pikachu;Eevee;Pidgey',
      @listViewScrollIndex, @listViewActive);
    GuiListViewEx(RectangleCreate(165, 162 + 20, 140, 184),
      @listViewExList[0], 8,
      @listViewExScrollIndex, @listViewExActive, @listViewExFocus);
    GuiSetStyle(LISTVIEW, LIST_ITEMS_BORDER_NORMAL, 0);

    GuiToggleGroup(RectangleCreate(165, 360 + 20, 140, 24),
      '#1#ONE'#10'#3#TWO'#10'#8#THREE'#10'#23#',
      @toggleGroupActive);
    GuiSetStyle(SLIDER, SLIDER_PADDING, 2);
    GuiToggleSlider(RectangleCreate(165, 480 + 20, 140, 30),
      'ON;OFF', @toggleSliderActive);
    GuiSetStyle(SLIDER, SLIDER_PADDING, 0);

    // Third GUI column
    GuiPanel(RectangleCreate(320, 25 + 20, 225, 140), 'Panel Info');
    GuiColorPicker(RectangleCreate(320, 185 + 20, 196, 192), nil, @colorPickerValue);

    GuiSlider(RectangleCreate(355, 400 + 20, 165, 20), 'TEST',
      TextFormat('%2.2f', sliderValue), @sliderValue, -50, 100);
    GuiSliderBar(RectangleCreate(320, 430 + 20, 200, 20), nil,
      TextFormat('%i', Trunc(sliderBarValue)), @sliderBarValue, 0, 100);

    GuiProgressBar(RectangleCreate(320, 460 + 20, 200, 20), nil,
      TextFormat('%i%%', Trunc(progressValue * 100)),
      @progressValue, 0.0, 1.0);
    GuiEnable();

    GuiScrollPanel(RectangleCreate(560, 25 + 20, 102, 354), nil,
      RectangleCreate(560, 25, 300, 1200), @viewScroll, @view);

    GuiGrid(RectangleCreate(560, 25 + 180 + 195 + 20, 100, 120), nil,
      20, 3, @mouseCell);

    GuiColorBarAlpha(RectangleCreate(320, 490 + 20, 200, 30), nil, @alphaValue);

    GuiSetStyle(DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_TOP);
    GuiSetStyle(DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_WORD);
    if GuiClicked(GuiTextBox(RectangleCreate(678, 25 + 20, 258, 492), textBoxMultiText, 1024, textBoxMultiEditMode)) then
      textBoxMultiEditMode := not textBoxMultiEditMode;
    GuiSetStyle(DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_NONE);
    GuiSetStyle(DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_MIDDLE);

    GuiSetStyle(DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
    GuiStatusBar(RectangleCreate(0, GetScreenHeight() - 20, GetScreenWidth(), 20),
      'This is a status bar');
    GuiSetStyle(DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

    if showMessageBox then
    begin
      DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
      btnActive := -1;
      GuiMessageBox(
        RectangleCreate(GetScreenWidth() / 2 - 125, GetScreenHeight() / 2 - 50, 250, 100),
        GuiIconText(ICON_EXIT, 'Close Window'),
        'Do you really want to exit?', 'Yes;No', @btnActive);

      if (btnActive = 0) or (btnActive = 2) then
        showMessageBox := False
      else if btnActive = 1 then
        exitWindow := True;
    end;

    if showTextInputBox then
    begin
      GuiUnlock();

      DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
      btnActive := -1;
      GuiTextInputBox(
        RectangleCreate(GetScreenWidth() / 2 - 120, GetScreenHeight() / 2 - 60, 240, 140),
        GuiIconText(ICON_FILE_SAVE, 'Save file as...'),
        'Introduce output file name:', textInput, 255, 'Ok;Cancel',
        @btnActive, nil);

      if btnActive = 1 then
      begin
        // TODO: Validate textInput value and save
        TextCopy(textInputFileName, textInput);
      end;

      if (btnActive = 0) or (btnActive = 1) or (btnActive = 2) then
      begin
        showTextInputBox := False;
        TextCopy(textInput, '');
      end;
    end;

    EndDrawing();
  end;

  CloseWindow();
end.
