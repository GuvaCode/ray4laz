{*******************************************************************************************
*
*   raygui - controls test suite
*
*   TEST CONTROLS:
*       - GuiDropdownBox()
*       - GuiCheckBox()
*       - GuiSpinner()
*       - GuiValueBox()
*       - GuiTextBox()
*       - GuiButton()
*       - GuiComboBox()
*       - GuiListView()
*       - GuiToggleGroup()
*       - GuiColorPicker()
*       - GuiSlider()
*       - GuiSliderBar()
*       - GuiProgressBar()
*       - GuiColorBarAlpha()
*       - GuiScrollPanel()
*
*
*   DEPENDENCIES:
*       raylib 4.5          - Windowing/input management and drawing
*       raygui 3.5          - Immediate-mode GUI controls with custom styling and icons
*
*   COMPILATION (Windows - MinGW):
*       gcc -o $(NAME_PART).exe $(FILE_NAME) -I../../src -lraylib -lopengl32 -lgdi32 -std=c99
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2016-2024 Ramon Santamaria (@raysan5)
*
*   Pascal translation (c) 2024 Vadim Gunko (@guvacode)
*
**********************************************************************************************}
program controls_test_suite;

{$mode objfpc}{$H+}

uses cmem, raygui, raylib;

const
  screenWidth = 960;
  screenHeight = 560;

var
  dropdownBox000Active: integer = 0;
  dropDown000EditMode: boolean = false;

  dropdownBox001Active: integer = 0;
  dropDown001EditMode: boolean = false;

  spinner001Value: integer = 0;
  spinnerEditMode: boolean = false;

  valueBox002Value: integer = 0;
  valueBoxEditMode: boolean = false;

  textBoxText: Char;// = 'Text box';
  textBoxEditMode: boolean = false;

  textBoxMultiText:  PChar;

  textBoxMultiEditMode: boolean = false;

  listViewScrollIndex: integer = 0;
  listViewActive: integer = -1;

  listViewExScrollIndex: integer = 0;
  listViewExActive: integer = 2;
  listViewExFocus: integer = -1;

  const listViewExList: array [0..7] of PChar = ('This', 'is', 'a', 'list view', 'with', 'disable', 'elements', 'amazing!');

  var
  colorPickerValue: TColorB = (r: 230; g: 41; b: 55; a: 255);
  sliderValue: single = 50.0;
  sliderBarValue: single = 60;
  progressValue: single = 0.1;
  forceSquaredChecked: boolean = false;
  alphaValue: single = 0.5;
  visualStyleActive: integer = 0;
  prevVisualStyleActive: integer = 0;

  toggleGroupActive: integer = 0;
  toggleSliderActive: integer = 0;

  viewScroll: TVector2;

  exitWindow: boolean = false;
  showMessageBox: boolean = false;

  textInput: Char;
  {%H-}textInputFileName: PChar;
  showTextInputBox: boolean = false;

  alpha: single = 1.0;

  droppedFiles: TFilePathList;
  view : TRectangle;
  mouseCell: TVector2;

  ExResult, BoxResult: integer;



begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raygui - controls test suite');
  SetExitKey(0);
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  textBoxMultiText := 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.' +#10+#10+
                      'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.' + #10 + #10 +
                      'Thisisastringlongerthanexpectedwithoutspacestotestcharbreaksforthosecases,checkingifworkingasexpected.' + #10 +#10 +
                      'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';


  //--------------------------------------------------------------------------------------
  // Main game loop
  while not exitWindow do
    begin
      // Update
      exitWindow := WindowShouldClose();

      if IsKeyPressed(KEY_ESCAPE) then showMessageBox := not showMessageBox;

      if (IsKeyDown(KEY_LEFT_CONTROL) and IsKeyPressed(KEY_S))  then showTextInputBox := true;

      if (IsFileDropped()) then
      begin
        droppedFiles := LoadDroppedFiles();
        if ((droppedFiles.count > 0) and IsFileExtension(droppedFiles.paths[0], '.rgs')) then
        GuiLoadStyle(droppedFiles.paths[0]);
        UnloadDroppedFiles(droppedFiles);    // Clear internal buffers
      end;

      if (alpha < 0.0) then alpha := 0.0;
      if (IsKeyPressed(KEY_SPACE)) then alpha := 1.0;
      GuiSetAlpha(alpha);

      if (IsKeyPressed(KEY_LEFT)) then progressValue -= 0.1
      else if (IsKeyPressed(KEY_RIGHT)) then progressValue += 0.1;

      if (progressValue > 1.0) then progressValue := 1.0
      else if (progressValue < 0.0) then progressValue := 0.0;

      if visualStyleActive <> prevVisualStyleActive then
      begin
        case visualStyleActive of
          0: GuiLoadStyleDefault;      // Default style
          1: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_jungle.rgs'));
          2: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_lavanda.rgs'));
          3: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_dark.rgs'));
          4: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_bluish.rgs'));
          5: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_cyber.rgs'));
          6: GuiLoadStyle(PChar(GetApplicationDirectory + 'resources/gui_styles/style_terminal.rgs'));
        end;
          GuiSetStyle(LABELS, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
          prevVisualStyleActive := visualStyleActive;
      end;
      // Draw
      //------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(GetColor(GuiGetStyle(DEFAULT, BACKGROUND_COLOR)));
        //raygui: controls drawing ---------------------------------------------
        // Check all possible events that require GuiLock
        if (dropDown000EditMode or dropDown001EditMode) then GuiLock();

        // First GUI column
        GuiCheckBox(RectangleCreate( 25, 108, 15, 15 ), 'FORCE CHECK!', @forceSquaredChecked);
        GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

        if GuiSpinner(RectangleCreate( 25, 135, 125, 30 ), nil, @spinner001Value, 0, 100, spinnerEditMode) <> 0
        then spinnerEditMode := not spinnerEditMode;

        if GuiValueBox(RectangleCreate( 25, 175, 125, 30 ), nil, @valueBox002Value, 0, 100, valueBoxEditMode) <> 0
        then valueBoxEditMode := not valueBoxEditMode;
        GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);

        if GuiTextBox(RectangleCreate( 25, 215, 125, 30 ), @textBoxText, 64, textBoxEditMode) <> 0
        then textBoxEditMode :=  not textBoxEditMode;
        GuiSetStyle(BUTTON, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

        if GuiButton(RectangleCreate( 25, 255, 125, 30 ), GuiIconText(ICON_FILE_SAVE, 'Save File')) <> 0
        then showTextInputBox := true;

        GuiGroupBox(RectangleCreate( 25, 310, 125, 150 ), 'STATES');

        GuiSetState(STATE_NORMAL);
        GuiButton(RectangleCreate( 30, 320, 115, 30 ), 'NORMAL');

        GuiSetState(STATE_FOCUSED);
        GuiButton(RectangleCreate( 30, 355, 115, 30 ), 'FOCUSED');

        GuiSetState(STATE_PRESSED);
        GuiButton(RectangleCreate( 30, 390, 115, 30 ), '#15#PRESSED');

        GuiSetState(STATE_DISABLED);
        GuiButton(RectangleCreate( 30, 425, 115, 30 ), 'DISABLED');

        GuiSetState(STATE_NORMAL);


        GuiComboBox(RectangleCreate( 25, 480, 125, 30 ), 'default;Jungle;Lavanda;Dark;Bluish;Cyber;Terminal', @visualStyleActive);

        // NOTE: GuiDropdownBox must draw after any other control that can be covered on unfolding
        GuiUnlock();
        GuiSetStyle(DROPDOWNBOX, TEXT_PADDING, 4);
        GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);

        if  GuiDropdownBox(RectangleCreate( 25, 65, 125, 30 ), '#01#ONE;#02#TWO;#03#THREE;#04#FOUR',
        @dropdownBox001Active, dropDown001EditMode) <> 0 then dropDown001EditMode := not dropDown001EditMode;

        GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);
        GuiSetStyle(DROPDOWNBOX, TEXT_PADDING, 0);

        if GuiDropdownBox(RectangleCreate( 25, 25, 125, 30 ), 'ONE;TWO;THREE', @dropdownBox000Active, dropDown000EditMode) <> 0
        then dropDown000EditMode := not dropDown000EditMode;

        // Second GUI column
        GuiListView(RectangleCreate( 165, 25, 140, 124 ), 'Charmander;Bulbasaur;#18#Squirtel;Pikachu;Eevee;Pidgey',
        @listViewScrollIndex, @listViewActive);

        GuiListViewEx(RectangleCreate( 165, 162, 140, 184 ), listViewExList, 8, @listViewExScrollIndex, @listViewExActive, @listViewExFocus);

        GuiToggleGroup(RectangleCreate( 165, 360, 140, 24 ), Pchar('#1#ONE' + #10+ '#3#TWO' + #10 + '#8#THREE'), @toggleGroupActive);

        GuiSetStyle(SLIDER, SLIDER_PADDING, 2);
        GuiToggleSlider(RectangleCreate( 165, 480, 140, 30 ), 'ON;OFF', @toggleSliderActive);
        GuiSetStyle(SLIDER, SLIDER_PADDING, 0);

        // Third GUI column
         GuiPanel(RectangleCreate( 320, 25, 225, 140 ), 'Panel Info');
         GuiColorPicker(RectangleCreate( 320, 185, 196, 192 ), nil, @colorPickerValue);

         GuiSlider(RectangleCreate( 355, 400, 165, 20 ), 'TEST', TextFormat('%2.2f', sliderValue), @sliderValue, -50, 100);

         GuiSliderBar(RectangleCreate( 320, 430, 200, 20 ), nil, TextFormat('%i', Trunc(sliderBarValue)), @sliderBarValue, 0, 100);

         GuiProgressBar(RectangleCreate( 320, 460, 200, 20 ), nil, TextFormat('%i%%', Trunc(progressValue*200)), @progressValue, 0.0, 1.0);
         GuiEnable();

         // NOTE: View rectangle could be used to perform some scissor test
         GuiScrollPanel(RectangleCreate( 560, 25, 102, 354 ), nil, RectangleCreate( 560, 25, 300, 1200 ), @viewScroll, @view);

         GuiGrid(RectangleCreate ( 560, 25 + 180 + 195, 100, 120 ), nil, 20, 3, @mouseCell);

         GuiColorBarAlpha(RectangleCreate( 320, 490, 200, 30 ), nil, @alphaValue);


         GuiSetStyle(DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_TOP);   // WARNING: Word-wrap does not work as expected in case of no-top alignment
         GuiSetStyle(DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_WORD);            // WARNING: If wrap mode enabled, text editing is not supported
         if GuiTextBox(RectangleCreate( 678, 25, 258, 492 ), textBoxMultiText, 1024, textBoxMultiEditMode) <> 0
         then textBoxMultiEditMode := not textBoxMultiEditMode;
         GuiSetStyle(DEFAULT, TEXT_WRAP_MODE, TEXT_WRAP_NONE);
         GuiSetStyle(DEFAULT, TEXT_ALIGNMENT_VERTICAL, TEXT_ALIGN_MIDDLE);

         GuiSetStyle(DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
         GuiStatusBar(RectangleCreate( 0, GetScreenHeight() - 20, GetScreenWidth(), 20) , 'This is a status bar');
         GuiSetStyle(DEFAULT, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);

         if showMessageBox then
         begin
           DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
           ExResult := GuiMessageBox(RectangleCreate(GetScreenWidth()/2 - 125, GetScreenHeight()/2 - 50, 250, 100 ),
           GuiIconText(ICON_EXIT, 'Close Window'), 'Do you really want to exit?', 'Yes;No');

           if (ExResult = 0) or (ExResult = 2) then showMessageBox := false
           else if (ExResult = 1) then exitWindow := true;
         end;


         if (showTextInputBox) then
         begin
           DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
           BoxResult := GuiTextInputBox(RectangleCreate( GetScreenWidth()/2 - 120, GetScreenHeight()/2 - 60, 240, 140 ),
           GuiIconText(ICON_FILE_SAVE, 'Save file as...'), 'Introduce output file name:', 'Ok;Cancel', @textInput, 64, nil);

             if (BoxResult = 1) then
             begin
                 // TODO: Validate textInput value and save
                 TextCopy(@textInput, '@textInput');
             end;

             if (BoxResult = 0) or (BoxResult = 1) or (BoxResult = 2) then
             begin
                 showTextInputBox := false;
                 TextCopy(@textInput, 'text.txt');
             end;
         end;

      EndDrawing();



    end;
  // De-Initialization
  //----------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //----------------------------------------------------------------------------

end.

