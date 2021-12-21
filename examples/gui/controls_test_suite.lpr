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
*       - GuiTextBoxMulti()
*       - GuiColorPicker()
*       - GuiSlider()
*       - GuiSliderBar()
*       - GuiProgressBar()
*       - GuiColorBarAlpha()
*       - GuiScrollPanel()
*
*
*   DEPENDENCIES:
*       raylib 4.0 - Windowing/input management and drawing.
*       raygui 3.0 - Immediate-mode GUI controls.
*
*   COMPILATION (Windows - MinGW):
*       gcc -o $(NAME_PART).exe $(FILE_NAME) -I../../src -lraylib -lopengl32 -lgdi32 -std=c99
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2016-2021 Ramon Santamaria (@raysan5)
*   Pascal translation (c) 2021 Vadim Gunko (@guvacode)
*
**********************************************************************************************}
program controls_test_suite;

{$mode objfpc}{$H+}

uses raygui, raylib;

const
  screenWidth = 690;
  screenHeight = 560;

var
   //----------------------------------------------------------------------------------
    dropdownBox000Active: longint = 0;
    dropDown000EditMode: boolean = false;
    dropdownBox001Active: longint = 0;
    dropDown001EditMode: boolean = false;
    spinner001Value: longint = 0;
    spinnerEditMode: boolean = false;
    valueBox002Value: longint = 0;
    valueBoxEditMode: boolean = false;
    listViewScrollIndex: longint = 0;
    listViewActive: longint = -1;
    listViewExScrollIndex: longint = 0;
    listViewExActive: longint = 2;
    listViewExFocus: longint = -1;
    listViewExList: array [0..7] of PChar = ('This', 'is', 'a', 'list view', 'with', 'disable', 'elements', 'amazing!');
    multiTextBoxText: array [0..254] of Char = 'Multi text box';
    multiTextBoxEditMode: boolean = false;
    colorPickerValue: TColor = (r: 230; g: 41; b: 55; a: 255);    // Red// = RED;
    sliderValue: longint = 50;
    sliderBarValue: longint = 60;
    progressValue: single = 0.4;
    forceSquaredChecked: boolean = false;
    alphaValue: single = 0.5;
    comboBoxActive: longint = 1;
    toggleGroupActive: longint = 0;
    viewScroll: TVector2;
    exitWindow, showMessageBox, showTextInputBox: boolean;
    textInput: array [0..254] of Char;
    view: TRectangle;
    result: longint;
    dropFileCount: longint;
    droppedFiles: PPChar;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raygui - controls test suite');
  SetExitKey(0);
    exitWindow := false;
    showMessageBox := false;
    showTextInputBox := false;
    RectangleSet(@View,0,0,0,0);
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not exitWindow do
    begin
      // Update
      //----------------------------------------------------------------------------------
      exitWindow:=WindowShouldClose;

       if IsKeyPressed(KEY_ESCAPE) then showMessageBox :=  not showMessageBox;
       if IsKeyDown(KEY_LEFT_CONTROL) and IsKeyPressed(KEY_S) then showTextInputBox := true;


       if IsFileDropped() then
        begin
            dropFileCount := 0;
            droppedFiles := GetDroppedFiles(@dropFileCount);
            if ((dropFileCount > 0) and IsFileExtension(droppedFiles[0], '.rgs')) then  GuiLoadStyle(droppedFiles[0]);
            ClearDroppedFiles();    // Clear internal buffers
        end;

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(GetColor(GuiGetStyle(DEFAULT, BACKGROUND_COLOR)));
        // raygui: controls drawing
        //----------------------------------------------------------------------------------

        if (dropDown000EditMode or dropDown001EditMode) then GuiLock()
         else
          if ( not dropDown000EditMode and  not dropDown001EditMode) then GuiUnlock();

        forceSquaredChecked := GuiCheckBox(RectangleCreate( 25, 108, 15, 15 ), 'FORCE CHECK!', forceSquaredChecked);

        GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, GUI_TEXT_ALIGN_CENTER);
        if GuiSpinner(RectangleCreate( 25, 135, 125, 30 ), nil,
        @spinner001Value, 0, 100, spinnerEditMode) then spinnerEditMode := not spinnerEditMode;

        if GuiValueBox(RectangleCreate( 25, 175, 125, 30 ), nil,
        @valueBox002Value, 0, 100, valueBoxEditMode) then valueBoxEditMode := not valueBoxEditMode;
        GuiSetStyle(TEXTBOX, TEXT_ALIGNMENT, GUI_TEXT_ALIGN_LEFT);


        if GuiButton(RectangleCreate( 25, 255, 125, 30 ), GuiIconText(RICON_FILE_SAVE, 'Save File')) then
        showTextInputBox := true;

        GuiGroupBox(RectangleCreate( 25, 310, 125, 150 ), 'STATES');

        GuiSetState(GUI_STATE_NORMAL);
        GuiButton(RectangleCreate( 30, 320, 115, 30 ), 'NORMAL');

        GuiSetState(GUI_STATE_FOCUSED);
        GuiButton(RectangleCreate( 30, 355, 115, 30 ), 'FOCUSED');

        GuiSetState(GUI_STATE_PRESSED);
        GuiButton(RectangleCreate( 30, 390, 115, 30 ), '#15#PRESSED');

        GuiSetState(GUI_STATE_DISABLED);
        GuiButton(RectangleCreate( 30, 425, 115, 30 ), 'DISABLED');
        GuiSetState(GUI_STATE_NORMAL);

        comboBoxActive := GuiComboBox(RectangleCreate( 25, 470, 125, 30 ), 'ONE;TWO;THREE;FOUR', comboBoxActive);

        // NOTE: GuiDropdownBox must draw after any other control that can be covered on unfolding
        GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, GUI_TEXT_ALIGN_LEFT);
        if GuiDropdownBox(RectangleCreate( 25, 65, 125, 30 ), '#01#ONE;#02#TWO;#03#THREE;#04#FOUR',
        @dropdownBox001Active, dropDown001EditMode) then dropDown001EditMode :=  not dropDown001EditMode;

        GuiSetStyle(DROPDOWNBOX, TEXT_ALIGNMENT, GUI_TEXT_ALIGN_CENTER);
        if GuiDropdownBox(RectangleCreate( 25, 25, 125, 30 ), 'ONE;TWO;THREE',
        @dropdownBox000Active, dropDown000EditMode) then dropDown000EditMode := not dropDown000EditMode;


         // Second GUI column
         listViewActive := GuiListView(RectangleCreate( 165, 25, 140, 140 ),
         'Charmander;Bulbasaur;#18#Squirtel;Pikachu;Eevee;Pidgey', @listViewScrollIndex, listViewActive);
         listViewExActive := GuiListViewEx(RectangleCreate( 165, 180, 140, 200 ), listViewExList, 8,
         @listViewExFocus, @listViewExScrollIndex, listViewExActive);


         toggleGroupActive := GuiToggleGroup(RectangleCreate( 165, 400, 140, 25 ),
         '#1#ONE'+#10+'#3#TWO+'+#10+'#8#THREE'+#13, toggleGroupActive);


       // Third GUI column
       if GuiTextBoxMulti(RectangleCreate( 320, 25, 225, 140 ), multiTextBoxText, 256,
       multiTextBoxEditMode) then multiTextBoxEditMode := not multiTextBoxEditMode;

       colorPickerValue := GuiColorPicker(RectangleCreate( 320, 185, 196, 192 ), colorPickerValue);

       sliderValue := round(GuiSlider(RectangleCreate( 355, 400, 165, 20 ), 'TEST',
       TextFormat('%2.2', [sliderValue]), sliderValue, -50, 100));


       sliderBarValue :=round( GuiSliderBar(RectangleCreate( 320, 430, 200, 20 ), nil,
       TextFormat('%i', [sliderBarValue]), sliderBarValue, 0, 100));
       progressValue := GuiProgressBar(RectangleCreate( 320, 460, 200, 20 ), nil, nil, progressValue, 0, 1);

       // NOTE: View rectangle could be used to perform some scissor test
       view := GuiScrollPanel(RectangleCreate( 560, 25, 100, 160 ),
       RectangleCreate( 560, 25, 200, 400 ), @viewScroll);

       GuiStatusBar(RectangleCreate( 0, GetScreenHeight() - 20, GetScreenWidth(), 20 ), 'This is a status bar');

       alphaValue := GuiColorBarAlpha(RectangleCreate( 320, 490, 200, 30 ), alphaValue);

       if showMessageBox then
         begin
           DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
           result := GuiMessageBox(RectangleCreate( GetScreenWidth()/2 - 125, GetScreenHeight()/2 - 50, 250, 100 ),
           GuiIconText(RICON_EXIT, 'Close Window'), 'Do you really want to exit?', 'Yes;No');

           if ((result = 0) or (result = 2)) then showMessageBox := false
           else if (result = 1) then exitWindow := true;
         end;


         if showTextInputBox then
          begin
            DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
            result := GuiTextInputBox(RectangleCreate( GetScreenWidth()/2 - 120, GetScreenHeight()/2 - 60, 240, 140 ),
            GuiIconText(RICON_FILE_SAVE, 'Save file as...'), 'Introduce a save file name', 'Ok;Cancel', textInput);

             if result = 1 then
                begin
                    // TODO: Validate textInput value and save
                  //  strcpy(textInputFileName, textInput);
                end;

                if ((result = 0) or (result = 1) or (result = 2)) then
                begin
                    showTextInputBox := false;
                   // strcpy(textInput, '\0');
                end;
            end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------

end.

