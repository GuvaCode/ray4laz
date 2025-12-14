program custom_file_dialog;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, raylib, raygui, CustomFileDialog;

const
  {$IFDEF WINDOWS}
  PATH_SEPARATOR = '\';
  {$ELSE}
  PATH_SEPARATOR = '/';
  {$ENDIF}

var
  screenWidth, screenHeight: Integer;
  fileDialogState: TGuiWindowFileDialogState;
  exitWindow: Boolean;
  fileNameToLoad: array[0..511] of Char;
  texture: TTexture;
  guiResult: LongInt;

begin
  // Initialization
  screenWidth := 800;
  screenHeight := 560;

  InitWindow(screenWidth, screenHeight, 'raygui - custom modal dialog');
  SetExitKey(0);

  // Custom file dialog
  fileDialogState := InitGuiWindowFileDialog(GetWorkingDirectory());
  exitWindow := False;
  fileNameToLoad[0] := #0;
  // texture := Default(TTexture);

  SetTargetFPS(60);

  // Main game loop
  while not exitWindow do
  begin
    // Update
    exitWindow := WindowShouldClose();

    if fileDialogState.SelectFilePressed then
    begin
      // Load image file (if supported extension)
      if IsFileExtension(fileDialogState.fileNameText, '.png') then
      begin
        StrLCopy(@fileNameToLoad[0],
                 PChar(fileDialogState.dirPathText + PATH_SEPARATOR + fileDialogState.fileNameText),
                 511);
        UnloadTexture(texture{%H-});
        texture := LoadTexture(@fileNameToLoad[0]);
      end;

      fileDialogState.SelectFilePressed := False;
    end;

    // Draw
    BeginDrawing();

      ClearBackground(GetColor(GuiGetStyle(DEFAULT, BACKGROUND_COLOR)));

      if texture.id > 0 then
      begin
        DrawTexture(texture,
                   screenWidth div 2 - texture.width div 2,
                   screenHeight div 2 - texture.height div 2 - 5,
                   WHITE);
        DrawRectangleLines(screenWidth div 2 - texture.width div 2,
                          screenHeight div 2 - texture.height div 2 - 5,
                          texture.width, texture.height, BLACK);
      end;

      DrawText(@fileNameToLoad[0], 208, screenHeight - 20, 10, GRAY);

      // raygui: controls drawing
      if fileDialogState.windowActive then
        GuiLock();

      // GuiButton returns LongInt in ray4laz, convert to Boolean
      guiResult := GuiButton(RectangleCreate(20, 20, 140, 30),
                            GuiIconText(ICON_FILE_OPEN, 'Open Image'));
      if guiResult <> 0 then
        fileDialogState.windowActive := True;

      GuiUnlock();

      // GUI: Dialog Window
      GuiWindowFileDialog(fileDialogState);

    EndDrawing();
  end;

  // De-Initialization
  if texture.id > 0 then
    UnloadTexture(texture);

  CloseWindow();
end.

