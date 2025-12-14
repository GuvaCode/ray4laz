unit CustomFileDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, raylib, raygui, FileUtil;

type
  { TGuiWindowFileDialogState }
  TGuiWindowFileDialogState = record
    // Window management variables
    windowActive: Boolean;
    windowBounds: TRectangle;
    panOffset: TVector2;
    dragMode: Boolean;
    supportDrag: Boolean;

    // UI variables
    dirPathEditMode: Boolean;
    dirPathText: array[0..1023] of Char;

    filesListScrollIndex: Integer;
    filesListEditMode: Boolean;
    filesListActive: Integer;

    fileNameEditMode: Boolean;
    fileNameText: array[0..1023] of Char;
    SelectFilePressed: Boolean;
    CancelFilePressed: Boolean;
    fileTypeActive: Integer;
    itemFocused: Integer;

    // Custom state variables
    dirFiles: TFilePathList;
    filterExt: array[0..255] of Char;
    dirPathTextCopy: array[0..1023] of Char;
    fileNameTextCopy: array[0..1023] of Char;

    prevFilesListActive: Integer;

    saveFileMode: Boolean;
  end;

function InitGuiWindowFileDialog(initPath: PChar): TGuiWindowFileDialogState;
function GuiWindowFileDialog(var state: TGuiWindowFileDialogState): Boolean;

implementation

const
  MAX_DIRECTORY_FILES = 2048;
  MAX_ICON_PATH_LENGTH = 512;

  {$IFDEF WINDOWS}
  PATH_SEPARATOR = '\';
  {$ELSE}
  PATH_SEPARATOR = '/';
  {$ENDIF}

var
  dirFilesIcon: array of PChar;

function GetPrevDirectoryPath(const dirPath: string): string;
var
  lastSepPos: Integer;
begin
  Result := dirPath;
  lastSepPos := LastDelimiter(PATH_SEPARATOR, Result);
  if lastSepPos > 1 then
    SetLength(Result, lastSepPos - 1)
  else if lastSepPos = 1 then
    Result := PATH_SEPARATOR;
end;

procedure ReloadDirectoryFiles(var state: TGuiWindowFileDialogState);
var
  i: Integer;
  fileName, iconText: string;
  filter: PChar;
begin
  UnloadDirectoryFiles(state.dirFiles);

  if state.filterExt[0] = #0 then
    filter := nil
  else
    filter := @state.filterExt[0];

  state.dirFiles := LoadDirectoryFilesEx(state.dirPathText, filter, False);
  state.itemFocused := 0;

  // Reset dirFilesIcon memory
  SetLength(dirFilesIcon, MAX_DIRECTORY_FILES);
  for i := 0 to High(dirFilesIcon) do
  begin
    if Assigned(dirFilesIcon[i]) then
      FreeMem(dirFilesIcon[i]);
    GetMem(dirFilesIcon[i], MAX_ICON_PATH_LENGTH);
    FillChar(dirFilesIcon[i]^, MAX_ICON_PATH_LENGTH, 0);
  end;

  // Copy paths as icon + fileNames into dirFilesIcon
  for i := 0 to state.dirFiles.count - 1 do
  begin
    fileName := GetFileName(state.dirFiles.paths[i]);

    if IsPathFile(state.dirFiles.paths[i]) then
    begin
      // Path is a file, add file icon for convenience
      if IsFileExtension(state.dirFiles.paths[i], '.png;.bmp;.tga;.gif;.jpg;.jpeg;.psd;.hdr;.qoi;.dds;.pkm;.ktx;.pvr;.astc') then
        iconText := '#12#' + fileName
      else if IsFileExtension(state.dirFiles.paths[i], '.wav;.mp3;.ogg;.flac;.xm;.mod;.it;.wma;.aiff') then
        iconText := '#11#' + fileName
      else if IsFileExtension(state.dirFiles.paths[i], '.txt;.info;.md;.nfo;.xml;.json;.c;.cpp;.cs;.lua;.py;.glsl;.vs;.fs') then
        iconText := '#10#' + fileName
      else if IsFileExtension(state.dirFiles.paths[i], '.exe;.bin;.raw;.msi') then
        iconText := '#200#' + fileName
      else
        iconText := '#218#' + fileName;
    end
    else
    begin
      // Path is a directory, add a directory icon
      iconText := '#1#' + fileName;
    end;

    StrLCopy(dirFilesIcon[i], PChar(iconText), MAX_ICON_PATH_LENGTH - 1);
  end;
end;

function InitGuiWindowFileDialog(initPath: PChar): TGuiWindowFileDialogState;
var
  state: TGuiWindowFileDialogState;
begin
  FillChar(state, SizeOf(state), 0);

  // Init window data
  state.windowBounds := RectangleCreate(
    GetScreenWidth div 2 - 440 div 2,
    GetScreenHeight div 2 - 310 div 2,
    440, 310
  );
  state.windowActive := False;
  state.supportDrag := True;
  state.dragMode := False;
  state.panOffset := Vector2Create(0, 0);

  // Init path data
  state.dirPathEditMode := False;
  state.filesListActive := -1;
  state.prevFilesListActive := state.filesListActive;
  state.filesListScrollIndex := 0;
  state.fileNameEditMode := False;
  state.SelectFilePressed := False;
  state.CancelFilePressed := False;
  state.fileTypeActive := 0;
  state.fileNameText[0] := #0;

  // Custom variables initialization
  if (initPath <> nil) and DirectoryExists(initPath) then
    StrLCopy(@state.dirPathText[0], initPath, 1023)
  else if (initPath <> nil) and FileExists(initPath) then
  begin
    StrLCopy(@state.dirPathText[0], GetDirectoryPath(initPath), 1023);
    StrLCopy(@state.fileNameText[0], GetFileName(initPath), 1023);
  end
  else
    StrLCopy(@state.dirPathText[0], GetWorkingDirectory(), 1023);

  // TODO: Why we keep a copy?
  StrLCopy(@state.dirPathTextCopy[0], @state.dirPathText[0], 1023);
  StrLCopy(@state.fileNameTextCopy[0], @state.fileNameText[0], 1023);

  state.filterExt[0] := #0;
  state.dirFiles.count := 0;

  Result := state;
end;

function GuiWindowFileDialog(var state: TGuiWindowFileDialogState): Boolean;
var
  mousePosition: TVector2;
  prevTextAlignment, prevElementsHeight: Integer;
  fileNameStr, fullPath: string;
  i: Integer;
  tempStr: string;
  dirFilesIconArray: PPChar;
  guiResult: LongInt;
begin
  Result := state.windowActive;

  if state.windowActive then
  begin
    // Update window dragging
    if state.supportDrag then
    begin
      mousePosition := GetMousePosition;

      if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
      begin
        // Window can be dragged from the top window bar
        if CheckCollisionPointRec(mousePosition,
           RectangleCreate(
             state.windowBounds.x,
             state.windowBounds.y,
             state.windowBounds.width,
             RAYGUI_WINDOWBOX_STATUSBAR_HEIGHT
           )) then
        begin
          state.dragMode := True;
          state.panOffset.x := mousePosition.x - state.windowBounds.x;
          state.panOffset.y := mousePosition.y - state.windowBounds.y;
        end;
      end;

      if state.dragMode then
      begin
        state.windowBounds.x := mousePosition.x - state.panOffset.x;
        state.windowBounds.y := mousePosition.y - state.panOffset.y;

        // Check screen limits to avoid moving out of screen
        if state.windowBounds.x < 0 then
          state.windowBounds.x := 0
        else if state.windowBounds.x > (GetScreenWidth - state.windowBounds.width) then
          state.windowBounds.x := GetScreenWidth - state.windowBounds.width;

        if state.windowBounds.y < 0 then
          state.windowBounds.y := 0
        else if state.windowBounds.y > (GetScreenHeight - state.windowBounds.height) then
          state.windowBounds.y := GetScreenHeight - state.windowBounds.height;

        if IsMouseButtonReleased(MOUSE_LEFT_BUTTON) then
          state.dragMode := False;
      end;
    end;

    // Load dirFilesIcon lazily on windows open
    if Length(dirFilesIcon) = 0 then
    begin
      SetLength(dirFilesIcon, MAX_DIRECTORY_FILES);
      for i := 0 to High(dirFilesIcon) do
      begin
        GetMem(dirFilesIcon[i], MAX_ICON_PATH_LENGTH);
        FillChar(dirFilesIcon[i]^, MAX_ICON_PATH_LENGTH, 0);
      end;
    end;

    // Load current directory files
    if state.dirFiles.paths = nil then
      ReloadDirectoryFiles(state);

    // Draw window and controls
    // GuiWindowBox returns LongInt in ray4laz, convert to Boolean
    guiResult := GuiWindowBox(state.windowBounds, '#198# Select File Dialog');
    if guiResult <> 0 then
      state.windowActive := False;

    // Draw previous directory button + logic
    guiResult := GuiButton(RectangleCreate(
      state.windowBounds.x + state.windowBounds.width - 48,
      state.windowBounds.y + 24 + 12,
      40, 24), '< ..');
    if guiResult <> 0 then
    begin
      // Move dir path one level up
      tempStr := GetPrevDirectoryPath(state.dirPathText);
      StrLCopy(@state.dirPathText[0], PChar(tempStr), 1023);

      // Reload directory files (frees previous list)
      ReloadDirectoryFiles(state);

      state.filesListActive := -1;
      FillChar(state.fileNameText[0], 1024, 0);
      FillChar(state.fileNameTextCopy[0], 1024, 0);
    end;

    // Draw current directory text box info + path editing logic
    guiResult := GuiTextBox(RectangleCreate(
      state.windowBounds.x + 8,
      state.windowBounds.y + 24 + 12,
      state.windowBounds.width - 48 - 16,
      24), @state.dirPathText[0], 1024, state.dirPathEditMode);
    if guiResult <> 0 then
    begin
      state.dirPathEditMode := not state.dirPathEditMode;

      if not state.dirPathEditMode then
      begin
        // Verify if a valid path has been introduced
        if DirectoryExists(state.dirPathText) then
        begin
          // Reload directory files (frees previous list)
          ReloadDirectoryFiles(state);
          StrLCopy(@state.dirPathTextCopy[0], @state.dirPathText[0], 1023);
        end
        else
          StrLCopy(@state.dirPathText[0], @state.dirPathTextCopy[0], 1023);
      end;
    end;

    // List view elements are aligned left
    prevTextAlignment := GuiGetStyle(LISTVIEW, TEXT_ALIGNMENT);
    prevElementsHeight := GuiGetStyle(LISTVIEW, LIST_ITEMS_HEIGHT);
    GuiSetStyle(LISTVIEW, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
    GuiSetStyle(LISTVIEW, LIST_ITEMS_HEIGHT, 24);

    // Prepare array for GuiListViewEx
    if (Length(dirFilesIcon) > 0) and (state.dirFiles.count > 0) then
    begin
      GetMem(dirFilesIconArray, SizeOf(PChar) * state.dirFiles.count);
      try
        for i := 0 to state.dirFiles.count - 1 do
          dirFilesIconArray[i] := dirFilesIcon[i];

        GuiListViewEx(RectangleCreate(
          state.windowBounds.x + 8,
          state.windowBounds.y + 48 + 20,
          state.windowBounds.width - 16,
          state.windowBounds.height - 60 - 16 - 68),
          dirFilesIconArray,
          state.dirFiles.count,
          @state.filesListScrollIndex,
          @state.filesListActive,
          @state.itemFocused);
      finally
        FreeMem(dirFilesIconArray);
      end;
    end;

    GuiSetStyle(LISTVIEW, TEXT_ALIGNMENT, prevTextAlignment);
    GuiSetStyle(LISTVIEW, LIST_ITEMS_HEIGHT, prevElementsHeight);

    // Check if a path has been selected, if it is a directory, move to that directory
    if (state.filesListActive >= 0) and (state.filesListActive <> state.prevFilesListActive) then
    begin
      StrLCopy(@state.fileNameText[0],
               GetFileName(state.dirFiles.paths[state.filesListActive]),
               1023);

      tempStr := state.dirPathText + PATH_SEPARATOR + state.fileNameText;
      if DirectoryExists(PChar(tempStr)) then
      begin
        if state.fileNameText = '..' then
        begin
          tempStr := GetPrevDirectoryPath(state.dirPathText);
          StrLCopy(@state.dirPathText[0], PChar(tempStr), 1023);
        end
        else
        begin
          if state.dirPathText = '/' then
            fileNameStr := state.fileNameText
          else
            fileNameStr := state.dirPathText + PATH_SEPARATOR + state.fileNameText;
          StrLCopy(@state.dirPathText[0], PChar(fileNameStr), 1023);
        end;

        StrLCopy(@state.dirPathTextCopy[0], @state.dirPathText[0], 1023);

        // Reload directory files (frees previous list)
        ReloadDirectoryFiles(state);

        StrLCopy(@state.dirPathTextCopy[0], @state.dirPathText[0], 1023);

        state.filesListActive := -1;
        state.fileNameText[0] := #0;
        StrLCopy(@state.fileNameTextCopy[0], @state.fileNameText[0], 1023);
      end;

      state.prevFilesListActive := state.filesListActive;
    end;

    // Draw bottom controls
    GuiLabel(RectangleCreate(
      state.windowBounds.x + 8,
      state.windowBounds.y + state.windowBounds.height - 68,
      60, 24), 'File name:');

    // GuiTextBox returns LongInt in ray4laz, convert to Boolean
    guiResult := GuiTextBox(RectangleCreate(
      state.windowBounds.x + 72,
      state.windowBounds.y + state.windowBounds.height - 68,
      state.windowBounds.width - 184,
      24), @state.fileNameText[0], 128, state.fileNameEditMode);
    if guiResult <> 0 then
    begin
      state.fileNameEditMode := not state.fileNameEditMode;

      if not state.fileNameEditMode and (state.fileNameText[0] <> #0) then
      begin
        // Verify if a valid filename has been introduced
        fullPath := state.dirPathText + PATH_SEPARATOR + state.fileNameText;
        if FileExists(PChar(fullPath)) then
        begin
          // Select filename from list view
          for i := 0 to state.dirFiles.count - 1 do
          begin
            tempStr := GetFileName(state.dirFiles.paths[i]);
            if state.fileNameText = tempStr then
            begin
              state.filesListActive := i;
              StrLCopy(@state.fileNameTextCopy[0], @state.fileNameText[0], 1023);
              Break;
            end;
          end;
        end
        else if not state.saveFileMode then
        begin
          StrLCopy(@state.fileNameText[0], @state.fileNameTextCopy[0], 1023);
        end;
      end;
    end;

    GuiLabel(RectangleCreate(
      state.windowBounds.x + 8,
      state.windowBounds.y + state.windowBounds.height - 24 - 12,
      68, 24), 'File filter:');

    // GuiComboBox returns LongInt in ray4laz, convert to Boolean
    guiResult := GuiComboBox(RectangleCreate(
      state.windowBounds.x + 72,
      state.windowBounds.y + state.windowBounds.height - 24 - 12,
      state.windowBounds.width - 184,
      24), 'All files', @state.fileTypeActive);
    // We can ignore the result or handle if needed

    // GuiButton returns LongInt in ray4laz, convert to Boolean
    guiResult := GuiButton(RectangleCreate(
      state.windowBounds.x + state.windowBounds.width - 96 - 8,
      state.windowBounds.y + state.windowBounds.height - 68,
      96, 24), 'Select');
    if guiResult <> 0 then
      state.SelectFilePressed := True;

    guiResult := GuiButton(RectangleCreate(
      state.windowBounds.x + state.windowBounds.width - 96 - 8,
      state.windowBounds.y + state.windowBounds.height - 24 - 12,
      96, 24), 'Cancel');
    if guiResult <> 0 then
      state.windowActive := False;

    // Exit on file selected
    if state.SelectFilePressed then
      state.windowActive := False;

    // File dialog has been closed, free all memory before exit
    if not state.windowActive then
    begin
      // Free dirFilesIcon memory
      for i := 0 to High(dirFilesIcon) do
        if Assigned(dirFilesIcon[i]) then
          FreeMem(dirFilesIcon[i]);
      SetLength(dirFilesIcon, 0);

      // Unload directory file paths
      UnloadDirectoryFiles(state.dirFiles);

      // Reset state variables
      state.dirFiles.count := 0;
      state.dirFiles.capacity := 0;
      state.dirFiles.paths := nil;
    end;

    Result := state.windowActive;
  end;
end;

end.
