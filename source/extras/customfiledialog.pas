unit CustomFileDialog;

{$mode objfpc}{$H+}
{$WARN 4110 off : range check error while evaluating constants ($1 must be between $2 and $3)}
interface

uses
  Classes, SysUtils, raylib, raygui;

type
  { TCustomFileDialog }
  TCustomFileDialog = class
  private
    // Window management variables
    FWindowActive: Boolean;
    FWindowBounds: TRectangle;
    FPanOffset: TVector2;
    FDragMode: Boolean;
    FSupportDrag: Boolean;

    // UI variables
    FDirPathEditMode: Boolean;
    FDirPathText: array[0..1023] of Char;

    FFilesListScrollIndex: Integer;
    FFilesListEditMode: Boolean;
    FFilesListActive: Integer;

    FFileNameEditMode: Boolean;
    FFileNameText: array[0..1023] of Char;
    FSelectFilePressed: Boolean;
    FCancelFilePressed: Boolean;
    FItemFocused: Integer;

    // Custom state variables
    FDirFiles: TFilePathList;
    FFilterExt: array[0..255] of Char;
    FDirPathTextCopy: array[0..1023] of Char;
    FFileNameTextCopy: array[0..1023] of Char;

    FPrevFilesListActive: Integer;

    FSaveFileMode: Boolean;

    FDirFilesIcon: array of PChar;

    procedure ReloadDirectoryFiles;
    function GetPrevDirectoryPath(const dirPath: string): string;
    function IsRootPath(const dirPath: string): Boolean;
    procedure UpdateWindowDragging(mousePosition: TVector2);
    procedure HandlePrevDirButton;
    procedure HandleDirPathTextBox;
    procedure DrawFileListView;
    procedure HandleFileSelection;
    procedure DrawBottomControls;
    procedure HandleFileNameTextBox;
    procedure HandleSelectButton;
    procedure HandleCancelButton;
    procedure CleanupOnClose;
    function GetFileNameText: string;
    function GetDirPathText: string;
    function GetFullPath: string;
    function GetFilterExt: string;
    procedure SetFilterExt(const Value: string);

  public
    constructor Create(initPath: PChar = nil);
    destructor Destroy; override;

    procedure OpenDialog;
    procedure CloseDialog;
    function UpdateAndDraw: Boolean;

    // Properties
    property WindowActive: Boolean read FWindowActive;
    property SelectFilePressed: Boolean read FSelectFilePressed write FSelectFilePressed;
    property FileNameText: string read GetFileNameText;
    property DirPathText: string read GetDirPathText;
    property FullPath: string read GetFullPath;
    property SaveFileMode: Boolean read FSaveFileMode write FSaveFileMode;
    property FilterExt: string read GetFilterExt write SetFilterExt;

  end;

implementation

const
  MAX_DIRECTORY_FILES = 2048;
  MAX_ICON_PATH_LENGTH = 512;
  SAVE_FILE = 'Save file';
  OPEN_FILE = 'Open file';
  DEFAULT_FILTER = '*.*';

  {$IFDEF WINDOWS}
  PATH_SEPARATOR = '\';
  {$ELSE}
  PATH_SEPARATOR = '/';
  {$ENDIF}

{ TCustomFileDialog }

constructor TCustomFileDialog.Create(initPath: PChar);
var
  i: Integer;
begin
  inherited Create;

  // Initialize window data
  FWindowBounds := RectangleCreate(
    GetScreenWidth div 2 - 440 div 2,
    GetScreenHeight div 2 - 310 div 2,
    440, 310
  );
  FWindowActive := False;
  FSupportDrag := True;
  FDragMode := False;
  FPanOffset := Vector2Create(0, 0);

  // Initialize path data
  FDirPathEditMode := False;
  FFilesListActive := -1;
  FPrevFilesListActive := FFilesListActive;
  FFilesListScrollIndex := 0;
  FFileNameEditMode := False;
  FSelectFilePressed := False;
  FCancelFilePressed := False;
  FillChar(FFileNameText, SizeOf(FFileNameText), 0);

  // По умолчанию - режим открытия файла
  FSaveFileMode := False;

  // Устанавливаем фильтр по умолчанию
  StrLCopy(@FFilterExt[0], PChar(DEFAULT_FILTER), 255);

  // Custom variables initialization
  if (initPath <> nil) and DirectoryExists(initPath) then
    StrLCopy(@FDirPathText[0], initPath, 1023)
  else if (initPath <> nil) and FileExists(initPath) then
  begin
    StrLCopy(@FDirPathText[0], GetDirectoryPath(initPath), 1023);
    StrLCopy(@FFileNameText[0], GetFileName(initPath), 1023);
  end
  else
    StrLCopy(@FDirPathText[0], GetWorkingDirectory(), 1023);

  // Keep a copy for validation
  StrLCopy(@FDirPathTextCopy[0], @FDirPathText[0], 1023);
  StrLCopy(@FFileNameTextCopy[0], @FFileNameText[0], 1023);

  FDirFiles.count := 0;

  // Initialize dirFilesIcon array
  SetLength(FDirFilesIcon, MAX_DIRECTORY_FILES);
  for i := 0 to High(FDirFilesIcon) do
  begin
    GetMem(FDirFilesIcon[i], MAX_ICON_PATH_LENGTH);
    FillChar(FDirFilesIcon[i]^, MAX_ICON_PATH_LENGTH, 0);
  end;
end;

destructor TCustomFileDialog.Destroy;
var
  i: Integer;
begin
  // Free dirFilesIcon memory
  for i := 0 to High(FDirFilesIcon) do
  begin
    if Assigned(FDirFilesIcon[i]) then
      FreeMem(FDirFilesIcon[i]);
  end;
  SetLength(FDirFilesIcon, 0);

  // Unload directory file paths
  UnloadDirectoryFiles(FDirFiles);

  inherited Destroy;
end;

procedure TCustomFileDialog.OpenDialog;
begin
  FWindowActive := True;
  FSelectFilePressed := False;

  // Reload directory files if needed
  if FDirFiles.paths = nil then
    ReloadDirectoryFiles;
end;

procedure TCustomFileDialog.CloseDialog;
begin
  FWindowActive := False;
  CleanupOnClose;
end;

function TCustomFileDialog.UpdateAndDraw: Boolean;
var TopText: PChar;
begin
  Result := FWindowActive;

  if not FWindowActive then
    Exit;

  // Update window dragging
  UpdateWindowDragging(GetMousePosition);

  if SaveFileMode then TopText := SAVE_FILE
  else TopText := OPEN_FILE;

  // Draw window and controls
  if GuiWindowBox(FWindowBounds, PChar('#198# ' + TopText)) <> 0 then
  begin
    CloseDialog;
    Exit(False);
  end;

  // Draw previous directory button
  HandlePrevDirButton;

  // Draw current directory text box
  HandleDirPathTextBox;

  // Draw file list view
  DrawFileListView;

  // Handle file selection
  HandleFileSelection;

  // Draw bottom controls
  DrawBottomControls;

  // Handle select button
  HandleSelectButton;

  // Handle cancel button
  HandleCancelButton;

  // Exit if file selected
  if FSelectFilePressed then
    CloseDialog;

  Result := FWindowActive;
end;

procedure TCustomFileDialog.ReloadDirectoryFiles;
var
  i, dirCount, fileCount: Integer;
  fileName, iconText: string;
  filter: PChar;
  dirPaths, filePaths: array of PChar;
begin
  UnloadDirectoryFiles(FDirFiles);

  // Используем фильтр, если он не '*.*'
  if (FFilterExt[0] = #0) or (string(FFilterExt) = '*.*') then
    filter := nil
  else
    filter := @FFilterExt[0];

  FDirFiles := LoadDirectoryFilesEx(FDirPathText, filter, False);
  FItemFocused := 0;

  // Reset dirFilesIcon memory
  for i := 0 to High(FDirFilesIcon) do
  begin
    if Assigned(FDirFilesIcon[i]) then
      FillChar(FDirFilesIcon[i]^, MAX_ICON_PATH_LENGTH, 0);
  end;

  // Сначала считаем папки и файлы
  dirCount := 0;
  fileCount := 0;
  for i := 0 to FDirFiles.count - 1 do
  begin
    if IsPathFile(FDirFiles.paths[i]) then
      Inc(fileCount)
    else
      Inc(dirCount);
  end;

  // Создаем временные массивы
  SetLength(dirPaths, dirCount);
  SetLength(filePaths, fileCount);

  // Заполняем массивы
  dirCount := 0;
  fileCount := 0;
  for i := 0 to FDirFiles.count - 1 do
  begin
    if IsPathFile(FDirFiles.paths[i]) then
    begin
      filePaths[fileCount] := FDirFiles.paths[i];
      Inc(fileCount);
    end
    else
    begin
      dirPaths[dirCount] := FDirFiles.paths[i];
      Inc(dirCount);
    end;
  end;

  // Объединяем в FDirFiles: сначала папки, потом файлы
  for i := 0 to dirCount - 1 do
  begin
    FDirFiles.paths[i] := dirPaths[i];
    fileName := GetFileName(dirPaths[i]);
    iconText := '#1#' + fileName;
    StrLCopy(FDirFilesIcon[i], PChar(iconText), MAX_ICON_PATH_LENGTH - 1);
  end;

  for i := 0 to fileCount - 1 do
  begin
    FDirFiles.paths[dirCount + i] := filePaths[i];
    fileName := GetFileName(filePaths[i]);

    // Определяем иконку для файла
    if IsFileExtension(filePaths[i], '.png;.bmp;.tga;.gif;.jpg;.jpeg;.psd;.hdr;.qoi;.dds;.pkm;.ktx;.pvr;.astc') then
      iconText := '#12#' + fileName
    else if IsFileExtension(filePaths[i], '.wav;.mp3;.ogg;.flac;.xm;.mod;.it;.wma;.aiff') then
      iconText := '#11#' + fileName
    else if IsFileExtension(filePaths[i], '.txt;.info;.md;.nfo;.xml;.json;.c;.cpp;.cs;.lua;.py;.glsl;.vs;.fs') then
      iconText := '#10#' + fileName
    else if IsFileExtension(filePaths[i], '.exe;.bin;.raw;.msi') then
      iconText := '#200#' + fileName
    else
      iconText := '#218#' + fileName;

    StrLCopy(FDirFilesIcon[dirCount + i], PChar(iconText), MAX_ICON_PATH_LENGTH - 1);
  end;

  SetLength(dirPaths, 0);
  SetLength(filePaths, 0);
end;

function TCustomFileDialog.GetPrevDirectoryPath(const dirPath: string): string;
var
  lastSepPos: Integer;
begin
  Result := dirPath;

  // Проверяем, является ли путь корневым
  if IsRootPath(dirPath) then
    Exit; // Возвращаем тот же путь, если уже в корне

  lastSepPos := LastDelimiter(PATH_SEPARATOR, Result);

  // Удаляем последний сегмент пути
  if lastSepPos > 1 then
    SetLength(Result, lastSepPos - 1)
  else if lastSepPos = 1 then
    Result := PATH_SEPARATOR; // Для Unix-систем: переход в корень
end;

function TCustomFileDialog.IsRootPath(const dirPath: string): Boolean;
begin
  Result := False;

  {$IFDEF WINDOWS}
  // Для Windows: путь вида "C:\" или "\\server\share\"
  if (Length(dirPath) = 3) and (dirPath[2] = ':') and (dirPath[3] = '\') then
    Result := True
  else if (Length(dirPath) >= 2) and (dirPath[1] = '\') and (dirPath[2] = '\') then
  begin
    // UNC путь - проверяем, является ли он корневым
    if (Pos('\', Copy(dirPath, 3, MaxInt)) = 0) or
       (Pos('\', Copy(dirPath, 3, MaxInt)) = Length(Copy(dirPath, 3, MaxInt))) then
      Result := True;
  end;
  {$ELSE}
  // Для Unix-систем: путь "/"
  Result := (dirPath = '/');
  {$ENDIF}
end;

procedure TCustomFileDialog.UpdateWindowDragging(mousePosition: TVector2);
begin
  if not FSupportDrag then
    Exit;

  if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
  begin
    // Window can be dragged from the top window bar
    if CheckCollisionPointRec(mousePosition,
       RectangleCreate(
         FWindowBounds.x,
         FWindowBounds.y,
         FWindowBounds.width,
         RAYGUI_WINDOWBOX_STATUSBAR_HEIGHT
       )) then
    begin
      FDragMode := True;
      FPanOffset.x := mousePosition.x - FWindowBounds.x;
      FPanOffset.y := mousePosition.y - FWindowBounds.y;
    end;
  end;

  if FDragMode then
  begin
    FWindowBounds.x := mousePosition.x - FPanOffset.x;
    FWindowBounds.y := mousePosition.y - FPanOffset.y;

    // Check screen limits to avoid moving out of screen
    if FWindowBounds.x < 0 then
      FWindowBounds.x := 0
    else if FWindowBounds.x > (GetScreenWidth - FWindowBounds.width) then
      FWindowBounds.x := GetScreenWidth - FWindowBounds.width;

    if FWindowBounds.y < 0 then
      FWindowBounds.y := 0
    else if FWindowBounds.y > (GetScreenHeight - FWindowBounds.height) then
      FWindowBounds.y := GetScreenHeight - FWindowBounds.height;

    if IsMouseButtonReleased(MOUSE_LEFT_BUTTON) then
      FDragMode := False;
  end;
end;

procedure TCustomFileDialog.HandlePrevDirButton;
var
  tempStr: string;
  guiResult: LongInt;
  prevState: Integer;
begin
  // Проверяем, находимся ли мы в корневом каталоге
  if IsRootPath(FDirPathText) then
  begin
    // Если в корне - кнопка должна быть неактивна
    prevState := GuiGetState();
    GuiSetState(Ord(STATE_DISABLED));

    guiResult := GuiButton(RectangleCreate(
      FWindowBounds.x + FWindowBounds.width - 48,
      FWindowBounds.y + 24 + 12,
      40, 24),  PChar('#3# '+ '...'));

    GuiSetState(prevState);
  end
  else
  begin
    guiResult := GuiButton(RectangleCreate(
      FWindowBounds.x + FWindowBounds.width - 48,
      FWindowBounds.y + 24 + 12,
      40, 24),  PChar('#3# '+ '...'));

    if guiResult <> 0 then
    begin
      // Move dir path one level up
      tempStr := GetPrevDirectoryPath(FDirPathText);
      StrLCopy(@FDirPathText[0], PChar(tempStr), 1023);

      // Reload directory files
      ReloadDirectoryFiles;

      FFilesListActive := -1;
      FillChar(FFileNameText, SizeOf(FFileNameText), 0);
      FillChar(FFileNameTextCopy, SizeOf(FFileNameTextCopy), 0);
    end;
  end;
end;

procedure TCustomFileDialog.HandleDirPathTextBox;
var
  guiResult: LongInt;
begin
  guiResult := GuiTextBox(RectangleCreate(
    FWindowBounds.x + 8,
    FWindowBounds.y + 24 + 12,
    FWindowBounds.width - 48 - 16,
    24), @FDirPathText[0], 1024, FDirPathEditMode);

  if guiResult <> 0 then
  begin
    FDirPathEditMode := not FDirPathEditMode;

    if not FDirPathEditMode then
    begin
      // Verify if a valid path has been introduced
      if DirectoryExists(FDirPathText) then
      begin
        // Reload directory files
        ReloadDirectoryFiles;
        StrLCopy(@FDirPathTextCopy[0], @FDirPathText[0], 1023);
      end
      else
        StrLCopy(@FDirPathText[0], @FDirPathTextCopy[0], 1023);
    end;
  end;
end;

procedure TCustomFileDialog.DrawFileListView;
var
  prevTextAlignment, prevElementsHeight: Integer;
  dirFilesIconArray: PPChar;
  i: Integer;
begin
  // List view elements are aligned left
  prevTextAlignment := GuiGetStyle(LISTVIEW, TEXT_ALIGNMENT);
  prevElementsHeight := GuiGetStyle(LISTVIEW, LIST_ITEMS_HEIGHT);
  GuiSetStyle(LISTVIEW, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);
  GuiSetStyle(LISTVIEW, LIST_ITEMS_HEIGHT, 24);

  // Prepare array for GuiListViewEx
  if (Length(FDirFilesIcon) > 0) and (FDirFiles.count > 0) then
  begin
    GetMem(dirFilesIconArray, SizeOf(PChar) * FDirFiles.count);
    try
      for i := 0 to FDirFiles.count - 1 do
        dirFilesIconArray[i] := FDirFilesIcon[i];

      GuiListViewEx(RectangleCreate(
        FWindowBounds.x + 8,
        FWindowBounds.y + 48 + 20,
        FWindowBounds.width - 16,
        FWindowBounds.height - 60 - 16 - 68),
        dirFilesIconArray,
        FDirFiles.count ,
        @FFilesListScrollIndex,
        @FFilesListActive,
        @FItemFocused);
    finally
      FreeMem(dirFilesIconArray);
    end;
  end;
  // Если нет файлов рисуем рамку
  if FDirFiles.count <=0 then
  GuiGroupBox(RectangleCreate(
        FWindowBounds.x + 8,
        FWindowBounds.y + 48 + 20,
        FWindowBounds.width - 16,
        FWindowBounds.height - 60 - 16 - 68), nil);

  GuiSetStyle(LISTVIEW, TEXT_ALIGNMENT, prevTextAlignment);
  GuiSetStyle(LISTVIEW, LIST_ITEMS_HEIGHT, prevElementsHeight);
end;

procedure TCustomFileDialog.HandleFileSelection;
var
  tempStr, fileNameStr: string;
begin
  // Если кликнули на пустое место в списке, сбрасываем выбор
  if (FFilesListActive < 0) and (FPrevFilesListActive >= 0) then
  begin
    FillChar(FFileNameText, SizeOf(FFileNameText), 0);
    StrLCopy(@FFileNameTextCopy[0], @FFileNameText[0], 1023);
    FPrevFilesListActive := FFilesListActive;
    Exit;
  end;

  if (FFilesListActive >= 0) and (FFilesListActive <> FPrevFilesListActive) then
  begin
    StrLCopy(@FFileNameText[0],
             GetFileName(FDirFiles.paths[FFilesListActive]),
             1023);

    tempStr := FDirPathText + PATH_SEPARATOR + FFileNameText;
    if DirectoryExists(PChar(tempStr)) then
    begin
      if FFileNameText = '..' then
      begin
        // Проверяем, не находимся ли мы уже в корне
        if not IsRootPath(FDirPathText) then
        begin
          tempStr := GetPrevDirectoryPath(FDirPathText);
          StrLCopy(@FDirPathText[0], PChar(tempStr), 1023);

          // Reload directory files
          ReloadDirectoryFiles;

          StrLCopy(@FDirPathTextCopy[0], @FDirPathText[0], 1023);

          FFilesListActive := -1;
          FillChar(FFileNameText, SizeOf(FFileNameText), 0);
          StrLCopy(@FFileNameTextCopy[0], @FFileNameText[0], 1023);
        end;
      end
      else
      begin
        // Переход в подкаталог
        if IsRootPath(FDirPathText) and (FDirPathText = PATH_SEPARATOR) then
          fileNameStr := FDirPathText + FFileNameText
        else
          fileNameStr := FDirPathText + PATH_SEPARATOR + FFileNameText;

        StrLCopy(@FDirPathText[0], PChar(fileNameStr), 1023);

        // Reload directory files
        ReloadDirectoryFiles;

        StrLCopy(@FDirPathTextCopy[0], @FDirPathText[0], 1023);

        FFilesListActive := -1;
        FillChar(FFileNameText, SizeOf(FFileNameText), 0);
        StrLCopy(@FFileNameTextCopy[0], @FFileNameText[0], 1023);
      end;
    end;

    FPrevFilesListActive := FFilesListActive;
  end;
end;

procedure TCustomFileDialog.DrawBottomControls;
begin
  // Смещаем поле ввода имени файла на уровень кнопки "Cancel"
  GuiLabel(RectangleCreate(
    FWindowBounds.x + 8,
    FWindowBounds.y + FWindowBounds.height - 68,
    60, 24), 'File name:');

  HandleFileNameTextBox;
end;

procedure TCustomFileDialog.HandleFileNameTextBox;
var
  guiResult: LongInt;
  fullPath_, tempStr: string;
  i: Integer;
begin

  guiResult := GuiTextBox(RectangleCreate(
    FWindowBounds.x + 72,
    FWindowBounds.y + FWindowBounds.height - 68,
    FWindowBounds.width - 96 + 16,
    24), @FFileNameText[0], 128, FFileNameEditMode);

  if guiResult <> 0 then
  begin
    FFileNameEditMode := not FFileNameEditMode;

    if not FFileNameEditMode and (FFileNameText[0] <> #0) then
    begin
      // Verify if a valid filename has been introduced
      if FDirPathText[Length(FDirPathText)] = PATH_SEPARATOR then
        fullPath_ := FDirPathText + FFileNameText
      else
        fullPath_ := FDirPathText + PATH_SEPARATOR + FFileNameText;

      if FileExists(PChar(fullPath_)) then
      begin
        // Select filename from list view
        for i := 0 to FDirFiles.count - 1 do
        begin
          tempStr := GetFileName(FDirFiles.paths[i]);
          if FFileNameText = tempStr then
          begin
            FFilesListActive := i;
            StrLCopy(@FFileNameTextCopy[0], @FFileNameText[0], 1023);
            Break;
          end;
        end;
      end
      else if not FSaveFileMode then
      begin
        StrLCopy(@FFileNameText[0], @FFileNameTextCopy[0], 1023);
      end;
    end;
  end;
end;

procedure TCustomFileDialog.HandleSelectButton;
var
  guiResult: LongInt;
  selectButtonRect: TRectangle;
  selectEnabled: Boolean;
  prevState: Integer;
begin
  selectButtonRect := RectangleCreate(
    FWindowBounds.x + FWindowBounds.width - 96 - 8,
    FWindowBounds.y + FWindowBounds.height - 24 - 12,
    96, 24
  );

  // Кнопка активна, если:
  // 1. Режим сохранения (SaveFileMode) - всегда активна
  // 2. Режим открытия - только если выбран файл или введено имя файла
  selectEnabled := FSaveFileMode or
                   (FFilesListActive >= 0) or
                   (FFileNameText[0] <> #0);

  // Сохраняем предыдущее состояние GUI
  prevState := GuiGetState();

  // Устанавливаем состояние кнопки
  if not selectEnabled then
    GuiSetState(Ord(STATE_DISABLED))
  else
    GuiSetState(Ord(STATE_NORMAL));

  // Рисуем кнопку
  if not FSaveFileMode then
  guiResult := GuiButton(selectButtonRect, '#5# Open')
  else
  guiResult := GuiButton(selectButtonRect, '#6# Save');

  // Восстанавливаем состояние GUI
  GuiSetState(prevState);

  // Обрабатываем нажатие только если кнопка была активна
  if (guiResult <> 0) and selectEnabled then
    FSelectFilePressed := True;
end;

procedure TCustomFileDialog.HandleCancelButton;
var
  guiResult: LongInt;
begin
  guiResult := GuiButton(RectangleCreate(
    FWindowBounds.x + FWindowBounds.width - 96*2 - 8*2,
    FWindowBounds.y + FWindowBounds.height - 24 - 12,
    96, 24), '#159# Cancel');

  if guiResult <> 0 then
    CloseDialog;
end;

procedure TCustomFileDialog.CleanupOnClose;
begin
  // Reset state variables
  FDirFiles.count := 0;
  FDirFiles.capacity := 0;
  FDirFiles.paths := nil;
end;

function TCustomFileDialog.GetFileNameText: string;
begin
  Result := string(FFileNameText);
end;

function TCustomFileDialog.GetDirPathText: string;
begin
  Result := string(FDirPathText);
end;

function TCustomFileDialog.GetFullPath: string;
begin
  if FFileNameText[0] <> #0 then
  begin
    if (FDirPathText[Length(FDirPathText)] = PATH_SEPARATOR) or
       (IsRootPath(FDirPathText) and (FDirPathText <> PATH_SEPARATOR)) then
      Result := GetDirPathText + GetFileNameText
    else
      Result := GetDirPathText + PATH_SEPARATOR + GetFileNameText;
  end
  else
    Result := GetDirPathText;
end;

function TCustomFileDialog.GetFilterExt: string;
begin
  Result := string(FFilterExt);
end;

procedure TCustomFileDialog.SetFilterExt(const Value: string);
begin
  StrLCopy(@FFilterExt[0], PChar(Value), 255);
  // Перезагружаем файлы при изменении фильтра
  if FWindowActive then
    ReloadDirectoryFiles;
end;

end.
