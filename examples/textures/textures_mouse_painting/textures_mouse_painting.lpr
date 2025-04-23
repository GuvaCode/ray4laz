program textures_mouse_painting;

{$mode objfpc}{$H+}

uses 
cmem, raylib, rlgl;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_COLORS_COUNT = 23; // Number of colors available
var
  Colors: array of TColor;
  ColorsRecs: array of TRectangle;
  ColorSelected, ColorSelectedPrev, ColorMouseHover: Integer;
  BrushSize: Single;
  MouseWasPressed: Boolean;
  BtnSaveRec: TRectangle;
  BtnSaveMouseHover, ShowSaveMessage: Boolean;
  SaveMessageCounter: Integer;
  I: Integer;
  Target: TRenderTexture2D;
  Image: TImage;
  MousePos: TVector2;

begin
  // Initialization
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [textures] example - mouse painting');

  // Colors to choose from
  Colors := [
      RAYWHITE, YELLOW, GOLD, ORANGE, PINK, RED, MAROON, GREEN, LIME, DARKGREEN,
      SKYBLUE, BLUE, DARKBLUE, PURPLE, VIOLET, DARKPURPLE, BEIGE, BROWN, DARKBROWN,
      LIGHTGRAY, GRAY, DARKGRAY, BLACK];

  // Define colorsRecs data (for every rectangle)
  SetLength(ColorsRecs, MAX_COLORS_COUNT);

  for I := 0 to MAX_COLORS_COUNT - 1 do
  begin
    ColorsRecs[I].X := 10 + 30.0 * I + 2 * I;
    ColorsRecs[I].Y := 10;
    ColorsRecs[I].Width := 30;
    ColorsRecs[I].Height := 30;
  end;

  ColorSelected := 0;
  ColorSelectedPrev := ColorSelected;
  ColorMouseHover := 0;
  BrushSize := 20.0;
  MouseWasPressed := False;

  BtnSaveRec := RectangleCreate(750, 10, 40, 30);
  ShowSaveMessage := False;
  SaveMessageCounter := 0;

  // Create a RenderTexture2D to use as a canvas
  Target := LoadRenderTexture(ScreenWidth, ScreenHeight);

  // Clear render texture before entering the game loop
  BeginTextureMode(Target);
  ClearBackground(Colors[0]);
  EndTextureMode();

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    MousePos := GetMousePosition();

    // Move between colors with keys
    if IsKeyPressed(KEY_RIGHT) then
      Inc(ColorSelected)
    else if IsKeyPressed(KEY_LEFT) then
      Dec(ColorSelected);

    if ColorSelected >= MAX_COLORS_COUNT then
      ColorSelected := MAX_COLORS_COUNT - 1
    else if ColorSelected < 0 then
      ColorSelected := 0;

    // Choose color with mouse
    for I := 0 to MAX_COLORS_COUNT - 1 do
    begin
      if CheckCollisionPointRec(MousePos, ColorsRecs[I]) then
      begin
        ColorMouseHover := i;
        break;
      end else
        Dec(ColorMouseHover);
    end;

    if (ColorMouseHover >= 0) and IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
    begin
      ColorSelected := ColorMouseHover;
      ColorSelectedPrev := ColorSelected;
    end;

    // Change brush size
    BrushSize := BrushSize + GetMouseWheelMove() * 5;
    if BrushSize < 2 then BrushSize := 2;
    if BrushSize > 50 then BrushSize := 50;

    if IsKeyPressed(KEY_C) then
    begin
      // Clear render texture to clear color
      BeginTextureMode(Target);
      ClearBackground(Colors[0]);
      EndTextureMode();
    end;

    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) or (GetGestureDetected() = GESTURE_DRAG) then
    begin
      // Paint circle into render texture
      // NOTE: To avoid discontinuous circles, we could store
      // previous-next mouse points and just draw a line using brush size
      BeginTextureMode(Target);
      if MousePos.Y > 50 then
        DrawCircle(Trunc(MousePos.X), Trunc(MousePos.Y), BrushSize, Colors[ColorSelected]);
      EndTextureMode();
    end;


    if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
    begin
      if not MouseWasPressed then
      begin
        ColorSelectedPrev := ColorSelected;
        ColorSelected := 0;
      end;

      MouseWasPressed := True;

      // Erase circle from render texture
      BeginTextureMode(Target);
      if MousePos.Y > 50 then
        DrawCircle(Trunc(MousePos.X), Trunc(MousePos.Y), BrushSize, Colors[0]);
      EndTextureMode();
    end
    else if IsMouseButtonReleased(MOUSE_BUTTON_RIGHT) and MouseWasPressed then
    begin
      ColorSelected := ColorSelectedPrev;
      MouseWasPressed := False;
    end;

    // Check mouse hover save button
    if CheckCollisionPointRec(MousePos, BtnSaveRec) then
      BtnSaveMouseHover := True
    else
      BtnSaveMouseHover := False;

    // Image saving logic
    // NOTE: Saving painted texture to a default named image
    if (BtnSaveMouseHover and IsMouseButtonReleased(MOUSE_BUTTON_LEFT)) or IsKeyPressed(KEY_S) then
    begin
      Image := LoadImageFromTexture(Target.Texture);
      ImageFlipVertical(@Image);
      ExportImage(Image, UTF8String('my_amazing_texture_painting.png'));
      UnloadImage(Image);
      ShowSaveMessage := True;
    end;

    if ShowSaveMessage then
    begin
      // On saving, show a full screen message for 2 seconds
      Inc(SaveMessageCounter);
      if SaveMessageCounter > 240 then
      begin
        ShowSaveMessage := False;
        SaveMessageCounter := 0;
      end;
    end;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

    ClearBackground(RAYWHITE);

    // NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
    DrawTextureRec(Target.Texture, RectangleCreate(0, 0, Target.Texture.Width, -Target.Texture.Height), Vector2Create(0, 0), WHITE);

    // Draw drawing circle for reference
    if MousePos.Y > 50 then
    begin
      if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
        DrawCircleLines(Trunc(MousePos.X), Trunc(MousePos.Y), BrushSize, GRAY)
      else
        DrawCircle(GetMouseX(), GetMouseY(), brushSize, Colors[ColorSelected]);
    end;

    // Draw top panel
    DrawRectangle(0, 0, GetScreenWidth(), 50, RAYWHITE);
    DrawLine(0, 50, GetScreenWidth(), 50, LIGHTGRAY);

    // Draw color selection rectangles
    for I := 0 to MAX_COLORS_COUNT - 1 do
      DrawRectangleRec(ColorsRecs[I], Colors[I]);
    DrawRectangleLines(10, 10, 30, 30, LIGHTGRAY);

    if ColorMouseHover >= 0 then
      DrawRectangleRec(ColorsRecs[ColorMouseHover], Fade(WHITE, 0.6));

    DrawRectangleLinesEx(RectangleCreate(ColorsRecs[ColorSelected].X - 2, ColorsRecs[ColorSelected].Y - 2,
                         ColorsRecs[ColorSelected].Width + 4, ColorsRecs[ColorSelected].Height + 4), 2, BLACK);

    // Draw save image button
    if BtnSaveMouseHover then
      DrawRectangleLinesEx(BtnSaveRec, 2, RED)
    else
      DrawRectangleLinesEx(BtnSaveRec, 2, BLACK);

    if BtnSaveMouseHover then
      DrawText('SAVE!', 755, 20, 10, RED)
    else
      DrawText('SAVE!', 755, 20, 10, BLACK);

    // Draw save image message
    if ShowSaveMessage then
    begin
      DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), Fade(RAYWHITE, 0.8));
      DrawRectangle(0, 150, GetScreenWidth(), 80, BLACK);
      DrawText('IMAGE SAVED:  my_amazing_texture_painting.png', 150, 180, 20, RAYWHITE);
    end;

    EndDrawing();

    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadRenderTexture(Target); // Texture unloading

  CloseWindow(); // Close window and OpenGL context
end.

