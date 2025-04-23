program textures_image_processing;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

type
  TImageProcess = (None, ColorGrayscale, ColorTint, ColorInvert, ColorContrast,
                   ColorBrightness, FlipVertical, FlipHorizontal);

const
  screenWidth = 800;
  screenHeight = 450;
  ProcessText: array [TImageProcess] of string = (
     'NO PROCESSING',
     'COLOR GRAYSCALE',
     'COLOR TINT',
     'COLOR INVERT',
     'COLOR CONTRAST',
     'COLOR BRIGHTNESS',
     'FLIP VERTICAL',
     'FLIP HORIZONTAL'
   );

var
  ImOrigin: TImage;
  Texture: TTexture2D;
  ImCopy: TImage;
  CurrentProcess: TImageProcess;
  TextureReload: Boolean;
  ToggleRecs: array [TImageProcess] of TRectangle;
  MouseHoverRec: TImageProcess;
  Item: TImageProcess;
  Pixels: PColor;
  Color: TColor;

begin
   // Initialization
  //---------------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_HIGHDPI);
  InitWindow(ScreenWidth, ScreenHeight, UTF8String('raylib [textures] example - image processing'));

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)

  ImOrigin := LoadImage(PChar(GetApplicationDirectory + 'resources/parrots.png')); // Loaded in CPU memory (RAM)
  ImageFormat(@ImOrigin, PIXELFORMAT_UNCOMPRESSED_R8G8B8A8); // Format image to RGBA 32bit (required for texture update) <-- ISSUE
  Texture := LoadTextureFromImage(ImOrigin); // Image converted to texture, GPU memory (VRAM)

  ImCopy := ImageCopy(ImOrigin);

  CurrentProcess := None;
  TextureReload := False;

  for Item := Low(TImageProcess) to High(TImageProcess) do
    ToggleRecs[Item] := RectangleCreate(40.0, (50 + 32 * Integer(Item)), 150.0, 30.0);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------

    // Mouse toggle group logic
    for Item := Low(TImageProcess) to High(TImageProcess) do
    begin
      if CheckCollisionPointRec(GetMousePosition(), ToggleRecs[Item]) then
      begin
        MouseHoverRec := Item;

        if IsMouseButtonReleased(MOUSE_BUTTON_LEFT) then
        begin
          CurrentProcess := Item;
          TextureReload := True;
        end;
        break;
      end else
        MouseHoverRec := TImageProcess(none);
    end;

    // Keyboard toggle group logic
    if IsKeyPressed(KEY_DOWN) then
    begin
      if CurrentProcess < High(TImageProcess) then
        Inc(CurrentProcess)
      else
        CurrentProcess := Low(TImageProcess);
      TextureReload := True;
    end
    else if IsKeyPressed(KEY_UP) then
    begin
      if CurrentProcess > Low(TImageProcess) then
        Dec(CurrentProcess)
      else
        CurrentProcess := High(TImageProcess);
      TextureReload := True;
    end;

    // Reload texture when required
    if TextureReload then
    begin
      UnloadImage(ImCopy);                // Unload image-copy data
      ImCopy := ImageCopy(ImOrigin);     // Restore image-copy from image-origin

      // NOTE: Image processing is a costly CPU process to be done every frame,
      // If image processing is required in a frame-basis, it should be done
      // with a texture and by shaders
      case CurrentProcess of
        ColorGrayscale: ImageColorGrayscale(@ImCopy);
        ColorTint: ImageColorTint(@ImCopy, GREEN);
        ColorInvert: ImageColorInvert(@ImCopy);
        ColorContrast: ImageColorContrast(@ImCopy, -40);
        ColorBrightness: ImageColorBrightness(@ImCopy, -80);
        FlipVertical: ImageFlipVertical(@ImCopy);
        FlipHorizontal: ImageFlipHorizontal(@ImCopy);
      end;

      Pixels := LoadImageColors(ImCopy);    // Load pixel data from image (RGBA 32bit)
      UpdateTexture(Texture, Pixels);       // Update texture with new image data
      UnloadImageColors(Pixels);            // Unload pixels data from RAM

      TextureReload := False;
    End;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText(UTF8String('IMAGE PROCESSING:'), 40, 30, 10, DARKGRAY);

      // Draw rectangles
      for Item := Low(TImageProcess) to High(TImageProcess) do
      begin
        if (Item = CurrentProcess) or (Item = MouseHoverRec) then
          Color := SKYBLUE
        else
          Color := LIGHTGRAY;
        DrawRectangleRec(ToggleRecs[Item], Color);

        if (Item = CurrentProcess) or (Item = MouseHoverRec) then
          Color := BLUE
        else
          Color := GRAY;
        DrawRectangleLines(Trunc(ToggleRecs[Item].X), Trunc(ToggleRecs[Item].Y), Trunc(ToggleRecs[Item].Width), Trunc(ToggleRecs[Item].Height), Color);

        if (Item = CurrentProcess) or (Item = MouseHoverRec) then
          Color := DARKBLUE
        else
          Color := DARKGRAY;
        DrawText(PAnsiChar(UTF8String(ProcessText[Item])), Trunc(ToggleRecs[Item].X + ToggleRecs[Item].Width / 2 - MeasureText(PAnsiChar(UTF8String(ProcessText[Item])), 10) / 2), Trunc(ToggleRecs[Item].Y + 11), 10, Color);
      end;

      DrawTexture(Texture, ScreenWidth - Texture.Width - 60, ScreenHeight div 2 - Texture.Height div 2, WHITE);
      DrawRectangleLines(ScreenWidth - Texture.Width - 60, ScreenHeight div 2 - Texture.Height div 2, Texture.Width, Texture.Height, BLACK);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(Texture);       // Unload texture from VRAM
  UnloadImage(ImOrigin);        // Unload image-origin from RAM
  UnloadImage(ImCopy);          // Unload image-copy from RAM

  CloseWindow(); // Close window and OpenGL context
end.

