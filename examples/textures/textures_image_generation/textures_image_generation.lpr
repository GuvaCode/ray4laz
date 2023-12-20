program textures_image_generation;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  NUM_TEXTURES = 6; // Currently we have 7 generation algorithms

var
  VerticalGradient, HorizontalGradient, RadialGradient, Checked, WhiteNoise, Cellular: TImage;
  Textures: array [0..NUM_TEXTURES-1] of TTexture2D;
  CurrentTexture, I: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - procedural images generation');
  VerticalGradient := GenImageGradientLinear(ScreenWidth, ScreenHeight,0, RED, BLUE);
  HorizontalGradient := GenImageGradientLinear(ScreenWidth, ScreenHeight,90, RED, BLUE);
  RadialGradient := GenImageGradientRadial(ScreenWidth, ScreenHeight, 0.0, WHITE, BLACK);
  Checked := GenImageChecked(ScreenWidth, ScreenHeight, 32, 32, RED, BLUE);
  WhiteNoise := GenImageWhiteNoise(ScreenWidth, ScreenHeight, 0.5);
  Cellular := GenImageCellular(ScreenWidth, ScreenHeight, 32);

  Textures[0] := LoadTextureFromImage(VerticalGradient);
  Textures[1] := LoadTextureFromImage(HorizontalGradient);
  Textures[2] := LoadTextureFromImage(RadialGradient);
  Textures[3] := LoadTextureFromImage(Checked);
  Textures[4] := LoadTextureFromImage(WhiteNoise);
  Textures[5] := LoadTextureFromImage(Cellular);

  // Unload image data (CPU RAM)
  UnloadImage(VerticalGradient);
  UnloadImage(HorizontalGradient);
  UnloadImage(RadialGradient);
  UnloadImage(Checked);
  UnloadImage(WhiteNoise);
  UnloadImage(Cellular);

  CurrentTexture := 0;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) or IsKeyPressed(KEY_RIGHT) then
         CurrentTexture := (CurrentTexture + 1) mod  NUM_TEXTURES; // Cycle between the textures
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       DrawTexture(Textures[CurrentTexture], 0, 0, WHITE);

       DrawRectangle(30, 400, 325, 30, Fade(SKYBLUE, 0.5));
       DrawRectangleLines(30, 400, 325, 30, Fade(WHITE, 0.5));
       DrawText('MOUSE LEFT BUTTON to CYCLE PROCEDURAL TEXTURES', 40, 410, 10, WHITE);

       case CurrentTexture of
         0: DrawText('VERTICAL GRADIENT', 560, 10, 20, RAYWHITE);
         1: DrawText('HORIZONTAL GRADIENT', 540, 10, 20, RAYWHITE);
         2: DrawText('RADIAL GRADIENT', 580, 10, 20, LIGHTGRAY);
         3: DrawText('CHECKED', 680, 10, 20, RAYWHITE);
         4: DrawText('WHITE NOISE', 640, 10, 20, RED);
         5: DrawText('CELLULAR', 670, 10, 20, RAYWHITE);
       end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  // Unload textures data (GPU VRAM)
  for I := 0 to NUM_TEXTURES - 1 do
    UnloadTexture(Textures[I]);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

