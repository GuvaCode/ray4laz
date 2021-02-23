program textures_image_generation;

{$MODE objfpc}

uses cmem, ray_header, math;

const
  screenWidth = 800;
  screenHeight = 450;

  NUM_TEXTURES = 7;

var
  verticalGradient,
  horizontalGradient,
  radialGradient,
  checked,
  whiteNoise,
  perlinNoise,
  cellular         : TImage;
  textures         : array [0 .. (NUM_TEXTURES) - 1] of TTexture2D;
  currentTexture, i: integer;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - procedural images generation');

  verticalGradient := GenImageGradientV(screenWidth, screenHeight, RED, BLUE);
  horizontalGradient := GenImageGradientH(screenWidth, screenHeight, RED, BLUE);
  radialGradient := GenImageGradientRadial(screenWidth, screenHeight, 0.0, WHITE, BLACK);
  checked := GenImageChecked(screenWidth, screenHeight, 32, 32, RED, BLUE);
  whiteNoise := GenImageWhiteNoise(screenWidth, screenHeight, 0.5);
  perlinNoise := GenImagePerlinNoise(screenWidth, screenHeight, 50, 50, 4.0);
  cellular := GenImageCellular(screenWidth, screenHeight, 32);

  textures[0] := LoadTextureFromImage(verticalGradient);
  textures[1] := LoadTextureFromImage(horizontalGradient);
  textures[2] := LoadTextureFromImage(radialGradient);
  textures[3] := LoadTextureFromImage(checked);
  textures[4] := LoadTextureFromImage(whiteNoise);
  textures[5] := LoadTextureFromImage(perlinNoise);
  textures[6] := LoadTextureFromImage(cellular);

  UnloadImage(verticalGradient);
  UnloadImage(horizontalGradient);
  UnloadImage(radialGradient);
  UnloadImage(checked);
  UnloadImage(whiteNoise);
  UnloadImage(perlinNoise);
  UnloadImage(cellular);

  currentTexture := 0;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
    begin
      currentTexture := (currentTexture + 1) mod NUM_TEXTURES;
    end;
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawTexture(textures[currentTexture], 0, 0, WHITE);
    DrawRectangle(30, 400, 325, 30, Fade(SKYBLUE, 0.5));
    DrawRectangleLines(30, 400, 325, 30, Fade(WHITE, 0.5));
    DrawText('MOUSE LEFT BUTTON to CYCLE PROCEDURAL TEXTURES', 40, 410,
      10, WHITE);
    case currentTexture of
      0: DrawText('VERTICAL GRADIENT', 560, 10, 20, RAYWHITE);
      1: DrawText('HORIZONTAL GRADIENT', 540, 10, 20, RAYWHITE);
      2: DrawText('RADIAL GRADIENT', 580, 10, 20, LIGHTGRAY);
      3: DrawText('CHECKED', 680, 10, 20, RAYWHITE);
      4: DrawText('WHITE NOISE', 640, 10, 20, RED);
      5: DrawText('PERLIN NOISE', 630, 10, 20, RAYWHITE);
      6: DrawText('CELLULAR', 670, 10, 20, RAYWHITE);
    else
      break;
    end;
    EndDrawing();
  end;
  for i := 0 to NUM_TEXTURES - 1 do
    UnloadTexture(textures[i]);
  CloseWindow();

end.
