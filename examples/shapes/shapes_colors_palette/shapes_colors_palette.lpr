program shapes_colors_palette;

{$mode objfpc}{$H+}

uses 
cmem, raylib, Math;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_COLORS_COUNT = 21; // Number of colors available
var
  Colors: array of TColor;
  ColorNames: array of string;
  ColorsRecs: array [0..MAX_COLORS_COUNT-1] of TRectangle;
  ColorState: array [0..MAX_COLORS_COUNT-1] of Boolean;
  I: Integer;
  MousePoint: TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [shapes] example - colors palette');

  Colors := [
    DARKGRAY, MAROON, ORANGE, DARKGREEN, DARKBLUE, DARKPURPLE, DARKBROWN,
    GRAY, RED, GOLD, LIME, BLUE, VIOLET, BROWN, LIGHTGRAY, PINK, YELLOW,
    GREEN, SKYBLUE, PURPLE, BEIGE];

  ColorNames := [
    'DARKGRAY', 'MAROON', 'ORANGE', 'DARKGREEN', 'DARKBLUE', 'ARKPURPLE', 'DARKBROWN',
    'GRAY', 'RED', 'GOLD', 'LIME', 'BLUE', 'VIOLET', 'BROWN', 'LIGHTGRAY', 'PINK', 'YELLOW',
    'GREEN', 'SKYBLUE', 'PURPLE', 'BEIGE'];

  for I := 0 to High(Colors) do
  begin
    ColorsRecs[I].X := 20 + 100 * (I mod 7) + 10 * (I mod 7);
    ColorsRecs[I].Y := 80 + 100 * (I div 7) + 10 * (I div 7);
    ColorsRecs[I].Width := 100;
    ColorsRecs[I].Height := 100;
  end;

  FillChar(ColorState, SizeOf(ColorState), False);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      MousePoint := GetMousePosition();

      for I := 0 to High(Colors) do
        ColorState[I] := CheckCollisionPointRec(MousePoint, ColorsRecs[I]);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

        ClearBackground(RAYWHITE);

        DrawText('raylib colors palette', 28, 42, 20, BLACK);
        DrawText('press SPACE to see all colors', GetScreenWidth() - 180, GetScreenHeight() - 40, 10, GRAY);

        for I := 0 to High(Colors) do // Draw all rectangles
        begin
          DrawRectangleRec(ColorsRecs[I], Fade(Colors[I], IfThen(ColorState[i], 0.6, 1.0)));
          //DrawRectangleRec(colorsRecs[i], Fade(colors[i], colorState[i]? 0.6f : 1.0f));
          if IsKeyDown(KEY_SPACE) or ColorState[I] then
          begin
            DrawRectangle(Trunc(ColorsRecs[I].X), Trunc(ColorsRecs[I].Y + ColorsRecs[I].Height - 26), Trunc(ColorsRecs[I].Width), 20, BLACK);
            DrawRectangleLinesEx(ColorsRecs[I], 6, Fade(BLACK, 0.3));
            DrawText(PChar(ColorNames[I]), Trunc(ColorsRecs[I].X + ColorsRecs[I].Width - MeasureText(PChar(colorNames[i]), 10) - 12),
              Trunc(ColorsRecs[I].Y + ColorsRecs[I].Height - 20), 10, Colors[I]);
          end;
        end;

      EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

