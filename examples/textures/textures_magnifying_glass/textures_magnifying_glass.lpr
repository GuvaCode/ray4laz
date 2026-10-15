program textures_magnifying_glass;

{$mode objfpc}{$H+}

uses cmem, raylib, raymath, rlgl;

const
  screenWidth = 800;
  screenHeight = 450;

var
  bunny, parrots: TTexture2D;
  circle: TImage;
  mask: TTexture2D;
  magnifiedWorld: TRenderTexture2D;
  camera: TCamera2D;
  mPos: TVector2;
  rx, ry: Single;
begin
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - magnifying glass');

  bunny := LoadTexture(PChar(GetApplicationDirectory + 'resources/raybunny.png'));
  parrots := LoadTexture(PChar(GetApplicationDirectory + 'resources/parrots.png'));

  circle := GenImageColor(256, 256, BLANK);
  ImageDrawCircle(@circle, 128, 128, 128, WHITE);
  mask := LoadTextureFromImage(circle);
  UnloadImage(circle);

  magnifiedWorld := LoadRenderTexture(256, 256);

  camera.zoom := 2;
  camera.offset := Vector2Multiply(Vector2Create( 128, 128 ), GetWindowScaleDPI());
  camera.target := Vector2Create(0, 0);
  camera.rotation := 0;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    mPos := GetMousePosition();
    camera.target := mPos;

    BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawTexture(parrots, 144, 33, WHITE);
      DrawText('Use the magnifying glass to find hidden bunnies!', 154, 6, 20, BLACK);

      BeginTextureMode(magnifiedWorld);
        ClearBackground(RAYWHITE);

        BeginMode2D(camera);
          DrawTexture(parrots, 144, 33, WHITE);
          DrawText('Use the magnifying glass to find hidden bunnies!', 154, 6, 20, BLACK);

          BeginBlendMode(BLEND_MULTIPLIED);
            DrawTexture(bunny, 250, 350, WHITE);
            DrawTexture(bunny, 500, 100, WHITE);
            DrawTexture(bunny, 420, 300, WHITE);
            DrawTexture(bunny, 650, 10, WHITE);
          EndBlendMode();
        EndMode2D();

        BeginBlendMode(BLEND_CUSTOM_SEPARATE);
          rlSetBlendFactorsSeparate(RL_ZERO, RL_ONE, RL_ONE, RL_ZERO, RL_FUNC_ADD, RL_FUNC_ADD);
          DrawTexture(mask, 0, 0, WHITE);
        EndBlendMode();
      EndTextureMode();

      DrawTextureRec(magnifiedWorld.texture, RectangleCreate(0, 0, 256, -256),
        Vector2Create(mPos.x - 128, mPos.y - 128), WHITE);

      DrawRing(mPos, 126, 130, 0, 360, 64, BLACK);

      rx := mPos.x / 800;
      ry := mPos.y / 800;
      DrawCircle(Round(mPos.x - 64 * rx) - 32, Round(mPos.y - 64 * ry) - 32, 4, ColorAlpha(WHITE, 0.5));

    EndDrawing();
  end;

  UnloadTexture(parrots);
  UnloadTexture(bunny);
  UnloadTexture(mask);
  UnloadRenderTexture(magnifiedWorld);
  CloseWindow();
end.
