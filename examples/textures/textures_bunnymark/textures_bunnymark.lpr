program textures_bunnymark;

{$mode objfpc}{$H+}

uses
  cmem, sysutils, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_BUNNIES = 80000;          // 80K bunnies limit
  MAX_BATCH_ELEMENTS = 8192;

type
  TBunny = record
    position: TVector2;
    speed: TVector2;
    color: TColorB;
  end;

var
  bunnies: array[0..MAX_BUNNIES - 1] of TBunny;  // Bunnies array
  bunniesCount: Integer;
  texBunny: TTexture2D;
  i: Integer;
  paused: Boolean;

begin
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - bunnymark');

  // Load bunny texture
  texBunny := LoadTexture(PChar(GetApplicationDirectory + 'resources/raybunny.png'));

  bunniesCount := 0;
  paused := False;
  SetTargetFPS(0);               // Set our game to run with an uncapped framerate

  // Main game loop
  while not WindowShouldClose() do
  begin
    // Update
    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      // Create more bunnies
      for i := 0 to 99 do
      begin
        if bunniesCount < MAX_BUNNIES then
        begin
          bunnies[bunniesCount].position := GetMousePosition();
          bunnies[bunniesCount].speed.x := GetRandomValue(-250, 250);
          bunnies[bunniesCount].speed.y := GetRandomValue(-250, 250);
          bunnies[bunniesCount].color := ColorCreate(
            GetRandomValue(50, 240),
            GetRandomValue(80, 240),
            GetRandomValue(100, 240),
            255
          );
          Inc(bunniesCount);
        end;
      end;
    end;

    if IsKeyPressed(KEY_P) then paused := not paused;

    if not paused then
    begin
      // Update bunnies
      for i := 0 to bunniesCount - 1 do
      begin
        bunnies[i].position.x := bunnies[i].position.x + bunnies[i].speed.x * GetFrameTime();
        bunnies[i].position.y := bunnies[i].position.y + bunnies[i].speed.y * GetFrameTime();

        if ((bunnies[i].position.x + texBunny.width / 2) > GetScreenWidth()) or
           ((bunnies[i].position.x + texBunny.width / 2) < 0) then
          bunnies[i].speed.x := -bunnies[i].speed.x;

        if ((bunnies[i].position.y + texBunny.height / 2) > GetScreenHeight()) or
           ((bunnies[i].position.y + texBunny.height / 2 - 40) < 0) then
          bunnies[i].speed.y := -bunnies[i].speed.y;
      end;
    end;

    // Draw
    BeginDrawing();
      ClearBackground(RAYWHITE);

      for i := 0 to bunniesCount - 1 do
      begin
        // NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
        // a draw call is launched and buffer starts being filled again
        DrawTexture(texBunny,
          Round(bunnies[i].position.x),
          Round(bunnies[i].position.y),
          bunnies[i].color
        );
      end;

      DrawRectangle(0, 0, screenWidth, 40, BLACK);
      DrawText(PChar(Format('bunnies: %d', [bunniesCount])), 120, 10, 20, GREEN);
      DrawText(PChar(Format('batched draw calls: %d', [1 + bunniesCount div MAX_BATCH_ELEMENTS])), 320, 10, 20, MAROON);

      DrawFPS(10, 10);

    EndDrawing();
  end;

  // De-Initialization
  UnloadTexture(texBunny);
  CloseWindow();
end.
