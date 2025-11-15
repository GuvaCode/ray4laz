{*******************************************************************************************
*
*   raylib [audio] example - Module playing (streaming)
*
*   This example has been created using raylib 1.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2016 Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021-2024 Gunko Vadim (@guvacode)
********************************************************************************************}
program audio_module_playing;

{$mode objfpc}{$H+}

uses
  raylib;

const
  MAX_CIRCLES = 64;

type
  TCircleWave = record
    position: TVector2;
    radius: Single;
    alpha: Single;
    speed: Single;
    color: TColor;
  end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  colors: array[0..13] of TColor;
  circles: array[0..MAX_CIRCLES-1] of TCircleWave;
  music: TMusic;
  pitch, timePlayed: Single;
  pause: Boolean;
  i: Integer;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  SetConfigFlags(FLAG_MSAA_4X_HINT);  // NOTE: Try to enable MSAA 4X

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - module playing');

  InitAudioDevice();                  // Initialize audio device

  // Define colors
  colors[0] := ORANGE;
  colors[1] := RED;
  colors[2] := GOLD;
  colors[3] := LIME;
  colors[4] := BLUE;
  colors[5] := VIOLET;
  colors[6] := BROWN;
  colors[7] := LIGHTGRAY;
  colors[8] := PINK;
  colors[9] := YELLOW;
  colors[10] := GREEN;
  colors[11] := SKYBLUE;
  colors[12] := PURPLE;
  colors[13] := BEIGE;

  // Initialize circles
  for i := 0 to MAX_CIRCLES - 1 do
  begin
    circles[i].alpha := 0.0;
    circles[i].radius := GetRandomValue(10, 40);
    circles[i].position.x := GetRandomValue(Trunc(circles[i].radius), screenWidth - Trunc(circles[i].radius));
    circles[i].position.y := GetRandomValue(Trunc(circles[i].radius), screenHeight - Trunc(circles[i].radius));
    circles[i].speed := GetRandomValue(1, 100) / 2000.0;
    circles[i].color := colors[GetRandomValue(0, 13)];
  end;

  music := LoadMusicStream('resources/mini1111.xm');
  music.looping := false;
  pitch := 1.0;

  PlayMusicStream(music);

  timePlayed := 0.0;
  pause := false;

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    UpdateMusicStream(music);      // Update music buffer with new stream data

    // Restart music playing (stop and play)
    if IsKeyPressed(KEY_SPACE) then
    begin
      StopMusicStream(music);
      PlayMusicStream(music);
      pause := false;
    end;

    // Pause/Resume music playing
    if IsKeyPressed(KEY_P) then
    begin
      pause := not pause;

      if pause then
        PauseMusicStream(music)
      else
        ResumeMusicStream(music);
    end;

    if IsKeyDown(KEY_DOWN) then
      pitch := pitch - 0.01
    else if IsKeyDown(KEY_UP) then
      pitch := pitch + 0.01;

    SetMusicPitch(music, pitch);

    // Get timePlayed scaled to bar dimensions
    timePlayed := GetMusicTimePlayed(music) / GetMusicTimeLength(music) * (screenWidth - 40);

    // Color circles animation
    if not pause then
    begin
      for i := MAX_CIRCLES - 1 downto 0 do
      begin
        circles[i].alpha := circles[i].alpha + circles[i].speed;
        circles[i].radius := circles[i].radius + circles[i].speed * 10.0;

        if circles[i].alpha > 1.0 then
          circles[i].speed := circles[i].speed * -1;

        if circles[i].alpha <= 0.0 then
        begin
          circles[i].alpha := 0.0;
          circles[i].radius := GetRandomValue(10, 40);
          circles[i].position.x := GetRandomValue(Trunc(circles[i].radius), screenWidth - Trunc(circles[i].radius));
          circles[i].position.y := GetRandomValue(Trunc(circles[i].radius), screenHeight - Trunc(circles[i].radius));
          circles[i].color := colors[GetRandomValue(0, 13)];
          circles[i].speed := GetRandomValue(1, 100) / 2000.0;
        end;
      end;
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      for i := MAX_CIRCLES - 1 downto 0 do
      begin
        DrawCircleV(circles[i].position, circles[i].radius, Fade(circles[i].color, circles[i].alpha));
      end;

      // Draw time bar
      DrawRectangle(20, screenHeight - 20 - 12, screenWidth - 40, 12, LIGHTGRAY);
      DrawRectangle(20, screenHeight - 20 - 12, Trunc(timePlayed), 12, MAROON);
      DrawRectangleLines(20, screenHeight - 20 - 12, screenWidth - 40, 12, GRAY);

      // Draw help instructions
      DrawRectangle(20, 20, 425, 145, WHITE);
      DrawRectangleLines(20, 20, 425, 145, GRAY);
      DrawText('PRESS SPACE TO RESTART MUSIC', 40, 40, 20, BLACK);
      DrawText('PRESS P TO PAUSE/RESUME', 40, 70, 20, BLACK);
      DrawText('PRESS UP/DOWN TO CHANGE SPEED', 40, 100, 20, BLACK);
      DrawText(TextFormat('SPEED: %.2f', pitch), 40, 130, 20, MAROON);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadMusicStream(music);          // Unload music stream buffers from RAM

  CloseAudioDevice();     // Close audio device (music streaming is automatically stopped)

  CloseWindow();          // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
