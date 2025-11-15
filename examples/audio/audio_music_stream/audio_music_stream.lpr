(*******************************************************************************************
*
*   raylib [audio] example - music stream
*
*   Example complexity rating: [★☆☆☆] 1/4
*
*   Example originally created with raylib 1.3, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021-2025 Gunko Vadim (@guvacode)
*
********************************************************************************************)
program audio_music_stream;

{$mode objfpc}{$H+}

uses
  raylib;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  music: TMusic;
  timePlayed: Single;
  pause: Boolean;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - music stream');

  InitAudioDevice();              // Initialize audio device

  music := LoadMusicStream('resources/country.mp3');

  PlayMusicStream(music);

  timePlayed := 0.0;             // Time played normalized [0.0..1.0]
  pause := false;                // Music playing paused

  SetTargetFPS(30);               // Set our game to run at 30 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    UpdateMusicStream(music);   // Update music buffer with new stream data

    // Restart music playing (stop and play)
    if IsKeyPressed(KEY_SPACE) then
    begin
      StopMusicStream(music);
      PlayMusicStream(music);
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

    // Get normalized time played for current music stream
    timePlayed := GetMusicTimePlayed(music) / GetMusicTimeLength(music);

    if timePlayed > 1.0 then
      timePlayed := 1.0;   // Make sure time played is no longer than music
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('MUSIC SHOULD BE PLAYING!', 255, 150, 20, LIGHTGRAY);

      DrawRectangle(200, 200, 400, 12, LIGHTGRAY);
      DrawRectangle(200, 200, Trunc(timePlayed * 400.0), 12, MAROON);
      DrawRectangleLines(200, 200, 400, 12, GRAY);

      DrawText('PRESS SPACE TO RESTART MUSIC', 215, 250, 20, LIGHTGRAY);
      DrawText('PRESS P TO PAUSE/RESUME MUSIC', 208, 280, 20, LIGHTGRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadMusicStream(music);   // Unload music stream buffers from RAM

  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
