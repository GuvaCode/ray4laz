(*******************************************************************************************
*
*   raylib [audio] example - mixed processor
*
*   Example complexity rating: [★★★★] 4/4
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example contributed by hkc (@hatkidchan) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023-2025 hkc (@hatkidchan)
*   Pascal tranlation 2025 (@GuvaCode)
*
********************************************************************************************)
program audio_mixed_processor;

{$mode objfpc}{$H+}

uses
  Math, SysUtils,
  raylib;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 450;

var
  exponent: Single = 1.0;                    // Audio exponentiation value
  averageVolume: array[0..399] of Single;    // Average volume history

//------------------------------------------------------------------------------------
// Audio processing function
//------------------------------------------------------------------------------------
procedure ProcessAudio(buffer: Pointer; frames: LongWord); cdecl;
var
  samples: PSingle;
  frame: LongWord;
  left, right: PSingle;
  average: Single;
  i: Integer;
begin
  samples := PSingle(buffer);   // Samples internally stored as <float>s
  average := 0.0;               // Temporary average volume

  for frame := 0 to frames - 1 do
  begin
    left := @samples[frame * 2 + 0];
    right := @samples[frame * 2 + 1];

    if left^ >= 0 then
      left^ := Power(abs(left^), exponent)
    else
      left^ := -Power(abs(left^), exponent);

    if right^ >= 0 then
      right^ := Power(abs(right^), exponent)
    else
      right^ := -Power(abs(right^), exponent);

    average := average + abs(left^) / frames;   // accumulating average volume
    average := average + abs(right^) / frames;
  end;

  // Moving history to the left
  for i := 0 to 398 do
    averageVolume[i] := averageVolume[i + 1];

  averageVolume[399] := average;         // Adding last average value
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  music: TMusic;
  sound: TSound;
  i: Integer;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  // Initialize average volume array
  for i := 0 to 399 do
    averageVolume[i] := 0.0;

  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, 'raylib [audio] example - mixed processor');

  InitAudioDevice();              // Initialize audio device

  AttachAudioMixedProcessor(@ProcessAudio);

  music := LoadMusicStream('resources/country.mp3');
  sound := LoadSound('resources/coin.wav');

  PlayMusicStream(music);

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    UpdateMusicStream(music);   // Update music buffer with new stream data

    // Modify processing variables
    //----------------------------------------------------------------------------------
    if IsKeyPressed(KEY_LEFT) then exponent := exponent - 0.05;
    if IsKeyPressed(KEY_RIGHT) then exponent := exponent + 0.05;

    if exponent <= 0.5 then exponent := 0.5;
    if exponent >= 3.0 then exponent := 3.0;

    if IsKeyPressed(KEY_SPACE) then PlaySound(sound);

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('MUSIC SHOULD BE PLAYING!', 255, 150, 20, LIGHTGRAY);

      DrawText(TextFormat('EXPONENT = %.2f', exponent), 215, 180, 20, LIGHTGRAY);

      DrawRectangle(199, 199, 402, 34, LIGHTGRAY);
      for i := 0 to 399 do
      begin
        DrawLine(201 + i, 232 - Trunc(averageVolume[i] * 32), 201 + i, 232, MAROON);
      end;
      DrawRectangleLines(199, 199, 402, 34, GRAY);

      DrawText('PRESS SPACE TO PLAY OTHER SOUND', 200, 250, 20, LIGHTGRAY);
      DrawText('USE LEFT AND RIGHT ARROWS TO ALTER DISTORTION', 140, 280, 20, LIGHTGRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadMusicStream(music);   // Unload music stream buffers from RAM

  DetachAudioMixedProcessor(@ProcessAudio);  // Disconnect audio processor

  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
