{*******************************************************************************************
*
*   raylib [audio] example - Mixed audio processing
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example contributed by hkc (@hatkidchan) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 hkc (@hatkidchan)
*   Pascal conversion 2023 guvacode(@guvacode)
*
********************************************************************************************}
program audio_mixed_processor;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  exponent: single = 1.0;                  // Audio exponentiation value
  averageVolume: array[0..400] of single;  // Average volume history
  music: TMusic;
  sound: TSound;
  i: integer;
//------------------------------------------------------------------------------------
// Audio processing function
//------------------------------------------------------------------------------------
procedure ProcessAudio(buffer: pointer; frames:LongWord); cdecl;
var samples, left, right: psingle;
    average: single;
    frame: integer;
    i: integer;
begin
  samples:= buffer; // Samples internally stored as <float>s
  average:= 0.0;    // Temporary average volume

  for frame:=0 to frames-1 do
  begin
    left := @samples[frame * 2 + 0];
    right := @samples[frame * 2 + 1];
    if left^ < 0.0 then  left^:=power(abs(left^),exponent) * -1.0
    else
    left^:=power(abs(left^),exponent) * 1.0;

    if right^< 0.0 then right^ :=power(abs(right^),exponent) * -1.0
    else
    right^ :=power(abs(right^),exponent) * 1.0;
    average += abs(left^) / frames;   // accumulating average volume
    average += abs(right^) / frames;
  end;

  // Moving history to the left
  for  i := 0 to 399 do averageVolume[i] := averageVolume[i + 1];
  averageVolume[399] := average;         // Adding last average value
end;


begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'Raylib [audio] example - processing mixed output');
  InitAudioDevice();              // Initialize audio device

  AttachAudioMixedProcessor(@ProcessAudio);

  music := LoadMusicStream(PChar(GetApplicationDirectory + 'resources/country.mp3'));
  sound := LoadSound(PChar(GetApplicationDirectory + 'resources/coin.wav'));

  PlayMusicStream(music);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update

      //----------------------------------------------------------------------------------
      UpdateMusicStream(music);   // Update music buffer with new stream data

      // Modify processing variables
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_LEFT) then exponent -= 0.05;
      if IsKeyPressed(KEY_RIGHT)then exponent += 0.05;

      if exponent <= 0.5 then exponent := 0.5;
      if exponent >= 3.0 then exponent := 3.0;

      if IsKeyPressed(KEY_SPACE) then PlaySound(sound);

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText('MUSIC SHOULD BE PLAYING!', 255, 150, 20, LIGHTGRAY);

        DrawText(TextFormat('EXPONENT = %.2f', exponent), 215, 180, 20, LIGHTGRAY);

        DrawRectangle(199, 199, 402, 34, LIGHTGRAY);

        for i:=0 to 400 do
        begin
        DrawLine(201 + i, Round(232 - averageVolume[i] * 32), 201 + i, 232, MAROON);
        end;
            DrawRectangleLines(199, 199, 402, 34, GRAY);
            DrawText('PRESS SPACE TO PLAY OTHER SOUND', 200, 250, 20, LIGHTGRAY);
            DrawText('USE LEFT AND RIGHT ARROWS TO ALTER DISTORTION', 140, 280, 20, LIGHTGRAY);
      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

