(*******************************************************************************************
*
*   raylib [audio] example - stream effects
*
*   Example complexity rating: [★★★★] 4/4
*
*   Example originally created with raylib 4.2, last time updated with raylib 5.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2022-2025 Ramon Santamaria (@raysan5)
*   Pascal translation (c) 2022-2025 Vadim Gunko(@GuvaCode);
*
********************************************************************************************)

program audio_stream_effects;

{$mode objfpc}{$H+}

uses
  raylib;

//----------------------------------------------------------------------------------
// Global Variables Definition
//----------------------------------------------------------------------------------
var
  delayBuffer: PSingle = nil;
  delayBufferSize: LongWord = 0;
  delayReadIndex: LongWord = 2;
  delayWriteIndex: LongWord = 0;
  lpfState: array[0..1] of Single = (0.0, 0.0);  // Перемещено сюда

  // Audio effect: lowpass filter
  procedure AudioProcessEffectLPF(buffer: Pointer; frames: LongWord); cdecl;
  var
    cutoff, k: Single;
    bufferData: PSingle;
    i: LongWord;
    l, r: Single;
  begin
    cutoff := 70.0 / 44100.0; // 70 Hz lowpass filter
    k := cutoff / (cutoff + 0.1591549431); // RC filter formula

    // Converts the buffer data before using it
    bufferData := PSingle(buffer);
    i := 0;
    while i < frames * 2 do
    begin
      l := bufferData[i];
      r := bufferData[i + 1];

      lpfState[0] := lpfState[0] + k * (l - lpfState[0]);
      lpfState[1] := lpfState[1] + k * (r - lpfState[1]);

      bufferData[i] := lpfState[0];
      bufferData[i + 1] := lpfState[1];

      i := i + 2;
    end;
  end;

  // Audio effect: delay
  procedure AudioProcessEffectDelay(buffer: Pointer; frames: LongWord); cdecl;
  var
    bufferData: PSingle;
    i: LongWord;
    leftDelay, rightDelay: Single;
  begin
    bufferData := PSingle(buffer);
    i := 0;
    while i < frames * 2 do
    begin
      leftDelay := delayBuffer[delayReadIndex];
      delayReadIndex := delayReadIndex + 1;
      rightDelay := delayBuffer[delayReadIndex];
      delayReadIndex := delayReadIndex + 1;

      if delayReadIndex >= delayBufferSize then
        delayReadIndex := 0;

      bufferData[i] := 0.5 * bufferData[i] + 0.5 * leftDelay;
      bufferData[i + 1] := 0.5 * bufferData[i + 1] + 0.5 * rightDelay;

      delayBuffer[delayWriteIndex] := bufferData[i];
      delayWriteIndex := delayWriteIndex + 1;
      delayBuffer[delayWriteIndex] := bufferData[i + 1];
      delayWriteIndex := delayWriteIndex + 1;

      if delayWriteIndex >= delayBufferSize then
        delayWriteIndex := 0;

      i := i + 2;
    end;
  end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  music: TMusic;
  timePlayed: Single;
  pause: Boolean;
  enableEffectLPF, enableEffectDelay: Boolean;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - stream effects');

  InitAudioDevice();              // Initialize audio device

  music := LoadMusicStream('resources/country.mp3');

  // Allocate buffer for the delay effect
  delayBufferSize := 48000 * 2;      // 1 second delay (device sampleRate*channels)
  delayBuffer := GetMem(delayBufferSize * SizeOf(Single));
  FillChar(delayBuffer^, delayBufferSize * SizeOf(Single), 0);

  PlayMusicStream(music);

  timePlayed := 0.0;             // Time played normalized [0.0..1.0]
  pause := false;                // Music playing paused

  enableEffectLPF := false;      // Enable effect low-pass-filter
  enableEffectDelay := false;    // Enable effect delay (1 second)

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
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

    // Add/Remove effect: lowpass filter
    if IsKeyPressed(KEY_F) then
    begin
      enableEffectLPF := not enableEffectLPF;
      if enableEffectLPF then
        AttachAudioStreamProcessor(music.stream, @AudioProcessEffectLPF)
      else
        DetachAudioStreamProcessor(music.stream, @AudioProcessEffectLPF);
    end;

    // Add/Remove effect: delay
    if IsKeyPressed(KEY_D) then
    begin
      enableEffectDelay := not enableEffectDelay;
      if enableEffectDelay then
        AttachAudioStreamProcessor(music.stream, @AudioProcessEffectDelay)
      else
        DetachAudioStreamProcessor(music.stream, @AudioProcessEffectDelay);
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

      DrawText('MUSIC SHOULD BE PLAYING!', 245, 150, 20, LIGHTGRAY);

      DrawRectangle(200, 180, 400, 12, LIGHTGRAY);
      DrawRectangle(200, 180, Trunc(timePlayed * 400.0), 12, MAROON);
      DrawRectangleLines(200, 180, 400, 12, GRAY);

      DrawText('PRESS SPACE TO RESTART MUSIC', 215, 230, 20, LIGHTGRAY);
      DrawText('PRESS P TO PAUSE/RESUME MUSIC', 208, 260, 20, LIGHTGRAY);

      if enableEffectLPF then
        DrawText('PRESS F TO TOGGLE LPF EFFECT: ON', 200, 320, 20, GRAY)
      else
        DrawText('PRESS F TO TOGGLE LPF EFFECT: OFF', 200, 320, 20, GRAY);

      if enableEffectDelay then
        DrawText('PRESS D TO TOGGLE DELAY EFFECT: ON', 180, 350, 20, GRAY)
      else
        DrawText('PRESS D TO TOGGLE DELAY EFFECT: OFF', 180, 350, 20, GRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadMusicStream(music);   // Unload music stream buffers from RAM

  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  FreeMem(delayBuffer);       // Free delay buffer

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

