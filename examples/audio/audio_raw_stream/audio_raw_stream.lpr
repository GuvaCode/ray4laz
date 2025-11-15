(*******************************************************************************************
*
*   raylib [audio] example - raw stream
*
*   Example complexity rating: [★★★☆] 3/4
*
*   Example originally created with raylib 1.6, last time updated with raylib 4.2
*
*   Example created by Ramon Santamaria (@raysan5) and reviewed by James Hofmann (@triplefox)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2025 Ramon Santamaria (@raysan5) and James Hofmann (@triplefox)
*   Pascal translation 2021-2025 Vadim Gunko (@guvacode)
*
********************************************************************************************)
program audio_raw_stream;

{$mode objfpc}{$H+}

uses
  Math, SysUtils,
  raylib;

const
  MAX_SAMPLES = 512;
  MAX_SAMPLES_PER_UPDATE = 4096;

var
  frequency: Single = 440.0;           // Cycles per second (hz)
  audioFrequency: Single = 440.0;      // Audio frequency, for smoothing
  oldFrequency: Single = 1.0;          // Previous value, used to test if sine needs to be rewritten
  sineIdx: Single = 0.0;               // Index for audio rendering

// Audio input processing callback
procedure AudioInputCallback(buffer: Pointer; frames: LongWord); cdecl;
var
  incr: Single;
  d: PSmallInt;
  i: LongWord;
begin
  audioFrequency := frequency + (audioFrequency - frequency) * 0.95;
  incr := audioFrequency / 44100.0;
  d := PSmallInt(buffer);

  for i := 0 to frames - 1 do
  begin
    d[i] := Trunc(32000.0 * Sin(2 * PI * sineIdx));
    sineIdx := sineIdx + incr;
    if sineIdx > 1.0 then
      sineIdx := sineIdx - 1.0;
  end;
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  stream: TAudioStream;
  data: PSmallInt;
  writeBuf: PSmallInt;
  mousePosition: TVector2;
  waveLength: Integer;
  position: TVector2;
  i, j: Integer;
  fp, pan: Single;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - raw stream');

  InitAudioDevice();              // Initialize audio device

  SetAudioStreamBufferSizeDefault(MAX_SAMPLES_PER_UPDATE);

  // Init raw audio stream (sample rate: 44100, sample size: 16bit-short, channels: 1-mono)
  stream := LoadAudioStream(44100, 16, 1);

  SetAudioStreamCallback(stream, @AudioInputCallback);

  // Buffer for the single cycle waveform we are synthesizing
  data := GetMem(SizeOf(SmallInt) * MAX_SAMPLES);

  // Frame buffer, describing the waveform when repeated over the course of a frame
  writeBuf := GetMem(SizeOf(SmallInt) * MAX_SAMPLES_PER_UPDATE);

  PlayAudioStream(stream);        // Start processing stream buffer (no data loaded currently)

  // Position read in to determine next frequency
  mousePosition.x := -100.0;
  mousePosition.y := -100.0;

  // Computed size in samples of the sine wave
  waveLength := 1;

  position.x := 0;
  position.y := 0;

  SetTargetFPS(30);               // Set our game to run at 30 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    mousePosition := GetMousePosition();

    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      fp := mousePosition.y;
      frequency := 40.0 + fp;

      pan := mousePosition.x / screenWidth;
      SetAudioStreamPan(stream, pan);
    end;

    // Rewrite the sine wave
    // Compute two cycles to allow the buffer padding, simplifying any modulation, resampling, etc.
    if frequency <> oldFrequency then
    begin
      // Compute wavelength. Limit size in both directions
      waveLength := Trunc(22050 / frequency);
      if waveLength > MAX_SAMPLES div 2 then
        waveLength := MAX_SAMPLES div 2;
      if waveLength < 1 then
        waveLength := 1;

      // Write sine wave
      for i := 0 to waveLength * 2 - 1 do
      begin
        data[i] := Trunc(Sin(2 * PI * i / waveLength) * 32000);
      end;

      // Make sure the rest of the line is flat
      for j := waveLength * 2 to MAX_SAMPLES - 1 do
      begin
        data[j] := 0;
      end;

      oldFrequency := frequency;
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText(TextFormat('sine frequency: %i', Trunc(frequency)), GetScreenWidth() - 220, 10, 20, RED);
      DrawText('click mouse button to change frequency or pan', 10, 10, 20, DARKGRAY);

      // Draw the current buffer state proportionate to the screen
      for i := 0 to screenWidth - 1 do
      begin
        position.x := i;
        position.y := 250 + 50 * data[i * MAX_SAMPLES div screenWidth] / 32000.0;

        DrawPixelV(position, RED);
      end;

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  FreeMem(data);                 // Unload sine wave data
  FreeMem(writeBuf);             // Unload write buffer

  UnloadAudioStream(stream);   // Close raw audio stream and delete buffers from RAM
  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
