(*******************************************************************************************
*
*   raylib [audio] example - Raw audio streaming
*
*   Example originally created with raylib 1.6, last time updated with raylib 4.2
*
*   Example created by Ramon Santamaria (@raysan5) and reviewed by James Hofmann (@triplefox)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2022 Ramon Santamaria (@raysan5) and James Hofmann (@triplefox)
*
********************************************************************************************)
unit audio_raw_stream;

{$IFDEF FPC}{$MODE DELPHIUNICODE}{$ENDIF}

interface

procedure Main();

implementation

uses
  SysUtils,
  raylib;

const
  MAX_SAMPLES = 512;
  MAX_SAMPLES_PER_UPDATE = 4096;

var
  // Cycles per second (hz)
  Frequency: Single = 440.0;

  // Audio frequency, for smoothing
  AudioFrequency: Single = 440.0;

  // Previous value, used to test if sine needs to be rewritten, and to smoothly modulate frequency
  OldFrequency: Single = 1.0;

  // Index for audio rendering
  SineIdx: Single = 0.0;

{$POINTERMATH ON}

{$RANGECHECKS OFF}

// Audio input processing callback
procedure AudioInputCallback(Buffer: Pointer; Frames: Cardinal); cdecl;
var
  Incr: Single;
  I: Integer;
begin
  AudioFrequency := Frequency + (AudioFrequency - Frequency) * 0.95;
  AudioFrequency := AudioFrequency + 1.0;
  AudioFrequency := AudioFrequency - 1.0;// WTF????
  Incr := AudioFrequency / 44100.0;
  //short *d = (short *)buffer;

  for I := 0 to Integer(Frames) - 1 do
  begin
    PSmallInt(Buffer)[I] := Trunc(32000.0 * Sin(2 * PI * SineIdx));
    SineIdx := SineIdx + Incr;
    if SineIdx > 1.0 then
      SineIdx := SineIdx - 1.0;
  end;
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
procedure Main();
const
  ScreenWidth = 800;
  ScreenHeight = 450;
var
  Stream: TAudioStream;
  Data: array of SmallInt;
  WriteBuf: array of SmallInt;
  MousePosition, Position: TVector2;
  WaveLength, I: Integer;
  Pan, Fp: Single;
begin
  // Initialization
  //---------------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_HIGHDPI);
  InitWindow(ScreenWidth, ScreenHeight, UTF8String('raylib [audio] example - music playing (streaming)'));

  InitAudioDevice();              // Initialize audio device

  SetAudioStreamBufferSizeDefault(MAX_SAMPLES_PER_UPDATE);

  // Init raw audio stream (sample rate: 44100, sample size: 16bit-short, channels: 1-mono)
  Stream := LoadAudioStream(44100, 16, 1);

  SetAudioStreamCallback(Stream, @AudioInputCallback);

  // Buffer for the single cycle waveform we are synthesizing
  SetLength(Data, MAX_SAMPLES);

  // Frame buffer, describing the waveform when repeated over the course of a frame
  SetLength(WriteBuf, MAX_SAMPLES_PER_UPDATE);

  PlayAudioStream(Stream);        // Start processing stream buffer (no data loaded currently)

  // Position read in to determine next frequency
  MousePosition := Vector2Create(-100.0, -100.0);

  Position := Vector2Create(0, 0);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    // Sample mouse input.
    MousePosition := GetMousePosition();

    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      Fp := MousePosition.Y;
      Frequency := 40.0 + Fp;

      Pan := MousePosition.X / ScreenWidth;
      SetAudioStreamPan(Stream, Pan);
    end;

    // Rewrite the sine wave
    // Compute two cycles to allow the buffer padding, simplifying any modulation, resampling, etc.
    if Frequency <> OldFrequency then
    begin
      // Compute wavelength. Limit size in both directions.
      //int oldWavelength = waveLength;
      WaveLength := Trunc(22050 / Frequency);
      if WaveLength > MAX_SAMPLES div 2 then
        WaveLength := MAX_SAMPLES div 2;
      if WaveLength < 1 then
        WaveLength := 1;

      // Write sine wave
      for I := 0 to WaveLength * 2 - 1 do
      begin
        Data[I] := Trunc(Sin(((2 * PI * I / WaveLength))) * 32000);
      end;
      // Make sure the rest of the line is flat
      for I := WaveLength * 2 to MAX_SAMPLES - 1 do
      begin
        Data[I] := 0;
      end;

      // Scale read cursor's position to minimize transition artifacts
      //readCursor = (int)(readCursor * ((float)waveLength / (float)oldWavelength));
      OldFrequency := Frequency;
    end;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText(TextFormat(UTF8String('sine frequency: %i'), Integer(Trunc(Frequency))), GetScreenWidth() - 220, 10, 20, RED);
      DrawText(UTF8String('click mouse button to change frequency or pan'), 10, 10, 20, DARKGRAY);

      // Draw the current buffer state proportionate to the screen
      for I := 0 to ScreenWidth - 1 do
      begin
        Position.X := I;
        Position.Y := 250 + 50 * Data[I * MAX_SAMPLES div ScreenWidth] / 32000.0;

        DrawPixelV(Position, RED);
      end;

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadAudioStream(Stream);   // Close raw audio stream and delete buffers from RAM
  CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)

  CloseWindow(); // Close window and OpenGL context
  //---------------------------------------------------------------------------------------------
end;

end.

