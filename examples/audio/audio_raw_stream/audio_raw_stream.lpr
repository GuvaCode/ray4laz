program Game;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;

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

var
  Stream: TAudioStream;
  Data: array of SmallInt;
  WriteBuf: array of SmallInt;
  MousePosition, Position: TVector2;
  WaveLength, I: Integer;
  Pan, Fp: Single;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - music playing (streaming)');
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

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here
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

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);

         DrawText(TextFormat('sine frequency: %i', Integer(Trunc(Frequency))), GetScreenWidth() - 220, 10, 20, RED);
         DrawText('click mouse button to change frequency or pan', 10, 10, 20, DARKGRAY);

         // Draw the current buffer state proportionate to the screen
         for I := 0 to ScreenWidth - 1 do
         begin
           Position.X := I;
           Position.Y := 250 + 50 * Data[I * MAX_SAMPLES div ScreenWidth] / 32000.0;

           DrawPixelV(Position, RED);
         end;


        DrawText('Congrats! You created yaasaour first window!', 190, 200, 20, LIGHTGRAY);
      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

