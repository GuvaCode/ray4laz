{*******************************************************************************************
*
*   raylib [audio] example - Raw audio streaming
*
*   This example has been created using raylib 1.6 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example created by Ramon Santamaria (@raysan5) and reviewed by James Hofmann (@triplefox)
*
*   Copyright (c) 2015-2019 Ramon Santamaria (@raysan5) and James Hofmann (@triplefox)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program audio_sound_loading;

{$mode objfpc}{$H+}

uses ray_header,
     cmem, // Required for: malloc(), free()
     math; // Required for: sinf()

const
 screenWidth = 800;
 screenHeight = 450;
 MAX_SAMPLES = 512;
 MAX_SAMPLES_PER_UPDATE = 4096;

var
 stream:TAudioStream;
 data:array of Pointer;
 writeBuf: pointer;
 mousePosition, position : TVector2;
 frequency, Oldfrequency, fp:single;
 readCursor, waveLength, oldWavelength:integer;
 i: integer;
begin
 // Initialization
 InitWindow(screenWidth, screenHeight, 'raylib [audio] example - raw audio streaming');

 InitAudioDevice();              // Initialize audio device

 SetAudioStreamBufferSizeDefault(MAX_SAMPLES_PER_UPDATE);

 // Init raw audio stream (sample rate: 22050, sample size: 16bit-short, channels: 1-mono)
  stream := LoadAudioStream(44100, 16, 1);

 // Buffer for the single cycle waveform we are synthesizing
 data:=malloc(SizeOf(MAX_SAMPLES));
 // Frame buffer, describing the waveform when repeated over the course of a frame
 writeBuf:=malloc(sizeof(MAX_SAMPLES_PER_UPDATE));

 PlayAudioStream(stream);        // Start processing stream buffer (no data loaded currently)

 // Position read in to determine next frequency
 mousePosition := Vector2Create(-100.0, -100.0);


 // Cycles per second (hz)
 frequency := 440.0;

 // Previous value, used to test if sine needs to be rewritten, and to smoothly modulate frequency
 oldFrequency := 1.0;

 // Cursor to read and copy the samples of the sine wave buffer
 readCursor := 0;

 // Computed size in samples of the sine wave
 waveLength := 1;

 position := Vector2Create(0,0);


 // Main game loop
 SetTargetFPS(30);               // Set our game to run at 30 frames-per-second

 while not WindowShouldClose() do // Detect window close button or ESC key
 begin
  // Update
   // Sample mouse input.
   mousePosition := GetMousePosition();

   if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
     begin
       fp := (mousePosition.y);
       frequency:= 40.0 + fp;
     end;

         // Rewrite the sine wave.
        // Compute two cycles to allow the buffer padding, simplifying any modulation, resampling, etc.
        if frequency <> oldFrequency then
        begin
            // Compute wavelength. Limit size in both directions.
            oldWavelength := waveLength;
            waveLength := round(22050/frequency);
            if (waveLength > MAX_SAMPLES/2)  then waveLength := MAX_SAMPLES div 2;
            if (waveLength < 1) then waveLength := 1;

            // Write sine wave.
            for  i:=0 to  waveLength*2 do   //(int i = 0; i < waveLength*2; i++)
            begin
                data[i]^ := sin(2*PI*i/waveLength);//*32000;
            end;

            // Scale read cursor's position to minimize transition artifacts
            readCursor = (int)(readCursor * ((float)waveLength / (float)oldWavelength));
            oldFrequency = frequency;
        end;










  BeginDrawing();
  ClearBackground(RAYWHITE);

  DrawText('raylib in lazarus !!!', 20, 20, 20, SKYBLUE);

  EndDrawing(); 
 end;
CloseWindow(); 

end.

