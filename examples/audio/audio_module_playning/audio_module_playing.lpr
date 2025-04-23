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

uses raylib, math;

const
  MAX_CIRCLES = 16;
  screenWidth = 635;
  screenHeight = 450;

  colors:  array [0..13] of TColorB =(
  (r: 255; g: 161; b: 0; a: 255),(r: 230; g: 41; b: 55; a: 255),
  (r: 255; g: 203; b: 0; a: 255),(r: 0; g: 158; b: 47; a: 255),
  (r: 0; g: 121; b: 241; a: 255),(r: 135; g: 60; b: 190; a: 255),
  (r: 211; g: 176; b: 131; a: 255),(r: 200; g: 200; b: 200; a: 255),
  (r: 255; g: 109; b: 194; a: 255),(r: 253; g: 249; b: 0; a: 255),
  (r: 0; g: 228; b: 48; a: 255), (r: 102; g: 191; b: 255; a: 255),
  (r: 200; g: 122; b: 255; a: 255), (r: 211; g: 176; b: 131; a: 255));

  type
    TFFTData  = array [0..800] of Single;


    CircleWave = record
      position: TVector2;
      radius:   single;
      alpha:    single;
      speed:    single;
      color:    TColorB;
      end;

var
    circles: array [0..64] of CircleWave;
    i:integer;
    music:TMusic;
    timePlayed:single;
    pause:boolean;
    pitch:single;
    exponent: single = 1.0;                  // Audio exponentiation value

    FFTPeacks  : array [0..600] of Integer;
    FFTFallOff : array [0..600] of Integer;
    FDATA: TFFTData;


procedure SpectrumDraw(FFTData : TFFTData; X, Y : Integer);
var i, YPos : LongInt; YVal : Single;
begin




  for i := 0 to 600 do
  begin
  YVal := Abs(FFTData[(i * {DrawRes} 1) + 5]);
  YPos := Trunc((YVal) * 100);
  if YPos > ScreenHeight then YPos := {SpecHeight} 300;

  if YPos >= FFTFallOff[i] then FFTFallOff[i] := YPos
  else FFTFallOff[i] := FFTFallOff[i] - 4 {LineFall};

  if (ScreenHeight - FFTPeacks[i]) > ScreenHeight then FFTPeacks[i] := 0;
  if (ScreenHeight - FFTFallOff[i]) > ScreenHeight then FFTFallOff[i] := 0;

  DrawLine(X + i, Y + Round(ScreenHeight), X + i, Y + Round(ScreenHeight) - FFTFallOff[i] , ColorCreate(166,202,240,200));

  end;
end;



procedure ProcessAudio(buffer: pointer; frames:LongWord); cdecl;
var samples, left, right: psingle;
    centr: single;
    frame: integer;

begin
  samples:= buffer; // Samples internally stored as <float>s
 for frame :=0 to 600 do
  begin
    left := @samples[frame * 2];
    right := @samples[frame * 2 + 1];

    centr := left^ + right^;

    if centr< 0.0 then centr :=power(abs(centr),exponent) * -1.0
    else
    centr :=power(abs(centr),exponent) * 1.0;
    FDATA[frame] := centr ;
  end;
end;


begin
 // Add to colors

 // Initialization
 SetConfigFlags(FLAG_MSAA_4X_HINT);  // NOTE: Try to enable MSAA 4X

 InitWindow(screenWidth, screenHeight, 'raylib [audio] example - module playing (streaming)');

 InitAudioDevice();                  // Initialize audio device

 for i:= Max_Circles downto  0  do
    begin
        circles[i].alpha := 0.0;
        circles[i].radius := GetRandomValue(10, 40);
        circles[i].position.x := GetRandomValue(round(circles[i].radius), round((screenWidth - circles[i].radius)));
        circles[i].position.y := GetRandomValue(round(circles[i].radius), (round(screenHeight - circles[i].radius)));
        circles[i].speed := GetRandomValue(1, 100)/2000.0;
        circles[i].color := colors[GetRandomValue(0, 13)];
    end;

  music := LoadMusicStream(PChar(GetApplicationDirectory + 'resources/mini1111.xm'));
  music.looping := false;

  pitch := 1.0;

  AttachAudioMixedProcessor(@ProcessAudio);

  PlayMusicStream(music);

  timePlayed := 0.0;
  pause := false;

  SetTargetFPS(60);  // Set our game to run at 60 frames-per-second

 while not WindowShouldClose() do 
 begin
   // Update
   UpdateMusicStream(music);      // Update music buffer with new stream data
   // Restart music playing (stop and play)
   if IsKeyPressed(KEY_SPACE) then
     begin
       StopMusicStream(music);
       PlayMusicStream(music);
       timePlayed := 0;
     end;
   // Pause/Resume music playing
   if IsKeyPressed(KEY_P) then
     begin
       pause := not pause;
       if pause then PauseMusicStream(music)
       else ResumeMusicStream(music);
     end;

   if IsKeyDown(KEY_DOWN) then pitch -= 0.01
     else
       if IsKeyDown(KEY_UP) then pitch += 0.01;

   SetMusicPitch(music, pitch);

   // Get timePlayed scaled to bar dimensions
   timePlayed := (GetMusicTimePlayed(music)/GetMusicTimeLength(music))*(screenWidth - 40);

   // Color circles animation
   for i:= MAX_CIRCLES downto 0 do
      if not pause then
      begin
        circles[i].alpha += circles[i].speed;
        circles[i].radius += circles[i].speed*10.0;

        if (circles[i].alpha > 1.0) then circles[i].speed *= -1;

        if (circles[i].alpha <= 0.0) then
          begin
            circles[i].alpha := 0.0;
            circles[i].radius := GetRandomValue(10, 40);
            circles[i].position.x := GetRandomValue(round(circles[i].radius), round((screenWidth - circles[i].radius)));
            circles[i].position.y := GetRandomValue(round(circles[i].radius), (round(screenHeight - circles[i].radius)));
            circles[i].color := colors[GetRandomValue(0, 13)];
            circles[i].speed := GetRandomValue(1, 100)/2000.0;
          end;
      end;

  // Draw
  BeginDrawing();

    ClearBackground(RAYWHITE);
     for i := MAX_CIRCLES downto 0 do
      DrawCircleV(circles[i].position, circles[i].radius, Fade(circles[i].color, circles[i].alpha));

    SpectrumDraw(FDATA,20,-30);


  // Draw time bar
  DrawRectangle(20, screenHeight - 20 - 12, screenWidth - 40, 12, LIGHTGRAY);
  DrawRectangle(20, screenHeight - 20 - 12, round(timePlayed), 12, MAROON);
  DrawRectangleLines(20, screenHeight - 20 - 12, screenWidth - 40, 12, GRAY);

  EndDrawing();

 end;
 // De-Initialization
 UnloadMusicStream(music);  // Unload music stream buffers from RAM
 CloseAudioDevice();       // Close audio device (music streaming is automatically stopped)
 CloseWindow();          // Close window and OpenGL context
end.

