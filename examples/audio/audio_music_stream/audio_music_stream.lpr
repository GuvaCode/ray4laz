{*******************************************************************************************
*
*   raylib [audio] example - Music playing (streaming)
*
*   This example has been created using raylib 1.3 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2015 Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program audio_music_stream;

{$mode objfpc}{$H+}

uses raylib;

const
 screenWidth = 800;
 screenHeight = 450;

var
  music:TMusic;
  pause:boolean;
  timePlayed:single;

begin

 InitWindow(screenWidth, screenHeight, 'raylib [audio] example - music playing (streaming)');

 InitAudioDevice();              // Initialize audio device

 music := LoadMusicStream(PChar(GetApplicationDirectory + 'resources/country.mp3'));

 PlayMusicStream(music);

 timePlayed := 0.0;
 pause := false;

 SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

 // Main game loop
 while not WindowShouldClose() do // Detect window close button or ESC key
 begin
   // Update
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
          if pause then PauseMusicStream(music)
            else ResumeMusicStream(music);
        end;

   // Get timePlayed scaled to bar dimensions (400 pixels)
   timePlayed := GetMusicTimePlayed(music)/GetMusicTimeLength(music)*400;

   if (timePlayed > 400) then StopMusicStream(music);

   // Draw
   BeginDrawing();

     ClearBackground(RAYWHITE);

     DrawText('MUSIC SHOULD BE PLAYING!', 255, 150, 20, LIGHTGRAY);

     DrawRectangle(200, 200, 400, 12, LIGHTGRAY);
     DrawRectangle(200, 200, round(timePlayed), 12, MAROON);
     DrawRectangleLines(200, 200, 400, 12, GRAY);

     DrawText('PRESS SPACE TO RESTART MUSIC', 215, 250, 20, LIGHTGRAY);
     DrawText('PRESS P TO PAUSE/RESUME MUSIC', 208, 280, 20, LIGHTGRAY);

   EndDrawing();
 end;
 // De-Initialization
 UnloadMusicStream(music);   // Unload music stream buffers from RAM
 CloseAudioDevice();         // Close audio device (music streaming is automatically stopped)
 CloseWindow();              // Close window and OpenGL context
end.

