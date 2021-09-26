{*******************************************************************************************
*
*   raylib [audio] example - Multichannel sound playing
*
*   This example has been created using raylib 2.6 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2019 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
********************************************************************************************}

program audio_multichannel_sound;

{$mode objfpc}{$H+}

uses cmem,
ray_header;

const
 screenWidth = 800;
 screenHeight = 450;

var
  fxWav,fxOgg: TSound;

begin

 InitWindow(screenWidth, screenHeight, 'raylib [audio] example - Multichannel sound playing');

 InitAudioDevice();      // Initialize audio device

 fxWav := LoadSound('resources/sound.wav');         // Load WAV audio file
 fxOgg := LoadSound('resources/target.ogg');        // Load OGG audio file

 SetSoundVolume(fxWav, 0.2);

 SetTargetFPS(60);  // Set our game to run at 60 frames-per-second

 // Main game loop
 while not WindowShouldClose() do // Detect window close button or ESC key
 begin
   // Update
   if IsKeyPressed(KEY_ENTER) then PlaySoundMulti(fxWav);     // Play a new wav sound instance
   if IsKeyPressed(KEY_SPACE) then PlaySoundMulti(fxOgg);     // Play a new ogg sound instance
   // Draw
   BeginDrawing();

   ClearBackground(RAYWHITE);

   DrawText('MULTICHANNEL SOUND PLAYING', 20, 20, 20, GRAY);
   DrawText('Press SPACE to play new ogg instance!', 200, 120, 20, LIGHTGRAY);
   DrawText('Press ENTER to play new wav instance!', 200, 180, 20, LIGHTGRAY);

   DrawText(TextFormat('CONCURRENT SOUNDS PLAYING: %02i', [GetSoundsPlaying]), 220, 280, 20, RED);

   EndDrawing();
 end;
 // De-Initialization
 StopSoundMulti();       // We must stop the buffer pool before unloading
 UnloadSound(fxWav);     // Unload sound data
 UnloadSound(fxOgg);     // Unload sound data
 CloseAudioDevice();     // Close audio device
 CloseWindow();          // Close window and OpenGL context

end.

