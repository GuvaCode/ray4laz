{*******************************************************************************************
*
*   raylib [audio] example - Playing sound multiple times
*
*   Example originally created with raylib 4.6
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Jeffery Myers (@JeffM2501)
*
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
********************************************************************************************}

program audio_multichannel_sound;

{$mode objfpc}{$H+}

uses cmem,raylib;

const
 screenWidth = 800;
 screenHeight = 450;
 MAX_SOUNDS = 10;

var
  soundArray: array[0..MAX_SOUNDS] of TSound;
  currentSound, i: Integer;
  dir: string;
begin

 InitWindow(screenWidth, screenHeight, 'raylib [audio] example - playing sound multiple times');

 InitAudioDevice();      // Initialize audio device

 // load the sound list
 soundArray[0] := LoadSound(PChar(GetApplicationDirectory + 'resources/sound.wav')); // Load WAV audio file into the first slot as the 'source' sound
                                                                                     // this sound owns the sample data
 for i := 1 to MAX_SOUNDS do
 soundArray[i] := LoadSoundAlias(soundArray[0]); // Load an alias of the sound into slots 1-9. These do not own the sound data, but can be played
 currentSound := 0;  // set the sound list to the start

 SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

 // Main game loop
 while not WindowShouldClose() do // Detect window close button or ESC key
 begin
   // Update

   if IsKeyPressed(KEY_SPACE) then
   begin
     PlaySound(soundArray[currentSound]);            // play the next open sound slot
     Inc(currentSound);                                 // increment the sound slot
     if (currentSound >= MAX_SOUNDS) then                 // if the sound slot is out of bounds, go back to 0
     currentSound := 0;
   end;


   // Draw
   BeginDrawing();

   ClearBackground(RAYWHITE);

   DrawText('Press SPACE to PLAY a WAV sound!', 200, 180, 20, LIGHTGRAY);

   EndDrawing();
 end;
 // De-Initialization
 for i := 1 to MAX_SOUNDS do
 UnloadSoundAlias(soundArray[i]);     // Unload sound aliases
 UnloadSound(soundArray[0]);          // Unload source sound data
 CloseAudioDevice();     // Close audio device
 CloseWindow();          // Close window and OpenGL contex
end.

