(*******************************************************************************************
*
*   raylib [audio] example - sound multi
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023-2025 Jeffery Myers (@JeffM2501)
*   Pascal translation (c) 2023 Vadim Gunko (@GuvaCode)
*
********************************************************************************************)
program audio_sound_multi;

{$mode objfpc}{$H+}

uses
  raylib;

const
  MAX_SOUNDS = 10;

var
  soundArray: array[0..MAX_SOUNDS-1] of TSound;
  currentSound: Integer;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  i: Integer;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - sound multi');

  InitAudioDevice();      // Initialize audio device

  // Load audio file into the first slot as the 'source' sound,
  // this sound owns the sample data
  soundArray[0] := LoadSound('resources/sound.wav');

  // Load an alias of the sound into slots 1-9. These do not own the sound data, but can be played
  for i := 1 to MAX_SOUNDS - 1 do
    soundArray[i] := LoadSoundAlias(soundArray[0]);

  currentSound := 0;               // Set the sound list to the start

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do    // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    if IsKeyPressed(KEY_SPACE) then
    begin
      PlaySound(soundArray[currentSound]);    // Play the next open sound slot
      currentSound := currentSound + 1;       // Increment the sound slot

      // If the sound slot is out of bounds, go back to 0
      if currentSound >= MAX_SOUNDS then
        currentSound := 0;

      // NOTE: Another approach would be to look at the list for the first sound
      // that is not playing and use that slot
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('Press SPACE to PLAY a WAV sound!', 200, 180, 20, LIGHTGRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  for i := 1 to MAX_SOUNDS - 1 do
    UnloadSoundAlias(soundArray[i]); // Unload sound aliases
  UnloadSound(soundArray[0]); // Unload source sound data

  CloseAudioDevice();     // Close audio device

  CloseWindow();          // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
