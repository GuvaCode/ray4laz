program audio_sound_loading;

{$MODE objfpc}

uses  cmem, ray_header, math;

const screenWidth = 800;
      screenHeight = 450;

var
    fxWav:TSound;
    fxOgg:TSound;


begin
   InitWindow(screenWidth, screenHeight, 'raylib [audio] example - sound loading and playing');
   InitAudioDevice();      // Initialize audio device

   fxWav := LoadSound('resources/sound/sound.wav');         // Load WAV audio file
   fxOgg := LoadSound('resources/sound/tanatana.ogg');        // Load OGG audio file

   SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

   // Main game loop
   while not WindowShouldClose() do
    begin
     if (IsKeyPressed(KEY_SPACE)) then PlaySound(fxWav);      // Play WAV sound
     if (IsKeyPressed(KEY_ENTER)) then PlaySound(fxOgg);      // Play OGG sound

       BeginDrawing();
            ClearBackground(RAYWHITE);
            DrawText('Press SPACE to PLAY the WAV sound!', 200, 180, 20, BLUE);
            DrawText('Press ENTER to PLAY the OGG sound!', 200, 220, 20, RED);
        EndDrawing();

    end;

    UnloadSound(fxWav);     // Unload sound data
    UnloadSound(fxOgg);     // Unload sound data

    CloseAudioDevice();     // Close audio device
    CloseWindow();          // Close window and OpenGL context
end.

