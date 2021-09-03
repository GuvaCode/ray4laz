program audio_multichannel_sound;

{$MODE objfpc}

uses  cmem, ray_header, math;

const screenWidth = 800;
      screenHeight = 450;

var
    fxWav:TSound;
    fxOgg:TSound;
    i:integer;
begin
    InitWindow(screenWidth, screenHeight, 'raylib [audio] example - Multichannel sound playing');
    InitAudioDevice();      // Initialize audio device

    fxWav := LoadSound('resources/sound/sound.wav');         // Load WAV audio file
    fxOgg := LoadSound('resources/sound/tanatana.ogg');        // Load OGG audio file

    SetSoundVolume(fxWav, 0.2);
    SetTargetFPS(60);       // Set our game to run at 60 frames-per-second
    //--------------------------------------------------------------------------------------
   while not WindowShouldClose() do
    begin
      if IsKeyPressed(KEY_ENTER) then PlaySoundMulti(fxWav);     // Play a new wav sound instance
      if IsKeyPressed(KEY_SPACE) then PlaySoundMulti(fxOgg);     // Play a new ogg sound instance


     BeginDrawing();
           ClearBackground(RAYWHITE);
            DrawText('MULTICHANNEL SOUND PLAYING', 20, 20, 20, GRAY);
            DrawText('Press SPACE to play new ogg instance!', 200, 120, 20, LIGHTGRAY);
            DrawText('Press ENTER to play new wav instance!', 200, 180, 20, LIGHTGRAY);
           { DrawText(
            TextFormat('CONCURRENT SOUNDS PLAYING: %02i', GetSoundsPlaying)
            , 220, 280, 20, RED);}

            DrawText(
            TextFormat('CONCURRENT SOUNDS PLAYING: %02i', [GetSoundsPlaying]), 220, 280, 20, RED);

        EndDrawing();
     end;

    StopSoundMulti();       // We must stop the buffer pool before unloading

     UnloadSound(fxOgg);     // Unload sound data
     UnloadSound(fxWav);     // Unload sound data

    CloseAudioDevice();     // Close audio device (music streaming is automatically stopped)
    CloseWindow();          // Close window and OpenGL context
end.

