program audio_sound_loadig;

{$mode objfpc}{$H+}
uses
cmem, 
raylib;

const
 screenWidth = 800;
 screenHeight = 450;


var fxWav, fxOgg:TSound;

begin
 // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - sound loading and playing');
  InitAudioDevice();      // Initialize audio device

  fxWav := LoadSound(PChar(GetApplicationDirectory + 'resources/sound.wav'));         // Load WAV audio file
  fxOgg := LoadSound(PChar(GetApplicationDirectory + 'resources/target.ogg'));        // Load OGG audio file

  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
 //--------------------------------------------------------------------------------------

 while not WindowShouldClose() do
 begin

  // Update
  //----------------------------------------------------------------------------------
  if IsKeyPressed(KEY_SPACE) then PlaySound(fxWav);      // Play WAV sound
  if IsKeyPressed(KEY_ENTER) then PlaySound(fxOgg);      // Play OGG sound
  //----------------------------------------------------------------------------------

  // Draw
  //----------------------------------------------------------------------------------
  BeginDrawing();
  ClearBackground(RAYWHITE);

  DrawText('Press SPACE to PLAY the WAV sound!', 200, 180, 20, LIGHTGRAY);
  DrawText('Press ENTER to PLAY the OGG sound!', 200, 220, 20, LIGHTGRAY);

  EndDrawing();
 end;
 // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadSound(fxWav);     // Unload sound data
  UnloadSound(fxOgg);     // Unload sound data

  CloseAudioDevice();     // Close audio device

  CloseWindow();          // Close window and OpenGL context
  //--------------------------------------------------------------------------------------

end.
