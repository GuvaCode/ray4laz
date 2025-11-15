(*******************************************************************************************
*
*   raylib [audio] example - sound positioning
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 5.5, last time updated with raylib 5.5
*
*   Example contributed by Le Juez Victor (@Bigfoot71) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2025 Le Juez Victor (@Bigfoot71)
*   Pascal translation (c) 2025 Vadim Gunko(@GuvaCode)
*
********************************************************************************************)

program audio_sound_positioning;

{$mode objfpc}{$H+}

uses
  Math,
  raylib,
  raymath;
//------------------------------------------------------------------------------------
// Module Functions Definition
//------------------------------------------------------------------------------------
// Set sound 3d position
procedure SetSoundPosition(listener: TCamera; sound_: TSound; position: TVector3; maxDist: Single);
var
  direction: TVector3;
  distance: Single;
  attenuation: Single;
  normalizedDirection: TVector3;
  forward: TVector3;
  right: TVector3;
  dotProduct: Single;
  pan: Single;
begin
  // Calculate direction vector and distance between listener and sound source
  direction := Vector3Subtract(position, listener.position);
  distance := Vector3Length(direction);

  // Apply logarithmic distance attenuation and clamp between 0-1
  attenuation := 1.0 / (1.0 + (distance / maxDist));
  attenuation := Clamp(attenuation, 0.0, 1.0);

  // Calculate normalized vectors for spatial positioning
  normalizedDirection := Vector3Normalize(direction);
  forward := Vector3Normalize(Vector3Subtract(listener.target, listener.position));
  right := Vector3Normalize(Vector3CrossProduct(listener.up, forward));

  // Reduce volume for sounds behind the listener
  dotProduct := Vector3DotProduct(forward, normalizedDirection);
  if dotProduct < 0.0 then
    attenuation := attenuation * (1.0 + dotProduct * 0.5);

  // Set stereo panning based on sound position relative to listener
  pan := 0.5 + 0.5 * Vector3DotProduct(normalizedDirection, right);

  // Apply final sound properties
  SetSoundVolume(sound_, attenuation);
  SetSoundPan(sound_, pan);
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  sound: TSound;
  camera: TCamera;
  th: Single;
  spherePos: TVector3;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [audio] example - sound positioning');

  InitAudioDevice();

  sound := LoadSound('resources/coin.wav');

  camera.position := Vector3Create(0, 5, 5);
  camera.target := Vector3Create(0, 0, 0);
  camera.up := Vector3Create(0, 1, 0);
  camera.fovy := 60;
  camera.projection := CAMERA_PERSPECTIVE;

  DisableCursor();

  SetTargetFPS(60);
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do
  begin
    // Update
    //----------------------------------------------------------------------------------
    UpdateCamera(@camera, CAMERA_FREE);

    th := GetTime();

    spherePos.x := 5.0 * Cos(th);
    spherePos.y := 0.0;
    spherePos.z := 5.0 * Sin(th);

    SetSoundPosition(camera, sound, spherePos, 20.0);
    if not IsSoundPlaying(sound) then
      PlaySound(sound);

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      BeginMode3D(camera);
        DrawGrid(10, 2);
        DrawSphere(spherePos, 0.5, RED);
      EndMode3D();

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadSound(sound);
  CloseAudioDevice();     // Close audio device

  CloseWindow();          // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.



