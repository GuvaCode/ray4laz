program audio_spatial_sound;

uses
  {$IFDEF LINUX} cthreads,{$ENDIF} raylib, raymath, math;

//------------------------------------------------------------------------------------
// Sound positioning procedure
//------------------------------------------------------------------------------------
procedure SetSoundPosition(listener: TCamera; sound: TSound; position: TVector3; maxDist: Single);
var
  direction, normalizedDirection, forward, right: TVector3;
  distance, attenuation, dotProduct, pan: Single;
begin
  // Calculate direction vector and distance between listener and sound source
  direction := Vector3Subtract(position, listener.position);
  distance := Vector3Length(direction);

  // Apply logarithmic distance attenuation and clamp between 0-1
  attenuation := 1.0 / (1.0 + (distance / maxDist));
  attenuation := Clamp(attenuation, 0.0, 1.0);

  // Calculate normalized vectors for spatial positioning
  if distance > 0 then
    normalizedDirection := Vector3Normalize(direction)
  else
    normalizedDirection := Vector3Create(0, 0, 0);

  forward := Vector3Normalize(Vector3Subtract(listener.target, listener.position));
  right := Vector3Normalize(Vector3CrossProduct(forward, listener.up));

  // Reduce volume for sounds behind the listener
  dotProduct := Vector3DotProduct(forward, normalizedDirection);
  if dotProduct < 0.0 then
    attenuation := attenuation * (1.0 + dotProduct * 0.5);

  // Set stereo panning based on sound position relative to listener
  pan := 0.5 + 0.5 * Vector3DotProduct(normalizedDirection, right);

  // Apply final sound properties
  SetSoundVolume(sound, attenuation);
  SetSoundPan(sound, pan);
end;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  sound: TSound;
  camera: TCamera;
  th: Single;
  spherePos: TVector3;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(800, 600, 'Quick Spatial Sound');
  InitAudioDevice();

  SetTargetFPS(60);
  DisableCursor();

  sound := LoadSound('resources/coin.wav');

  camera.position := Vector3Create(0, 5, 5);
  camera.target := Vector3Create(0, 0, 0);
  camera.up := Vector3Create(0, 1, 0);
  camera.fovy := 60;
  camera.projection := CAMERA_PERSPECTIVE;
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
    if not IsSoundPlaying(sound) then PlaySound(sound);
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();
      ClearBackground(BLACK);

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
  CloseAudioDevice();
  CloseWindow();
end.
