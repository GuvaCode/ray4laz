program basic;

{$mode objfpc}{$H+}



uses
  raylib, raymedia;

const
  screenWidth = 800;
  screenHeight = 450;
  MovieFile = 'resources/clips/001.mp4';
var
  videoMedia: TMediaStream;
  videoPosX, videoPosY: integer;

begin
  // Setup: Initialize window, audio, and load media
  InitWindow(screenWidth, screenHeight, '01 - Basics');
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  InitAudioDevice();
  // Load the media stream with default settings
  videoMedia := LoadMedia(MovieFile);

  // Verify if the media has loaded correctly
  if not IsMediaValid(videoMedia) then
  begin
    TraceLog(LOG_ERROR, 'Failed to load media file: %s', MovieFile);
    CloseAudioDevice();
    CloseWindow();
    Exit;
  end;

  // Set the media to play in a continuous loop
  SetMediaLooping(videoMedia, true);

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update the media stream based on frame timing
      UpdateMedia(@videoMedia);

      // Draw
      BeginDrawing();
      ClearBackground(DARKPURPLE);

      // Calculate the coordinates to center the video in the window
      videoPosX := (GetScreenWidth() - videoMedia.videoTexture.width) div 2;
      videoPosY := (GetScreenHeight() - videoMedia.videoTexture.height) div 2;

      // Draw the video stream texture at the calculated position
      DrawTexture(videoMedia.videoTexture, videoPosX, videoPosY, WHITE);
      EndDrawing();
    end;

    // Cleanup: Unload media and close devices
    UnloadMedia(@videoMedia);
    CloseAudioDevice();
  CloseWindow();        // Close window and OpenGL context
end.

