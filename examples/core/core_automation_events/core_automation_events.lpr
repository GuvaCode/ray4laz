{*******************************************************************************************
*
*   raylib [core] example - automation events
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example based on 2d_camera_platformer example by arvyy (@arvyy)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program core_automation_events;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

  GRAVITY = 400;
  PLAYER_JUMP_SPD = 350.0;
  PLAYER_HOR_SPD = 200.0;
  MAX_ENVIRONMENT_ELEMENTS = 4;

type
   PPlayer = ^TPLayer;
   TPlayer = record
    position: TVector2;
    speed: single;
    canJump: boolean;
   end;

   PEnvElement = ^TEnvElement;
   TEnvElement = record
     rect: TRectangle;
     blocking: integer;
     color: TColorB;
   end;

var
  Player: TPlayer;
  envElements: array[0..MAX_ENVIRONMENT_ELEMENTS] of TEnvElement;
  camera: TCamera2D;
  aelist: TAutomationEventList;
  eventRecording, eventPlaying: boolean;
  frameCounter, playFrameCounter, currentPlayFrame: LongWord;
  deltaTime, minX, minY, maxX, maxY : single;
  droppedFiles: TFilePathList;
  hitObstacle, i: integer;
  element: PEnvElement;
  p: PVector2;
  maxV, minV: TVector2;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - automation events');

  // Define player
  player.position := Vector2Create( 400, 280 );
  player.speed := 0;
  player.canJump := false;

  // Define environment elements (platforms)
  envElements[0].rect := RectangleCreate(0, 0, 1000, 400);
  envElements[0].blocking := 0;
  envElements[0].color := LIGHTGRAY;
  envElements[1].rect := RectangleCreate(0, 400, 1000, 200);
  envElements[1].blocking := 1;
  envElements[1].color := GRAY;
  envElements[2].rect := RectangleCreate(300, 200, 400, 10);
  envElements[2].blocking := 1;
  envElements[2].color := GRAY;
  envElements[3].rect := RectangleCreate(250, 300, 100, 10);
  envElements[3].blocking := 1;
  envElements[3].color := GRAY;
  envElements[4].rect := RectangleCreate(650, 300, 100, 10);
  envElements[4].blocking := 1;
  envElements[4].color := GRAY;

  // Define camera
  camera.target := player.position;
  camera.offset := Vector2Create( screenWidth /2.0, screenHeight / 2.0 );
  camera.rotation := 0.0;
  camera.zoom := 1.0;

  // Automation events
  aelist := LoadAutomationEventList(nil);  // Initialize list of automation events to record new events
  SetAutomationEventList(@aelist);
  eventRecording := false;
  eventPlaying := false;

  frameCounter := 0;
  playFrameCounter := 0;
  currentPlayFrame := 0;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      deltaTime := 0.015;//GetFrameTime()
      // Dropped files logic
      //----------------------------------------------------------------------------------
      if (IsFileDropped()) then
      begin
        droppedFiles := LoadDroppedFiles();
        // Supports loading .rgs style files (text or binary) and .png style palette images
        if (IsFileExtension(droppedFiles.paths[0], '.txt;.rae')) then
        begin
          UnloadAutomationEventList(aelist);
          aelist := LoadAutomationEventList(droppedFiles.paths[0]);
          eventRecording := false;
          // Reset scene state to play
          eventPlaying := true;
          playFrameCounter := 0;
          currentPlayFrame := 0;

          player.position := Vector2Create( 400, 280 );
          player.speed := 0;
          player.canJump := false;

          camera.target := player.position;
          camera.offset := Vector2Create( screenWidth/2.0, screenHeight/2.0 );
          camera.rotation := 0.0;
          camera.zoom := 1.0;
        end;
          UnloadDroppedFiles(droppedFiles);   // Unload filepaths from memory
      end;
      //----------------------------------------------------------------------------------

      // Update player
      //----------------------------------------------------------------------------------
      if (IsKeyDown(KEY_LEFT)) then player.position.x -= PLAYER_HOR_SPD*deltaTime;
      if (IsKeyDown(KEY_RIGHT)) then player.position.x += PLAYER_HOR_SPD*deltaTime;
      if (IsKeyDown(KEY_SPACE) and player.canJump) then
      begin
        player.speed := -PLAYER_JUMP_SPD;
        player.canJump := false;
      end;

      hitObstacle := 0;
      for i := 0 to MAX_ENVIRONMENT_ELEMENTS do //(int i = 0; i < MAX_ENVIRONMENT_ELEMENTS; i++)
      begin
        element := @envElements[i];
        p := @player.position;
        if ((element^.blocking > 0) and
           (element^.rect.x <= p^.x) and
           (element^.rect.x + element^.rect.width >= p^.x) and
           (element^.rect.y >= p^.y) and
           (element^.rect.y <= p^.y + player.speed*deltaTime)) then
        begin
          hitObstacle := 1;
          player.speed := 0.0;
          p^.y := element^.rect.y;
        end;
      end;

      if (hitObstacle <=0 ) then
      begin
        player.position.y += player.speed*deltaTime;
        player.speed += GRAVITY*deltaTime;
        player.canJump := false;
      end
      else player.canJump := true;

      camera.zoom += (GetMouseWheelMove()*0.05);

      if (camera.zoom > 3.0) then camera.zoom := 3.0
      else if (camera.zoom < 0.25) then camera.zoom := 0.25;

      if (IsKeyPressed(KEY_R)) then
      begin
      // Reset game state
      player.position := Vector2Create( 400, 280 );
      player.speed := 0;
      player.canJump := false;

      camera.target := player.position;
      camera.offset := Vector2Create( screenWidth/2.0, screenHeight/2.0 );
      camera.rotation := 0.0;
      camera.zoom := 1.0;
      end;

      // Update camera
      //----------------------------------------------------------------------------------
      camera.target := player.position;
      camera.offset := Vector2Create( screenWidth/2.0, screenHeight/2.0 );
      minX := 1000; minY := 1000; maxX := -1000; maxY := -1000;

      for i :=0 to MAX_ENVIRONMENT_ELEMENTS do //(int i = 0; i < MAX_ENVIRONMENT_ELEMENTS; i++)
      begin
        element := @envElements[i];
        minX := min(element^.rect.x, minX);
        maxX := max(element^.rect.x + element^.rect.width, maxX);
        minY := min(element^.rect.y, minY);
        maxY := max(element^.rect.y + element^.rect.height, maxY);
      end;

      maxV := GetWorldToScreen2D(Vector2Create( maxX, maxY ), camera);
      minV := GetWorldToScreen2D(Vector2Create( minX, minY ), camera);

      if (maxV.x < screenWidth) then  camera.offset.x := screenWidth - (maxV.x - screenWidth/2);
      if (maxV.y < screenHeight) then camera.offset.y := screenHeight - (maxV.y - screenHeight/2);
      if (minV.x > 0) then camera.offset.x := screenWidth/2 - minV.x;
      if (minV.y > 0) then camera.offset.y := screenHeight/2 - minV.y;
      //----------------------------------------------------------------------------------
      // Toggle events recording
      if (IsKeyPressed(KEY_S)) then
      begin
          if (not eventPlaying) then
          begin
              if (eventRecording) then
              begin
                  StopAutomationEventRecording();
                  eventRecording := false;

                  ExportAutomationEventList(aelist, 'automation.rae');

                  TraceLog(LOG_INFO, 'RECORDED FRAMES: %i', aelist.count);
              end
              else
              begin
                  SetAutomationEventBaseFrame(180);
                  StartAutomationEventRecording();
                  eventRecording := true;
              end;
          end;
      end
      else if (IsKeyPressed(KEY_A)) then
      begin
          if (not eventRecording and (aelist.count > 0)) then
          begin
              // Reset scene state to play
              eventPlaying := true;
              playFrameCounter := 0;
              currentPlayFrame := 0;

              player.position := Vector2Create( 400, 280 );
              player.speed := 0;
              player.canJump := false;

              camera.target := player.position;
              camera.offset := Vector2Create( screenWidth/2.0, screenHeight/2.0 );
              camera.rotation := 0.0;
              camera.zoom := 1.0;
          end;
      end;

      if (eventPlaying) then
      begin
          // NOTE: Multiple events could be executed in a single frame
          while (playFrameCounter = aelist.events[currentPlayFrame].frame) do
          begin
              TraceLog(LOG_INFO, 'PLAYING: PlayFrameCount: %i | currentPlayFrame: %i | Event Frame: %i, param: %i',
                  playFrameCounter, currentPlayFrame, aelist.events[currentPlayFrame].frame, aelist.events[currentPlayFrame].params[0]);

              PlayAutomationEvent(aelist.events[currentPlayFrame]);
              Inc(currentPlayFrame);

              if (currentPlayFrame = aelist.count) then
              begin
                  eventPlaying := false;
                  currentPlayFrame := 0;
                  playFrameCounter := 0;

                  TraceLog(LOG_INFO, 'FINISH PLAYING!');
                  break;
              end;
          end;

          Inc(playFrameCounter);
      end;

      if (eventRecording or eventPlaying) then  Inc(frameCounter)
      else frameCounter := 0;
      //----------------------------------------------------------------------------------

      // Draw -----------

      BeginDrawing();

          ClearBackground(LIGHTGRAY);

          BeginMode2D(camera);

              // Draw environment elements
              for i := 0 to MAX_ENVIRONMENT_ELEMENTS do //  (int i = 0; i < MAX_ENVIRONMENT_ELEMENTS; i++)
              begin
                  DrawRectangleRec(envElements[i].rect, envElements[i].color);
              end;

              // Draw player rectangle
              DrawRectangleRec(RectangleCreate( player.position.x - 20, player.position.y - 40, 40, 40 ), RED);

          EndMode2D();

          // Draw game controls
          DrawRectangle(10, 10, 290, 145, Fade(SKYBLUE, 0.5));
          DrawRectangleLines(10, 10, 290, 145, Fade(BLUE, 0.8));

          DrawText('Controls:', 20, 20, 10, BLACK);
          DrawText('- RIGHT | LEFT: Player movement', 30, 40, 10, DARKGRAY);
          DrawText('- SPACE: Player jump', 30, 60, 10, DARKGRAY);
          DrawText('- R: Reset game state', 30, 80, 10, DARKGRAY);

          DrawText('- S: START/STOP RECORDING INPUT EVENTS', 30, 110, 10, BLACK);
          DrawText('- A: REPLAY LAST RECORDED INPUT EVENTS', 30, 130, 10, BLACK);

          // Draw automation events recording indicator
          if (eventRecording) then
          begin
              DrawRectangle(10, 160, 290, 30, Fade(RED, 0.3));
              DrawRectangleLines(10, 160, 290, 30, Fade(MAROON, 0.8));
              DrawCircle(30, 175, 10, MAROON);

              if (((frameCounter/15) mod 2) = 1) then
              DrawText(TextFormat('RECORDING EVENTS... [%i]', aelist.count), 50, 170, 10, MAROON);
          end
          else if (eventPlaying) then
          begin
              DrawRectangle(10, 160, 290, 30, Fade(LIME, 0.3));
              DrawRectangleLines(10, 160, 290, 30, Fade(DARKGREEN, 0.8));
              DrawTriangle(Vector2Create( 20, 155 + 10 ), Vector2Create( 20, 155 + 30 ), Vector2Create( 40, 155 + 20 ), DARKGREEN);

              if (((frameCounter/15) mod 2) = 1) then
              DrawText(TextFormat('PLAYING RECORDED EVENTS... [%i]', currentPlayFrame), 50, 170, 10, DARKGREEN);
          end;


      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

