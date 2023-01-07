program core_custom_frame_control;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  PreviousTime: Double;
  CurrentTime: Double;
  UpdateDrawTime: Double;
  WaitTime: Double;
  DeltaTime: Single;
  TimeCounter: Single;
  Position: Single;
  Pause: Boolean;
  TargetFPS: Integer;
  I: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - frame control (see comment)');

  // Custom timming variables
  PreviousTime := GetTime();    // Previous time measure
  DeltaTime := 0.0;             // Frame time (Update + Draw + Wait time)
  TimeCounter := 0.0;           // Accumulative time counter (seconds)
  Position := 0.0;              // Circle position
  Pause := False;               // Pause control flag

  TargetFPS := 60;              // Our initial target fps
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      PollInputEvents(); // Poll input events (SUPPORT_CUSTOM_FRAME_CONTROL)

        if IsKeyPressed(KEY_SPACE) then
          Pause := not Pause;

        if IsKeyPressed(KEY_UP) then
          TargetFPS := TargetFPS + 20
        else if IsKeyPressed(KEY_DOWN) then
          TargetFPS := TargetFPS - 20;

        if TargetFPS < 0 then
          TargetFPS := 0;

        if not Pause then
        begin
          Position := Position + 200 * DeltaTime; // We move at 200 pixels per second
          if Position >= GetScreenWidth() then
            Position := 0;
          TimeCounter := TimeCounter + DeltaTime; // We count time (seconds)
        end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

        for I := 0 to Trunc(GetScreenWidth() / 200) - 1 do
          DrawRectangle(200*i, 0, 1, GetScreenHeight(), SKYBLUE);

        DrawCircle(Trunc(Position), Trunc(GetScreenHeight() / 2 - 25), 50, RED);

        DrawText(TextFormat('%03.0f ms', Single(TimeCounter * 1000.0)), Trunc(Position - 40), Trunc(GetScreenHeight() / 2 - 100), 20, MAROON);
        DrawText(TextFormat('PosX: %03.0f', Single(Position)), Trunc(Position - 50), Trunc(GetScreenHeight() / 2 + 40), 20, BLACK);

        DrawText('Circle is moving at a constant 200 pixels/sec,'#10'independently of the frame rate.', 10, 10, 20, DARKGRAY);
        DrawText('PRESS SPACE to PAUSE MOVEMENT', 10, GetScreenHeight() - 60, 20, GRAY);
        DrawText('PRESS UP | DOWN to CHANGE TARGET FPS', 10, GetScreenHeight() - 30, 20, GRAY);
        DrawText(TextFormat('TARGET FPS: %i', TargetFPS), GetScreenWidth() - 220, 10, 20, LIME);
        DrawText(TextFormat('CURRENT FPS: %i', Trunc(1.0 / DeltaTime)), GetScreenWidth() - 220, 40, 20, GREEN);

      EndDrawing();

      // NOTE: In case raylib is configured to SUPPORT_CUSTOM_FRAME_CONTROL,
      // Events polling, screen buffer swap and frame time control must be managed by the user

      SwapScreenBuffer(); // Flip the back buffer to screen (front buffer)

      CurrentTime := GetTime();
      UpdateDrawTime := CurrentTime - PreviousTime;

      if TargetFPS > 0 then // We want a fixed frame rate
      begin
        WaitTime := (1.0 / TargetFPS) - UpdateDrawTime;
        if WaitTime > 0.0 then
        begin
          raylib.WaitTime(WaitTime);
          CurrentTime := GetTime();
          DeltaTime := (CurrentTime - PreviousTime);
        end;
      end else
        DeltaTime := UpdateDrawTime; // Framerate could be variable

      PreviousTime := CurrentTime;
      //-------------------------------------------------------------------------------------------
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

