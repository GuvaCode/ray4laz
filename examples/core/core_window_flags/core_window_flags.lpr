{*******************************************************************************************
*
*   raylib [core] example - window flags
*
*   This example has been created using raylib 3.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2020 Ramon Santamaria (@raysan5)
*   Pascal translation 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}

program core_window_flags;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
//raymath,
//rlgl,
raylib, sysutils;

const
  screenWidth = 800;
  screenHeight = 450;

var
  ballPosition,ballSpeed : TVector2;
  ballRadius: single;
  framesCounter : integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  // Possible window flags
    {
    FLAG_VSYNC_HINT
    FLAG_FULLSCREEN_MODE    -> not working properly -> wrong scaling!
    FLAG_WINDOW_RESIZABLE
    FLAG_WINDOW_UNDECORATED
    FLAG_WINDOW_TRANSPARENT
    FLAG_WINDOW_HIDDEN
    FLAG_WINDOW_MINIMIZED   -> Not supported on window creation
    FLAG_WINDOW_MAXIMIZED   -> Not supported on window creation
    FLAG_WINDOW_UNFOCUSED
    FLAG_WINDOW_TOPMOST
    FLAG_WINDOW_HIGHDPI     -> errors after minimize-resize, fb size is recalculated
    FLAG_WINDOW_ALWAYS_RUN
    FLAG_MSAA_4X_HINT
    }

    // Set configuration flags for window creation
   SetConfigFlags(FLAG_VSYNC_HINT or FLAG_MSAA_4X_HINT or FLAG_WINDOW_HIGHDPI);
   InitWindow(screenWidth, screenHeight, 'raylib [core] example - window flags');
   Vector2Set(@ballPosition ,GetScreenWidth() / 2.0, GetScreenHeight() / 2.0);
   Vector2Set(@ballSpeed, 5.0,4.0);
   ballRadius := 20;
   framesCounter := 0;

   SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_F) then
        begin
        ToggleFullscreen();  // modifies window size when scaling!
        SetWindowSize(GetMonitorWidth(GetCurrentMonitor), GetMonitorHeight(GetCurrentMonitor));
        end;

      if IsKeyPressed(KEY_R) then
        begin
            if IsWindowState(FLAG_WINDOW_RESIZABLE) then ClearWindowState(FLAG_WINDOW_RESIZABLE)
            else SetWindowState(FLAG_WINDOW_RESIZABLE);
        end;

      if IsKeyPressed(KEY_D) then
        begin
            if IsWindowState(FLAG_WINDOW_UNDECORATED) then ClearWindowState(FLAG_WINDOW_UNDECORATED)
            else SetWindowState(FLAG_WINDOW_UNDECORATED);
        end;

      if IsKeyPressed(KEY_H) then
        begin
            if not IsWindowState(FLAG_WINDOW_HIDDEN) then SetWindowState(FLAG_WINDOW_HIDDEN);
            framesCounter := 0;
        end;

      if IsWindowState(FLAG_WINDOW_HIDDEN) then
        begin
            inc(framesCounter);
            if framesCounter >= 240 then ClearWindowState(FLAG_WINDOW_HIDDEN); // Show window after 3 seconds
        end;


      if IsKeyPressed(KEY_N) then
        begin
            if not IsWindowState(FLAG_WINDOW_MINIMIZED) then MinimizeWindow();
            framesCounter := 0;
        end;


      if IsWindowState(FLAG_WINDOW_MINIMIZED) then
        begin
            inc(framesCounter);
            if framesCounter >= 240 then RestoreWindow(); // Restore window after 3 seconds
        end;

      if IsKeyPressed(KEY_M) then
        begin
            // NOTE: Requires FLAG_WINDOW_RESIZABLE enabled!
            if IsWindowState(FLAG_WINDOW_MAXIMIZED) then RestoreWindow()
            else MaximizeWindow();
        end;

      if IsKeyPressed(KEY_U) then
        begin
            if IsWindowState(FLAG_WINDOW_UNFOCUSED) then ClearWindowState(FLAG_WINDOW_UNFOCUSED)
            else SetWindowState(FLAG_WINDOW_UNFOCUSED);
        end;

      if IsKeyPressed(KEY_T) then
        begin
            if IsWindowState(FLAG_WINDOW_TOPMOST) then ClearWindowState(FLAG_WINDOW_TOPMOST)
            else SetWindowState(FLAG_WINDOW_TOPMOST);
        end;

      if IsKeyPressed(KEY_A) then
        begin
            if IsWindowState(FLAG_WINDOW_ALWAYS_RUN) then ClearWindowState(FLAG_WINDOW_ALWAYS_RUN)
            else SetWindowState(FLAG_WINDOW_ALWAYS_RUN);
        end;

      if IsKeyPressed(KEY_V) then
        begin
            if IsWindowState(FLAG_VSYNC_HINT) then ClearWindowState(FLAG_VSYNC_HINT)
            else SetWindowState(FLAG_VSYNC_HINT);
        end;

      // Bouncing ball logic
       ballPosition.x += ballSpeed.x;
       ballPosition.y += ballSpeed.y;
        if ((ballPosition.x >= (GetScreenWidth() - ballRadius)) or (ballPosition.x <= ballRadius)) then ballSpeed.x *= -1.0;
        if ((ballPosition.y >= (GetScreenHeight() - ballRadius)) or (ballPosition.y <= ballRadius)) then ballSpeed.y *= -1.0;

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing;
        if IsWindowState(FLAG_WINDOW_TRANSPARENT) then ClearBackground(BLANK)
        else ClearBackground(RAYWHITE);

        DrawCircleV(ballPosition, ballRadius, MAROON);
        DrawRectangleLinesEx(RectangleCreate(0, 0, GetScreenWidth, GetScreenHeight), 4, RAYWHITE);

        DrawCircleV(GetMousePosition, 10, DARKBLUE);

        DrawFPS(10, 10);

        DrawText(TextFormat('Screen Size: [%i, %i]',GetScreenWidth, GetScreenHeight), 10, 40, 10, GREEN);

        // Draw window state info
        DrawText('Following flags can be set after window creation:', 10, 60, 10, GRAY);

        if IsWindowState(FLAG_FULLSCREEN_MODE) then DrawText('[F] FLAG_FULLSCREEN_MODE: on', 10, 80, 10, LIME)
        else DrawText('[F] FLAG_FULLSCREEN_MODE: off', 10, 80, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_RESIZABLE) then DrawText('[R] FLAG_WINDOW_RESIZABLE: on', 10, 100, 10, LIME)
        else DrawText('[R] FLAG_WINDOW_RESIZABLE: off', 10, 100, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_UNDECORATED) then DrawText('[D] FLAG_WINDOW_UNDECORATED: on', 10, 120, 10, LIME)
        else DrawText('[D] FLAG_WINDOW_UNDECORATED: off', 10, 120, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_HIDDEN) then DrawText('[H] FLAG_WINDOW_HIDDEN: on', 10, 140, 10, LIME)
        else DrawText('[H] FLAG_WINDOW_HIDDEN: off', 10, 140, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_MINIMIZED) then DrawText('[N] FLAG_WINDOW_MINIMIZED: on', 10, 160, 10, LIME)
        else DrawText('[N] FLAG_WINDOW_MINIMIZED: off', 10, 160, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_MAXIMIZED) then DrawText('[M] FLAG_WINDOW_MAXIMIZED: on', 10, 180, 10, LIME)
        else DrawText('[M] FLAG_WINDOW_MAXIMIZED: off', 10, 180, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_UNFOCUSED) then DrawText('[G] FLAG_WINDOW_UNFOCUSED: on', 10, 200, 10, LIME)
        else DrawText('[U] FLAG_WINDOW_UNFOCUSED: off', 10, 200, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_TOPMOST) then DrawText('[T] FLAG_WINDOW_TOPMOST: on', 10, 220, 10, LIME)
        else DrawText('[T] FLAG_WINDOW_TOPMOST: off', 10, 220, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_ALWAYS_RUN) then DrawText('[A] FLAG_WINDOW_ALWAYS_RUN: on', 10, 240, 10, LIME)
        else DrawText('[A] FLAG_WINDOW_ALWAYS_RUN: off', 10, 240, 10, MAROON);
        if IsWindowState(FLAG_VSYNC_HINT) then DrawText('[V] FLAG_VSYNC_HINT: on', 10, 260, 10, LIME)
        else DrawText('[V] FLAG_VSYNC_HINT: off', 10, 260, 10, MAROON);

        DrawText('Following flags can only be set before window creation:', 10, 300, 10, GRAY);
        if IsWindowState(FLAG_WINDOW_HIGHDPI) then DrawText('FLAG_WINDOW_HIGHDPI: on', 10, 320, 10, LIME)
        else DrawText('FLAG_WINDOW_HIGHDPI: off', 10, 320, 10, MAROON);
        if IsWindowState(FLAG_WINDOW_TRANSPARENT) then DrawText('FLAG_WINDOW_TRANSPARENT: on', 10, 340, 10, LIME)
        else DrawText('FLAG_WINDOW_TRANSPARENT: off', 10, 340, 10, MAROON);
        if IsWindowState(FLAG_MSAA_4X_HINT) then DrawText('FLAG_MSAA_4X_HINT: on', 10, 360, 10, LIME)
        else DrawText('FLAG_MSAA_4X_HINT: off', 10, 360, 10, MAROON);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

