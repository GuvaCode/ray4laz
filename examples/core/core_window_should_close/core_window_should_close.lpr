{*******************************************************************************************
*
*   raylib [core] example - Window should close
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2013-2022 Ramon Santamaria (@raysan5)
*   Pascal translation 2022 Vadim Gunkoa(@guvacode)
*
********************************************************************************************}
program core_window_should_close;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  exitWindowRequested, exitWindow : boolean;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - window should close');

  SetExitKey(KEY_NULL);       // Disable KEY_ESCAPE to close window, X-button still works

  exitWindowRequested := false;   // Flag to request window to exit
  exitWindow := false;    // Flag to set window to exit

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not exitWindow do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Detect if X-button or KEY_ESCAPE have been pressed to close window
      if (WindowShouldClose() or IsKeyPressed(KEY_ESCAPE)) then exitWindowRequested := true;

      if (exitWindowRequested) then
       begin
           // A request for close window has been issued, we can save data before closing
           // or just show a message asking for confirmation

           if (IsKeyPressed(KEY_Y)) then exitWindow := true
           else
            if (IsKeyPressed(KEY_N)) then exitWindowRequested := false;
        end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
      if (exitWindowRequested) then
      begin
          DrawRectangle(0, 100, screenWidth, 200, BLACK);
          DrawText('Are you sure you want to exit program? [Y/N]', 40, 180, 30, WHITE);
      end
      else
       DrawText('Try to close the window to get confirmation message!', 120, 200, 20, LIGHTGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

