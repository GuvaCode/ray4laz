{*******************************************************************************************
*
*   raygui - portable window
*
*   DEPENDENCIES:
*       raylib 4.0  - Windowing/input management and drawing.
*       raygui 3.0  - Immediate-mode GUI controls.
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2016-2021 Ramon Santamaria (@raysan5)
*   Pascal translation (c) 2021 Vadim Gunko (@guvacode)
*
**********************************************************************************************}
program portable_window;

{$mode objfpc}{$H+}

uses 
raygui,
raylib; 

const
  screenWidth = 800;
  screenHeight = 600;

var
  windowPosition, mousePosition, panOffset : TVector2;
  dragWindow, exitWindow : boolean;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_WINDOW_UNDECORATED);
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

    // General variables
    Vector2Set(@mousePosition,0,0);
    Vector2Set(@windowPosition,0,0);
    panOffset := mousePosition;
    dragWindow := false;
    SetWindowPosition(Round(windowPosition.x),Round(windowPosition.y));
    exitWindow := false;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while ( not exitWindow and not WindowShouldClose) do
    begin
      // Update
      mousePosition.x := GetMouseX;
      mousePosition.y := GetMouseY;
      //GetMousePosition;

      if IsMouseButtonPressed(MOUSE_LEFT_BUTTON) then
      begin
          if CheckCollisionPointRec(mousePosition,RectangleCreate( 0, 0, screenWidth, 20 )) then
          begin
              dragWindow := true;
              panOffset := mousePosition;
          end;
      end;

      if dragWindow then
      begin
          windowPosition.x += (mousePosition.x - panOffset.x);
          windowPosition.y += (mousePosition.y - panOffset.y);

          if IsMouseButtonReleased(MOUSE_LEFT_BUTTON) then dragWindow := false;
             SetWindowPosition(trunc(windowPosition.x), trunc(windowPosition.y));
      end;

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        exitWindow := GuiWindowBox(RectangleCreate( 0, 0, screenWidth, screenHeight ), '#198# PORTABLE WINDOW');
        DrawText(TextFormat('Mouse Position: [ %.0, %.0 ]', [mousePosition.x,mousePosition.y]), 10, 40, 10, DARKGRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

