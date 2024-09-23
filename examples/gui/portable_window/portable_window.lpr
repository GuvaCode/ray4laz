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
*   Copyright (c) 2016-2024 Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunko Vadim
*
**********************************************************************************************}
program portable_window;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib, raygui, mouse;

const
  screenWidth = 800;
  screenHeight = 450;

var
  mousePosition, windowPosition, panOffset: TVector2;
  dragWindow, exitWindow: Boolean;


begin
  // Initialization
  SetConfigFlags(FLAG_WINDOW_UNDECORATED);
  InitWindow(screenWidth, screenHeight, 'raygui - portable window');

  // General variables
  mousePosition := Vector2Create(0,0);
  windowPosition := Vector2Create( 500, 200 );
  panOffset := Vector2Create(0,0);   ;
  dragWindow := false;

  SetWindowPosition(Round(windowPosition.x), Trunc(windowPosition.y));

  exitWindow := false;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while ((not exitWindow) and (not WindowShouldClose)) do    // Detect window close button or ESC key
    begin
      // Update
      mousePosition.x :=  Round(GetMousePosition.x);
      mousePosition.y := Round(GetMousePosition.y);

      if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON) and not dragWindow) then
      begin
          if (CheckCollisionPointRec(mousePosition, RectangleCreate( 0, 0, screenWidth, 20 ))) then
          begin
            dragWindow := true;

          end;
      end;

      if (dragWindow) then
      begin
          windowPosition.x := windowPosition.x + (mousePosition.x - panOffset.x);
          windowPosition.y := windowPosition.y + (mousePosition.y - panOffset.y);
          SetWindowPosition(Round(windowPosition.x), Round(windowPosition.y));
          if (IsMouseButtonReleased(MOUSE_LEFT_BUTTON)) then
          begin
            dragWindow := false;
            panOffset:=  GetMousePosition;
          end;
      end;


      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);

        if GuiWindowBox(RectangleCreate( 0, 0, screenWidth, screenHeight ), '#198# PORTABLE WINDOW') = 0 then
        exitWindow := False else exitWindow := True;

        DrawText(TextFormat('Mouse Position: [ %.0f, %.0f ]', mousePosition.x, mousePosition.y), 10, 40, 10, DARKGRAY);
        DrawText(TextFormat('Window Position: [ %.0f, %.0f ]', windowPosition.x, windowPosition.y), 10, 60, 10, DARKGRAY);
      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

