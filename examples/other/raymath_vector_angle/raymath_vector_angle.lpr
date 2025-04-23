{
*******************************************************************************************
*
*   raylib [shapes] example - Vector Angle
*
*   Example originally created with raylib 1.0, last time updated with raylib 4.6
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Ramon Santamaria (@raysan5)
*   Pascal conversion 2024 Vadim Gunko (@guvacode)
*
********************************************************************************************
}
program raymath_vector_angle;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  v0, v1, v2, v1Normal, v2Normal : TVector2;
  angle, startangle: Single;
  angleMode: Boolean;



begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [math] example - vector angle"');

  v0 := Vector2Create(screenWidth/2, screenHeight/2 );
  v1 := Vector2Add(v0, Vector2Create( 100.0, 80.0 ));
  v2 := Vector2Zero;  // Updated with mouse position

  angle := 0.0;    // Angle in degrees
  angleMode := false;  // 0-Vector2Angle(), 1-Vector2LineAngle()

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here
      startangle := 0.0;

      if (angleMode = false) then startangle := -Vector2LineAngle(v0, v1)*RAD2DEG;
      if (angleMode = true) then startangle := 0.0;

      v2 := GetMousePosition();

      if (IsKeyPressed(KEY_SPACE)) then angleMode :=  not angleMode;

      if(angleMode = false) and IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then v1 := GetMousePosition();

      if (angleMode = false) then
      begin
        // Calculate angle between two vectors, considering a common origin (v0)
        v1Normal := Vector2Normalize(Vector2Subtract(v1, v0));
        v2Normal := Vector2Normalize(Vector2Subtract(v2, v0));
        angle := Vector2Angle(v1Normal, v2Normal)*RAD2DEG;
      end
      else if (angleMode = true) then
      begin
        // Calculate angle defined by a two vectors line, in reference to horizontal line
        angle := Vector2LineAngle(v0, v2)*RAD2DEG;
      end;
      // Draw
      BeginDrawing();

      ClearBackground(RAYWHITE);

      if (angleMode = false) then
      begin
        DrawText('MODE 0: Angle between V1 and V2', 10, 10, 20, BLACK);
        DrawText('Right Click to Move V2', 10, 30, 20, DARKGRAY);

        DrawLineEx(v0, v1, 2.0, BLACK);
        DrawLineEx(v0, v2, 2.0, RED);

        DrawCircleSector(v0, 40.0, startangle, startangle + angle, 32, Fade(GREEN, 0.6));
      end
      else if (angleMode = true) then
      begin
        DrawText('MODE 1: Angle formed by line V1 to V2', 10, 10, 20, BLACK);

        DrawLine(0, screenHeight div 2, screenWidth, screenHeight div 2, LIGHTGRAY);
        DrawLineEx(v0, v2, 2.0, RED);

        DrawCircleSector(v0, 40.0, startangle, startangle - angle, 32, Fade(GREEN, 0.6));
      end;

      DrawText('v0', Round(v0.x), Round(v0.y), 10, DARKGRAY);

      // If the line from v0 to v1 would overlap the text, move it's position up 10
      if (angleMode = false) and (Vector2Subtract(v0, v1).y > 0.0) then DrawText('v1', Round(v1.x), Round(v1.y-10.0), 10, DARKGRAY);
      if (angleMode = false) and (Vector2Subtract(v0, v1).y < 0.0) then DrawText('v1', Round(v1.x), Round(v1.y), 10, DARKGRAY);

      // If angle mode 1, use v1 to emphasize the horizontal line
      if (angleMode = true) then DrawText('v1', Round(v0.x + 40.0), Round(v0.y), 10, DARKGRAY);

      // position adjusted by -10 so it isn't hidden by cursor
      DrawText('v2', Round(v2.x-10.0), Round(v2.y-10.0), 10, DARKGRAY);

      DrawText('Press SPACE to change MODE', 460, 10, 20, DARKGRAY);
      DrawText(TextFormat('ANGLE: %2.2f', angle), 10, 70, 20, LIME);
      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

