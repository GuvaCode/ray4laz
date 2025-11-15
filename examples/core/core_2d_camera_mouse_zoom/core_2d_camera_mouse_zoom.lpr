(*******************************************************************************************
*
*   raylib [core] example - 2d camera mouse zoom
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 4.2, last time updated with raylib 4.2
*
*   Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2022-2025 Jeffery Myers (@JeffM2501)
*   Pascal translation (c) 2022-2025 Vadim Gunko(@GuvaCode)
*
********************************************************************************************)
program core_2d_camera_mouse_zoom;

{$mode objfpc}{$H+}

uses
  Math,
  raylib,
  rlgl,
  raymath;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  camera: TCamera2D;
  zoomMode: Integer;
  delta: TVector2;
  wheel: Single;
  mouseWorldPos: TVector2;
  scale, deltaX: Single;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 2d camera mouse zoom');

  camera.zoom := 1.0;
  camera.offset := Vector2Create(0, 0);
  camera.target := Vector2Create(0, 0);
  camera.rotation := 0;

  zoomMode := 0;   // 0-Mouse Wheel, 1-Mouse Move

  SetTargetFPS(60);                   // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do        // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    if IsKeyPressed(KEY_ONE) then
      zoomMode := 0
    else if IsKeyPressed(KEY_TWO) then
      zoomMode := 1;

    // Translate based on mouse right click
    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
    begin
      delta := GetMouseDelta();
      delta := Vector2Scale(delta, -1.0 / camera.zoom);
      camera.target := Vector2Add(camera.target, delta);
    end;

    if zoomMode = 0 then
    begin
      // Zoom based on mouse wheel
      wheel := GetMouseWheelMove();
      if wheel <> 0 then
      begin
        // Get the world point that is under the mouse
        mouseWorldPos := GetScreenToWorld2D(GetMousePosition(), camera);

        // Set the offset to where the mouse is
        camera.offset := GetMousePosition();

        // Set the target to match, so that the camera maps the world space point
        // under the cursor to the screen space point under the cursor at any zoom
        camera.target := mouseWorldPos;

        // Zoom increment
        // Uses log scaling to provide consistent zoom speed
        scale := 0.2 * wheel;
        camera.zoom := Clamp(Exp(Ln(camera.zoom) + scale), 0.125, 64.0);
      end;
    end
    else
    begin
      // Zoom based on mouse right click
      if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
      begin
        // Get the world point that is under the mouse
        mouseWorldPos := GetScreenToWorld2D(GetMousePosition(), camera);

        // Set the offset to where the mouse is
        camera.offset := GetMousePosition();

        // Set the target to match, so that the camera maps the world space point
        // under the cursor to the screen space point under the cursor at any zoom
        camera.target := mouseWorldPos;
      end;
      if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
      begin
        // Zoom increment
        // Uses log scaling to provide consistent zoom speed
        deltaX := GetMouseDelta().x;
        scale := 0.005 * deltaX;
        camera.zoom := Clamp(Exp(Ln(camera.zoom) + scale), 0.125, 64.0);
      end;
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode2D(camera);

        // Draw the 3d grid, rotated 90 degrees and centered around 0,0
        // just so we have something in the XY plane
        rlPushMatrix();
          rlTranslatef(0, 25 * 50, 0);
          rlRotatef(90, 1, 0, 0);
          DrawGrid(100, 50);
        rlPopMatrix();

        // Draw a reference circle
        DrawCircle(screenWidth div 2, screenHeight div 2, 50, MAROON);

      EndMode2D();

      // Draw mouse reference
      DrawCircleV(GetMousePosition(), 4, DARKGRAY);
      DrawTextEx(GetFontDefault(), TextFormat('%i, %i', GetMouseX(), GetMouseY()),
                Vector2Add(GetMousePosition(), Vector2Create(-44, -24)), 20, 2, BLACK);

      DrawText('[1][2] Select mouse zoom mode (Wheel or Move)', 20, 20, 20, DARKGRAY);
      if zoomMode = 0 then
        DrawText('Mouse left button drag to move, mouse wheel to zoom', 20, 50, 20, DARKGRAY)
      else
        DrawText('Mouse left button drag to move, mouse press and move to zoom', 20, 50, 20, DARKGRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
