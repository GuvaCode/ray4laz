(*******************************************************************************************
*
*   raylib [core] example - 2d camera
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 1.5, last time updated with raylib 3.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
*   Pascal translation (c) 2021-2025 Vadim Gunko (@GuvaCode)
*
********************************************************************************************)
program core_2d_camera;

{$mode objfpc}{$H+}

uses
  Math,
  raylib;

const
  MAX_BUILDINGS = 100;

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
var
  screenWidth, screenHeight: Integer;
  player: TRectangle;
  buildings: array[0..MAX_BUILDINGS-1] of TRectangle;
  buildColors: array[0..MAX_BUILDINGS-1] of TColor;
  spacing, i: Integer;
  camera: TCamera2D;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  screenWidth := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 2d camera');

  // Initialize player
  player.x := 400;
  player.y := 280;
  player.width := 40;
  player.height := 40;

  // Initialize buildings
  spacing := 0;
  for i := 0 to MAX_BUILDINGS - 1 do
  begin
    buildings[i].width := GetRandomValue(50, 200);
    buildings[i].height := GetRandomValue(100, 800);
    buildings[i].y := screenHeight - 130.0 - buildings[i].height;
    buildings[i].x := -6000.0 + spacing;

    spacing := spacing + Trunc(buildings[i].width);

    buildColors[i].r := GetRandomValue(200, 240);
    buildColors[i].g := GetRandomValue(200, 240);
    buildColors[i].b := GetRandomValue(200, 250);
    buildColors[i].a := 255;
  end;

  // Initialize camera
  camera.target.x := player.x + 20.0;
  camera.target.y := player.y + 20.0;
  camera.offset.x := screenWidth / 2.0;
  camera.offset.y := screenHeight / 2.0;
  camera.rotation := 0.0;
  camera.zoom := 1.0;

  SetTargetFPS(60);                   // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do        // Detect window close button or ESC key
  begin
    // Update
    //----------------------------------------------------------------------------------
    // Player movement
    if IsKeyDown(KEY_RIGHT) then
      player.x := player.x + 2
    else if IsKeyDown(KEY_LEFT) then
      player.x := player.x - 2;

    // Camera target follows player
    camera.target.x := player.x + 20;
    camera.target.y := player.y + 20;

    // Camera rotation controls
    if IsKeyDown(KEY_A) then
      camera.rotation := camera.rotation - 1
    else if IsKeyDown(KEY_S) then
      camera.rotation := camera.rotation + 1;

    // Limit camera rotation to 80 degrees (-40 to 40)
    if camera.rotation > 40 then
      camera.rotation := 40
    else if camera.rotation < -40 then
      camera.rotation := -40;

    // Camera zoom controls
    // Uses log scaling to provide consistent zoom speed
    camera.zoom := Exp(Ln(camera.zoom) + (GetMouseWheelMove() * 0.1));

    if camera.zoom > 3.0 then
      camera.zoom := 3.0
    else if camera.zoom < 0.1 then
      camera.zoom := 0.1;

    // Camera reset (zoom and rotation)
    if IsKeyPressed(KEY_R) then
    begin
      camera.zoom := 1.0;
      camera.rotation := 0.0;
    end;
    //----------------------------------------------------------------------------------

    // Draw
    //----------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      BeginMode2D(camera);

        DrawRectangle(-6000, 320, 13000, 8000, DARKGRAY);

        for i := 0 to MAX_BUILDINGS - 1 do
          DrawRectangleRec(buildings[i], buildColors[i]);

        DrawRectangleRec(player, RED);

        DrawLine(Trunc(camera.target.x), -screenHeight * 10, Trunc(camera.target.x), screenHeight * 10, GREEN);
        DrawLine(-screenWidth * 10, Trunc(camera.target.y), screenWidth * 10, Trunc(camera.target.y), GREEN);

      EndMode2D();

      DrawText('SCREEN AREA', 640, 10, 20, RED);

      DrawRectangle(0, 0, screenWidth, 5, RED);
      DrawRectangle(0, 5, 5, screenHeight - 10, RED);
      DrawRectangle(screenWidth - 5, 5, 5, screenHeight - 10, RED);
      DrawRectangle(0, screenHeight - 5, screenWidth, 5, RED);

      DrawRectangle(10, 10, 250, 113, Fade(SKYBLUE, 0.5));
      DrawRectangleLines(10, 10, 250, 113, BLUE);

      DrawText('Free 2d camera controls:', 20, 20, 10, BLACK);
      DrawText('- Right/Left to move Offset', 40, 40, 10, DARKGRAY);
      DrawText('- Mouse Wheel to Zoom in-out', 40, 60, 10, DARKGRAY);
      DrawText('- A / S to Rotate', 40, 80, 10, DARKGRAY);
      DrawText('- R to reset Zoom and Rotation', 40, 100, 10, DARKGRAY);

    EndDrawing();
    //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
