program core_2d_camera_mouse_zoom;

{$mode objfpc}{$H+}
uses
  SysUtils,
  raylib,
  raymath,
  rlgl;

const
  ScreenWidth = 800;
  ScreenHeight = 450;
  ZoomIncrement = 0.125;
var
  Camera: TCamera2D;
  Delta, MouseWorldPos: TVector2;
  Wheel: Single;

begin
  // Initialization
  //---------------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);

  InitWindow(ScreenWidth, ScreenHeight, UTF8String('raylib [core] example - 2d camera mouse zoom'));

  Camera := Default(TCamera2D);
  Camera.Zoom := 1.0;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    // Translate based on mouse right click
    if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
    begin
      Delta := GetMouseDelta();
      Delta := Vector2Scale(Delta, -1.0 / Camera.Zoom);

      Camera.Target := Vector2Add(Camera.Target, Delta);
    end;

    // Zoom based on mouse wheel
    Wheel := GetMouseWheelMove();
    if Wheel <> 0 then
    begin
      // Get the world point that is under the mouse
      MouseWorldPos := GetScreenToWorld2D(GetMousePosition(), Camera);

      // Set the offset to where the mouse is
      Camera.Offset := GetMousePosition();

      // Set the target to match, so that the camera maps the world space point
      // under the cursor to the screen space point under the cursor at any zoom
      Camera.Target := mouseWorldPos;

      Camera.Zoom := Camera.Zoom + (Wheel * ZoomIncrement);
      if Camera.Zoom < ZoomIncrement then
        Camera.Zoom := ZoomIncrement;
    end;

    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(BLACK);

      BeginMode2D(Camera);

        // Draw the 3d grid, rotated 90 degrees and centered around 0,0
        // just so we have something in the XY plane
        rlPushMatrix();
          rlTranslatef(0, 25*50, 0);
          rlRotatef(90, 1, 0, 0);
          DrawGrid(100, 50);
        rlPopMatrix();

        // Draw a reference circle
        DrawCircle(100, 100, 50, YELLOW);

      EndMode2D();

      DrawText(UTF8String('Mouse right button drag to move, mouse wheel to zoom'), 10, 10, 20, WHITE);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  CloseWindow(); // Close window and OpenGL context
  //---------------------------------------------------------------------------------------------


end.
