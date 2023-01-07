program shapes_rectangle_scaling;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  MOUSE_SCALE_MARK_SIZE = 12;

var
  Rec: TRectangle;
  MousePosition: TVector2;
  MouseScaleReady, MouseScaleMode: Boolean;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - rectangle scaling mouse');

  Rec := RectangleCreate(100, 100, 200, 80);
  MousePosition := Vector2Create(0, 0);
  MouseScaleMode := False;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      MousePosition := GetMousePosition();

      if CheckCollisionPointRec(MousePosition,
      RectangleCreate(Rec.X + Rec.Width - MOUSE_SCALE_MARK_SIZE,
                      Rec.Y + Rec.Height - MOUSE_SCALE_MARK_SIZE,
                      MOUSE_SCALE_MARK_SIZE, MOUSE_SCALE_MARK_SIZE )) then
      begin
        MouseScaleReady := True;
        if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
          MouseScaleMode := True;
      end else
        MouseScaleReady := False;

      if MouseScaleMode then
      begin
        MouseScaleReady := True;

        Rec.Width := (MousePosition.X - Rec.X);
        Rec.Height := (MousePosition.Y - Rec.Y);

        // Check minimum rec size
        if Rec.Width < MOUSE_SCALE_MARK_SIZE then
          Rec.Width := MOUSE_SCALE_MARK_SIZE;
        if Rec.Height < MOUSE_SCALE_MARK_SIZE then
          Rec.Height := MOUSE_SCALE_MARK_SIZE;

        // Check maximum rec size
        if Rec.Width > (GetScreenWidth() - Rec.X) then
          Rec.Width := GetScreenWidth() - Rec.X;
        if Rec.Height > (GetScreenHeight() - Rec.Y) then
          Rec.Height := GetScreenHeight() - Rec.Y;

        if IsMouseButtonReleased(MOUSE_BUTTON_LEFT) then
          MouseScaleMode := False;
      end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

            ClearBackground(RAYWHITE);

            DrawText(UTF8String('Scale rectangle dragging from bottom-right corner!'), 10, 10, 20, GRAY);

            DrawRectangleRec(Rec, Fade(GREEN, 0.5));

            if MouseScaleReady then
            begin
              DrawRectangleLinesEx(Rec, 1, RED);
              DrawTriangle(
                Vector2Create(Rec.X + Rec.Width - MOUSE_SCALE_MARK_SIZE, Rec.Y + Rec.Height),
                Vector2Create(Rec.x + Rec.Width, Rec.Y + Rec.Height),
                Vector2Create(Rec.x + Rec.Width, Rec.Y + Rec.Height - MOUSE_SCALE_MARK_SIZE),
                RED
              );
            end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

