program shapes_bouncing_ball;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  BallPosition, BallSpeed: TVector2;
  BallRadius: Integer;
  Pause: Boolean;
  FramesCounter: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - bouncing ball');

  BallPosition := Vector2Create(GetScreenWidth() / 2.0, GetScreenHeight() / 2.0);
  BallSpeed := Vector2Create(5.0, 4.0);
  BallRadius := 20;

  Pause := False;
  FramesCounter := 0;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_SPACE) then
        Pause := not Pause;

      if not Pause then
      begin
          BallPosition.X := BallPosition.X + BallSpeed.X;
          BallPosition.Y := BallPosition.Y + BallSpeed.Y;

          // Check walls collision for bouncing
          if ((BallPosition.X >= (GetScreenWidth() - BallRadius)) or (BallPosition.X <= BallRadius)) then
            BallSpeed.X := -BallSpeed.X;
          if ((BallPosition.Y >= (GetScreenHeight() - BallRadius)) or (BallPosition.Y <= BallRadius)) then
            BallSpeed.Y := -BallSpeed.Y;
      end else
        Inc(FramesCounter);

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawCircleV(BallPosition, BallRadius, MAROON);
      DrawText('PRESS SPACE to PAUSE BALL MOVEMENT', 10, GetScreenHeight() - 25, 20, LIGHTGRAY);

      // On pause, we draw a blinking message
      if Pause and (((FramesCounter div 30) mod 2) <> 0) then
        DrawText('PAUSED', 350, 200, 30, GRAY);

      DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

