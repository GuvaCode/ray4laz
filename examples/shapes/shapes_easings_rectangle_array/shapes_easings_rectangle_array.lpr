program shapes_easings_rectangle_array;

{$mode objfpc}{$H+}

uses 
cmem, 
Math,
raylib,
reasings;

const
  screenWidth = 800;
  screenHeight = 450;
  RECS_WIDTH = 50;
  RECS_HEIGHT = 50;
  MAX_RECS_X = 800 div RECS_WIDTH;
  MAX_RECS_Y = 450 div RECS_HEIGHT;
  PLAY_TIME_IN_FRAMES = 240; // At 60 fps = 4 seconds
var
  State: Integer;
  FramesCounter: Integer;
  Rotation: Single;
  X, Y, I: Integer;
  Recs: array [0..MAX_RECS_X*MAX_RECS_Y-1] of TRectangle;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - easings rectangle array');

  for Y := 0 to MAX_RECS_Y - 1 do
  begin
    for X := 0 to MAX_RECS_X - 1 do
    begin
      Recs[Y * MAX_RECS_X + X].X := RECS_WIDTH / 2.0 + RECS_WIDTH * X;
      Recs[Y * MAX_RECS_X + X].Y := RECS_HEIGHT / 2.0 + RECS_HEIGHT * Y;
      Recs[Y * MAX_RECS_X + X].Width := RECS_WIDTH;
      Recs[Y * MAX_RECS_X + X].Height := RECS_HEIGHT;
    end;
  end;

  State := 0;
  FramesCounter := 0;
  Rotation := 0;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if State = 0 then
      begin
        FramesCounter := FramesCounter + 2;

        for I := 0 to MAX_RECS_X * MAX_RECS_Y - 1 do
        begin
          Recs[I].Height := EaseCircOut(FramesCounter, RECS_HEIGHT, -RECS_HEIGHT, PLAY_TIME_IN_FRAMES);
          Recs[I].Width := EaseCircOut(FramesCounter, RECS_WIDTH, -RECS_WIDTH, PLAY_TIME_IN_FRAMES);

          if Recs[I].Height < 0 then
            Recs[I].Height := 0;
          if Recs[I].Width < 0 then
            Recs[I].Width := 0;

          if ((Recs[I].Height = 0) and (Recs[I].Width = 0)) then
            State := 1; // Finish playing

          Rotation := EaseLinearIn(FramesCounter, 0.0, 360.0, PLAY_TIME_IN_FRAMES);
        end;
      end
      else if (State = 1) and IsKeyPressed(KEY_SPACE) then
      begin
        // When animation has finished, press space to restart
        FramesCounter := 0;

        for I := 0 to MAX_RECS_X * MAX_RECS_Y - 1 do
        begin
          Recs[I].Height := RECS_HEIGHT;
          Recs[I].Width := RECS_WIDTH;
        end;

        State := 0;
      end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       if State = 0 then
       begin
         for I := 0 to MAX_RECS_X * MAX_RECS_Y - 1 do
           DrawRectanglePro(Recs[I], Vector2Create(Recs[I].Width / 2, Recs[I].Height / 2), Rotation, RED);
       end
       else if State = 1 then
         DrawText('PRESS [SPACE] TO PLAY AGAIN!', 240, 200, 20, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

