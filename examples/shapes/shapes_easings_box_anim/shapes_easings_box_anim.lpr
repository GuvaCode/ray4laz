program shapes_easings_box_anim;

{$mode objfpc}{$H+}

uses 
cmem, raylib, reasings, math;

const
  screenWidth = 800;
  screenHeight = 450;
var
  State: Integer;
  FramesCounter: Integer;
  Rec: TRectangle;
  Alpha, Rotation: Single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - easings box anim');

  // Box variables to be animated with easings
  Rec := RectangleCreate(GetScreenWidth() / 2.0, -100, 100, 100);
  Rotation := 0.0;
  Alpha := 1.0;
  State := 0;
  FramesCounter := 0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      case State of
        0: // Move box down to center of screen
        begin
          Inc(FramesCounter);
          // NOTE: Remember that 3rd parameter of easing function refers to
          // desired value variation, do not confuse it with expected final value!
          Rec.Y := EaseElasticOut(FramesCounter, -100, GetScreenHeight() / 2.0 + 100, 120);

          if FramesCounter >= 120 then
          begin
            FramesCounter := 0;
            State := 1;
          end;
        end;
        1: // Scale box to an horizontal bar
        begin
          Inc(FramesCounter);
          Rec.Height := EaseBounceOut(FramesCounter, 100, -90, 120);
          Rec.Width := EaseBounceOut(FramesCounter, 100, GetScreenWidth(), 120);

          if FramesCounter >= 120 then
          begin
            FramesCounter := 0;
            State := 2;
          end;
        end;
        2: // Rotate horizontal bar rectangle
        begin
          Inc(FramesCounter);
          Rotation := EaseQuadOut(FramesCounter, 0.0, 270.0, 240);

          if FramesCounter >= 240 then
          begin
            FramesCounter := 0;
            State := 3;
          end;
        end;
        3: // Increase bar size to fill all screen
        begin
          Inc(FramesCounter);
          Rec.Height := EaseCircOut(FramesCounter, 10, GetScreenWidth(), 120);

          if FramesCounter >= 120 then
          begin
            FramesCounter := 0;
            State := 4;
          end;
        end;
        4: // Fade out animation
        begin
          Inc(FramesCounter);
          Alpha := EaseSineOut(FramesCounter, 1.0, -1.0, 160);

          if FramesCounter >= 160 then
          begin
            FramesCounter := 0;
            State := 5;
          end;
        end;
      end;

      // Reset animation at any moment
      if IsKeyPressed(KEY_SPACE) then
      begin
        Rec := RectangleCreate(GetScreenWidth() / 2.0, -100, 100, 100);
        Rotation := 0.0;
        Alpha := 1.0;
        State := 0;
        FramesCounter := 0;
      end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawRectanglePro(Rec, Vector2Create(Rec.Width / 2, Rec.Height / 2), Rotation, Fade(BLACK, Alpha));

        DrawText('PRESS [SPACE] TO RESET BOX ANIMATION!', 10, GetScreenHeight() - 25, 20, LIGHTGRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

