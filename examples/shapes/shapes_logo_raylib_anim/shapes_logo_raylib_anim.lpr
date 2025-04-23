program shapes_logo_raylib_anim;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  LogoPositionX: Integer;
  LogoPositionY: Integer;
  FramesCounter: Integer;
  LettersCount: Integer;
  TopSideRecWidth: Integer;
  LeftSideRecHeight: Integer;
  BottomSideRecWidth: Integer;
  RightSideRecHeight: Integer;
  State: Integer;
  Alpha: Single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - raylib logo using shapes');

  LogoPositionX := ScreenWidth div 2 - 128;
  LogoPositionY := ScreenHeight div 2 - 128;

  FramesCounter := 0;
  LettersCount := 0;

  TopSideRecWidth := 16;
  LeftSideRecHeight := 16;

  BottomSideRecWidth := 16;
  RightSideRecHeight := 16;

  State := 0; // Tracking animation states (State Machine)
  Alpha := 1.0; // Useful for fading


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if State = 0 then // State 0: Small box blinking
      begin
        Inc(FramesCounter);

        if FramesCounter = 120 then
        begin
          State := 1;
          FramesCounter := 0; // Reset counter... will be used later...
        end;
      end
      else if State = 1 then // State 1: Top and left bars growing
      begin
        Inc(TopSideRecWidth, 4);
        Inc(LeftSideRecHeight, 4);

        if TopSideRecWidth = 256 then
          State := 2;
      end
      else if State = 2 then // State 2: Bottom and right bars growing
      begin
        Inc(BottomSideRecWidth, 4);
        Inc(RightSideRecHeight, 4);

        if BottomSideRecWidth = 256 then
          State := 3;
      end
      else if State = 3 then // State 3: Letters appearing (one by one)
      begin
        Inc(FramesCounter);

        if FramesCounter div 12 <> 0 then // Every 12 frames, one more letter!
        begin
          Inc(LettersCount);
          FramesCounter := 0;
        end;

        if LettersCount >= 10 then // When all letters have appeared, just fade out everything
        begin
          Alpha := Alpha - 0.02;

          if Alpha <= 0.0 then
          begin
            Alpha := 0.0;
            State := 4;
          end;
        end;
      end
      else if State = 4 then // State 4: Reset and Replay
      begin
        if IsKeyPressed(KEY_R) then
        begin
          FramesCounter := 0;
          LettersCount := 0;

          TopSideRecWidth := 16;
          LeftSideRecHeight := 16;

          BottomSideRecWidth := 16;
          RightSideRecHeight := 16;

          Alpha := 1.0;
          State := 0; // Return to State 0
        end;
      end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

        if State = 0 then
        begin
          if (framesCounter div 15) mod 2 <> 0 then
            DrawRectangle(LogoPositionX, LogoPositionY, 16, 16, BLACK);
        end
        else if State = 1 then
        begin
          DrawRectangle(LogoPositionX, LogoPositionY, TopSideRecWidth, 16, BLACK);
          DrawRectangle(LogoPositionX, LogoPositionY, 16, LeftSideRecHeight, BLACK);
        end
        else if State = 2 then
        begin
          DrawRectangle(LogoPositionX, LogoPositionY, TopSideRecWidth, 16, BLACK);
          DrawRectangle(LogoPositionX, LogoPositionY, 16, LeftSideRecHeight, BLACK);

          DrawRectangle(LogoPositionX + 240, LogoPositionY, 16, RightSideRecHeight, BLACK);
          DrawRectangle(LogoPositionX, LogoPositionY + 240, BottomSideRecWidth, 16, BLACK);
        end
        else if State = 3 then
        begin
          DrawRectangle(LogoPositionX, LogoPositionY, TopSideRecWidth, 16, Fade(BLACK, Alpha));
          DrawRectangle(LogoPositionX, LogoPositionY + 16, 16, LeftSideRecHeight - 32, Fade(BLACK, Alpha));

          DrawRectangle(LogoPositionX + 240, LogoPositionY + 16, 16, RightSideRecHeight - 32, Fade(BLACK, Alpha));
          DrawRectangle(LogoPositionX, LogoPositionY + 240, BottomSideRecWidth, 16, Fade(BLACK, Alpha));

          DrawRectangle(GetScreenWidth() div 2 - 112, GetScreenHeight() div 2 - 112, 224, 224, Fade(RAYWHITE, Alpha));

          DrawText(TextSubtext('raylib', 0, LettersCount), GetScreenWidth() div 2 - 44, GetScreenHeight() div 2 + 48, 50, Fade(BLACK, Alpha));
        end
        else if State = 4 then
        begin
          DrawText('[R] REPLAY', 340, 200, 20, GRAY);
        end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

