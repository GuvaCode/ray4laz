//raylib - Standard Game template
program standard_game;

{$mode objfpc}{$H+}
uses cmem, ray_headers, math;

type TGameScreen = (LOGO = 0, TITLE, GAMEPLAY, ENDING);

var
  CurrentScreen: TGameScreen;
  FramesCounter:integer;

const
  screenWidth = 800;
  screenHeight = 450;

begin
  InitWindow(screenWidth, screenHeight, 'raylib template - simple game');
   CurrentScreen := LOGO;
   // TODO: Initialize all required variables and load all required data here!
   FramesCounter:=0; // Useful to count frames
   SetTargetFPS(60);// Set desired framerate (frames-per-second)

   while not WindowShouldClose do // Main game loop
   begin
    // Update
     case CurrentScreen of
      LOGO:
      begin
       // TODO: Update LOGO screen variables here!
       inc(FramesCounter);
       // Wait for 2 seconds (120 frames) before jumping to TITLE screen
       if framesCounter > 120 then currentScreen := TITLE;
      end;
      TITLE:
      begin
       // TODO: Update TITLE screen variables here!
       // Press enter to change to GAMEPLAY screen
       if IsKeyPressed(KEY_ENTER) or  IsGestureDetected(GESTURE_TAP) then
       CurrentScreen := GAMEPLAY;
      end;
      GAMEPLAY:
      begin
       // TODO: Update TITLE screen variables here!
       // Press enter to change to GAMEPLAY screen
       if IsKeyPressed(KEY_ENTER) or  IsGestureDetected(GESTURE_TAP) then
       CurrentScreen := ENDING;
      end;
      ENDING:
      begin
       // TODO: Update TITLE screen variables here!
       // Press enter to change to GAMEPLAY screen
       if IsKeyPressed(KEY_ENTER) or  IsGestureDetected(GESTURE_TAP) then
       CurrentScreen := TITLE;
      end;
     end;  // end case

    // Draw
    BeginDrawing();
    ClearBackground(RAYWHITE);

    case CurrentScreen of
    LOGO: begin
     // TODO: Draw LOGO screen here!
     DrawText('LOGO SCREEN', 20, 20, 40, LIGHTGRAY);
     DrawText('WAIT for 2 SECONDS...', 290, 220, 20, GRAY);
    end;
    TITLE: begin
     // TODO: Draw TITLE screen here!
     DrawRectangle(0, 0, screenWidth, screenHeight, GREEN);
     DrawText('TITLE SCREEN', 20, 20, 40, DARKGREEN);
     DrawText('PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN', 120, 220, 20, DARKGREEN);
    end;
    GAMEPLAY: begin
    // TODO: Draw GAMEPLAY screen here!
    DrawRectangle(0, 0, screenWidth, screenHeight, PURPLE);
    DrawText('GAMEPLAY SCREEN', 20, 20, 40, MAROON);
    DrawText('PRESS ENTER or TAP to JUMP to ENDING SCREEN', 130, 220, 20, MAROON);
    end;
    ENDING: begin
    // TODO: Draw ENDING screen here!
    DrawRectangle(0, 0, screenWidth, screenHeight, BLUE);
    DrawText('ENDING SCREEN', 20, 20, 40, DARKBLUE);
    DrawText('PRESS ENTER or TAP to RETURN to TITLE SCREEN', 120, 220, 20, DARKBLUE);
    end;

   end;

    EndDrawing();
   end;
   // De-Initialization
   // TODO: Unload all loaded data (textures, fonts, audio) here!
   CloseWindow();        // Close window and OpenGL context
 end.

