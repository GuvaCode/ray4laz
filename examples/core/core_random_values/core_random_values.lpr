program core_random_values;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  RandValue: Integer;
  FramesCounter: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - generate random values');

  RandValue := GetRandomValue(-8, 5); // Get a random integer number between -8 and 5 (both included)

  FramesCounter := 0; // Variable used to count frames

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      FramesCounter := FramesCounter + 1;
      // Every two seconds (120 frames) a new random value is generated
      if (FramesCounter div 120) mod 2 = 1 then
      begin
        RandValue := GetRandomValue(-8, 5);
        FramesCounter := 0;
      end;
      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText('Every 2 seconds a new random value is generated:', 130, 100, 20, MAROON);
        DrawText(TextFormat('%i', randValue), 360, 180, 80, LIGHTGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

