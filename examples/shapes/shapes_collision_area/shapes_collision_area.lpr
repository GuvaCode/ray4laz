program shapes_collision_area;

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
var
  BoxA: TRectangle;
  BoxASpeedX: Integer;
  BoxB: TRectangle;
  BoxCollision: TRectangle;
  ScreenUpperLimit: Integer;
  Pause, Collision: Boolean;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - collision area');

  // Box A: Moving box
  BoxA := RectangleCreate(10, GetScreenHeight() / 2.0 - 50, 200, 100);
  BoxASpeedX := 4;

  // Box B: Mouse moved box
  BoxB := RectangleCreate(GetScreenWidth() / 2.0 - 30, GetScreenHeight() / 2.0 - 30, 60, 60);

  BoxCollision := Default(TRectangle); // Collision rectangle
  ScreenUpperLimit := 40; // Top menu limits
  Pause := False; // Movement pause
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Move box if not paused
      if not Pause then
        BoxA.X := BoxA.X + BoxASpeedX;

      // Bounce box on x screen limits
      if (((BoxA.X + BoxA.Width) >= GetScreenWidth()) or (BoxA.X <= 0)) then
        BoxASpeedX := -BoxASpeedX;

      // Update player-controlled-box (box02)
      BoxB.X := GetMouseX() - BoxB.Width / 2;
      BoxB.Y := GetMouseY() - BoxB.Height / 2;

      // Make sure Box B does not go out of move area limits
      if ((BoxB.X + BoxB.Width) >= GetScreenWidth()) then
        BoxB.X := GetScreenWidth() - BoxB.width
      else if BoxB.X <= 0 then
        BoxB.X := 0;

      if (BoxB.Y + BoxB.Height) >= GetScreenHeight() then
        BoxB.Y := GetScreenHeight() - BoxB.Height
      else if BoxB.Y <= ScreenUpperLimit then
        BoxB.Y := ScreenUpperLimit;

      // Check boxes collision
      Collision := CheckCollisionRecs(BoxA, BoxB);

      // Get collision rectangle (only on collision)
      if Collision then
        BoxCollision := GetCollisionRec(BoxA, BoxB);

      // Pause Box A movement
      if IsKeyPressed(KEY_SPACE) then
        Pause := not Pause;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      if Collision then
        DrawRectangle(0, 0, ScreenWidth, ScreenUpperLimit, RED)
      else
        DrawRectangle(0, 0, ScreenWidth, ScreenUpperLimit, BLACK);

      DrawRectangleRec(BoxA, GOLD);
      DrawRectangleRec(BoxB, BLUE);

      if Collision then
      begin
        // Draw collision area
        DrawRectangleRec(BoxCollision, LIME);
        // Draw collision message
        DrawText('COLLISION!', GetScreenWidth() div 2 - MeasureText('COLLISION!', 20) div 2, ScreenUpperLimit div 2 - 10, 20, BLACK);
        // Draw collision area
        DrawText(TextFormat('Collision Area: %i', Integer(Trunc(BoxCollision.Width * BoxCollision.Height))), GetScreenWidth() div 2 - 100, ScreenUpperLimit + 10, 20, BLACK);
      end;

      DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

