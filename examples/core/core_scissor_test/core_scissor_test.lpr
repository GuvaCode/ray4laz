program core_scissor_test;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
var
  ScissorArea: TRectangle;
  ScissorMode: Boolean;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - scissor test');

  ScissorArea := RectangleCreate(0, 0, 300, 300);
  ScissorMode := True;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_S) then
        ScissorMode := not ScissorMode;

      // Centre the scissor area around the mouse position
      ScissorArea.X := GetMouseX() - ScissorArea.Width / 2;
      ScissorArea.Y := GetMouseY() - ScissorArea.Height / 2;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
      if ScissorMode then
        BeginScissorMode(Trunc(ScissorArea.X), Trunc(ScissorArea.Y), Trunc(ScissorArea.Width), Trunc(ScissorArea.Height));

      // Draw full screen rectangle and some text
      // NOTE: Only part defined by scissor area will be rendered
      DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), RED);
      DrawText('Move the mouse around to reveal this text!', 190, 200, 20, LIGHTGRAY);

      if ScissorMode then
        EndScissorMode();

      DrawRectangleLinesEx(ScissorArea, 1, BLACK);
      DrawText('Press S to toggle scissor test', 10, 10, 20, BLACK);


      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

