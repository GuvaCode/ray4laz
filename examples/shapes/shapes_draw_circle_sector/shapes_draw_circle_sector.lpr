program shapes_draw_circle_sector;

{$mode objfpc}{$H+}

uses
raylib, raygui, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  center: TVector2;
  outerRadius: single;

  startAngle,endAngle: single;

  segments: single;
  minsegments: longint;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - draw circle sector');

  center := Vector2Create((GetScreenWidth - 300) /2.0 , GetScreenHeight / 2.0);
  outerRadius := 180.0;
  startAngle := 0;
  endAngle := 180.0;
  segments := 0;
  minSegments := 4;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawLine(500, 0, 500, GetScreenHeight(), Fade(LIGHTGRAY, 0.6));
      DrawRectangle(500, 0, GetScreenWidth() - 500, GetScreenHeight(), Fade(LIGHTGRAY, 0.3));

     if startAngle = endAngle then endAngle:=endAngle-0.1;

     try
      DrawCircleSector(center, outerRadius, startAngle, endAngle, Round(segments), Fade(MAROON, 0.3));
      DrawCircleSectorLines(center, outerRadius, startAngle, endAngle, Round(segments), Fade(MAROON, 0.6));
     except
       writeln('except Draw');
     end;

      // Draw GUI controls
      //------------------------------------------------------------------------------
      GuiSliderBar(RectangleCreate( 600, 40, 120, 20), 'StartAngle',nil, @startAngle, 0, 720);
      GuiSliderBar(RectangleCreate( 600, 70, 120, 20), 'EndAngle', nil, @endAngle, 0, 720);
      GuiSliderBar(RectangleCreate( 600, 140, 120, 20), 'Radius', nil, @outerRadius, 0, 200);
      GuiSliderBar(RectangleCreate( 600, 170, 120, 20), 'Segments', nil, @segments, 0, 100);
      //------------------------------------------------------------------------------

      minSegments:= ceil((endAngle - startAngle) / 90);

      if segments >= minSegments then
      DrawText('MANUAL',600,20,10,DARKGRAY) else
      DrawText('AUTO',600,20,10,DARKGRAY);

      DrawFPS(10, 10);
      EndDrawing();
     end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

