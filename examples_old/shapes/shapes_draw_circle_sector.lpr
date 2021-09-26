(*******************************************************************************************
*
*   raylib [shapes] example - draw circle sector (with gui options)
*
*   This example has been created using raylib 2.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2018 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
*   Pascal translation Gunko Vadim (@guvacode)
********************************************************************************************)
program shapes_draw_circle_sector;

{$mode objfpc}{$H+}

uses cmem,
{uncomment if necessary}
//ray_math, 
//ray_rlgl, 
ray_gui,
ray_header; 

const
 screenWidth = 800;
 screenHeight = 450;

var
     center:TVector2;
     outerRadius:single = 180.0;
     startAngle:single = 0.0;
     endAngle:single = 180.0;
     segments:integer = 0;
     minSegments:integer = 4;

begin

 InitWindow(screenWidth, screenHeight, 'raylib pascal - basic window');
 Vector2Create((GetScreenWidth() - 300)/2, GetScreenHeight()/2);
 SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
  BeginDrawing();
  ClearBackground(RAYWHITE);

  DrawLine(500, 0, 500, GetScreenHeight(), Fade(LIGHTGRAY, 0.6));
  DrawRectangle(500, 0, GetScreenWidth() - 500, GetScreenHeight(), Fade(LIGHTGRAY, 0.3));

  DrawCircleSector(center, outerRadius, startAngle, endAngle, segments, Fade(MAROON, 0.3));
  DrawCircleSectorLines(center, outerRadius, startAngle, endAngle, segments, Fade(MAROON, 0.6));

  // Draw GUI controls
  //------------------------------------------------------------------------------
  //startAngle =
  GuiSliderBar(RectangleCreate( 600, 40, 120, 20), 'StartAngle', nil, startAngle, 0, 720);
  //endAngle =
  GuiSliderBar(RectangleCreate( 600, 70, 120, 20), 'EndAngle', nil, endAngle, 0, 720);

           // outerRadius =
  GuiSliderBar(RectangleCreate( 600, 140, 120, 20), 'Radius', nil, outerRadius, 0, 200);
            //segments = GuiSliderBar((Rectangle){ 600, 170, 120, 20}, "Segments", NULL, segments, 0, 100);
            //------------------------------------------------------------------------------

       //     minSegments = (int)ceilf((endAngle - startAngle) / 90);
        //    DrawText(TextFormat("MODE: %s", (segments >= minSegments)? "MANUAL" : "AUTO"), 600, 200, 10, (segments >= minSegments)? MAROON : DARKGRAY);

        //    DrawFPS(10, 10);

  EndDrawing(); 
 end;
CloseWindow(); 

end.

