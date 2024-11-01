program shapes_splines_drawing;

{*******************************************************************************************
*
*   raylib [shapes] example - splines drawing
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Ramon Santamaria (@raysan5)
*   Pascal conversion (2024) Gunko Vadim (@guvacode)
*
********************************************************************************************}

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl,
raygui, // Required for UI controls
raylib, math;


// Cubic Bezier spline control points
// NOTE: Every segment has two control points
type
  PControlPoint = ^TControlPoint;
  TControlPoint = record
    start: TVector2;
    end_ : TVector2;
  end;

  PSplineType = ^TSplineType;
  TSplineType =  Integer;
  const // Spline types
    SPLINE_LINEAR     = TSplineType(0); // Linear
    SPLINE_BASIS      = TSplineType(1); // B-Spline
    SPLINE_CATMULLROM = TSplineType(2); // Catmull-Rom
    SPLINE_BEZIER     = TSplineType(3); // Cubic Bezier

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_SPLINE_POINTS = 32;

var
  points: array[0..MAX_SPLINE_POINTS] of TVector2;
  // Array required for spline bezier-cubic,
  // including control points interleaved with start-end segment points
  pointsInterleaved: array[0..(3 * (MAX_SPLINE_POINTS - 1) + 1)] of TVector2;
  pointCount, selectedPoint, focusedPoint, i: integer;
  selectedControlPoint, focusedControlPoint: PVector2;
  pointColor: TColorB;
  // Cubic Bezier control points
  control: array[0..MAX_SPLINE_POINTS-1] of TControlPoint;

  // Spline config variables
  splineThickness: single = 8.0;
  splineTypeActive: TSplineType = SPLINE_LINEAR; // 0-Linear, 1-BSpline, 2-CatmullRom, 3-Bezier
  splineTypeEditMode: boolean = false;
  splineHelpersActive: boolean = true;

begin
  // Initialization
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - splines drawing');

  points[0] := Vector2Create(50.0, 400.0);
  points[1] := Vector2Create(160.0, 220.0);
  points[2] := Vector2Create(340.0, 380.0);
  points[3] := Vector2Create(520.0, 60.0);
  points[4] := Vector2Create(710, 260.0);


  pointCount := 5;
  selectedPoint := -1;
  focusedPoint := -1;
  selectedControlPoint := nil;
  focusedControlPoint := nil;
  {
  splineThickness := 8.0;
  splineTypeActive := SPLINE_LINEAR; // 0-Linear, 1-BSpline, 2-CatmullRom, 3-Bezier
  splineTypeEditMode := false;
  splineHelpersActive := true;
   }

  for i := 0 to  pointCount-1 do //(int i = 0; i < pointCount - 1; i++)
  begin
    control[i].start := Vector2Create(points[i].x + 50, points[i].y);
    control[i].end_ := Vector2Create(points[i + 1].x - 50, points[i + 1].y);
  end;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // Spline points creation logic (at the end of spline)
      if (IsMouseButtonPressed(MOUSE_RIGHT_BUTTON) and (pointCount < MAX_SPLINE_POINTS)) then
      begin
          points[pointCount] := GetMousePosition();
          i := pointCount - 1;
          control[i].start := Vector2Create( points[i].x + 50, points[i].y );
          control[i].end_ := Vector2Create( points[i + 1].x - 50, points[i + 1].y );
          Inc(pointCount);
      end;

      // Spline point focus and selection logic
      for i := 0 to  pointCount-1 do
      begin
        if (CheckCollisionPointCircle(GetMousePosition(), points[i], 8.0)) then
        begin
          focusedPoint := i;
          if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then selectedPoint := i;
          break;
        end
          else focusedPoint := -1;
      end;

      // Spline point movement logic
      if (selectedPoint >= 0) then
      begin
        points[selectedPoint] := GetMousePosition();
        if (IsMouseButtonReleased(MOUSE_LEFT_BUTTON)) then selectedPoint := -1;
      end;

      // Cubic Bezier spline control points logic
      if ((splineTypeActive = SPLINE_BEZIER) and (focusedPoint = -1)) then
      begin
          // Spline control point focus and selection logic
          for i := 0 to pointCount -1 do ///(int i = 0; i < pointCount - 1; i++)
          begin
              if (CheckCollisionPointCircle(GetMousePosition(), control[i].start, 6.0)) then
              begin
                  focusedControlPoint := @control[i].start;
                  if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then selectedControlPoint := @control[i].start;
                  break;
              end
              else if (CheckCollisionPointCircle(GetMousePosition(), control[i].end_, 6.0)) then
              begin
                  focusedControlPoint := @control[i].end_;
                  if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then selectedControlPoint := @control[i].end_;
                  break;
              end
              else focusedControlPoint := nil;
          end;

          // Spline control point movement logic
          if (selectedControlPoint <> nil) then
          begin
              selectedControlPoint^ := GetMousePosition();
              if (IsMouseButtonReleased(MOUSE_LEFT_BUTTON)) then selectedControlPoint := nil;
          end;
      end;

      // Spline selection logic
      if (IsKeyPressed(KEY_ONE)) then splineTypeActive := 0
      else if (IsKeyPressed(KEY_TWO)) then splineTypeActive := 1
      else if (IsKeyPressed(KEY_THREE)) then splineTypeActive := 2
      else if (IsKeyPressed(KEY_FOUR)) then splineTypeActive := 3;

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
           if (splineTypeActive = SPLINE_LINEAR) then
            begin
              // Draw spline: linear
              DrawSplineLinear(@points, pointCount, splineThickness, RED);
            end
            else if (splineTypeActive = SPLINE_BASIS) then
            begin
              // Draw spline: basis
              DrawSplineBasis(@points, pointCount, splineThickness, RED);  // Provide connected points array
            end
            else if (splineTypeActive = SPLINE_CATMULLROM) then
            begin
              // Draw spline: catmull-rom
              DrawSplineCatmullRom(@points, pointCount, splineThickness, RED); // Provide connected points array
            end
            else if (splineTypeActive = SPLINE_BEZIER) then
            begin
              // NOTE: Cubic-bezier spline requires the 2 control points of each segnment to be
              // provided interleaved with the start and end point of every segment
              for i:=0 to pointCount -1 do
              begin
                pointsInterleaved[3*i] := points[i];
                pointsInterleaved[3*i + 1] := control[i].start;
                pointsInterleaved[3*i + 2] := control[i].end_;
              end;
              pointsInterleaved[3*(pointCount - 1)] := points[pointCount - 1];
              // Draw spline: cubic-bezier (with control points)
              DrawSplineBezierCubic(@pointsInterleaved, 3*(pointCount - 1) + 1, splineThickness, RED);

              // Draw spline control points
              for i:=0 to pointCount -1 do
              begin
                // Every cubic bezier point have two control points
                DrawCircleV(control[i].start, 6, GOLD);
                DrawCircleV(control[i].end_, 6, GOLD);
                if (focusedControlPoint = @control[i].start) then DrawCircleV(control[i].start, 8, GREEN)
                else if (focusedControlPoint = @control[i].end_) then DrawCircleV(control[i].end_, 8, GREEN);
                DrawLineEx(points[i], control[i].start, 1.0, LIGHTGRAY);
                DrawLineEx(points[i + 1], control[i].end_, 1.0, LIGHTGRAY);

                // Draw spline control lines
                DrawLineV(points[i], control[i].start, GRAY);
                //DrawLineV(control[i].start, control[i].end, LIGHTGRAY);
                DrawLineV(control[i].end_, points[i + 1], GRAY);
              end;
            end;

            if (splineHelpersActive) then
            begin
            // Draw spline point helpers
            for i := 0 to pointCount -1 do //(int i = 0; i < pointCount; i++)
            begin
              if focusedPoint = i then pointColor := Blue else pointColor := DarkBlue;
              DrawCircleLinesV(points[i],
              ifthen(focusedPoint = i, 12.0, 8.0), pointColor);

              if ((splineTypeActive <> SPLINE_LINEAR) and
                  (splineTypeActive <> SPLINE_BEZIER) and
                  (i < pointCount - 1)) then DrawLineV(points[i], points[i + 1], GRAY);

              DrawText(TextFormat('[%.0f, %.0f]', points[i].x, points[i].y), Trunc(points[i].x), Trunc(points[i].y) + 10, 10, BLACK);
            end;
            end;


        // Check all possible UI states that require controls lock
        if (splineTypeEditMode) then GuiLock();

        // Draw spline config
        GuiLabel(RectangleCreate( 12, 62, 140, 24 ), TextFormat('Spline thickness: %i', Trunc(splineThickness)));
        GuiSliderBar(RectangleCreate( 12, 60 + 24, 140, 16 ), nil, nil, @splineThickness, 1.0, 40.0);

        GuiCheckBox(RectangleCreate( 12, 110, 20, 20 ), 'Show point helpers', @splineHelpersActive);

        GuiUnlock();

        GuiLabel(RectangleCreate( 12, 10, 140, 24 ), 'Spline type:');

        if (GuiDropdownBox(RectangleCreate( 12, 8 + 24, 140, 28 ), 'LINEAR;BSPLINE;CATMULLROM;BEZIER',
        @splineTypeActive, splineTypeEditMode)) <> 0 then splineTypeEditMode := not splineTypeEditMode;


      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

