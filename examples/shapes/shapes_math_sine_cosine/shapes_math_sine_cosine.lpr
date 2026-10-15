program shapes_math_sine_cosine;

{$mode objfpc}{$H+}

uses
  cmem, raylib, raymath, raygui, Math, SysUtils;

const
  screenWidth = 800;
  screenHeight = 450;
  WAVE_POINTS = 36;

// Draw dashed line (helper function)
procedure DrawLineDashed(startPos, endPos: TVector2; segments, length: Integer; color: TColorB);
var
  dx, dy, dist, segLen: Single;
  i, count: Integer;
  p: TVector2;
begin
  dx := endPos.x - startPos.x;
  dy := endPos.y - startPos.y;
  dist := Sqrt(dx * dx + dy * dy);
  if dist < 0.001 then Exit;
  dx := dx / dist;
  dy := dy / dist;
  segLen := length;
  count := Round(dist / (segLen * 2));
  for i := 0 to count - 1 do
  begin
    p.x := startPos.x + dx * (i * segLen * 2);
    p.y := startPos.y + dy * (i * segLen * 2);
    DrawLineV(p, Vector2Create(p.x + dx * segLen, p.y + dy * segLen), color);
  end;
end;

var
  sinePoints, cosPoints: array[0..WAVE_POINTS - 1] of TVector2;
  center, point, limitMin, limitMax: TVector2;
  tangentPoint, cotangentPoint: TVector2;
  startRec: TRectangle;
  radius, angle: Single;
  pause: Boolean;
  angleRad, cosRad, sinRad: Single;
  complementary, supplementary, explementary: Single;
  tangent, cotangent: Single;
  i: Integer;
  t, currentAngle: Single;

begin
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - math sine cosine');

  center := Vector2Create(screenWidth / 2.0 - 30.0, screenHeight / 2.0);
  startRec := RectangleCreate(20, screenHeight - 120, 200, 100);
  radius := 130.0;
  angle := 0.0;
  pause := False;

  // Pre-calculate sine and cosine wave points
  for i := 0 to WAVE_POINTS - 1 do
  begin
    t := i / (WAVE_POINTS - 1);
    currentAngle := t * 360.0 * DEG2RAD;
    sinePoints[i] := Vector2Create(startRec.x + t * startRec.width,
      startRec.y + startRec.height / 2.0 - Sin(currentAngle) * (startRec.height / 2.0));
    cosPoints[i] := Vector2Create(startRec.x + t * startRec.width,
      startRec.y + startRec.height / 2.0 - Cos(currentAngle) * (startRec.height / 2.0));
  end;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    // Update
    angleRad := angle * DEG2RAD;
    cosRad := Cos(angleRad);
    sinRad := Sin(angleRad);

    point := Vector2Create(center.x + cosRad * radius, center.y - sinRad * radius);
    limitMin := Vector2Create(center.x - radius, center.y - radius);
    limitMax := Vector2Create(center.x + radius, center.y + radius);

    complementary := 90.0 - angle;
    supplementary := 180.0 - angle;
    explementary := 360.0 - angle;

    tangent := Clamp(Tan(angleRad), -10.0, 10.0);
    if Abs(tangent) > 0.001 then
      cotangent := Clamp(1.0 / tangent, -radius, radius)
    else
      cotangent := 0.0;
    tangentPoint := Vector2Create(center.x + radius, center.y - tangent * radius);
    cotangentPoint := Vector2Create(center.x + cotangent * radius, center.y - radius);

    if not pause then
      angle := Wrap(angle + 1.0, 0.0, 360.0);

    // Draw
    BeginDrawing();
      ClearBackground(RAYWHITE);

      // Cotangent (orange)
      DrawLineEx(Vector2Create(center.x, limitMin.y), Vector2Create(cotangentPoint.x, limitMin.y), 2.0, ORANGE);
      DrawLineDashed(center, cotangentPoint, 10, 4, ORANGE);

      // Side background
      DrawLine(580, 0, 580, GetScreenHeight, ColorCreate(218, 218, 218, 255));
      DrawRectangle(580, 0, GetScreenWidth, GetScreenHeight, ColorCreate(232, 232, 232, 255));

      // Base circle and axes
      DrawCircleLinesV(center, radius, GRAY);
      DrawLineEx(Vector2Create(center.x, limitMin.y), Vector2Create(center.x, limitMax.y), 1.0, GRAY);
      DrawLineEx(Vector2Create(limitMin.x, center.y), Vector2Create(limitMax.x, center.y), 1.0, GRAY);

      // Wave graph axes
      DrawLineEx(Vector2Create(startRec.x, startRec.y), Vector2Create(startRec.x, startRec.y + startRec.height), 2.0, GRAY);
      DrawLineEx(Vector2Create(startRec.x + startRec.width, startRec.y), Vector2Create(startRec.x + startRec.width, startRec.y + startRec.height), 2.0, GRAY);
      DrawLineEx(Vector2Create(startRec.x, startRec.y + startRec.height / 2), Vector2Create(startRec.x + startRec.width, startRec.y + startRec.height / 2), 2.0, GRAY);

      // Wave graph axis labels
      DrawText('1', Round(startRec.x) - 8, Round(startRec.y), 6, GRAY);
      DrawText('0', Round(startRec.x) - 8, Round(startRec.y + startRec.height / 2) - 6, 6, GRAY);
      DrawText('-1', Round(startRec.x) - 12, Round(startRec.y + startRec.height) - 8, 6, GRAY);
      DrawText('0', Round(startRec.x) - 2, Round(startRec.y + startRec.height) + 4, 6, GRAY);
      DrawText('360', Round(startRec.x + startRec.width) - 8, Round(startRec.y + startRec.height) + 4, 6, GRAY);

      // Sine (red - vertical)
      DrawLineEx(Vector2Create(center.x, center.y), Vector2Create(center.x, point.y), 2.0, RED);
      DrawLineDashed(Vector2Create(point.x, center.y), Vector2Create(point.x, point.y), 10, 4, RED);
      DrawText(PChar(Format('Sine %.2f', [sinRad])), 640, 190, 6, RED);
      DrawCircleV(Vector2Create(startRec.x + (angle / 360.0) * startRec.width,
        startRec.y + ((-sinRad + 1) * startRec.height / 2.0)), 4.0, RED);
      DrawSplineLinear(@sinePoints[0], WAVE_POINTS, 1.0, RED);

      // Cosine (blue - horizontal)
      DrawLineEx(Vector2Create(center.x, center.y), Vector2Create(point.x, center.y), 2.0, BLUE);
      DrawLineDashed(Vector2Create(center.x, point.y), Vector2Create(point.x, point.y), 10, 4, BLUE);
      DrawText(PChar(Format('Cosine %.2f', [cosRad])), 640, 210, 6, BLUE);
      DrawCircleV(Vector2Create(startRec.x + (angle / 360.0) * startRec.width,
        startRec.y + ((-cosRad + 1) * startRec.height / 2.0)), 4.0, BLUE);
      DrawSplineLinear(@cosPoints[0], WAVE_POINTS, 1.0, BLUE);

      // Tangent (purple)
      DrawLineEx(Vector2Create(limitMax.x, center.y), Vector2Create(limitMax.x, tangentPoint.y), 2.0, PURPLE);
      DrawLineDashed(center, tangentPoint, 10, 4, PURPLE);
      DrawText(PChar(Format('Tangent %.2f', [tangent])), 640, 230, 6, PURPLE);

      // Cotangent (orange)
      DrawText(PChar(Format('Cotangent %.2f', [cotangent])), 640, 250, 6, ORANGE);

      // Complementary angle (beige)
      DrawCircleSectorLines(center, radius * 0.6, -angle, -90.0, 36, BEIGE);
      DrawText(PChar(Format('Complementary  %.0f' + #176, [complementary])), 640, 150, 6, BEIGE);

      // Supplementary angle (darkblue)
      DrawCircleSectorLines(center, radius * 0.5, -angle, -180.0, 36, DARKBLUE);
      DrawText(PChar(Format('Supplementary  %.0f' + #176, [supplementary])), 640, 130, 6, DARKBLUE);

      // Explementary angle (pink)
      DrawCircleSectorLines(center, radius * 0.4, -angle, -360.0, 36, PINK);
      DrawText(PChar(Format('Explementary  %.0f' + #176, [explementary])), 640, 170, 6, PINK);

      // Current angle - arc (lime), radius (black), endpoint (black)
      DrawCircleSectorLines(center, radius * 0.7, -angle, 0.0, 36, LIME);
      DrawLineEx(Vector2Create(center.x, center.y), point, 2.0, BLACK);
      DrawCircleV(point, 4.0, BLACK);

      // Draw GUI controls
      GuiSetStyle(LABEL_, TEXT_COLOR_NORMAL, ColorToInt(GRAY));
      GuiToggle(RectangleCreate(640, 70, 120, 20), 'Pause', @pause);
      GuiSetStyle(LABEL_, TEXT_COLOR_NORMAL, ColorToInt(LIME));
      GuiSliderBar(RectangleCreate(640, 40, 120, 20), 'Angle', PChar(Format('%.0f' + #176, [angle])), @angle, 0.0, 360.0);

      // Angle values panel
      GuiGroupBox(RectangleCreate(620, 110, 140, 170), 'Angle Values');

      DrawFPS(10, 10);

    EndDrawing();
  end;

  CloseWindow();
end.
