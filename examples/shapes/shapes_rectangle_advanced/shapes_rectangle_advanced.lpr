program shapes_rectangle_advanced;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
rlgl,
raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

// Draw rectangle with rounded edges and horizontal gradient, with options to choose side of roundness
// Adapted from both `DrawRectangleRounded` and `DrawRectangleGradientH`
procedure DrawRectangleRoundedGradientH(rec: TRectangle; roundnessLeft, roundnessRight: Single; segments: Integer; left, right: TColorB);
const NumPoints = 12; // Number of points for the rounded rectangle
var   recSize, radiusLeft, radiusRight,
      stepLength, radius, angle: single;
      point: array[0..NumPoints - 1] of TVector2; // Array to hold the points
      centers: array[0..4] of TVector2;
      angles: array[0..4] of Single;
      i,j: integer; color: TColorB;
      center: TVector2;
begin
  // Neither side is rounded
  if ((roundnessLeft <= 0.0) and (roundnessRight <= 0.0)) or (rec.width < 1) or (rec.height < 1) then
  begin
      DrawRectangleGradientEx(rec, left, left, right, right);
      Exit;
  end;

  if roundnessLeft >= 1.0 then
    roundnessLeft := 1.0;

  if roundnessRight >= 1.0 then
    roundnessRight := 1.0;

  // Calculate corner radius both from right and left
  recSize := IfThen(rec.width > rec.height, rec.height, rec.width);

  radiusLeft  := (recSize*roundnessLeft)/2;
  radiusRight := (recSize*roundnessRight)/2;

  if (radiusLeft <= 0.0) then radiusLeft := 0.0;
  if (radiusRight <= 0.0) then radiusRight := 0.0;
  if (radiusRight <= 0.0) and (radiusLeft <= 0.0) then exit;

  stepLength := 90.0/segments;

  Point[0] := Vector2Create(rec.x + radiusLeft, rec.y);
  Point[1] := Vector2Create(rec.x + rec.width - radiusRight, rec.y);
  Point[2] := Vector2Create(rec.x + rec.width, rec.y + radiusRight);
  Point[3] := Vector2Create(rec.x + rec.width, (rec.y + rec.height) - radiusRight);
  Point[4] := Vector2Create((rec.x + rec.width) - radiusRight, rec.y + rec.height);
  Point[5] := Vector2Create(rec.x + radiusLeft, rec.y + rec.height);
  Point[6] := Vector2Create(rec.x, (rec.y + rec.height) - radiusLeft);
  Point[7] := Vector2Create(rec.x, rec.y + radiusLeft);
  Point[8] := Vector2Create(rec.x + radiusLeft, rec.y + radiusLeft);
  Point[9] := Vector2Create((rec.x + rec.width) - radiusRight, rec.y + radiusRight);
  Point[10] := Vector2Create((rec.x + rec.width) - radiusRight, (rec.y + rec.height) - radiusRight);
  Point[11] := Vector2Create(rec.x + radiusLeft, (rec.y + rec.height) - radiusLeft);


  //const Vector2 centers[4] = { point[8], point[9], point[10], point[11] };
  centers[0] := point[8];
  centers[1] := point[9];
  centers[2] := point[10];
  centers[3] := point[11];

  angles[0] := 180;
  angles[1] := 270;
  angles[2] := 0.0;
  angles[3] := 90;

  // Here we use the `Diagram` to guide ourselves to which point receives what color.
  //
  // By choosing the color correctly associated with a pointe the gradient effect
  // will naturally come from OpenGL interpolation.
  // But this time instead of Quad, we think in triangles.
  //

rlBegin(RL_TRIANGLES);

    // Draw all of the 4 corners: [1] Upper Left Corner, [3] Upper Right Corner, [5] Lower Right Corner, [7] Lower Left Corner
    for j := 0  to 3 do
    begin
      if (j = 0) then begin color := left;  radius := radiusLeft; end;  // [1] Upper Left Corner
      if (j = 1) then begin color := right; radius := radiusRight; end; // [3] Upper Right Corner
      if (j = 2) then begin color := right; radius := radiusRight; end; // [5] Lower Right Corner
      if (j = 3) then begin color := left;  radius := radiusLeft; end;  // [7] Lower Left Corner
      angle := angles[j];
      center := centers[j];

      for i := 0 to segments -1 do //; i < segments; i++)
        begin
            rlColor4ub(color.r, color.g, color.b, color.a);
            rlVertex2f(center.x, center.y);
            rlVertex2f(center.x + cos(DEG2RAD*(angle + stepLength))*radius, center.y + sin(DEG2RAD*(angle + stepLength))*radius);
            rlVertex2f(center.x + cos(DEG2RAD*angle)*radius, center.y + sin(DEG2RAD*angle)*radius);
            angle += stepLength;
        end;
    end;

    // [2] Upper Rectangle
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[0].x, point[0].y);
    rlVertex2f(point[8].x, point[8].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[9].x, point[9].y);
    rlVertex2f(point[1].x, point[1].y);
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[0].x, point[0].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[9].x, point[9].y);

    // [4] Right Rectangle
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[9].x, point[9].y);
    rlVertex2f(point[10].x, point[10].y);
    rlVertex2f(point[3].x, point[3].y);
    rlVertex2f(point[2].x, point[2].y);
    rlVertex2f(point[9].x, point[9].y);
    rlVertex2f(point[3].x, point[3].y);

    // [6] Bottom Rectangle
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[11].x, point[11].y);
    rlVertex2f(point[5].x, point[5].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[4].x, point[4].y);
    rlVertex2f(point[10].x, point[10].y);
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[11].x, point[11].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[4].x, point[4].y);

    // [8] Left Rectangle
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[7].x, point[7].y);
    rlVertex2f(point[6].x, point[6].y);
    rlVertex2f(point[11].x, point[11].y);
    rlVertex2f(point[8].x, point[8].y);
    rlVertex2f(point[7].x, point[7].y);
    rlVertex2f(point[11].x, point[11].y);

    // [9] Middle Rectangle
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[8].x, point[8].y);
    rlVertex2f(point[11].x, point[11].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[10].x, point[10].y);
    rlVertex2f(point[9].x, point[9].y);
    rlColor4ub(left.r, left.g, left.b, left.a);
    rlVertex2f(point[8].x, point[8].y);
    rlColor4ub(right.r, right.g, right.b, right.a);
    rlVertex2f(point[10].x, point[10].y);
rlEnd();

end;

var rec : TRectangle;
    width, height: double;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here
      width := GetScreenWidth()/2.0;
      height := GetScreenHeight()/6.0;
      rec := RectangleCreate(
            GetScreenWidth() / 2.0 - width/2,
            GetScreenHeight() / 2.0 - (5)*(height/2),
            width, height);


      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);

        // Draw All Rectangles with different roundess  for each side and different gradients
         DrawRectangleRoundedGradientH(rec, 0.8, 0.8, 36, BLUE, RED);

         rec.y += rec.height + 1;
         DrawRectangleRoundedGradientH(rec, 0.5, 1.0, 36, RED, PINK);

         rec.y += rec.height + 1;
         DrawRectangleRoundedGradientH(rec, 1.0, 0.5, 36, RED, BLUE);

         rec.y += rec.height + 1;
         DrawRectangleRoundedGradientH(rec, 0.0, 1.0, 36, BLUE, BLACK);

         rec.y += rec.height + 1;
         DrawRectangleRoundedGradientH(rec, 1.0, 0.0, 36, BLUE, PINK);


      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

