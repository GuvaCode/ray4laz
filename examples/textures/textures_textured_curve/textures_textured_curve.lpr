program textures_textured_curve;

{$mode objfpc}{$H+}

uses 
cmem, rlgl,raylib, math , raymath;

const
  screenWidth = 800;
  screenHeight = 450;

var
  texRoad: TTexture;
  showCurve: Boolean = false;
  curveWidth: Single = 50;
  curveSegments: Integer = 24;

  curveStartPosition,
  curveStartPositionTangent,
  curveEndPosition,
  curveEndPositionTangent: TVector2;

  curveSelectedPoint: PVector2;

// Module Functions Declaration

procedure DrawCurve;
var mouse: TVector2;
begin
//	if (showCurve) then DrawLineBezierCubic(curveStartPosition, curveEndPosition, curveStartPositionTangent, curveEndPositionTangent, 2, BLUE);

	// Draw the various control points and highlight where the mouse is
	DrawLineV(curveStartPosition, curveStartPositionTangent, SKYBLUE);
	DrawLineV(curveEndPosition, curveEndPositionTangent, PURPLE);
	mouse := GetMousePosition();

	if (CheckCollisionPointCircle(mouse, curveStartPosition, 6)) then DrawCircleV(curveStartPosition, 7, YELLOW);
	DrawCircleV(curveStartPosition, 5, RED);

	if (CheckCollisionPointCircle(mouse, curveStartPositionTangent, 6)) then DrawCircleV(curveStartPositionTangent, 7, YELLOW);
	DrawCircleV(curveStartPositionTangent, 5, MAROON);

	if (CheckCollisionPointCircle(mouse, curveEndPosition, 6)) then DrawCircleV(curveEndPosition, 7, YELLOW);
	DrawCircleV(curveEndPosition, 5, GREEN);

	if (CheckCollisionPointCircle(mouse, curveEndPositionTangent, 6)) then DrawCircleV(curveEndPositionTangent, 7, YELLOW);
	DrawCircleV(curveEndPositionTangent, 5, DARKGREEN);
end;

procedure UpdateCurve;
var mouse: TVector2;
begin
	// If the mouse is not down, we are not editing the curve so clear the selection
	if ( not IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then
	begin
		curveSelectedPoint := nil;
		exit;
	end;

	// If a point was selected, move it
	if curveSelectedPoint > nil  then
	begin
	  curveSelectedPoint^ := Vector2Add(curveSelectedPoint^, GetMouseDelta());
		exit;
	end;

	// The mouse is down, and nothing was selected, so see if anything was picked
	mouse := GetMousePosition();

	if (CheckCollisionPointCircle(mouse, curveStartPosition, 6)) then curveSelectedPoint := @curveStartPosition
	else if (CheckCollisionPointCircle(mouse, curveStartPositionTangent, 6)) then curveSelectedPoint := @curveStartPositionTangent
	else if (CheckCollisionPointCircle(mouse, curveEndPosition, 6)) then curveSelectedPoint := @curveEndPosition
	else if (CheckCollisionPointCircle(mouse, curveEndPositionTangent, 6)) then  curveSelectedPoint := @curveEndPositionTangent;
end;

procedure DrawTexturedCurve;
var t, step, previousV: single;
  current,previous, previousTangent: TVector2;
  prevPosNormal,prevNegNormal,currentPosNormal,  currentNegNormal: TVector2;
  delta, normal: TVector2;
  tangentSet: Boolean;
  i: integer;
  a,b,c,d,v: single;
begin

        step := 1.0 / curveSegments;
        previous := curveStartPosition;
        previousTangent :=Default(TVector2);
        previousV := 0;

	// We can't compute a tangent for the first point, so we need to reuse the tangent from the first segment
        tangentSet := false;

	current := Default(TVector2);
        t := 0.0;

	for i:=1 to curveSegments do ///   (int i = 1; i <= curveSegments; i++)
	begin
		// Segment the curve
		t := step*i;
		 a := power(1 - t, 3);
		 b := 3*power(1 - t, 2)*t;
		 c := 3*(1 - t)*power(t, 2);
		 d := power(t, 3);

		// Compute the endpoint for this segment
		current.y := a*curveStartPosition.y + b*curveStartPositionTangent.y + c*curveEndPositionTangent.y + d*curveEndPosition.y;
		current.x := a*curveStartPosition.x + b*curveStartPositionTangent.x + c*curveEndPositionTangent.x + d*curveEndPosition.x;

		// Vector from previous to current
		delta := Vector2Create( current.x - previous.x, current.y - previous.y );

		// The right hand normal to the delta vector
	        normal := Vector2Normalize(Vector2Create( -delta.y, delta.x ));

		// The v texture coordinate of the segment (add up the length of all the segments so far)
		 v := previousV + Vector2Length(delta);

		// Make sure the start point has a normal
		if (not tangentSet)  then
		begin
			previousTangent := normal;
			tangentSet := true;
		end;

		// Extend out the normals from the previous and current points to get the quad for this segment
		 prevPosNormal := Vector2Add(previous, Vector2Scale(previousTangent, curveWidth));
		 prevNegNormal := Vector2Add(previous, Vector2Scale(previousTangent, -curveWidth));

		 currentPosNormal := Vector2Add(current, Vector2Scale(normal, curveWidth));
		 currentNegNormal := Vector2Add(current, Vector2Scale(normal, -curveWidth));

		// Draw the segment as a quad
		rlSetTexture(texRoad.id);
		rlBegin(RL_QUADS);

		rlColor4ub(255,255,255,255);
		rlNormal3f(0.0, 0.0, 1.0);

		rlTexCoord2f(0, previousV);
		rlVertex2f(prevNegNormal.x, prevNegNormal.y);

		rlTexCoord2f(1, previousV);
		rlVertex2f(prevPosNormal.x, prevPosNormal.y);

		rlTexCoord2f(1, v);
		rlVertex2f(currentPosNormal.x, currentPosNormal.y);

		rlTexCoord2f(0, v);
		rlVertex2f(currentNegNormal.x, currentNegNormal.y);

		rlEnd();

		// The current step is the start of the next step
		previous := current;
		previousTangent := normal;
		previousV := v;
	end;
end;

procedure UpdateOptions;
begin
	if (IsKeyPressed(KEY_SPACE)) then showCurve := not showCurve;

	// Update with
	if (IsKeyPressed(KEY_EQUAL)) then curveWidth += 2;
	if (IsKeyPressed(KEY_MINUS)) then curveWidth -= 2;

	if (curveWidth < 2) then curveWidth := 2;

	// Update segments
	if (IsKeyPressed(KEY_LEFT)) then curveSegments -= 2;
	if (IsKeyPressed(KEY_RIGHT)) then curveSegments += 2;

	if (curveSegments < 2) then curveSegments := 2;
end;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] examples - textured curve');

  // Load the road texture
  texRoad := LoadTexture(PChar(GetApplicationDirectory + 'resources/road.png'));
  SetTextureFilter(texRoad, TEXTURE_FILTER_BILINEAR);

  // Setup the curve
  curveStartPosition := Vector2Create( 80, 100 );
  curveStartPositionTangent := Vector2Create( 100, 300 );

  curveEndPosition := Vector2Create( 700, 350 );
  curveEndPositionTangent := Vector2Create( 600, 100 );

SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCurve();
      UpdateOptions();
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
                DrawTexturedCurve();
            DrawCurve();

            DrawText('Drag points to move curve, press SPACE to show/hide base curve', 10, 10, 10, DARKGRAY);
            DrawText(TextFormat('Curve width: %2.0 (Use + and - to adjust)', curveWidth), 10, 30, 10, DARKGRAY);
            DrawText(TextFormat('Curve segments: %d (Use LEFT and RIGHT to adjust)', curveSegments), 10, 50, 10, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(texRoad);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

