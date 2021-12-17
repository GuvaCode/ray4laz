program physics_movement;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib, physac, math;

const
  screenWidth = 800;
  screenHeight = 450;
  VELOCITY  = 0.5;

var
  floor, platformLeft, platformRight, wallLeft, wallRight : TPhysicsBody;
  body: TPhysicsBody;
  bodyID: longint;
  logoX, logoY, i, j, jj, bodiesCount, vertexCount : longint;
  vertexA, vertexB: TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [physac] example - physics movement');

  // Physac logo drawing position
  logoX := screenWidth - MeasureText('Physac', 30) - 10;
  logoY := 15;

  // Initialize physics and default physics bodies
   InitPhysics;

  // Create floor and walls rectangle physics body
  floor := CreatePhysicsBodyRectangle(Vector2Create( screenWidth/2.0, screenHeight ), screenWidth, 100, 10);
  platformLeft := CreatePhysicsBodyRectangle(Vector2Create( screenWidth*0.25, screenHeight*0.6 ), screenWidth*0.25, 10, 10);
  platformRight := CreatePhysicsBodyRectangle(Vector2Create( screenWidth*0.75, screenHeight*0.6 ), screenWidth*0.25, 10, 10);
  wallLeft := CreatePhysicsBodyRectangle(Vector2Create( -5, screenHeight/2.0 ), 10, screenHeight, 10);
  wallRight := CreatePhysicsBodyRectangle(Vector2Create( screenWidth + 5, screenHeight/2.0 ), 10, screenHeight, 10);

  // Disable dynamics to floor and walls physics bodies
  floor^.enabled := false;
  platformLeft^.enabled := false;
  platformRight^.enabled := false;
  wallLeft^.enabled := false;
  wallRight^.enabled := false;

  // Create movement physics body
  body := CreatePhysicsBodyRectangle(Vector2Create( screenWidth/2.0, screenHeight/2.0 ), 50, 50, 1);

  body^.freezeOrient := true;      // Constrain body rotation to avoid little collision torque amounts
  bodyId:=body^.id;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------

     UpdatePhysics;              // Update physics system

      body:=GetPhysicsBody(bodyID); // короче я хуй его знает но без этого коcтыля не работает.
                                    // где-то течЁт как сука

      if IsKeyDown(KEY_R) then      // Reset physics input
      begin
         // Reset movement physics body position, velocity and rotation
          body^.position := Vector2Create( screenWidth/2.0, screenHeight/2.0 );
          body^.velocity := Vector2Create( 0, 0 );
          SetPhysicsBodyRotation(body, 0);
      end;

      // Horizontal movement input
      if IsKeyDown(KEY_RIGHT) then (body^.velocity.x ):= VELOCITY
      else
        if IsKeyDown(KEY_LEFT) then body^.velocity.x := -VELOCITY;

      // Vertical movement input checking if player physics body is grounded
     if IsKeyDown(KEY_UP) and body^.isGrounded  then body^.velocity.y := -VELOCITY*4;

      //----------------------------------------------------------------------------------
      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

      ClearBackground(BLACK);

      DrawFPS(screenWidth - 90, screenHeight - 30);

      // Draw created physics bodies
      bodiesCount := GetPhysicsBodiesCount;
      for i:=0 to bodiesCount do
      begin
          body := GetPhysicsBody(i);
          vertexCount := GetPhysicsShapeVerticesCount(i);

          for j:=0 to  vertexCount -1 do
          begin
              // Get physics bodies shape vertices to draw lines
              // Note: GetPhysicsShapeVertex() already calculates rotation transformations
              vertexA := GetPhysicsShapeVertex(body, j);
              jj := ifthen(((j + 1) < vertexCount), j + 1 , 0);  // Get next vertex or first to close the shape
              vertexB := GetPhysicsShapeVertex(body, jj);
              DrawLineV(vertexA, vertexB, GREEN);     // Draw a line between two vertex positions
          end;
      end;

      DrawText('Use ''ARROWS'' to move player', 10, 10, 10, WHITE);
      DrawText('Press ''R'' to reset example', 10, 30, 10, WHITE);

      DrawText('Physac', logoX, logoY, 30, WHITE);
      DrawText('Powered by', logoX + 50, logoY - 7, 10, WHITE);

      EndDrawing();

    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  ClosePhysics();       // Unitialize physics
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

