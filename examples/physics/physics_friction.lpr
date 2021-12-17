program physics_friction;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
//raymath, 
//rlgl,
physac, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  logoX, logoY ,bodiesCount, i, j, jj, vertexCount : longint;
  floor, wall, rectLeft , rectRight, bodyA, bodyB , body : TPhysicsBody;
  vertexA, vertexB : TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [physac] example - physics friction');

  // Physac logo drawing position
  logoX := screenWidth - MeasureText('Physac', 30) - 10;
  logoY := 15;

  // Initialize physics and default physics bodies
  InitPhysics();

  // Create floor rectangle physics body
   floor := CreatePhysicsBodyRectangle(Vector2Create(screenWidth/2.0, screenHeight ), screenWidth, 100, 10);
   floor^.enabled := false; // Disable body state to convert it to static (no dynamics, but collisions)

    wall := CreatePhysicsBodyRectangle(Vector2Create( screenWidth/2.0, screenHeight*0.8 ), 10, 80, 10);
    wall^.enabled := false; // Disable body state to convert it to static (no dynamics, but collisions)

    // Create left ramp physics body
    rectLeft := CreatePhysicsBodyRectangle(Vector2Create( 25, screenHeight - 5 ), 250, 250, 10);
    rectLeft^.enabled := false; // Disable body state to convert it to static (no dynamics, but collisions)
    SetPhysicsBodyRotation(rectLeft, 30*DEG2RAD);

    // Create right ramp  physics body
    rectRight := CreatePhysicsBodyRectangle(Vector2Create(screenWidth - 25, screenHeight - 5 ), 250, 250, 10);
    rectRight^.enabled := false; // Disable body state to convert it to static (no dynamics, but collisions)
    SetPhysicsBodyRotation(rectRight, 330*DEG2RAD);

    // Create dynamic physics bodies
    bodyA := CreatePhysicsBodyRectangle(Vector2Create( 35, screenHeight*0.6 ), 40, 40, 10);
    bodyA^.staticFriction := 0.1;
    bodyA^.dynamicFriction := 0.1;
    SetPhysicsBodyRotation(bodyA, 30*DEG2RAD);

    bodyB := CreatePhysicsBodyRectangle(Vector2Create( screenWidth - 35, screenHeight*0.6 ), 40, 40, 10);
    bodyB^.staticFriction := 1.0;
    bodyB^.dynamicFriction := 1.0;
    SetPhysicsBodyRotation(bodyB, 330*DEG2RAD);

    SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdatePhysics;            // Update physics system

        if IsKeyPressed(KEY_R) then   // Reset physics system
          begin
            // Reset dynamic physics bodies position, velocity and rotation
            bodyA^.position := Vector2Create( 35, screenHeight*0.6 );
            bodyA^.velocity := Vector2Create( 0, 0 );
            bodyA^.angularVelocity := 0;
            SetPhysicsBodyRotation(bodyA, 30*DEG2RAD);

            bodyB^.position := Vector2Create( screenWidth - 35, screenHeight * 0.6 );
            bodyB^.velocity := Vector2Create( 0, 0 );
            bodyB^.angularVelocity := 0;
            SetPhysicsBodyRotation(bodyB, 330*DEG2RAD);
          end;


      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(BLACK);

        DrawFPS(screenWidth - 90, screenHeight - 30);

        // Draw created physics bodies
        bodiesCount := GetPhysicsBodiesCount;
        for i:=0 to bodiesCount  do
        begin
          body := GetPhysicsBody(i);

            if body <> nil then
            begin
                 vertexCount := GetPhysicsShapeVerticesCount(i);
                for j:=0 to vertexCount -1 do
                begin
                    // Get physics bodies shape vertices to draw lines
                    // Note: GetPhysicsShapeVertex() already calculates rotation transformations
                     vertexA := GetPhysicsShapeVertex(body, j);
                    jj := IfThen(j+1 < vertexCount , j+1, 0); // Get next vertex or first to close the shape
                    vertexB := GetPhysicsShapeVertex(body, jj);
                    DrawLineV(vertexA, vertexB, GREEN);     // Draw a line between two vertex positions
                end;
            end;
        end;

        DrawRectangle(0, screenHeight - 49, screenWidth, 49, BLACK);

        DrawText('Friction amount',(screenWidth - MeasureText('Friction amount', 30) ) div 2, 75, 30, WHITE);

        DrawText('0.1', trunc(bodyA^.position.x) - MeasureText('0.1', 20) div 2, trunc(bodyA^.position.y) - 7, 20, WHITE);
        DrawText('1', trunc(bodyB^.position.x) - MeasureText('1', 20) div 2, trunc(bodyB^.position.y) - 7, 20, WHITE);
        DrawText('Press ''R'' to reset example', 10, 10, 10, WHITE);

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

