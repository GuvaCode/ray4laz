program game;

{$mode objfpc}{$H+}

uses
raylib, physac, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  logoX,logoY, bodiesCount: longint;
  floor, circle, body: TPhysicsBody;
  i, j, jj: longint;
  vertexCount: integer;
  vertexA, vertexB: TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [physac] example - physics demo');

  // Physac logo drawing position
  logoX := screenWidth - MeasureText('Physac', 30) - 10;
  logoY := 15;

  // Initialize physics and default physics bodies
  InitPhysics();

  // Create floor rectangle physics body
  floor := CreatePhysicsBodyRectangle(Vector2Create(screenWidth/2.0, single(screenHeight)), 500, 100, 10);
  floor^.enabled:= false;         // Disable body state to convert it to static (no dynamics, but collisions)

  // Create obstacle circle physics body
  circle := CreatePhysicsBodyCircle(Vector2Create(screenWidth/2.0, screenHeight/2.0), 45, 10);
  circle^.enabled := false;        // Disable body state to convert it to static (no dynamics, but collisions)

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------

      UpdatePhysics();            // Update physics system

        if IsKeyPressed(KEY_R) then    // Reset physics system
        begin
            ResetPhysics();

            floor := CreatePhysicsBodyRectangle(Vector2Create( screenWidth/2.0, Single(screenHeight) ), 500, 100, 10);
            floor^.enabled := false;

            circle := CreatePhysicsBodyCircle(Vector2Create( screenWidth/2.0, screenHeight/2.0 ), 45, 10);
            circle^.enabled := false;
        end;

        // Physics body creation inputs
        if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
        CreatePhysicsBodyPolygon(GetMousePosition, Single(GetRandomValue(20, 80)), GetRandomValue(3, 8), 10)
        else
          if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
            CreatePhysicsBodyCircle(GetMousePosition, single(GetRandomValue(10, 45)), 10);

        // Destroy falling physics bodies
        bodiesCount := GetPhysicsBodiesCount;
        for i:=bodiesCount -1 downto 0 do
        begin
            body := GetPhysicsBody(i);

            if (body <> nil) and (body^.position.y > screenHeight*2) then DestroyPhysicsBody(body);
       end;

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

                if body <> nil then
                begin

                    vertexCount := GetPhysicsShapeVerticesCount(i);

                    for j:=0 to vertexCount-1 do
                    begin
                        // Get physics bodies shape vertices to draw lines
                        // Note: GetPhysicsShapeVertex() already calculates rotation transformations
                        vertexA := GetPhysicsShapeVertex(body, j);

                        jj:=longint(ifthen(((j + 1) < vertexCount), (j + 1) , 0));  // Get next vertex or first to close the shape

                        vertexB := GetPhysicsShapeVertex(body, jj);

                        DrawLineV(vertexA, vertexB, GREEN);     // Draw a line between two vertex positions
                    end;
                end;
            end;

            DrawText('Left mouse button to create a polygon', 10, 10, 10, WHITE);
            DrawText('Right mouse button to create a circle', 10, 25, 10, WHITE);
            DrawText('Press ''R'' to reset example', 10, 40, 10, WHITE);

            DrawText('Physac', logoX, logoY, 30, WHITE);
            DrawText('Powered by', logoX + 50, logoY - 7, 10, WHITE);


      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

