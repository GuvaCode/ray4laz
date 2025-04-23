program models_box_collisions;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
var
  Camera: TCamera;
  PlayerPosition, PlayerSize: TVector3;
  PlayerColor: TColor;
  EnemyBoxPos, EnemyBoxSize, EnemySpherePos: TVector3;
  EnemySphereSize: Single;
  Collision: Boolean;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - box collisions');

  Camera := Camera3DCreate(
     Vector3Create(0.0, 10.0, 10.0),
     Vector3Create(0.0, 0.0, 0.0),
     Vector3Create(0.0, 1.0, 0.0),
     45.0, 0);

   PlayerPosition := Vector3Create(0.0, 1.0, 2.0);
   PlayerSize := Vector3Create(1.0, 2.0, 1.0);
   PlayerColor := GREEN;

   EnemyBoxPos := Vector3Create(-4.0, 1.0, 0.0);
   EnemyBoxSize := Vector3Create(2.0, 2.0, 2.0);

   EnemySpherePos := Vector3Create(4.0, 0.0, 0.0);
   EnemySphereSize := 1.5;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Move player
      if IsKeyDown(KEY_RIGHT) then
        PlayerPosition.X := PlayerPosition.X + 0.2
      else if IsKeyDown(KEY_LEFT) then
        PlayerPosition.X := PlayerPosition.X - 0.2
      else if IsKeyDown(KEY_DOWN) then
        PlayerPosition.Z := PlayerPosition.Z + 0.2
      else if IsKeyDown(KEY_UP) then
        PlayerPosition.Z := PlayerPosition.Z - 0.2;

      Collision := False;

      // Check collisions player vs enemy-box
      if CheckCollisionBoxes(
        BoundingBoxCreate(
          Vector3Create(PlayerPosition.X - PlayerSize.X / 2, PlayerPosition.Y - PlayerSize.Y / 2, PlayerPosition.Z - PlayerSize.Z / 2),
          Vector3Create(PlayerPosition.X + PlayerSize.X / 2, PlayerPosition.Y + PlayerSize.Y / 2, PlayerPosition.Z + PlayerSize.Z / 2)
        ),
        BoundingBoxCreate(
          Vector3Create(EnemyBoxPos.X - EnemyBoxSize.X / 2, EnemyBoxPos.Y - EnemyBoxSize.Y / 2, EnemyBoxPos.Z - EnemyBoxSize.Z / 2),
          Vector3Create(EnemyBoxPos.X + enemyBoxSize.X / 2, EnemyBoxPos.Y + EnemyBoxSize.Y / 2, EnemyBoxPos.Z + EnemyBoxSize.Z / 2)
        )
      ) then Collision := True;

      // Check collisions player vs enemy-sphere
      if CheckCollisionBoxSphere(
        BoundingBoxCreate(
          Vector3Create(PlayerPosition.X - PlayerSize.X / 2, PlayerPosition.Y - PlayerSize.Y / 2, PlayerPosition.Z - PlayerSize.Z / 2),
          Vector3Create(PlayerPosition.X + PlayerSize.X / 2, PlayerPosition.Y + PlayerSize.Y / 2, PlayerPosition.Z + PlayerSize.Z / 2)
        ),
        EnemySpherePos,
        EnemySphereSize
      ) then Collision := True;

      if Collision then
        PlayerColor := RED
      else
        PlayerColor := GREEN;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);

        // Draw enemy-box
        DrawCube(EnemyBoxPos, EnemyBoxSize.X, EnemyBoxSize.Y, EnemyBoxSize.Z, GRAY);
        DrawCubeWires(EnemyBoxPos, EnemyBoxSize.X, EnemyBoxSize.Y, EnemyBoxSize.Z, DARKGRAY);

        // Draw enemy-sphere
        DrawSphere(EnemySpherePos, enemySphereSize, GRAY);
        DrawSphereWires(EnemySpherePos, EnemySphereSize, 16, 16, DARKGRAY);

        // Draw player
        DrawCubeV(PlayerPosition, PlayerSize, PlayerColor);

        DrawGrid(10, 1.0);        // Draw a grid

      EndMode3D();

      DrawText(UTF8String('Move player with cursors to collide'), 220, 40, 20, GRAY);

      DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

