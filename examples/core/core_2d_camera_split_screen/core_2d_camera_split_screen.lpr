program core_2d_camera_split_screen;

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
  PLAYER_SIZE = 40;
var
  player1, player2, splitScreenRect: TRectangle;
  camera1, camera2: TCamera2d;
  screenCamera1, screenCamera2: TRenderTexture;
  i, j: integer;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 2d camera split screen');

  player1 := RectangleCreate( 200, 200, PLAYER_SIZE, PLAYER_SIZE );
  player2 := RectangleCreate( 250, 200, PLAYER_SIZE, PLAYER_SIZE );


  camera1.target := Vector2Create( player1.x, player1.y );
  camera1.offset := Vector2Create( 200.0, 200.0 );
  camera1.rotation := 0.0;
  camera1.zoom := 1.0;

  camera2.target := Vector2Create( player2.x, player2.y );
  camera2.offset := Vector2Create( 200.0, 200.0 );
  camera2.rotation := 0.0;
  camera2.zoom := 1.0;

  screenCamera1 := LoadRenderTexture(screenWidth div 2, screenHeight);
  screenCamera2 := LoadRenderTexture(screenWidth div 2, screenHeight);

  // Build a flipped rectangle the size of the split view to use for drawing later
  splitScreenRect := RectangleCreate( 0.0, 0.0, screenCamera1.texture.width, -screenCamera1.texture.height);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if (IsKeyDown(KEY_S)) then player1.y += 3.0
      else if (IsKeyDown(KEY_W)) then player1.y -= 3.0;
      if (IsKeyDown(KEY_D)) then player1.x += 3.0
      else if (IsKeyDown(KEY_A)) then player1.x -= 3.0;

      if (IsKeyDown(KEY_UP)) then player2.y -= 3.0
      else if (IsKeyDown(KEY_DOWN)) then player2.y += 3.0;
      if (IsKeyDown(KEY_RIGHT)) then player2.x += 3.0
      else if (IsKeyDown(KEY_LEFT)) then player2.x -= 3.0;

      camera1.target := Vector2Create( player1.x, player1.y );
      camera2.target := Vector2Create( player2.x, player2.y );

      // Draw
      //----------------------------------------------------------------------------------
      BeginTextureMode(screenCamera1);
        ClearBackground(RAYWHITE);
        BeginMode2D(camera1);
        // Draw full scene with first camera
        for i := 0 to screenWidth div PLAYER_SIZE + 1 do //(int i = 0; i < screenWidth/PLAYER_SIZE + 1; i++)
        begin
          DrawLineV(Vector2Create(PLAYER_SIZE*i, 0) , Vector2Create(PLAYER_SIZE*i, screenHeight), LIGHTGRAY);
        end;

        for i := 0 to  screenHeight div PLAYER_SIZE + 1 do
        begin
          DrawLineV( Vector2Create(0, PLAYER_SIZE*i), Vector2Create( screenWidth, PLAYER_SIZE*i ), LIGHTGRAY);
        end;

        for i:=0 to  screenWidth div PLAYER_SIZE do
        begin
          for j := 0 to screenHeight div PLAYER_SIZE do
          begin
            DrawText(TextFormat('[%i,%i]', i, j), 10 + PLAYER_SIZE*i, 15 + PLAYER_SIZE*j, 10, LIGHTGRAY);
          end;
        end;

        DrawRectangleRec(player1, RED);
        DrawRectangleRec(player2, BLUE);
        EndMode2D();

        DrawRectangle(0, 0, GetScreenWidth() div 2, 30, Fade(RAYWHITE, 0.6));
        DrawText('PLAYER1: W/S/A/D to move', 10, 10, 10, MAROON);

      EndTextureMode();

      BeginTextureMode(screenCamera2);
        ClearBackground(RAYWHITE);
        BeginMode2D(camera2);
        // Draw full scene with second camera
        for i := 0 to  screenWidth div PLAYER_SIZE + 1 do
        begin
          DrawLineV( Vector2Create( PLAYER_SIZE*i, 0), Vector2Create( PLAYER_SIZE*i, screenHeight ), LIGHTGRAY);
        end;

        for i := 0 to screenHeight div PLAYER_SIZE + 1 do
        begin
          DrawLineV( Vector2Create( 0, PLAYER_SIZE*i ), Vector2Create(screenWidth, PLAYER_SIZE*i ), LIGHTGRAY);
        end;

        for i := 0 to screenWidth div PLAYER_SIZE do
        begin
          for j := 0 to screenHeight div PLAYER_SIZE do
          begin
            DrawText(TextFormat('[%i,%i]', i, j), 10 + PLAYER_SIZE*i, 15 + PLAYER_SIZE*j, 10, LIGHTGRAY);
          end;
        end;

        DrawRectangleRec(player1, RED);
        DrawRectangleRec(player2, BLUE);

        EndMode2D();

        DrawRectangle(0, 0, GetScreenWidth() div 2, 30, Fade(RAYWHITE, 0.6));
        DrawText('PLAYER2: UP/DOWN/LEFT/RIGHT to move', 10, 10, 10, DARKBLUE);

    EndTextureMode();

    // Draw both views render textures to the screen side by side
    BeginDrawing();
        ClearBackground(BLACK);
        DrawTextureRec(screenCamera1.texture, splitScreenRect, Vector2Create( 0, 0 ), WHITE);
        DrawTextureRec(screenCamera2.texture, splitScreenRect, Vector2Create( screenWidth/2.0, 0 ), WHITE);
        DrawRectangle(GetScreenWidth() div 2 - 2, 0, 4, GetScreenHeight(), LIGHTGRAY);
    EndDrawing();

    end;

  // De-Initialization
  UnloadRenderTexture(screenCamera1); // Unload render texture
  UnloadRenderTexture(screenCamera2); // Unload render texture
  CloseWindow();        // Close window and OpenGL context
end.

