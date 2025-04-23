program core_3d_camera_first_person;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_COLUMNS = 20;

var
  Camera: TCamera;
  heights: array[0..MAX_COLUMNS - 1] of single;
  positions: array[0..MAX_COLUMNS - 1] of TVector3;
  colors:  array[0..MAX_COLUMNS - 1] of TColorB;
  i:integer;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d camera first person');

  // Define the camera to look into our 3d world (position, target, up vector)
  Camera.position:=Vector3Create(4.0,2.0,4.0);
  Camera.target:=Vector3Create(0.0,1.8,0.0);
  Camera.up:=Vector3Create(0.0,1.0,0.0);
  Camera.fovy:=60.0;
  Camera.projection:=CAMERA_PERSPECTIVE;

  // Generates some random columns
  for i:=0 to MAX_COLUMNS -1 do
  begin
   heights[i]:=Round(GetRandomValue(1, 12));
   positions[i]:=Vector3Create(Round(GetRandomValue(-15, 15)),heights[i]/2, Round(GetRandomValue(-15, 15)));
   colors[i] := ColorCreate(GetRandomValue(20, 255), GetRandomValue(10, 55), 30, 255);
  end;


  SetTargetFPS(60);                           // Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose do
  begin
   // Update
   UpdateCamera(@camera,CAMERA_FIRST_PERSON);                  // Update camera
   // Draw
   BeginDrawing();
   ClearBackground(RAYWHITE);

   BeginMode3D(camera);
    DrawPlane(Vector3Create(0.0,0.0,0.0),Vector2Create(32.0,32.0),LIGHTGRAY);//Draw ground
    DrawCube(Vector3Create(-16.0,2.5,0.0),1.0,5.0,32.0, BLUE);// Draw a blue wall
    DrawCube(Vector3Create(16.0,2.5,0.0),1.0,5.0,32.0,LIME);// Draw a green wall
    DrawCube(Vector3Create(0.0,2.5,16.0),32.0,5.0,1.0,GOLD);// Draw a yellow wall

    for i:=0 to MAX_COLUMNS-1 do
    begin
     DrawCube(positions[i], 2.0, heights[i], 2.0, colors[i]);
     DrawCubeWires(positions[i], 2.0, heights[i], 2.0, MAROON);
    end;

    EndMode3D();

   DrawRectangle( 10, 10, 220, 70, Fade(SKYBLUE, 0.5));
   DrawRectangleLines( 10, 10, 220, 70, BLUE);

            DrawText('First person camera default controls:', 20, 20, 10, BLACK);
            DrawText('- Move with keys: W, A, S, D', 40, 40, 10, DARKGRAY);
            DrawText('- Mouse move to look around', 40, 60, 10, DARKGRAY);

        EndDrawing();
  end;
 // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

