(*******************************************************************************************
*
*   raylib [models] example - Tesseract view
*
*   NOTE: This example only works on platforms that support drag & drop (Windows, Linux, OSX, Html5?)
*
*   Example complexity rating: [★★☆☆] 2/4
*
*   Example originally created with raylib 5.6-dev, last time updated with raylib 5.6-dev
*
*   Example contributed by Timothy van der Valk (@arceryz) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2024-2025 Timothy van der Valk (@arceryz) and Ramon Santamaria (@raysan5)
*   Pascal conversion 2025 Vadim Gunko (@guvacode)
*
********************************************************************************************)
program models_tesseract_view;

{$mode objfpc}{$H+}

uses 
cmem, raymath, math, raylib;

const
  screenWidth = 800;
  screenHeight = 450;

  tesseract: array[0..15] of TVector4 = (
    (x:  1; y:  1; z:  1; w:  1), (x:  1; y:  1; z:  1; w: -1),
    (x:  1; y:  1; z: -1; w:  1), (x:  1; y:  1; z: -1; w: -1),
    (x:  1; y: -1; z:  1; w:  1), (x:  1; y: -1; z:  1; w: -1),
    (x:  1; y: -1; z: -1; w:  1), (x:  1; y: -1; z: -1; w: -1),
    (x: -1; y:  1; z:  1; w:  1), (x: -1; y:  1; z:  1; w: -1),
    (x: -1; y:  1; z: -1; w:  1), (x: -1; y:  1; z: -1; w: -1),
    (x: -1; y: -1; z:  1; w:  1), (x: -1; y: -1; z:  1; w: -1),
    (x: -1; y: -1; z: -1; w:  1), (x: -1; y: -1; z: -1; w: -1)
  );

var
  rotation: single = 0.0;
  transformed: array[0..15] of TVector3;
  wValues: array [0..15] of single;
  camera: TCamera3d;
  i,j, diff: integer;
  p, v1, v2: TVector4;
  rotXW: TVector2;
  c: single;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - tesseract view');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 4.0, 4.0, 4.0 );    // Camera position
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 0.0, 1.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 50.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      rotation := DEG2RAD*45.0*GetTime();
      for i := 0 to 15 do
        begin
          p := tesseract[i];

          // Rotate the XW part of the vector
          rotXW := Vector2Rotate(Vector2Create( p.x, p.w ), rotation);
          p.x := rotXW.x;
          p.w := rotXW.y;

          // Projection from XYZW to XYZ from perspective point (0, 0, 0, 3)
          // NOTE: Trace a ray from (0, 0, 0, 3) > p and continue until W = 0
          c := 3.0/(3.0 - p.w);
          p.x := c * p.x;
          p.y := c * p.y;
          p.z := c * p.z;

          // Split XYZ coordinate and W values later for drawing
          transformed[i] := Vector3Create( p.x, p.y, p.z );
          wValues[i] := p.w;
        end;

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginMode3D(camera);
        for i:=0 to 15 do
        begin
          // Draw spheres to indicate the W value
          DrawSphere(transformed[i], abs(wValues[i]*0.1), RED);

          for j:= 0 to 15 do
          begin
            // Two lines are connected if they differ by 1 coordinate
            // This way we dont have to keep an edge list
            v1 := tesseract[i];
            v2 := tesseract[j];
            diff := Ord(v1.x = v2.x) + Ord(v1.y = v2.y) + Ord(v1.z = v2.z) + Ord(v1.w = v2.w);
            // Draw only differing by 1 coordinate and the lower index only (duplicate lines)
            if (diff = 3) and (i < j) then DrawLine3D(transformed[i], transformed[j], MAROON);
          end;

        end;
        EndMode3D();
      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

