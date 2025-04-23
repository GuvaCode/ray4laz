{*******************************************************************************************
*
*   raylib [models] example - Drawing billboards
*
*   Example originally created with raylib 1.3, last time updated with raylib 3.5
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2015-2023 Ramon Santamaria (@raysan5)
*   Pascal translation 2023 Vadim Gunko (@GuVaCode)
*
********************************************************************************************}

program models_billboard;

{$mode objfpc}{$H+}
{$define Adv_Record}
uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;

var
  camera: TCamera;
  bill: TTexture2D;
  BillPositionStatic, BillPositionRotating, BillUp: TVector3;
  RotateOrigin: TVector2;
  Source: TRectangle;
  DistanceStatic, DistanceRotating, Rotation: Single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - drawing billboards');
  // Define the camera to look into our 3d world
  camera := default(TCamera);
  camera:= Camera3DCreate(Vector3Create(5.0,4.0,5.0), // position
                          Vector3Create(0.0,2.0,0.0), // target
                          Vector3Create(0.0,1.0,0.0), //up
                          45.0, CAMERA_PERSPECTIVE);

  bill := LoadTexture(PChar(GetApplicationDirectory + 'resources/billboard.png'));     // Our billboard texture
  billPositionStatic := Vector3Create(0, 2, 0); // Position of billboard
  billPositionRotating := Vector3Create(1, 2, 1);

  // Entire billboard texture, source is used to take a segment from a larger texture.
  Source := RectangleCreate(0.0, 0.0, Bill.Width, Bill.Height);

  // NOTE: Billboard locked on axis-Y
  BillUp := Vector3Create(0.0, 1.0, 0.0);

  // Rotate around origin
  // Here we choose to rotate around the image center
  // NOTE: (-1, 1) is the range where origin.x, origin.y is inside the texture
  RotateOrigin := Vector2Create(0.0, 0.0);


  // Distance is needed for the correct billboard draw order
  // Larger distance (further away from the camera) should be drawn prior to smaller distance.
  Rotation := 0.0;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_ORBITAL);
      Rotation := Rotation + 0.4;
      DistanceStatic := Vector3Distance(Camera.Position, BillPositionStatic);
      DistanceRotating := Vector3Distance(Camera.Position, BillPositionRotating);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

       BeginMode3D(Camera);
         DrawGrid(10, 1.0); // Draw a grid

         // Draw order matters!
         if DistanceStatic > DistanceRotating then
         begin
           DrawBillboard(Camera, Bill, BillPositionStatic, 2.0, WHITE);
           DrawBillboardPro(Camera, Bill, Source, BillPositionRotating, BillUp, Vector2Create(1.0, 1.0), RotateOrigin, Rotation, WHITE);
         end else
         begin
           DrawBillboardPro(Camera, Bill, Source, BillPositionRotating, BillUp, Vector2Create(1.0, 1.0), RotateOrigin, Rotation, WHITE);
           DrawBillboard(Camera, Bill, BillPositionStatic, 2.0, WHITE);
         end;

       EndMode3D();
       DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Bill); // Unload texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

