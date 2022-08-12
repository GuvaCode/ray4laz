{*******************************************************************************************
*
*   raylib [models] example - Waving cubes
*
*   This example has been created using raylib 2.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Codecat (@codecat) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2019 Codecat (@codecat) and Ramon Santamaria (@raysan5)
*   Pascal translation Gunko Vadim (@guvacode)
********************************************************************************************}


program models_waving_cubes;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

const
 screenWidth = 800;
 screenHeight = 450;
 numBlocks = 15; // Specify the amount of blocks in each direction



var
  camera: TCamera3d;
  time,cameraTime : double;
  scale,blockScale,scatter, cubeSize: single;
  x,y,z:integer;
  cubeColor:TColorB;
  cubePos:TVector3;
begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [models] example - waving cubes');
 SetTargetFPS(60);
 camera:=  Camera3DCreate(Vector3Create( 30.0, 20.0, 30.0),
                   Vector3Create( 0.0, 0.0, 0.0 ),
                   Vector3Create( 0.0, 1.0, 0.0),
                   70.0,CAMERA_PERSPECTIVE);

 {camera.position := Vector3Create( 30.0, 20.0, 30.0);
 camera.target := Vector3Create( 0.0, 0.0, 0.0 );
 camera.up := Vector3Create( 0.0, 1.0, 0.0);
 camera.fovy := 70.0;
 camera._type := CAMERA_PERSPECTIVE; }



 while not WindowShouldClose() do
 begin
   // Update
         time := GetTime();
        // Calculate time scale for cube position and size
         scale := (2.0 + sin(time))*0.7;
        // Move camera around the scene
        cameraTime := time*0.3;
        camera.position.x := cos(cameraTime)*40.0;
        camera.position.z := sin(cameraTime)*40.0;
        //----------------------------------------------------------------------------------
  BeginDrawing();

  ClearBackground(RAYWHITE);
   BeginMode3D(camera);
              DrawGrid(10, 5.0);
                for  x := 0 to numBlocks do
                begin
                    for y := 0 to numBlocks do
                    begin
                        for z := 0 to numBlocks do
                        begin
                            // Scale of the blocks depends on x/y/z positions
                            blockScale := (x + y + z) / 30.0;
                           // Scatter makes the waving effect by adding blockScale over time
                            scatter := Sin(blockScale*20.0 + (time*4.0));
                            // Calculate the cube position
                              Vector3Set(@cubePos,
                                (x - numBlocks/2)*(scale*3.0) + scatter,
                                (y - numBlocks/2)*(scale*2.0) + scatter,
                                (z - numBlocks/2)*(scale*3.0) + scatter);
                           // Pick a color with a hue depending on cube position for the rainbow color effect
                            // cubeColor := ColorFromHSV(Vector3Create((((x + y + z)*18)), 0.75, 0.9));
                             cubeColor := ColorFromHSV((x + y + z)*18,0.75, 0.9);
                             // Calculate cube size
                             cubeSize := (2.4 - scale)*blockScale;
                            // And finally, draw the cube!
                            DrawCube(cubePos, cubeSize, cubeSize, cubeSize, cubeColor);
                        end;
                     end;
                  end;
       EndMode3D();
       DrawFPS(10, 10);
       EndDrawing();
 end;
CloseWindow();

end.

