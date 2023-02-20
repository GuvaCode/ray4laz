{*******************************************************************************************
*
*   raylib [models] example - rlgl module usage with push/pop matrix transformations
*
*   This example uses [rlgl] module funtionality (pseudo-OpenGL 1.1 style coding)
*
*   This example has been created using raylib 2.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2018 Ramon Santamaria (@raysan5)
*   translate example to pascal Gunko Vadim
*
********************************************************************************************}
program models_rlgl_solar_system;

{$mode objfpc}{$H+}

uses cmem, raylib, rlgl;

const
 screenWidth = 800;
 screenHeight = 450;
 sunRadius = 4.0;
 earthRadius = 0.6;
 earthOrbitRadius = 8.0;
 moonRadius = 0.16;
 moonOrbitRadius = 1.5;

 var
   camera : TCamera;
   rotationSpeed :single = 0.2;         // General system rotation speed
   earthRotation :single = 0.0;         // Rotation of earth around itself (days) in degrees
   earthOrbitRotation :single = 0.0;    // Rotation of earth around the Sun (years) in degrees
   moonRotation :single = 0.0;          // Rotation of moon around itself
   moonOrbitRotation :single = 0.0;     // Rotation of moon around earth in degrees

   procedure  DrawSphereBasic(Color: TColorB); // Draw sphere without any matrix transformation
   var
       rings:integer = 16;
       slices:integer = 16;
       i,j : integer;
   begin

    // Make sure there is enough space in the internal render batch
    // buffer to store all required vertex, batch is reseted if required
    rlCheckRenderBatchLimit((rings + 2)*slices*6);

    rlBegin(RL_TRIANGLES);
        rlColor4ub(color.r, color.g, color.b, color.a);

       // for (int i = 0; i < (rings + 2); i++)
        for i:=0 to rings+2 do
         begin
         //   for (int j = 0; j < slices; j++)
          for j:=0 to slices do
          begin
                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*i))*sin(DEG2RAD*(j*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*i)),
                           cos(DEG2RAD*(270+(180/(rings + 1))*i))*cos(DEG2RAD*(j*360/slices)));
                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*sin(DEG2RAD*((j+1)*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*(i+1))),
                           cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*cos(DEG2RAD*((j+1)*360/slices)));
                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*sin(DEG2RAD*(j*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*(i+1))),
                           cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*cos(DEG2RAD*(j*360/slices)));

                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*i))*sin(DEG2RAD*(j*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*i)),
                           cos(DEG2RAD*(270+(180/(rings + 1))*i))*cos(DEG2RAD*(j*360/slices)));
                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*(i)))*sin(DEG2RAD*((j+1)*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*(i))),
                           cos(DEG2RAD*(270+(180/(rings + 1))*(i)))*cos(DEG2RAD*((j+1)*360/slices)));
                rlVertex3f(cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*sin(DEG2RAD*((j+1)*360/slices)),
                           sin(DEG2RAD*(270+(180/(rings + 1))*(i+1))),
                           cos(DEG2RAD*(270+(180/(rings + 1))*(i+1)))*cos(DEG2RAD*((j+1)*360/slices)));
            end;
        end;
    rlEnd();
    end;

begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [models] example - rlgl module usage with push/pop matrix transformations');

 // Define the camera to look into our 3d world
 camera.position := Vector3Create(16.0,16.0,16.0);
 camera.target := Vector3Create(0.0,0.0,0.0);
 camera.up := Vector3Create(0.0,1.0,0.0);
 camera.fovy := 45.0;
 camera.projection := CAMERA_PERSPECTIVE;

 DisableCursor;

SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin

  // Update
  //----------------------------------------------------------------------------------
   UpdateCamera(@camera,CAMERA_FREE);
   earthRotation += (5.0*rotationSpeed);
   earthOrbitRotation += (365/360.0*(5.0*rotationSpeed)*rotationSpeed);
   moonRotation += (2.0*rotationSpeed);
   moonOrbitRotation += (8.0*rotationSpeed);
  //----------------------------------------------------------------------------------

  // Draw
  //----------------------------------------------------------------------------------
   BeginDrawing();
     ClearBackground(RAYWHITE);
      BeginMode3D(camera);

                rlPushMatrix();
                    rlScalef(sunRadius, sunRadius, sunRadius);          // Scale Sun
                    DrawSphereBasic(GOLD);                              // Draw the Sun
                rlPopMatrix();

                rlPushMatrix();
                    rlRotatef(earthOrbitRotation, 0.0, 1.0, 0.0);    // Rotation for Earth orbit around Sun
                    rlTranslatef(earthOrbitRadius, 0.0, 0.0);         // Translation for Earth orbit
                    rlRotatef(-earthOrbitRotation, 0.0, 1.0, 0.0);   // Rotation for Earth orbit around Sun inverted

                    rlPushMatrix();
                        rlRotatef(earthRotation, 0.25, 1.0, 0.0);       // Rotation for Earth itself
                        rlScalef(earthRadius, earthRadius, earthRadius);// Scale Earth

                        DrawSphereBasic(BLUE);                          // Draw the Earth
                    rlPopMatrix();

                    rlRotatef(moonOrbitRotation, 0.0, 1.0, 0.0);     // Rotation for Moon orbit around Earth
                    rlTranslatef(moonOrbitRadius, 0.0, 0.0);          // Translation for Moon orbit
                    rlRotatef(-moonOrbitRotation, 0.0, 1.0, 0.0);    // Rotation for Moon orbit around Earth inverted
                    rlRotatef(moonRotation, 0.0, 1.0, 0.0);          // Rotation for Moon itself
                    rlScalef(moonRadius, moonRadius, moonRadius);       // Scale Moon

                    DrawSphereBasic(LIGHTGRAY);                         // Draw the Moon
                rlPopMatrix();

                // Some reference elements (not affected by previous matrix transformations)

                DrawCircle3D(Vector3Create( 0.0, 0.0, 0.0 ), earthOrbitRadius, Vector3Create( 1, 0, 0 ), 90.0, Fade(RED, 0.5));

                DrawGrid(20, 1.0);

            EndMode3D();

            DrawText('EARTH ORBITING AROUND THE SUN!', 400, 10, 20, MAROON);
            DrawFPS(10, 10);

        EndDrawing();
        //----------------------------------------------------------------------------------

 end;
CloseWindow(); 

end.

