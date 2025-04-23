{*******************************************************************************************
*
*   raylib [shaders] example - Raymarching shapes generation
*
*   NOTE: This example requires raylib OpenGL 3.3 for shaders support and only #version 330
*         is currently supported. OpenGL ES 2.0 platforms are not supported at the moment.
*
*   This example has been created using raylib 2.0 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2018 Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program shaders_raymarching;

{$mode objfpc}{$H+}

uses cmem,raylib;

const

 GLSL_VERSION  =330;

 var
    screenWidth: integer = 800;
   screenHeight: integer = 450;
   camera:TCamera;
   shader:TShader;
   viewEyeLoc,viewCenterLoc,runTimeLoc,resolutionLoc: integer;
   resolution: array[1..2] of single;// = (screenWidth,screenHeight);
   cameraPos: array[1..3] of single;// = ( camera.position.x, camera.position.y, camera.position.z );
   cameraTarget: array[1..3] of single;// = ( camera.target.x, camera.target.y, camera.target.z );
   runTime,deltaTime:single;

begin
 SetConfigFlags(FLAG_WINDOW_RESIZABLE);
 InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - raymarching shapes');

 Camera3DSet(@camera,Vector3Create(2.5,2.5,3.0),
                     Vector3Create(0.0,0.0,0.7),
                     Vector3Create(0.0,1.0,00.0),65,CAMERA_PERSPECTIVE);


 // Load raymarching shader
 // NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
 shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/raymarching.fs', GLSL_VERSION));

 // Get shader locations for required uniforms
  viewEyeLoc := GetShaderLocation(shader, 'viewEye');
  viewCenterLoc := GetShaderLocation(shader, 'viewCenter');
  runTimeLoc := GetShaderLocation(shader, 'runTime');
  resolutionLoc := GetShaderLocation(shader, 'resolution');

  resolution[1] := screenWidth;
  resolution[2] := screenHeight;

  SetShaderValue(shader, resolutionLoc, @resolution, SHADER_UNIFORM_VEC2);

  runTime := 0.0;
  SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
  // Update
  cameraPos[1]:= camera.position.x;
  cameraPos[2]:= camera.position.y;
  cameraPos[3]:= camera.position.z;
  cameraTarget[3]:= camera.target.x;
  cameraTarget[3]:= camera.target.y;
  cameraTarget[3]:= camera.target.z;

  deltaTime := GetFrameTime();
  runTime += deltaTime;

        // Set shader required uniform values
        SetShaderValue(shader, viewEyeLoc, @cameraPos, SHADER_UNIFORM_VEC3);
        SetShaderValue(shader, viewCenterLoc, @cameraTarget, SHADER_UNIFORM_VEC3);
        SetShaderValue(shader, runTimeLoc, @runTime, SHADER_UNIFORM_FLOAT);

        // Check if screen is resized
        if IsWindowResized then
        begin
            screenWidth := GetScreenWidth();
            screenHeight := GetScreenHeight();
            resolution[1] :=screenWidth; { (float)screenWidth, (float)screenHeight };
            resolution[2] := screenHeight;
            SetShaderValue(shader, resolutionLoc, @resolution, SHADER_UNIFORM_VEC2);
        end;
  //Draw
  BeginDrawing();
  ClearBackground(RAYWHITE);

  // We only draw a white full-screen rectangle,
  // frame is generated in shader using raymarching
  BeginShaderMode(shader);
  DrawRectangle(0, 0, screenWidth, screenHeight, WHITE);
  EndShaderMode();

  DrawText('(c) Raymarching shader by Iñigo Quilez. MIT License.', screenWidth - 280, screenHeight - 20, 10, BLACK);
  DrawFps(10,10);

  EndDrawing(); 
 end;
  UnloadShader(shader);           // Unload shader
  CloseWindow();

end.

