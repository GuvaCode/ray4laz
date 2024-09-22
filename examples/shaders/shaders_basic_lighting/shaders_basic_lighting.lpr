{*******************************************************************************************
*
*   raylib [shaders] example - basic lighting
*
*   NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
*         OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version.
*
*   NOTE: Shaders used in this example are #version 330 (OpenGL 3.3).
*
*   This example has been created using raylib 3.8 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Chris Camacho (@codifies, http://bedroomcoders.co.uk/) and
*   reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2019-2021 Chris Camacho (@codifies) and Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program shaders_basic_lighting;

{$mode objfpc}{$H+}

uses raylib, raymath, rlights;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  camera: TCamera;
  model,cube: TModel;
  shader: TShader;
  ambientLoc, i: integer;
  lights: array [0..MAX_LIGHTS] of TLight ;
  shaderVol, cameraPos: array [0..3] of single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);  // Enable Multi Sampling Anti Aliasing 4x (if available)
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create(2.0,4.0,6.0);    // Camera position
  camera.target := Vector3Create(0.0,0.5,0.0);      // Camera looking at point
  camera.up := Vector3Create(0.0,1.0, 0.0);         // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                              // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;          // Camera mode type

  // Load plane model from a generated mesh
  model := LoadModelFromMesh(GenMeshPlane(10.0, 10.0, 3, 3));
  cube := LoadModelFromMesh(GenMeshCube(2.0, 4.0, 2.0));

  shader := LoadShader(TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/lighting.vs'), GLSL_VERSION),
  TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/lighting.fs'), GLSL_VERSION));

  // Get some required shader loactions
  shader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shader, 'viewPos');
  // NOTE: "matModel" location name is automatically assigned on shader loading,
  // no need to get the location again if using that uniform name
  //shader.locs[SHADER_LOC_MATRIX_MODEL] = GetShaderLocation(shader, "matModel");

  // Ambient light level (some basic lighting)
  ambientLoc := GetShaderLocation(shader, 'ambient');

  shaderVol[0]:=0.1;
  shaderVol[1]:=0.1;
  shaderVol[2]:=0.1;
  shaderVol[3]:=0.1;
  SetShaderValue(shader, ambientLoc, @shaderVol, SHADER_UNIFORM_VEC4);

  // Assign out lighting shader to model
  model.materials[0].shader := shader;
  cube.materials[0].shader := shader;

  // Using 4 point lights: gold, red, green and blue
  lights[0] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1, -2 ), Vector3Zero(), YELLOW, shader);
  lights[1] := CreateLight(LIGHT_POINT, Vector3Create( 2, 1, 2 ), Vector3Zero(), RED, shader);
  lights[2] := CreateLight(LIGHT_POINT, Vector3Create( -2, 1, 2 ), Vector3Zero(), GREEN, shader);
  lights[3] := CreateLight(LIGHT_POINT, Vector3Create( 2, 1, -2 ), Vector3Zero(), BLUE, shader);



  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
     UpdateCamera(@camera,CAMERA_ORBITAL);              // Update camera

     // Check key inputs to enable/disable lights
     if IsKeyPressed(KEY_Y) then  lights[0].enabled := not lights[0].enabled;
     if IsKeyPressed(KEY_R) then  lights[1].enabled := not lights[1].enabled;
     if IsKeyPressed(KEY_G) then  lights[2].enabled := not lights[2].enabled;
     if IsKeyPressed(KEY_B) then  lights[3].enabled := not lights[3].enabled;

     // Update light values (actually, only enable/disable them)
     for  i := 0 to MAX_LIGHTS do UpdateLightValues(shader, lights[i]);

     // Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
     cameraPos[0] := camera.position.x;
     cameraPos[1] := camera.position.y;
     cameraPos[2] := camera.position.z;

     SetShaderValue(shader, shader.locs[SHADER_LOC_VECTOR_VIEW], @cameraPos, SHADER_UNIFORM_VEC3);
     //----------------------------------------------------------------------------------
     // Draw
     //----------------------------------------------------------------------------------
      BeginDrawing();
       ClearBackground(RAYWHITE);
       BeginMode3D(camera);

          DrawModel(model, Vector3Zero(), 1.0, WHITE);
          DrawModel(cube, Vector3Zero(), 1.0, WHITE);

          // Draw spheres to show where the lights are
          for i:=0 to MAX_LIGHTS do
          if (lights[i].enabled) then DrawSphereEx(lights[i].position, 0.2, 8, 8, lights[i].color)
          else
          DrawSphereWires(lights[i].position, 0.2, 8, 8, ColorAlpha(lights[i].color, 0.3));

          DrawGrid(10, 1.0);

        EndMode3D();
        DrawFPS(10, 10);
        DrawText('Use keys [Y][R][G][B] to toggle lights', 10, 40, 20, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadModel(model);     // Unload the model
  UnloadModel(cube);      // Unload the model
  UnloadShader(shader);   // Unload shader
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

