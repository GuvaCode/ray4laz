{*******************************************************************************************
*
*   raylib [shaders] example - Vertex displacement
*
*   Example originally created with raylib 5.0, last time updated with raylib 4.5
*
*   Example contributed by <Alex ZH> (@ZzzhHe) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 <Alex ZH> (@ZzzhHe)
*   Pascal conversion 20024 Gunko Vadim (@GuvaCode)
*
********************************************************************************************}
program shaders_vertex_displacement;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
rlgl,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;
var
  camera: TCamera;
  shader: TShader;
  perlinNoiseImage: TImage;
  perlinNoiseMap: TTexture;
  perlinNoiseMapLoc: Integer;
  planeMesh: TMesh;
  planeModel: TModel;
  time: Single;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - vertex displacement');
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // set up camera
  camera.position := Vector3Create(20.0, 5.0, -20.0);
  camera.target := Vector3Create(0.0, 0.0, 0.0);
  camera.up := Vector3Create(0.0, 1.0, 0.0);
  camera.fovy := 60.0;
  camera.projection := CAMERA_PERSPECTIVE;

  // Load vertex and fragment shaders
  shader := LoadShader(
      TextFormat('resources/shaders/glsl%i/vertex_displacement.vs', GLSL_VERSION),
      TextFormat('resources/shaders/glsl%i/vertex_displacement.fs', GLSL_VERSION));

  // Load perlin noise texture
  perlinNoiseImage := GenImagePerlinNoise(512, 512, 0, 0, 1.0);
  perlinNoiseMap := LoadTextureFromImage(perlinNoiseImage);
  UnloadImage(perlinNoiseImage);

  // Set shader uniform location
  perlinNoiseMapLoc := GetShaderLocation(shader, 'perlinNoiseMap');
  rlEnableShader(shader.id);
  rlActiveTextureSlot(1);
  rlEnableTexture(perlinNoiseMap.id);
  rlSetUniformSampler(perlinNoiseMapLoc, 1);

  // Create a plane mesh and model
  planeMesh := GenMeshPlane(50, 50, 50, 50);
  planeModel := LoadModelFromMesh(planeMesh);
  // Set plane model material
  planeModel.materials[0].shader := shader;

  time := 0.0;

  // Main game loop
  while not WindowShouldClose() do
    begin

      // Update
      UpdateCamera(@camera, CAMERA_FREE); // Update camera
      time += GetFrameTime(); // Update time variable
      SetShaderValue(shader, GetShaderLocation(shader, 'time'), @time, SHADER_UNIFORM_FLOAT); // Send time value to shader

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginMode3D(camera);
          BeginShaderMode(shader);
          // Draw plane model
            DrawModel(planeModel, Vector3Create( 0.0, 0.0, 0.0), 1.0, WHITE);
          EndShaderMode();
        EndMode3D();
        DrawText('Vertex displacement', 10, 10, 20, DARKGRAY);
        DrawFPS(10, 40);
      EndDrawing();
    end;

  // De-Initialization
  UnloadShader(shader);
  UnloadModel(planeModel);
  UnloadTexture(perlinNoiseMap);

  CloseWindow();        // Close window and OpenGL context
end.

