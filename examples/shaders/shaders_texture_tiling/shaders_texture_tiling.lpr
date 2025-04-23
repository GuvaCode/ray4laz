program shaders_texture_tiling;

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
  GLSL_VERSION = 330;

var
  camera: TCamera3d;
  cube: TMesh;
  model: TModel;
  texture: TTexture2D;
  tiling: array[0..1] of single;
  shader: TShader;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - texture tiling');    // Define the camera to look into our 3d world

  camera := Default(TCamera3d);
  camera.position := Vector3Create( 4.0, 4.0, 4.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 0.5, 0.0 );   // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );       // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                               // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;           // Camera projection type

  // Load a cube model
  cube := GenMeshCube(1.0, 1.0, 1.0);
  model := LoadModelFromMesh(cube);

  // Load a texture and assign to cube model
  texture := LoadTexture('resources/cubicmap_atlas.png');
  model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;

  // Set the texture tiling using a shader
  tiling[0] := 3.0;
  tiling[1] := 3.0;
  shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/tiling.fs', GLSL_VERSION));
  SetShaderValue(shader, GetShaderLocation(shader, 'tiling'), @tiling, SHADER_UNIFORM_VEC2);
  model.materials[0].shader := shader;

  DisableCursor();                    // Limit cursor to relative movement inside the window

  SetTargetFPS(60);                   // Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_FREE);
      if (IsKeyPressed(KEY_Z)) then camera.target := Vector3Create( 0.0, 0.5, 0.0 );

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);

        BeginMode3D(camera);

          BeginShaderMode(shader);
            DrawModel(model, Vector3Create( 0.0, 0.0, 0.0 ), 2.0, WHITE);
          EndShaderMode();

          DrawGrid(10, 1.0);

        EndMode3D();


        DrawText('Use mouse to rotate the camera', 10, 10, 20, DARKGRAY);
      EndDrawing();
    end;

  // De-Initialization
  UnloadModel(model);         // Unload model
  UnloadShader(shader);       // Unload shader
  UnloadTexture(texture);     // Unload texture

  CloseWindow();        // Close window and OpenGL context
end.

