program shaders_simple_mask;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;
var
  Shader: TShader;
  Camera: TCamera;
  Torus, Cube, Sphere: TMesh;
  Model1, Model2, Model3: TModel;
  TexDiffuse, TexMask: TTexture;
  ShaderFrame: Integer;
  FramesCounter: Integer;
  Rotation: TVector3;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - mask');

  // Define the camera to look into our 3d world
  Camera := Camera3DCreate(
    Vector3Create(0.0, 1.0, 2.0),      // position
    Vector3Create(0.0, 0.0, 0.0),      // target
    Vector3Create(0.0, 1.0, 0.0),      // up
    45.0, CAMERA_PERSPECTIVE             // fov, type
  );

  // Define our three models to show the shader on
  Torus := GenMeshTorus(0.3, 1, 16, 32);
  Model1 := LoadModelFromMesh(Torus);

  Cube := GenMeshCube(0.8, 0.8, 0.8);
  Model2 := LoadModelFromMesh(Cube);

  // Generate model to be shaded just to see the gaps in the other two
  Sphere := GenMeshSphere(1, 64, 64);
  Model3 := LoadModelFromMesh(Sphere);

  // Load the shader
  Shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/mask.fs', Integer(GLSL_VERSION)));

  // Load and apply the diffuse texture (colour map)
  TexDiffuse := LoadTexture('resources/plasma.png');
  Model1.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := TexDiffuse;
  Model2.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := TexDiffuse;
  Model3.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := TexDiffuse;
  // Using MATERIAL_MAP_EMISSION as a spare slot to use for 2nd texture
  // NOTE: Don't use MATERIAL_MAP_IRRADIANCE, MATERIAL_MAP_PREFILTER or  MATERIAL_MAP_CUBEMAP as they are bound as cube maps
  TexMask := LoadTexture('resources/mask.png');
  Model1.Materials[0].Maps[MATERIAL_MAP_EMISSION].Texture := TexMask;
  Model2.Materials[0].Maps[MATERIAL_MAP_EMISSION].Texture := TexMask;
  Model3.Materials[0].Maps[MATERIAL_MAP_EMISSION].Texture := TexMask;
  Shader.Locs[SHADER_LOC_MAP_EMISSION] := GetShaderLocation(Shader, 'mask');

  // Frame is incremented each frame to animate the shader
  ShaderFrame := GetShaderLocation(Shader, 'frame');

  // Apply the shader to the two models
  Model1.Materials[0].Shader := Shader;
  Model2.Materials[0].Shader := Shader;
  Model3.Materials[0].Shader := Shader;
  FramesCounter := 0;
  Rotation := Vector3Create(0, 0, 0);       // Model rotation angles

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera, CAMERA_FIRST_PERSON);

      Inc(FramesCounter);
      Rotation.X := Rotation.X + 0.01;
      Rotation.Y := Rotation.Y + 0.005;
      Rotation.Z := Rotation.Z - 0.0025;

      // Send frames counter to shader for animation
      SetShaderValue(Shader, ShaderFrame, @FramesCounter, SHADER_UNIFORM_INT);

      // Rotate one of the models
      Model1.Transform := MatrixRotateXYZ(Rotation);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

            ClearBackground(DARKBLUE);

            BeginMode3D(Camera);

              DrawModel(Model1, Vector3Create(0.5, 0, 0), 1, WHITE);
              DrawModelEx(Model2, Vector3Create(-0.5, 0, 0), Vector3Create(1, 1, 0), 50, Vector3Create(1, 1, 1), WHITE);
              DrawModel(Model3, Vector3Create(0, 0, -1.5), 1, WHITE);
              DrawGrid(10, 1.0);        // Draw a grid

            EndMode3D();

            DrawRectangle(16, 698, MeasureText(TextFormat('Frame: %i', FramesCounter), 20) + 8, 42, BLUE);
            DrawText(TextFormat('Frame: %i', FramesCounter), 20, 700, 20, WHITE);

            DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
   UnloadModel(Model1);
   UnloadModel(Model2);
   UnloadModel(Model3);

   UnloadTexture(TexDiffuse);  // Unload default diffuse texture
   UnloadTexture(TexMask);     // Unload texture mask

   UnloadShader(Shader);       // Unload shader

   CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

