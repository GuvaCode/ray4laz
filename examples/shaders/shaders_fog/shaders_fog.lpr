program shaders_fog;

{$mode objfpc}{$H+}

uses raylib, raymath, rlights;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  modelA,modelB,modelC: TModel;
  texture: TTexture2D;
  shader: TShader;
  camera: TCamera;
  fogDensity: single;
  fogDensityLoc, ambientLoc,i: integer;
  locValue: array [0..3] of single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);  // Enable Multi Sampling Anti Aliasing 4x (if available)
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');

  // Define the camera to look into our 3d world
  Camera3DSet(@camera,
  Vector3Create(2.0, 2.0, 6.0), // position
  Vector3Create(0.0, 0.5, 0.0), // target
  Vector3Create(0.0, 1.0, 0.0), // up
  45.0, CAMERA_PERSPECTIVE); // fov, type

  // Load models and texture
  modelA := LoadModelFromMesh(GenMeshTorus(0.4, 1.0, 16, 32));
  modelB := LoadModelFromMesh(GenMeshCube(1.0, 1.0, 1.0));
  modelC := LoadModelFromMesh(GenMeshSphere(0.5, 32, 32));
  texture := LoadTexture(PChar(GetApplicationDirectory +'resources/texel_checker.png'));

  // Assign texture to default model material
  modelA.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;
  modelB.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;
  modelC.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;


  // Load shader and set up some uniforms
  shader := LoadShader(TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/lighting.vs'), GLSL_VERSION),
                                 TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/fog.fs'), GLSL_VERSION));

  shader.locs[SHADER_LOC_MATRIX_MODEL] := GetShaderLocation(shader, 'matModel');
  shader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shader, 'viewPos');

  locValue[0]:=0.2;
  locValue[1]:=0.2;
  locValue[2]:=0.2;
  locValue[3]:=1.0;
  // Ambient light level

  ambientLoc := GetShaderLocation(shader, 'ambient');
  SetShaderValue(shader, ambientLoc, @locValue, SHADER_UNIFORM_VEC4);

  fogDensity := 0.15;
  fogDensityLoc := GetShaderLocation(shader, 'fogDensity');
  SetShaderValue(shader, fogDensityLoc, @fogDensity, SHADER_UNIFORM_FLOAT);

  // NOTE: All models share the same shader
  modelA.materials[0].shader := shader;
  modelB.materials[0].shader := shader;
  modelC.materials[0].shader := shader;

  // Using just 1 point lights
  CreateLight(LIGHT_POINT, Vector3Create( 0, 2, 6 ), Vector3Zero(), WHITE, shader);


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_ORBITAL);              // Update camera
      if IsKeyDown(KEY_UP) then
        begin
            fogDensity += 0.001;
            if (fogDensity > 1.0) then fogDensity := 1.0;
        end;

        if IsKeyDown(KEY_DOWN)  then
        begin
            fogDensity -= 0.001;
            if (fogDensity < 0.0) then fogDensity := 0.0;
        end;

        SetShaderValue(shader, fogDensityLoc, @fogDensity, SHADER_UNIFORM_FLOAT);

        // Rotate the torus
        modelA.transform := MatrixMultiply(modelA.transform, MatrixRotateX(-0.025));
        modelA.transform := MatrixMultiply(modelA.transform, MatrixRotateZ(0.012));

        // Update the light shader with the camera view position
        SetShaderValue(shader, shader.locs[SHADER_LOC_VECTOR_VIEW], @camera.position.x, SHADER_UNIFORM_VEC3);

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(GRAY);
            BeginMode3D(camera);
                // Draw the three models
                DrawModel(modelA, Vector3Zero(), 1.0, WHITE);
                DrawModel(modelB, Vector3Create( -2.6, 0, 0), 1.0, WHITE);
                DrawModel(modelC, Vector3Create( 2.6, 0, 0), 1.0, WHITE);

               for i:= -20 to 20 do
               begin

               DrawModel(modelA,Vector3Create(i*2,0,2),1.0,White);
               end;

            EndMode3D();

            DrawText(TextFormat('Use KEY_UP/KEY_DOWN to change fog density [%.2f]', fogDensity), 10, 10, 20, RAYWHITE);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
    UnloadModel(modelA);        // Unload the model A
    UnloadModel(modelB);        // Unload the model B
    UnloadModel(modelC);        // Unload the model C
    UnloadTexture(texture);     // Unload the texture
    UnloadShader(shader);       // Unload shader
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

