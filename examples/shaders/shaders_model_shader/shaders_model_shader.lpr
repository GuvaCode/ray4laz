program shaders_model_shader;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  Camera: TCamera;
  Model: TModel;
  Texture: TTexture2D;
  Shader: TShader;
  Position: TVector3;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - model shader');
  // Define the camera to look into our 3d world
  Camera := Default(TCamera);
  Camera.Position := Vector3Create(4.0, 4.0, 4.0);  // Camera position
  Camera.Target := Vector3Create(0.0, 1.0, -1.0);   // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);        // Camera up vector (rotation towards target)
  Camera.Fovy := 45.0;                                // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;            // Camera mode type

  Model := LoadModel('resources/models/watermill.obj');                   // Load OBJ model
  Texture :=  LoadTexture('resources/models/watermill_diffuse.png');      // Load model texture

  // Load basic lighting shader
  Shader := LoadShader(
    TextFormat('resources/shaders/glsl%i/lighting.vs', GLSL_VERSION),
    TextFormat('resources/shaders/glsl%i/lighting.fs', GLSL_VERSION));

  // Load shader for model
  // NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  Shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/grayscale.fs', Integer(GLSL_VERSION)));

  Model.Materials[0].Shader := Shader;                              // Set shader effect to 3d model
  Model.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture; // Bind texture to model

  Position := Vector3Create(0.0, 0.0, 0.0);    // Set model position

  DisableCursor;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_FREE);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);
      DrawModel(Model, Position, 0.2, WHITE);   // Draw 3d model with texture
      DrawGrid(10, 1.0);     // Draw a grid
      EndMode3D();
      DrawText('(c) Watermill 3D model by Alberto Cano', ScreenWidth - 210, ScreenHeight - 20, 10, GRAY);
      DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(Shader);       // Unload shader
  UnloadTexture(Texture);     // Unload texture
  UnloadModel(Model);         // Unload model
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

