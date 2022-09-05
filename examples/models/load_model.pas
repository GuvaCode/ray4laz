program load_model;

{$MODE objfpc}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  cam: TCamera;
  dwarf: TModel;
  texture: TTexture2d;
  position: TVector3;

begin
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  // Enable Multi Sampling Anti Aliasing 4x (if available)

  InitWindow(screenWidth, screenHeight,
    'raylib [shaders] example - load 3d model');

  // Define the camera to look into our 3d world
  cam.position := Vector3Create(3.0, 3.0, 3.0);
  cam.target := Vector3Create(0.0, 0, 0.0);
  cam.up := Vector3Create(0.0, 1.0, 0.0);
  cam.fovy := 45.0;
  cam.projection := CAMERA_PERSPECTIVE;

  dwarf := LoadModel('resources/models/Spacesuit.mtl'); // Load OBJ model
  texture := LoadTexture('resources/models/church_diffuse.png');
  // Load model texture (diffuse map)

  dwarf.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture:= texture; // Set map diffuse texture

  //SetMaterialTexture(@dwarf.materials[0], MATERIAL_MAP_DIFFUSE, texture); // Set map diffuse texture alt

  // Set dwarf model diffuse texture
  position := Vector3Create(0.0, 0.0, 0.0); // Set model position

 // Setup orbital camera
  SetCameraMode(cam, CAMERA_ORBITAL); // Set an orbital camera mode

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  // -------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose do // Detect window close button or ESC key
  begin
    // Update
    // -----------------------------------------------------------------------
    UpdateCamera(@cam); // Update camera
    // -----------------------------------------------------------------------
    // Draw
    // -----------------------------------------------------------------------
    BeginDrawing();

    ClearBackground(RAYWHITE);

    BeginMode3d(cam);

    DrawModel(dwarf, position, 0.050, WHITE); // Draw 3d model with texture
    DrawGrid(10, 0.5); // Draw a grid

    EndMode3d();


    DrawText('(c) Dwarf 3D model by David Moreno', screenWidth - 200, screenHeight - 20, 10, GRAY);
    DrawFPS(10, 10);

    EndDrawing();
    // ----------------------------------------------------------------------
  end;

  // De-Initialization
  // ------------------------------------------------------------------------
  UnloadTexture(texture); // Unload texture
  UnloadModel(dwarf); // Unload model
  CloseWindow(); // Close window and OpenGL context

end.
