program getting_started;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath, raygizmo;

const
  screenWidth = 800;
  screenHeight = 450;

var
  crateTransform: TTransform;
  crateTexture: TTexture;
  crateModel: TModel;
  cam: TCamera;
begin
  // Initialization
  SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_RESIZABLE);
  InitWindow(screenWidth, screenHeight, 'Example 01 - Getting Started');

  // This Transform stores the translation, rotation, and scaling of our crate.
  // It will be updated dynamically by the gizmo during the program.
  crateTransform := GizmoIdentity();
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Load the crate texture.
  crateTexture := LoadTexture('resources/models/crate_texture.jpg');
  GenTextureMipmaps(@crateTexture);
  SetTextureFilter(crateTexture, TEXTURE_FILTER_TRILINEAR);

  // Load the crate model and apply the texture.
  crateModel := LoadModel('resources/models/crate_model.obj');
  crateModel.materials[0].maps[MATERIAL_MAP_ALBEDO].texture := crateTexture;

  // Setup the 3D camera.
  cam.Create(Vector3Create(7.5, 5.5, 5.0), Vector3Create(0, 1.5, 0), Vector3Create(0, 1, 0),45.0, CAMERA_PERSPECTIVE);

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here

      // Draw
      BeginDrawing();

      // Clear the background with a dark blue color.
      ClearBackground(ColorCreate(0, 0, 25, 255) );

      BeginMode3D(cam);

      // Update the crate's transform matrix from the gizmo.
      crateModel.transform := GizmoToMatrix(crateTransform);

      // Draw the crate model using the updated transform.
      DrawModel(crateModel, Vector3Zero(), 1.0, WHITE);

      // Draw the translation gizmo and handle its input.
      // This will directly update the crateTransform variable.
      DrawGizmo3D(GIZMO_TRANSLATE, @crateTransform);

      EndMode3D();

      EndDrawing();
    end;

  // Unload resources and clean up.
  UnloadTexture(crateTexture);
  UnloadModel(crateModel);
  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

