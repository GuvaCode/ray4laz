program gizmo_types;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath, raygizmo;

const
  screenWidth = 800;
  screenHeight = 450;
  CRATE_COUNT = 4;

var
  // These Transforms store the translation, rotation, and scaling of the crates
  // They will be dynamically updated by the gizmos during the program
  crateTransforms: array [0..CRATE_COUNT -1] of TTransform;
  crateTexture: TTexture;
  crateModel: TModel;
  cam: TCamera;
  gizmoTypes: array[0..CRATE_COUNT -1] of integer;
  i: integer;

begin
  // Initialize the transforms with default values
  for  i:= 0 to CRATE_COUNT -1 do
  begin
    crateTransforms[i] := GizmoIdentity();
    crateTransforms[i].translation.x := -12.0 + 6.0 * i;  // Offset crates along the X-axis
  end;

  // Assign each crate a different gizmo type
  gizmoTypes[0] := GIZMO_TRANSLATE;
  gizmoTypes[1] := GIZMO_SCALE;
  gizmoTypes[2] := GIZMO_ROTATE;
  gizmoTypes[3] := GIZMO_ALL;

  // Setup: Initialize the window
  SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_RESIZABLE);
  InitWindow(screenWidth, screenHeight, 'Example 02 - Gizmo Types');
  SetTargetFPS(60);

  // Load the crate texture.
  crateTexture := LoadTexture('resources/models/crate_texture.jpg');
  GenTextureMipmaps(@crateTexture);
  SetTextureFilter(crateTexture, TEXTURE_FILTER_TRILINEAR);

  // Load the crate model and apply the texture.
  crateModel := LoadModel('resources/models/crate_model.obj');
  crateModel.materials[0].maps[MATERIAL_MAP_ALBEDO].texture := crateTexture;

  // Setup the 3D camera.
  cam.fovy := 45.0;
  cam.position := Vector3Create( -5.5, 10.5, 14.0 );
  cam.target := Vector3Create( -2.5, 0.0, 0 );
  cam.up := Vector3Create( 0, 1, 0 );
  cam.projection := CAMERA_PERSPECTIVE;

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Draw
      BeginDrawing();

        // Clear the background with a dark blue color.
        ClearBackground(ColorCreate( 0, 0, 25, 255 ));

        BeginMode3D(cam);

        // Draw the crates with their updated transforms
        for i := 0 to  CRATE_COUNT - 1 do
        begin
          crateModel.transform := GizmoToMatrix(crateTransforms[i]);
          DrawModel(crateModel, Vector3Zero(), 1.0, WHITE);
        end;

        // Draw the gizmos and handle user input
        for i := 0 to  CRATE_COUNT - 1 do
        DrawGizmo3D(gizmoTypes[i], @crateTransforms[i]);

        EndMode3D();
        EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

