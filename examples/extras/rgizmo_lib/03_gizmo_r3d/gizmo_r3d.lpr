program gizmo_r3d;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath, raygizmo, r3d, rlgl;

const
  screenWidth = 800;
  screenHeight = 450;

var
  crateTransform: TTransform;
//  crateTexture: TTexture;

  cam: TCamera;

  light: TR3D_Light;
    LightDir: TVector3;
  Model: TR3D_Model;


begin
  // Initialization
 // SetConfigFlags(FLAG_MSAA_4X_HINT or FLAG_WINDOW_RESIZABLE);
  InitWindow(screenWidth, screenHeight, 'Example 01 - Getting Started');

  // This Transform stores the translation, rotation, and scaling of our crate.
  // It will be updated dynamically by the gizmo during the program.
  crateTransform := GizmoIdentity();


  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetWindowState(FLAG_WINDOW_RESIZABLE);

  SetTargetFPS(60);

  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, 0, -1);
  R3D_SetLightDirection(Light, LightDir);
  R3D_SetLightActive(Light, True);




  // Load the crate model and apply the texture.


  Model := R3D_LoadModel('resources/dancer.glb');


  // Setup the 3D camera.
  cam.Create(Vector3Create(7.5, 5.5, 5.0), Vector3Create(0, 1.5, 0), Vector3Create(0, 1, 0),45.0, CAMERA_PERSPECTIVE);

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      // TODO: Update your variables here
           UpdateCamera(@Cam, CAMERA_CUSTOM);
           R3D_HasState(R3D_FLAG_BLIT_LINEAR);
      // Draw
      BeginDrawing();

      // Clear the background with a dark blue color.
       ClearBackground(RAYWHITE);

       R3D_Begin(cam);
        // rlPushMatrix();
         R3D_DrawModelPro(@Model, GizmoToMatrix(crateTransform));
        // rlPopMatrix();
       R3D_End();


      BeginMode3D(cam);
        DrawGizmo3D(GIZMO_ROTATE, @crateTransform);
      //  DrawBoundingBox(Model.aabb
      EndMode3D();

      EndDrawing();
    end;

  // Unload resources and clean up.

  //UnloadModel(crateModel);
  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

