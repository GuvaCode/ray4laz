program pbr_car;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,{$ENDIF}
  Classes, SysUtils, raylib, r3d, raymath;

var
  Model: TR3D_Model;
  Ground: TR3D_Mesh;
  GroundMat: TR3D_Material;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  ShowSkybox: Boolean = True;
const
  RESOURCES_PATH = 'resources/';

procedure Init;
var
  Flags: R3D_Flags;
  Light: TR3D_Light;
  SceneMin, SceneMax: TVector3;
begin
  InitWindow(800,600, '[r3d] - PBR car example');
  // Initialize with FXAA and transparent sorting
  Flags := R3D_FLAG_TRANSPARENT_SORTING or R3D_FLAG_FXAA;
  R3D_Init(GetScreenWidth(), GetScreenHeight(), Flags);
  SetTargetFPS(60);
  DisableCursor();

  // Configure scene lighting and effects
  R3D_SetBackgroundColor(BLACK);
  R3D_SetAmbientColor(DARKGRAY);
  R3D_SetSSAO(True);
  R3D_SetSSAORadius(2.0);
  R3D_SetBloomIntensity(0.1);
  R3D_SetBloomMode(R3D_BLOOM_MIX);
  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetSSR(True);

  // Load assets
  R3D_SetModelImportScale(0.01);
  Model := R3D_LoadModel(PChar(RESOURCES_PATH + 'pbr/car.glb'));
  Ground := R3D_GenMeshPlane(10.0, 10.0, 1, 1, True);

  // Configure ground material
  GroundMat := R3D_GetDefaultMaterial();
  GroundMat.albedo.color := ColorCreate(31, 31, 31, 255);
  GroundMat.orm.roughness := 0.0;
  GroundMat.orm.metalness := 0.5;

  // Setup skybox
  Skybox := R3D_LoadSkybox(PChar(RESOURCES_PATH + 'sky/skybox3.png'), CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(Skybox);

  // Configure camera
  Camera.position := Vector3Create(0, 0, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;
  Camera.projection := CAMERA_PERSPECTIVE;

  // Set scene bounds
  SceneMin := Vector3Create(-10, -10, -10);
  SceneMax := Vector3Create(10, 10, 10);
  R3D_SetSceneBounds(BoundingBoxCreate(SceneMin, SceneMax));

  // Create directional light with shadows
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  R3D_SetLightDirection(Light, Vector3Create(-1, -1, -1));
  R3D_EnableShadow(Light, 4096);
  R3D_SetLightActive(Light, True);


end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);

  // Toggle SSAO with O key
  if IsKeyPressed(KEY_O) then
    R3D_SetSSAO(not R3D_GetSSAO());

  // Toggle skybox with T key
  if IsKeyPressed(KEY_T) then
  begin
    ShowSkybox := not ShowSkybox;
    if ShowSkybox then
      R3D_EnableSkybox(Skybox)
    else
      R3D_DisableSkybox();
  end;
end;

procedure Draw;
var
  GroundTransform: TMatrix;
begin
  R3D_Begin(Camera);
    // Draw ground plane slightly below origin
    GroundTransform := MatrixTranslate(0.0, -0.4, 0.0);
    R3D_DrawMesh(@Ground, @GroundMat, GroundTransform);

    // Draw car model
    R3D_DrawModel(@Model, Vector3Create(0,0,0), 1.0);
  R3D_End();

  // Draw credits
  DrawText( 'Model made by MaximePages', 10, 580, 10, RAYWHITE);
end;

procedure Close;
begin
  R3D_UnloadModel(@Model, True);
  R3D_UnloadSkybox(Skybox);
  R3D_Close();
end;

begin

  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    BeginDrawing();
      ClearBackground(BLACK);
      Draw();
    EndDrawing();
  end;

  Close();
  CloseWindow();
end.
