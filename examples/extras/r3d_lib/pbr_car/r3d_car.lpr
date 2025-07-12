program r3d_car;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,{$ENDIF}
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Model: TR3D_Model;
  Ground: TR3D_Mesh;
  GroundMat: TR3D_Material;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  ShowSkybox: Boolean = True;



function Init: PChar;
var
  Flags: R3D_Flags;
  Light: TR3D_Light;
  LightDir: TVector3;
  GroundColor: TColor;
  SceneMin, SceneMax: TVector3;
begin
  // Initialize with FXAA and transparent sorting
  Flags := R3D_FLAG_TRANSPARENT_SORTING or R3D_FLAG_FXAA;
  R3D_Init(GetScreenWidth, GetScreenHeight, Flags);
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

  R3D_SetModelImportScale(0.01);
  // Load assets
  Model := R3D_LoadModel('resources/pbr/car.glb');
  Ground := R3D_GenMeshPlane(100.0, 100.0, 1, 1, True);

  // Configure ground material
  GroundMat := R3D_GetDefaultMaterial();
  GroundColor := ColorCreate(0, 31, 7, 255);
  GroundMat.albedo.color := GroundColor;

  // Setup skybox
  Skybox := R3D_LoadSkybox('resources/sky/skybox3.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(Skybox);

  // Configure camera
  Camera.position := Vector3Create(0, 0, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Set scene bounds
  SceneMin := Vector3Create(-10, -10, -10);
  SceneMax := Vector3Create(10, 10, 10);
  R3D_SetSceneBounds(BoundingBoxCreate(SceneMin, SceneMax));

  // Create directional light with shadows
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(-1, -1, -1);
  R3D_SetLightDirection(Light, LightDir);
  R3D_EnableShadow(Light, 4096);
  R3D_SetLightActive(Light, True);

  Result := '[r3d] - PBR car example';
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
  ModelTransform: TMatrix;
begin
  R3D_Begin(Camera);
    // Draw ground plane slightly below origin
    GroundTransform := MatrixTranslate(0.0, -0.4, 0.0);
    R3D_DrawMesh(@Ground, @GroundMat, GroundTransform);

    // Draw car model rotated -90 degrees around X axis
    ModelTransform := MatrixRotateX(-90.0 * DEG2RAD);
    R3D_DrawModelPro(@Model, ModelTransform);
  R3D_End();

  // Draw credits
  DrawText('Model made by MaximePages', 10, GetScreenHeight - 30, 20, WHITE);
end;

procedure Close;
begin
  R3D_UnloadModel(@Model, True);
  R3D_UnloadSkybox(Skybox);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'PBR Car Example');
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
