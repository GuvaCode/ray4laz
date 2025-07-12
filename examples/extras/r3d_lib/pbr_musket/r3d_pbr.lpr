program r3d_pbr;


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,{$ENDIF}
  Classes, SysUtils, CustApp, raylib, r3d, raymath, math;

var
  Model: TR3D_Model;
  ModelMatrix: TMatrix;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  ModelScale: Single = 1.0;

function Init: PChar;
var
  Light: TR3D_Light;
  LightDir: TVector3;
  Transform: TMatrix;
  i: Integer;
begin
  // Initialize with FXAA anti-aliasing
  R3D_Init(GetScreenWidth, GetScreenHeight, R3D_FLAG_FXAA);
  SetTargetFPS(60);

  // Configure PBR rendering settings
  R3D_SetSSAO(True);
  R3D_SetSSAORadius(4.0);
  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetTonemapExposure(0.75);
  R3D_SetTonemapWhite(1.25);
  R3D_SetModelImportScale(0.01);
  // Load musket model
  Model := R3D_LoadModel('resources/pbr/musket.glb');

  // Rotate model 90 degrees around Y axis and set texture filters
  Transform := MatrixRotateY(PI / 2);
  for i := 0 to Model.materialCount - 1 do
  begin
    SetTextureFilter(Model.materials[i].albedo.texture, TEXTURE_FILTER_BILINEAR);
    SetTextureFilter(Model.materials[i].orm.texture, TEXTURE_FILTER_BILINEAR);
  end;

  ModelMatrix := MatrixIdentity();

  // Setup skybox
  Skybox := R3D_LoadSkybox('resources/sky/skybox2.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(Skybox);

  // Configure camera
  Camera.position := Vector3Create(0, 0, 0.5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Create directional light
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, -1, -1);
  R3D_SetLightDirection(Light, LightDir);
  R3D_SetLightActive(Light, True);

  Result := '[r3d] - PBR musket example';
end;

procedure Update(delta: Single);
var
  Pitch, Yaw: Single;
  Rotate: TMatrix;
  RotAngles: TVector3;
begin
  // Zoom with mouse wheel
  ModelScale := Clamp(ModelScale + GetMouseWheelMove() * 0.1, 0.25, 2.5);

  // Rotate with left mouse button
  if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
  begin
    Pitch := (GetMouseDelta().y * 0.005) / ModelScale;
    Yaw := (GetMouseDelta().x * 0.005) / ModelScale;

    RotAngles := Vector3Create(Pitch, Yaw, 0.0);
    Rotate := MatrixRotateXYZ(RotAngles);
    ModelMatrix := MatrixMultiply(ModelMatrix, Rotate);
  end;
end;

procedure Draw;
var
  ScaleMatrix, Transform: TMatrix;
begin
  R3D_Begin(Camera);
    // Apply scale and rotation to model
    ScaleMatrix := MatrixScale(ModelScale, ModelScale, ModelScale);
    Transform := MatrixMultiply(ModelMatrix, ScaleMatrix);
    R3D_DrawModelPro(@Model, Transform);
  R3D_End();

  // Draw credits
  DrawText('Model made by TommyLingL', 10, GetScreenHeight - 30, 20, WHITE);
end;

procedure Close;
begin
  R3D_UnloadModel(@Model, True);
  R3D_UnloadSkybox(Skybox);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'PBR Musket Example');
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
