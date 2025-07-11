program emission;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Model: TR3D_Model;
  Plane: TR3D_Mesh;
  Material: TR3D_Material;
  Camera: TCamera3D;
  Light: TR3D_Light;
  RotModel: Single = 0.0;

procedure ToggleLight;
begin
  if R3D_IsLightActive(Light) then
  begin
    R3D_SetLightActive(Light, False);
    R3D_SetAmbientColor(BLACK);
  end
  else
  begin
    R3D_SetLightActive(Light, True);
    R3D_SetAmbientColor(DARKGRAY);
  end;
end;

function Init: PChar;
var
  LightPos, LightTarget: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  R3D_SetBackgroundColor(BLACK);
  R3D_SetAmbientColor(DARKGRAY);

  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetTonemapExposure(0.8);
  R3D_SetTonemapWhite(2.5);

  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  R3D_SetBloomSoftThreshold(0.2);
  R3D_SetBloomIntensity(0.2);
  R3D_SetBloomThreshold(0.6);

  Model := R3D_LoadModel('resources/emission.glb');

  Plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  Material := R3D_GetDefaultMaterial();

  Camera.position := Vector3Create(-1.0, 1.75, 1.75);
  Camera.target := Vector3Create(0, 0.5, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  Light := R3D_CreateLight(R3D_LIGHT_SPOT);
  LightPos := Vector3Create(0, 10, 5);
  LightTarget := Vector3Create(0, 0, 0);

  R3D_LightLookAt(Light, LightPos, LightTarget);
  R3D_SetLightOuterCutOff(Light, 45.0);
  R3D_SetLightInnerCutOff(Light, 22.5);
  R3D_EnableShadow(Light, 4096);
  R3D_SetLightActive(Light, True);

  Result := '[r3d] - Emission example';
end;

procedure Update(delta: Single);
var
  MouseDelta: TVector2;
begin
  if IsKeyPressed(KEY_SPACE) then
    ToggleLight();

  if IsMouseButtonDown(MOUSE_LEFT_BUTTON) then
  begin
    MouseDelta := GetMouseDelta();
    Camera.position.y := Clamp(Camera.position.y + 0.01 * MouseDelta.y, 0.25, 2.5);
    RotModel := RotModel + MouseDelta.x;
  end;
end;

procedure Draw;
var
  ModelPos, ModelAxis: TVector3;
  ModelScale: TVector3;
begin
  R3D_Begin(Camera);
    R3D_DrawMesh(@Plane, @Material, MatrixIdentity());

    ModelPos := Vector3Create(0, 0, 0);
    ModelAxis := Vector3Create(0, 1, 0);
    ModelScale := Vector3Create(10.0, 10.0, 10.0);
    R3D_DrawModelEx(@Model, ModelPos, ModelAxis, RotModel, ModelScale);
  R3D_End();

  DrawText('Press SPACE to toggle the light', 10, 10, 20, LIME);
  DrawText('Model by har15204405', 10, GetScreenHeight - 30, 20, LIME);
end;

procedure Close;
begin
  R3D_UnloadModel(@Model, True);
  R3D_UnloadMesh(@Plane);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Emission Example');
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
