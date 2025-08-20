program Emission;

uses
  SysUtils, Math,
  raylib, rlgl, raymath, r3d;

var
  model: TR3D_Model;
  plane: TR3D_Mesh;
  material: TR3D_Material;
  camera: TCamera3D;
  light: TR3D_Light;
  rotModel: Single = 0.0;
  Name: PChar;
const
  RESOURCES_PATH = 'resources/';

procedure ToggleLight;
begin
  if R3D_IsLightActive(light) then
  begin
    R3D_SetLightActive(light, False);
    R3D_SetAmbientColor(BLACK);
  end
  else
  begin
    R3D_SetLightActive(light, True);
    R3D_SetAmbientColor(DARKGRAY);
  end;
end;

procedure Init;
begin
  InitWindow(800,600, '[r3d] - Emission example');
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
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

  R3D_SetModelImportScale(0.01);

  model := R3D_LoadModel(PChar(RESOURCES_PATH + 'emission.glb'));

  plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  material := R3D_GetDefaultMaterial();

  camera.position := Vector3Create(-1.0, 1.75, 1.75);
  camera.target := Vector3Create(0, 0.5, 0);
  camera.up := Vector3Create(0, 1, 0);
  camera.fovy := 60;
  camera.projection := CAMERA_PERSPECTIVE;

  light := R3D_CreateLight(R3D_LIGHT_SPOT);
  R3D_LightLookAt(light, Vector3Create(0, 10, 5), Vector3Create(0,0,0));
  R3D_SetLightOuterCutOff(light, 45.0);
  R3D_SetLightInnerCutOff(light, 22.5);
  R3D_EnableShadow(light, 4096);
  R3D_SetLightActive(light, True);


end;

procedure Update(delta: Single);
var
  mouseDelta: TVector2;
begin
  if IsKeyPressed(KEY_SPACE) then
    ToggleLight();

  if IsMouseButtonDown(MOUSE_LEFT_BUTTON) then
  begin
    mouseDelta := GetMouseDelta();
    camera.position.y := Clamp(camera.position.y + 0.01 * mouseDelta.y, 0.25, 2.5);
    rotModel := rotModel + 0.01 * mouseDelta.x;
  end;
end;

procedure Draw;
begin
  R3D_Begin(camera);
    R3D_DrawMesh(@plane, @material, MatrixIdentity());
    R3D_DrawModelEx(@model, Vector3Create(0,0,0), Vector3Create(0, 1, 0), rotModel, Vector3Create(10.0, 10.0, 10.0));
  R3D_End();

  DrawText('Press SPACE to toggle the light', 10, 10, 20, LIME);
  DrawText('Model by har15204405',10, 580, 10, RAYWHITE);
end;

procedure Close;
begin
  R3D_UnloadModel(@model, True);
  R3D_UnloadMesh(@plane);
  R3D_Close();
end;

begin
  Init;

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    BeginDrawing();
      ClearBackground(BLACK);
      Draw();
    EndDrawing();
  end;
  Close();
end.
