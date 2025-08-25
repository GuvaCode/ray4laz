program AnimationExample;

uses
  SysUtils, Math,
  raylib, rlgl, raymath, r3d;

var
  plane: TR3D_Mesh;
  dancer: TR3D_Model;
  material: TR3D_Material;
  instances: array[0..3] of TMatrix;
  camera: TCamera3D;
  animCount: Integer;
  anims: PR3D_ModelAnimation;
  lights: array[0..1] of TR3D_Light;
const
  RESOURCES_PATH = 'resources/';
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;

function Init: PChar;
var
  checked: TImage;
  z, x: Integer;
begin
  InitWindow(800, 600, 'Animation Example');
  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_FXAA or R3D_FLAG_NO_FRUSTUM_CULLING);
  SetTargetFPS(60);

  R3D_SetSSAO(True);
  R3D_SetBloomIntensity(0.03);
  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  R3D_SetTonemapMode(R3D_TONEMAP_ACES);

  R3D_SetBackgroundColor(BLACK);
  R3D_SetAmbientColor(ColorCreate(7, 7, 7, 255));

  plane := R3D_GenMeshPlane(32, 32, 1, 1, True);
  dancer := R3D_LoadModel(PChar(RESOURCES_PATH + 'dancer.glb'));
  material := R3D_GetDefaultMaterial();

  for z := 0 to 1 do
    for x := 0 to 1 do
      instances[z * 2 + x] := MatrixTranslate(x - 0.5, 0, z - 0.5);

  checked := GenImageChecked(256, 256, 4, 4, ColorCreate(20, 20, 20, 255), WHITE);
  material.albedo.texture := LoadTextureFromImage(checked);
  UnloadImage(checked);

  material.orm.roughness := 0.5;
  material.orm.metalness := 0.5;

  anims := R3D_LoadModelAnimations(PChar(RESOURCES_PATH + 'dancer.glb'), @animCount, 60, false);

  camera.position := Vector3Create(0, 2.0, 3.5);
  camera.target := Vector3Create(0, 1.0, 1.5);
  camera.up := Vector3Create(0, 1, 0);
  camera.fovy := 60;
  camera.projection := CAMERA_PERSPECTIVE;

  lights[0] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(lights[0], Vector3Create(-10.0, 25.0, 0.0));
  R3D_EnableShadow(lights[0], 4096);
  R3D_SetLightActive(lights[0], True);

  lights[1] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(lights[1], Vector3Create(+10.0, 25.0, 0.0));
  R3D_EnableShadow(lights[1], 4096);
  R3D_SetLightActive(lights[1], True);

  DisableCursor();

  Result := '[r3d] - Animation example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@camera, CAMERA_FREE);
  dancer.anim := @anims[0];
  Inc(dancer.animFrame);

  R3D_SetLightColor(lights[0], ColorFromHSV(90.0 * GetTime() + 90.0, 1.0, 1.0));
  R3D_SetLightColor(lights[1], ColorFromHSV(90.0 * GetTime() - 90.0, 1.0, 1.0));
end;

procedure Draw;
begin
  R3D_Begin(camera);
    R3D_DrawMesh(@plane, @material, MatrixIdentity());
    R3D_DrawModel(@dancer, Vector3Create(0, 0, 1.5), 1.0);
    R3D_DrawModelInstanced(@dancer, @instances[0], 4);
  R3D_End();

  DrawText('Model made by zhuoyi0904', 10, 580, 10, RAYWHITE);
end;

procedure Close;
begin
  R3D_UnloadMesh(@plane);
  R3D_UnloadModel(@dancer, True);
  R3D_UnloadMaterial(@material);
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
end.
