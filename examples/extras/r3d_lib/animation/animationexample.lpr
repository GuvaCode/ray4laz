program animationexample;

uses
  Raylib, r3d, rayMath, Math, SysUtils, Unix;

const
  RESOURCES_PATH = 'resources';
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;

var
  Plane: TR3D_Mesh;
  Dancer: TR3D_Model;
  Material: TR3D_Material;
  Camera: TCamera3D;
  AnimCount: Integer;
  Anims: PR3D_ModelAnimation;
  Lights: array[0..1] of TR3D_Light;
  Checked: TImage;
  Frame: Integer = 0;

function Init: PChar;
var
  Title: string;
begin
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, '');

  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_FXAA or R3D_FLAG_NO_FRUSTUM_CULLING);
  SetTargetFPS(60);

  R3D_SetSSAO(True);
  R3D_SetBloomIntensity(0.03);
  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  R3D_SetTonemapMode(R3D_TONEMAP_ACES);

  R3D_SetBackgroundColor(BLACK);
  R3D_SetAmbientColor(ColorCreate(7, 7, 7, 255));

  Plane := R3D_GenMeshPlane(32, 32, 1, 1, True);
  Dancer := R3D_LoadModel(PChar(RESOURCES_PATH + '/dancer.glb'));

  Material := R3D_GetDefaultMaterial();

  Checked := GenImageChecked(256, 256, 4, 4, ColorCreate(20, 20, 20, 255), WHITE);
  Material.albedo.texture := LoadTextureFromImage(Checked);
  UnloadImage(Checked);

  Material.orm.roughness := 0.5;
  Material.orm.metalness := 0.5;

  Anims := R3D_LoadModelAnimations(PChar(RESOURCES_PATH + '/dancer.glb'), @AnimCount, 60);

  Camera.position := Vector3Create(0, 2.0, 2.0);
  Camera.target := Vector3Create(0, 1.0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  Lights[0] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(Lights[0], Vector3Create(-10.0, 25.0, 0.0));
  R3D_EnableShadow(Lights[0], 4096);
  R3D_SetLightActive(Lights[0], True);

  Lights[1] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(Lights[1], Vector3Create(+10.0, 25.0, 0.0));
  R3D_EnableShadow(Lights[1], 4096);
  R3D_SetLightActive(Lights[1], True);

  DisableCursor();

  Title := '[r3d] - Animation example';
  Result := PChar(Title);
end;

procedure Update(delta: Single);
var
  hue1, hue2: Single;
begin
  UpdateCamera(@Camera, CAMERA_FREE);

  if AnimCount > 0 then
  begin
    Dancer.anim := @Anims[0];
    Inc(Dancer.animFrame);
  end;

  hue1 := 90.0 * GetTime() + 90.0;
  hue2 := 90.0 * GetTime() - 90.0;

  R3D_SetLightColor(Lights[0], ColorFromHSV(hue1, 1.0, 1.0));
  R3D_SetLightColor(Lights[1], ColorFromHSV(hue2, 1.0, 1.0));
end;

procedure DrawCredits(text: PChar);
var
  len: Integer;
begin
  len := MeasureText(text, 16);

  DrawRectangle(0, GetScreenHeight() - 36, 20 + len, 36, Fade(BLACK, 0.5));
  DrawRectangleLines(0, GetScreenHeight() - 36, 20 + len, 36, BLACK);
  DrawText(text, 10, GetScreenHeight() - 26, 16, LIME);
end;

procedure Draw;
begin
  R3D_Begin(Camera);
    R3D_DrawMesh(@Plane, @Material, MatrixIdentity());
    R3D_DrawModel(@Dancer, Vector3Create(0, 0, 0), 100.0);
  R3D_End();

  DrawCredits('Model made by zhuoyi0904');
end;

procedure Close;
begin
  R3D_UnloadMesh(@Plane);
  R3D_UnloadModel(@Dancer, True);
  R3D_UnloadMaterial(@Material);
  R3D_Close();
  CloseWindow();
end;

begin

  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    BeginDrawing();
      Draw();
    EndDrawing();
  end;

  Close();
end.


