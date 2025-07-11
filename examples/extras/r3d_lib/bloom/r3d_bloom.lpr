program BloomExample;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Cube: TR3D_Mesh;
  Material: TR3D_Material;
  Camera: TCamera3D;
  HueCube: Single = 0.0;

function GetBloomModeName: PChar;
begin
  case R3D_GetBloomMode() of
    R3D_BLOOM_DISABLED: Result := 'Disabled';
    R3D_BLOOM_MIX: Result := 'Mix';
    R3D_BLOOM_ADDITIVE: Result := 'Additive';
    R3D_BLOOM_SCREEN: Result := 'Screen';
  else
    Result := 'Unknown';
  end;
end;

function Init: PChar;
begin
  R3D_Init(800, 600, 0);
  SetTargetFPS(60);

  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetBloomMode(R3D_BLOOM_MIX);
  R3D_SetBackgroundColor(BLACK);

  Cube := R3D_GenMeshCube(1.0, 1.0, 1.0, True);
  Material := R3D_GetDefaultMaterial();

  Material.emission.color := ColorFromHSV(HueCube, 1.0, 1.0);
  Material.emission.energy := 1.0;
  Material.albedo.color := BLACK;

  Camera.position := Vector3Create(0, 3.5, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  Result := '[r3d] - Bloom example';
end;

procedure Update(delta: Single);
var
  HueDir, IntensityDir, RadiusDir: Integer;
begin
  UpdateCamera(@Camera, CAMERA_ORBITAL);

  HueDir := Integer(IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) - Integer(IsMouseButtonDown(MOUSE_BUTTON_LEFT));
  if HueDir <> 0 then
  begin
    HueCube := Wrap(HueCube + HueDir * 90.0 * delta, 0, 360);
    Material.emission.color := ColorFromHSV(HueCube, 1.0, 1.0);
  end;

  IntensityDir := Integer(IsKeyPressedRepeat(KEY_RIGHT) or IsKeyPressed(KEY_RIGHT)) -
                  Integer(IsKeyPressedRepeat(KEY_LEFT) or IsKeyPressed(KEY_LEFT));

  if IntensityDir <> 0 then
  begin
    R3D_SetBloomIntensity(R3D_GetBloomIntensity() + IntensityDir * 0.01);
  end;

  RadiusDir := Integer(IsKeyPressedRepeat(KEY_UP) or IsKeyPressed(KEY_UP)) -
               Integer(IsKeyPressedRepeat(KEY_DOWN) or IsKeyPressed(KEY_DOWN));

  if RadiusDir <> 0 then
  begin
    R3D_SetBloomFilterRadius(R3D_GetBloomFilterRadius() + RadiusDir);
  end;

  if IsKeyPressed(KEY_SPACE) then
  begin
    R3D_SetBloomMode((R3D_GetBloomMode() + 1) mod (R3D_BLOOM_SCREEN + 1));
  end;
end;

procedure Draw;
var
  InfoStr: PChar;
  InfoLen: Integer;
begin
  R3D_Begin(Camera);
    R3D_DrawMesh(@Cube, @Material, MatrixIdentity());
  R3D_End();

  R3D_DrawBufferEmission(10, 10, 100, 100);
  R3D_DrawBufferBloom(120, 10, 100, 100);

  InfoStr := TextFormat('Mode: %s', GetBloomModeName());
  InfoLen := MeasureText(InfoStr, 20);
  DrawText(InfoStr, GetScreenWidth() - InfoLen - 10, 10, 20, LIME);

  InfoStr := TextFormat('Intensity: %.2f', R3D_GetBloomIntensity());
  InfoLen := MeasureText(InfoStr, 20);
  DrawText(InfoStr, GetScreenWidth() - InfoLen - 10, 40, 20, LIME);

  InfoStr := TextFormat('Filter Radius: %d', R3D_GetBloomFilterRadius());
  InfoLen := MeasureText(InfoStr, 20);
  DrawText(InfoStr, GetScreenWidth() - InfoLen - 10, 70, 20, LIME);
end;

procedure Close;
begin
  R3D_UnloadMesh(@Cube);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Bloom Example');
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
