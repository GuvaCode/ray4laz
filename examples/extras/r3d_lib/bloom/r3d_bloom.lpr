program BloomExample;

uses
 {$IFDEF LINUX} cthreads,{$ENDIF} raylib, rlgl, r3d, math;

var
  Cube: TModel;
  Camera: TCamera3D;
  HueCube: Single = 0.0;

function GetBloomModeName(): PChar;
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

procedure UpdateCubeColor;
var
  Color: TColor;
begin
  Color := ColorFromHSV(HueCube, 1.0, 1.0);
  R3D_SetMaterialAlbedo(@Cube.materials[0], nil, Color);
  R3D_SetMaterialOcclusion(@Cube.materials[0], nil, 1.0);
  R3D_SetMaterialEmission(@Cube.materials[0], nil, Color, 1.0);
end;

function Init: PChar;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetBloomMode(R3D_BLOOM_MIX);
  R3D_SetBackgroundColor(BLACK);

  Cube := LoadModelFromMesh(GenMeshCube(1.0, 1.0, 1.0));

  UpdateCubeColor();

  Camera.position := Vector3Create(0, 3.5, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  Result := '[r3d] - bloom example';
end;

function Wrap(Value, Min, Max: Single): Single;
var
  Range: Single;
begin
  Range := Max - Min;
  if Range = 0 then Exit(Min); // Avoid division by zero
  Result := Value - Range * Floor((Value - Min) / Range);
end;

procedure Update(delta: Single);
var
  HueDir, IntensityDir, RadiusDir: Integer;
begin
  UpdateCamera(@Camera, CAMERA_ORBITAL);

  HueDir := Integer(IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) -
            Integer(IsMouseButtonDown(MOUSE_BUTTON_LEFT));
  if HueDir <> 0 then
  begin
    HueCube := Wrap(HueCube + HueDir * 90.0 * delta, 0, 360);
    UpdateCubeColor();
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
    R3D_DrawModel(Cube, Vector3Create(0, 0, 0), 1.0);
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
  UnloadModel(Cube);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'bloom'); // for window settings, look at example - window flags
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
