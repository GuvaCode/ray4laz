program r3d_resize;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath, rlgl;

var
  Camera: TCamera3D;
  Sphere: TR3D_Mesh;
  Materials: array[0..4] of TR3D_Material;
  Light: TR3D_Light;

function Init: PChar;
var
  i: Integer;
  LightDir: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetWindowState(FLAG_WINDOW_RESIZABLE);
  SetTargetFPS(60);

  Sphere := R3D_GenMeshSphere(0.5, 64, 64, True);

  for i := 0 to 4 do
  begin
    Materials[i] := R3D_GetDefaultMaterial();
    Materials[i].albedo.color := ColorFromHSV(i / 5 * 330, 1.0, 1.0);
  end;

  Camera.position := Vector3Create(0, 2, 2);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, 0, -1);
  R3D_SetLightDirection(Light, LightDir);
  R3D_SetLightActive(Light, True);

  Result := '[r3d] - Resize example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_ORBITAL);

  if IsKeyPressed(KEY_R) then
  begin
    if R3D_HasState(R3D_FLAG_ASPECT_KEEP) then
      R3D_ClearState(R3D_FLAG_ASPECT_KEEP)
    else
      R3D_SetState(R3D_FLAG_ASPECT_KEEP);
  end;

  if IsKeyPressed(KEY_F) then
  begin
    if R3D_HasState(R3D_FLAG_BLIT_LINEAR) then
      R3D_ClearState(R3D_FLAG_BLIT_LINEAR)
    else
      R3D_SetState(R3D_FLAG_BLIT_LINEAR);
  end;
end;

procedure Draw;
var
  KeepAspect, LinearFilter: Boolean;
  i: Integer;
  ModeStr, FilterStr: string;
begin
  KeepAspect := R3D_HasState(R3D_FLAG_ASPECT_KEEP);
  LinearFilter := R3D_HasState(R3D_FLAG_BLIT_LINEAR);

  if KeepAspect then
    ClearBackground(BLACK);

  R3D_Begin(Camera);
    rlPushMatrix();
    for i := 0 to 4 do
    begin
      R3D_DrawMesh(@Sphere, @Materials[i], MatrixTranslate(i - 2, 0, 0));
    end;
    rlPopMatrix();
  R3D_End();

  // Draw mode info
  if KeepAspect then
    ModeStr := 'Resize mode: KEEP'
  else
    ModeStr := 'Resize mode: EXPAND';
  DrawText(PChar(ModeStr), 10, 10, 20, BLACK);

  if LinearFilter then
    FilterStr := 'Filter mode: LINEAR'
  else
    FilterStr := 'Filter mode: NEAREST';
  DrawText(PChar(FilterStr), 10, 40, 20, BLACK);
end;

procedure Close;
begin
  R3D_UnloadMesh(@Sphere);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Resize Example');
  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    BeginDrawing();
      ClearBackground(RAYWHITE);
      Draw();
    EndDrawing();
  end;

  Close();
  CloseWindow();
end.
