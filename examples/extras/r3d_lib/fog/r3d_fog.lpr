program r3d_fog;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Sponza: TR3D_Model;
  Camera: TCamera3D;
  Light: TR3D_Light;

function Init: PChar;
var
  LightDir: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Загрузка модели Sponza
  Sponza := R3D_LoadModel('resources/sponza.glb');

  // Настройка тумана
  R3D_SetFogMode(R3D_FOG_EXP);

  // Настройка направленного света
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, -1, 0);
  R3D_SetLightDirection(Light, LightDir);
  R3D_SetLightActive(Light, True);

  // Настройка камеры
  Camera.position := Vector3Create(0, 0, 0);
  Camera.target := Vector3Create(0, 0, -1);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  DisableCursor();

  Result := '[r3d] - Fog example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);
end;

procedure Draw;
var
  ModelPos: TVector3;
  ModelScale: Single;
begin
  R3D_Begin(Camera);
    ModelPos := Vector3Create(0, 0, 0);
    ModelScale := 1.0;
    R3D_DrawModel(@Sponza, ModelPos, ModelScale);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure Close;
begin
  R3D_UnloadModel(@Sponza, True);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Fog Example');
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
