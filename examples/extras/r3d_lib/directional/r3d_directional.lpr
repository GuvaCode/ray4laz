program r3d_directional;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Plane: TR3D_Mesh;
  Sphere: TR3D_Mesh;
  Material: TR3D_Material;
  Camera: TCamera3D;
  Transforms: array of TMatrix;
  Light: TR3D_Light;

function Init: PChar;
var
  x, z, index: Integer;
  LightDir: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  Plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  Sphere := R3D_GenMeshSphere(0.35, 16, 16, True);
  Material := R3D_GetDefaultMaterial();

  Camera.position := Vector3Create(0, 2, 2);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Initialize transforms array
  SetLength(Transforms, 100 * 100);

  for x := -50 to 49 do
  begin
    for z := -50 to 49 do
    begin
      index := (z + 50) * 100 + (x + 50);
      Transforms[index] := MatrixTranslate(x * 2, 0, z * 2);
    end;
  end;

  // Create and configure directional light
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, -1, -1);

  R3D_SetLightDirection(Light, LightDir);
  R3D_SetShadowUpdateMode(Light, R3D_SHADOW_UPDATE_MANUAL);
  R3D_SetShadowBias(Light, 0.005);
  R3D_EnableShadow(Light, 4096);
  R3D_SetLightActive(Light, True);

  DisableCursor();

  Result := '[r3d] - Directional light example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);
end;

procedure Draw;
begin
  R3D_Begin(Camera);
    R3D_DrawMesh(@Plane, @Material, MatrixTranslate(0, -0.5, 0));
    R3D_DrawMeshInstanced(@Sphere, @Material, @Transforms[0], 100 * 100);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure Close;
begin
  R3D_UnloadMesh(@Plane);
  R3D_UnloadMesh(@Sphere);
  R3D_UnloadMaterial(@Material);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Directional Light Example');
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

