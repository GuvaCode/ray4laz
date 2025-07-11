program r3d_skybox;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Sphere: TR3D_Mesh;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  Materials: array[0..48] of TR3D_Material; // 7x7 grid

function Init: PChar;
var
  x, y, i: Integer;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Create sphere mesh
  Sphere := R3D_GenMeshSphere(0.5, 64, 64, True);

  // Create materials grid (7x7)
  for x := 0 to 6 do
  begin
    for y := 0 to 6 do
    begin
      i := y * 7 + x;
      Materials[i] := R3D_GetDefaultMaterial();
      Materials[i].orm.metalness := x / 7;
      Materials[i].orm.roughness := y / 7;
      Materials[i].albedo.color := ColorFromHSV((x / 7) * 360, 1, 1);
    end;
  end;

  // Load skybox
  Skybox := R3D_LoadSkybox('resources/sky/skybox1.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(Skybox);

  // Setup camera
  Camera.position := Vector3Create(0, 0, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  DisableCursor();

  Result := '[r3d] - Skybox example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);
end;

procedure Draw;
var
  x, y: Integer;
begin
  R3D_Begin(Camera);
    // Draw 7x7 grid of spheres
    for x := 0 to 6 do
    begin
      for y := 0 to 6 do
      begin
        R3D_DrawMesh(@Sphere, @Materials[y * 7 + x], MatrixTranslate(x - 3, y - 3, 0.0));
      end;
    end;
  R3D_End();
end;

procedure Close;
begin
  R3D_UnloadMesh(@Sphere);
  R3D_UnloadSkybox(Skybox);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Skybox Example');
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
