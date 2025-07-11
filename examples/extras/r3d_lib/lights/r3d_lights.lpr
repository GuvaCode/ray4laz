program r3d_lights;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

const
  GRID_SIZE = 100;  // 100x100 grid for spheres
  LIGHT_COUNT = 100; // 10x10 grid for lights

var
  Plane: TR3D_Mesh;
  Sphere: TR3D_Mesh;
  Material: TR3D_Material;
  Camera: TCamera3D;
  Transforms: array of TMatrix;
  Lights: array[0..LIGHT_COUNT-1] of TR3D_Light;

function Init: PChar;
var
  x, z, index: Integer;
  LightPos: TVector3;
  LightColor: TColor;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Create plane and sphere meshes
  Plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  Sphere := R3D_GenMeshSphere(0.35, 16, 16, True);
  Material := R3D_GetDefaultMaterial();

  // Setup camera
  Camera.position := Vector3Create(0, 2, 2);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Initialize transforms for instanced spheres
  SetLength(Transforms, GRID_SIZE * GRID_SIZE);
  for x := -50 to 49 do
  begin
    for z := -50 to 49 do
    begin
      index := (z + 50) * GRID_SIZE + (x + 50);
      Transforms[index] := MatrixTranslate(x, 0, z);
    end;
  end;

  // Create 100 omni lights in a 10x10 grid
  for x := -5 to 4 do
  begin
    for z := -5 to 4 do
    begin
      index := (z + 5) * 10 + (x + 5);
      Lights[index] := R3D_CreateLight(R3D_LIGHT_OMNI);

      LightPos := Vector3Create(x * 10, 10, z * 10);
      R3D_SetLightPosition(Lights[index], LightPos);

      LightColor := ColorFromHSV(index / LIGHT_COUNT * 360, 1.0, 1.0);
      R3D_SetLightColor(Lights[index], LightColor);

      R3D_SetLightRange(Lights[index], 20.0);
      R3D_SetLightActive(Lights[index], True);
    end;
  end;

  Result := '[r3d] - Many lights example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_ORBITAL);
end;

procedure Draw;
var
  i: Integer;
begin
  R3D_Begin(Camera);
    // Draw plane slightly below origin
    R3D_DrawMesh(@Plane, @Material, MatrixTranslate(0, -0.5, 0));
    // Draw 10,000 instanced spheres
    R3D_DrawMeshInstanced(@Sphere, @Material, @Transforms[0], GRID_SIZE * GRID_SIZE);
  R3D_End();

  // Draw light shapes when SPACE is pressed
  if IsKeyDown(KEY_SPACE) then
  begin
    BeginMode3D(Camera);
    for i := 0 to LIGHT_COUNT - 1 do
    begin
      R3D_DrawLightShape(Lights[i]);
    end;
    EndMode3D();
  end;

  // Draw UI
  DrawFPS(10, 10);
  DrawText('Press SPACE to show the lights', 10, GetScreenHeight - 34, 24, BLACK);
end;

procedure Close;
begin
  R3D_UnloadMesh(@Plane);
  R3D_UnloadMesh(@Sphere);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Many Lights Example');
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

