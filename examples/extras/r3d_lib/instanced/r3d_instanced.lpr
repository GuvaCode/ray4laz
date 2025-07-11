program r3d_instanced;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

const
  INSTANCE_COUNT = 1000;

var
  Camera: TCamera3D;
  Mesh: TR3D_Mesh;
  Material: TR3D_Material;
  Transforms: array[0..INSTANCE_COUNT-1] of TMatrix;
  Colors: array[0..INSTANCE_COUNT-1] of TColor;
  Light: TR3D_Light;

function Init: PChar;
var
  i: Integer;
  Translate, Rotate, Scale, FinalTransform: TMatrix;
  RotAngles: TVector3;
  LightDir: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Create cube mesh and default material
  Mesh := R3D_GenMeshCube(1, 1, 1, True);
  Material := R3D_GetDefaultMaterial();

  // Generate random transforms and colors for instances
  for i := 0 to INSTANCE_COUNT - 1 do
  begin
    // Random position
    Translate := MatrixTranslate(
      GetRandomValue(-50000, 50000) / 1000,
      GetRandomValue(-50000, 50000) / 1000,
      GetRandomValue(-50000, 50000) / 1000
    );

    // Random rotation
    RotAngles := Vector3Create(
      GetRandomValue(-314000, 314000) / 100000,
      GetRandomValue(-314000, 314000) / 100000,
      GetRandomValue(-314000, 314000) / 100000
    );
    Rotate := MatrixRotateXYZ(RotAngles);

    // Random scale
    Scale := MatrixScale(
      GetRandomValue(100, 2000) / 1000,
      GetRandomValue(100, 2000) / 1000,
      GetRandomValue(100, 2000) / 1000
    );

    // Combine transformations
    FinalTransform := MatrixMultiply(MatrixMultiply(Scale, Rotate), Translate);
    Transforms[i] := FinalTransform;

    // Random color
    Colors[i] := ColorFromHSV(GetRandomValue(0, 360000) / 1000, 1.0, 1.0);
  end;

  // Setup camera
  Camera.position := Vector3Create(0, 2, 2);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Create directional light
  Light := R3D_CreateLight(R3D_LIGHT_DIR);
  LightDir := Vector3Create(0, -1, 0);
  R3D_SetLightDirection(Light, LightDir);
  R3D_SetLightActive(Light, True);

  DisableCursor();

  Result := '[r3d] - Instanced rendering example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);
end;

procedure Draw;
begin
  R3D_Begin(Camera);
    R3D_DrawMeshInstancedEx(@Mesh, @Material, @Transforms[0], @Colors[0], INSTANCE_COUNT);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure Close;
begin
  R3D_UnloadMaterial(@Material);
  R3D_UnloadMesh(@Mesh);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Instanced Rendering Example');
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
