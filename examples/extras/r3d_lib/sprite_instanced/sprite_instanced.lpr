program sprite_instanced;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

const
  INSTANCE_COUNT = 512;

var
  Camera: TCamera3D;
  Plane: TR3D_Mesh;
  Material: TR3D_Material;
  Texture: TTexture2D;
  Sprite: TR3D_Sprite;
  Transforms: array[0..INSTANCE_COUNT-1] of TMatrix;

function Init: PChar;
var
  i: Integer;
  Light: TR3D_Light;
  LightPos, LightTarget: TVector3;
  ScaleFactor: Single;
  ScaleMat, TranslateMat: TMatrix;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);
  DisableCursor();

  // Set background color
  R3D_SetBackgroundColor(SKYBLUE);

  // Create plane mesh with green material
  Plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  Material := R3D_GetDefaultMaterial();
  Material.albedo.color := GREEN;

  // Load sprite texture (single frame)
  Texture := LoadTexture('resources/tree.png');
  Sprite := R3D_LoadSprite(Texture, 1, 1);

  // Generate random transforms for instances
  for i := 0 to INSTANCE_COUNT - 1 do
  begin
    ScaleFactor := GetRandomValue(50, 100) / 10.0;
    ScaleMat := MatrixScale(ScaleFactor, ScaleFactor, 1.0);
    TranslateMat := MatrixTranslate(
      GetRandomValue(-500, 500),
      ScaleFactor,
      GetRandomValue(-500, 500)
    );
    Transforms[i] := MatrixMultiply(ScaleMat, TranslateMat);
  end;

  // Setup camera looking slightly down
  Camera.position := Vector3Create(0, 5, 0);
  Camera.target := Vector3Create(0, 5, -1);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Create spotlight
  Light := R3D_CreateLight(R3D_LIGHT_SPOT);
  LightPos := Vector3Create(0, 10, 10);
  LightTarget := Vector3Create(0, 0, 0);
  R3D_LightLookAt(Light, LightPos, LightTarget);
  R3D_SetLightActive(Light, True);

  Result := '[r3d] - Instanced sprites example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_FREE);
end;

procedure Draw;
begin
  R3D_Begin(Camera);
    // Draw ground plane
    R3D_DrawMesh(@Plane, @Material, MatrixTranslate(0, 0, 0));

    // Draw all sprite instances
    R3D_DrawSpriteInstanced(@Sprite, @Transforms[0], INSTANCE_COUNT);
  R3D_End();
end;

procedure Close;
begin
  R3D_UnloadSprite(@Sprite);
  R3D_UnloadMesh(@Plane);
  UnloadTexture(Texture);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Instanced Sprites Example');
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
