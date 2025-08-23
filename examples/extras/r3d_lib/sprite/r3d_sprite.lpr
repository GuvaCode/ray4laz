program r3d_sprite;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath, math;

var
  Camera: TCamera3D;
  Plane: TR3D_Mesh;
  Material: TR3D_Material;
  Texture: TTexture2D;
  Sprite: TR3D_Sprite;
  BirdPos: TVector3;
  BirdDirX: Single = 1.0;
  BirdScale: TVector2;

function Init: PChar;
var
  Light: TR3D_Light;
  LightPos, LightTarget: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Create plane mesh and material
  Plane := R3D_GenMeshPlane(1000, 1000, 1, 1, True);
  Material := R3D_GetDefaultMaterial();

  // Load sprite texture and create sprite
  Texture := LoadTexture('resources/spritesheet.png');
  Sprite := R3D_LoadSprite(Texture, 4, 1);

  SetTextureFilter(texture, TEXTURE_FILTER_BILINEAR);

  // Setup camera
  Camera.position := Vector3Create(0, 2, 5);
  Camera.target := Vector3Create(0, 0, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  // Create spotlight
  Light := R3D_CreateLight(R3D_LIGHT_SPOT);
  LightPos := Vector3Create(0, 10, 10);
  LightTarget := Vector3Create(0, 0, 0);
  R3D_LightLookAt(Light, LightPos, LightTarget);
  R3D_SetLightActive(Light, True);

  // Initialize bird position
  BirdPos := Vector3Create(0, 0.5, 0);

  Result := '[r3d] - Sprite example';
end;

procedure Update(delta: Single);
var
  BirdPosPrev: TVector3;
begin
  // Update sprite animation
  R3D_UpdateSprite(@Sprite, 10 * delta);

  // Update bird position
  BirdPosPrev := BirdPos;
  BirdPos.x := 2.0 * Sin(GetTime());
  BirdPos.y := 1.0 + Cos(GetTime() * 4.0) * 0.5;

  // Update bird direction
  if BirdPos.x - BirdPosPrev.x >= 0 then
    BirdDirX := 1.0
  else
    BirdDirX := -1.0;
end;

procedure Draw;


begin
  R3D_Begin(Camera);
    // Draw ground plane
    R3D_DrawMesh(@Plane, @Material, MatrixTranslate(0, -0.5, 0));

    // Draw sprite with current bird direction
    BirdScale := Vector2Create(BirdDirX, 1.0);
    R3D_DrawSpriteEx(@Sprite, BirdPos, BirdScale, 0.0);
  R3D_End();
end;

procedure Close;
begin
  R3D_UnloadSprite(@Sprite);
  R3D_UnloadMesh(@Plane);
  UnloadTexture(Texture);
  R3D_Close();
end;

function GetSpriteBoundingBox(sprite: PR3D_Sprite; position: TVector3; size: TVector2): TBoundingBox;
var
  halfSize: TVector3;
begin
  halfSize := Vector3Create(size.x / 2, size.y / 2, 0.1); // небольшая глубина для 2D спрайта

  Result.min := Vector3Create(
    position.x - halfSize.x,
    position.y - halfSize.y,
    position.z - halfSize.z
  );

  Result.max := Vector3Create(
    position.x + halfSize.x,
    position.y + halfSize.y,
    position.z + halfSize.z
  );
end;



begin
  InitWindow(800, 600, 'Sprite Example');
  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    bb := GetSpriteBoundingBox(@Sprite, BirdPos, BirdScale);
    BeginDrawing();
      ClearBackground(BLACK);
      Draw();
      BeginMode3d(Camera);
        DrawBoundingBox(bb,red);
      EndMode3d();

    EndDrawing();
  end;

  Close();
  CloseWindow();
end.
