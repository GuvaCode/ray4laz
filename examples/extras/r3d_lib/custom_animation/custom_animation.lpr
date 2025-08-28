program custom_animation;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, raylib, rlgl, raymath, r3d;

type
  //PMatrix = ^TMatrix;
  TMatrixArray = array of TMatrix;

const
  RESOURCES_PATH = 'resources/';

var
  plane: TR3D_Mesh;
  dancer: TR3D_Model;
  material: TR3D_Material;
  CustomMatrices: TMatrixArray;
  LocalMatrices: TMatrixArray;
  camera: TCamera3D;
  animCount: Integer;
  anims: PR3D_ModelAnimation;
  lights: array[0..1] of TR3D_Light;

function GetWorldMatrix(model: PR3D_Model; boneID: Integer): TMatrix;
var
  animFrame: Integer;
  localPose: PTransform;
  scaleMatrix, rotationMatrix, translationMatrix, pose: TMatrix;
begin
  animFrame := model^.animFrame mod model^.anim^.frameCount;
  localPose := @model^.anim^.frameLocalPoses[animFrame][boneID];

  scaleMatrix := MatrixScale(localPose^.scale.x, localPose^.scale.y, localPose^.scale.z);
  rotationMatrix := QuaternionToMatrix(localPose^.rotation);
  translationMatrix := MatrixTranslate(localPose^.translation.x, localPose^.translation.y, localPose^.translation.z);

  pose := MatrixMultiply(MatrixMultiply(scaleMatrix, rotationMatrix), translationMatrix);

  if model^.bones[boneID].parent <> -1 then
    pose := MatrixMultiply(pose, GetWorldMatrix(model, model^.bones[boneID].parent));

  Result := pose;
end;

procedure GeneratePoseFromLocal(outMatrices: PMatrix; model: PR3D_Model);
var
  boneID: Integer;
  scaleMatrix: TMatrix;
begin
  scaleMatrix := MatrixScale(0.01, 0.01, 0.01);

  for boneID := 0 to model^.boneCount - 1 do
  begin
    outMatrices[boneID] := MatrixMultiply(
      MatrixMultiply(model^.boneOffsets[boneID], GetWorldMatrix(model, boneID)),
      scaleMatrix
    );
  end;
end;

procedure GeneratePoseFromWorld(outMatrices: PMatrix; model: PR3D_Model);
var
  animFrame, boneID: Integer;
begin
  animFrame := model^.animFrame mod model^.anim^.frameCount;

  for boneID := 0 to model^.boneCount - 1 do
  begin
    outMatrices[boneID] := MatrixMultiply(
      model^.boneOffsets[boneID],
      model^.anim^.frameGlobalPoses[animFrame][boneID]
    );
  end;
end;

procedure Init;
var
  checked: TImage;
  q: Integer;
begin

  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_FXAA or R3D_FLAG_NO_FRUSTUM_CULLING);
  SetTargetFPS(60);

  R3D_SetSSAO(True);
  R3D_SetBloomIntensity(0.03);
  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  R3D_SetTonemapMode(R3D_TONEMAP_ACES);

  R3D_SetBackgroundColor(BLACK);
  R3D_SetAmbientColor(ColorCreate(7, 7, 7, 255));

  plane := R3D_GenMeshPlane(32, 32, 1, 1, True);
  dancer := R3D_LoadModel(PChar(RESOURCES_PATH + 'dancer.glb'));
  anims := R3D_LoadModelAnimations(PChar(RESOURCES_PATH + 'dancer.glb'), @animCount, 60);

  SetLength(CustomMatrices, dancer.boneCount);
  SetLength(LocalMatrices, dancer.boneCount);

  for q := 0 to dancer.boneCount - 1 do
  begin
    CustomMatrices[q] := MatrixIdentity();
    LocalMatrices[q] := MatrixIdentity();
  end;

  material := R3D_GetDefaultMaterial();

  checked := GenImageChecked(2, 2, 1, 1, ColorCreate(20, 20, 20, 255), WHITE);
  material.albedo.texture := LoadTextureFromImage(checked);
  UnloadImage(checked);

  SetTextureWrap(material.albedo.texture, TEXTURE_WRAP_REPEAT);

  material.orm.roughness := 0.5;
  material.orm.metalness := 0.5;
  material.uvScale.x := 64.0;
  material.uvScale.y := 64.0;

  lights[0] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(lights[0], Vector3Create(-10.0, 25.0, 0.0));
  R3D_EnableShadow(lights[0], 4096);
  R3D_SetLightActive(lights[0], True);

  lights[1] := R3D_CreateLight(R3D_LIGHT_OMNI);
  R3D_SetLightPosition(lights[1], Vector3Create(10.0, 25.0, 0.0));
  R3D_EnableShadow(lights[1], 4096);
  R3D_SetLightActive(lights[1], True);

  camera := Camera3DCreate(
    Vector3Create(0, 2.0, 3.5),
    Vector3Create(0, 1.0, 1.5),
    Vector3Create(0, 1, 0),
    60,
    CAMERA_PERSPECTIVE
  );

 // DisableCursor();
end;

procedure Update(delta: Single);
var
  time: Single;
begin
  UpdateCamera(@camera, CAMERA_FREE);
  dancer.anim := @anims[0];
  Inc(dancer.animFrame);

  GeneratePoseFromLocal(@LocalMatrices[0], @dancer);
  GeneratePoseFromWorld(@CustomMatrices[0], @dancer);

  time := GetTime();
  R3D_SetLightColor(lights[0], ColorFromHSV(90.0 * time + 90.0, 1.0, 1.0));
  R3D_SetLightColor(lights[1], ColorFromHSV(90.0 * time - 90.0, 1.0, 1.0));
end;

procedure Draw;
begin
  R3D_Begin(camera);
    R3D_DrawMesh(@plane, @material, MatrixIdentity());

    dancer.animationMode := R3D_ANIM_INTERNAL;
    R3D_DrawModel(@dancer, Vector3Create(0, 0, 1.5), 1.0);

    dancer.animationMode := R3D_ANIM_CUSTOM;
    dancer.boneOverride := @CustomMatrices[0];
    R3D_DrawModel(@dancer, Vector3Create(2, 0, 1.5), 1.0);

    dancer.boneOverride := @LocalMatrices[0];
    R3D_DrawModel(@dancer, Vector3Create(-2, 0, 1.5), 1.0);
  R3D_End();

  //DrawCredits('Model made by zhuoyi0904');
end;

procedure Close;
begin
  SetLength(CustomMatrices, 0);
  SetLength(LocalMatrices, 0);
  R3D_UnloadMesh(@plane);
  R3D_UnloadModel(@dancer, True);
  R3D_UnloadMaterial(@material);
  R3D_Close();
end;

begin
  InitWindow(800, 600, '[r3d] - Animation example');
  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());

    BeginDrawing();
      ClearBackground(BLACK);
      Draw();
      DrawFPS(10,10);
    EndDrawing();
  end;

  Close();
  CloseWindow();
end.
