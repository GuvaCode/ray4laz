program r3d_sponza;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath, rlgl, math;

var
  Sponza: TR3D_Model;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  Lights: array[0..1] of TR3D_Light;
  SkyEnabled: Boolean = False;

function Init: PChar;
var
  i: Integer;
  LightPos: TVector3;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Configure rendering effects
  R3D_SetSSAO(True);
  R3D_SetSSAORadius(4.0);
  R3D_SetBloomMode(R3D_BLOOM_MIX);
  R3D_SetAmbientColor(GRAY);

  // Load models
  Sponza := R3D_LoadModel('resources/sponza.glb');
  Skybox := R3D_LoadSkybox('resources/sky/skybox3.png', CUBEMAP_LAYOUT_AUTO_DETECT);

  // Set scene bounds based on model AABB
  R3D_SetSceneBounds(Sponza.aabb);

  // Create two omni lights
  for i := 0 to 1 do
  begin
    Lights[i] := R3D_CreateLight(R3D_LIGHT_OMNI);
    LightPos := Vector3Create(IfThen(i = 0, 10, -10), 20, 0);

    R3D_SetLightPosition(Lights[i], LightPos);
    R3D_SetLightActive(Lights[i], True);
    R3D_SetLightEnergy(Lights[i], 1.0);
    R3D_SetShadowUpdateMode(Lights[i], R3D_SHADOW_UPDATE_MANUAL);
    R3D_EnableShadow(Lights[i], 4096);
  end;

  // Setup camera
  Camera.position := Vector3Create(0, 0, 0);
  Camera.target := Vector3Create(0, 0, -1);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;

  DisableCursor();

  Result := '[r3d] - Sponza example';
end;

procedure Update(delta: Single);
var
  CurrentTonemap: R3D_Tonemap;
begin
  UpdateCamera(@Camera, CAMERA_FREE);

  // Toggle skybox with T key
  if IsKeyPressed(KEY_T) then
  begin
    SkyEnabled := not SkyEnabled;
    if SkyEnabled then
      R3D_EnableSkybox(Skybox)
    else
      R3D_DisableSkybox();
  end;

  // Toggle FXAA with F key
  if IsKeyPressed(KEY_F) then
  begin
    if R3D_HasState(R3D_FLAG_FXAA) then
      R3D_ClearState(R3D_FLAG_FXAA)
    else
      R3D_SetState(R3D_FLAG_FXAA);
  end;

  // Toggle SSAO with O key
  if IsKeyPressed(KEY_O) then
    R3D_SetSSAO(not R3D_GetSSAO());

  // Cycle tonemap modes with mouse buttons
  if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    CurrentTonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((CurrentTonemap + R3D_TONEMAP_COUNT - 1) mod R3D_TONEMAP_COUNT);
  end;

  if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
  begin
    CurrentTonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((CurrentTonemap + 1) mod R3D_TONEMAP_COUNT);
  end;
end;

procedure Draw;
var
  Tonemap: R3D_Tonemap;
  TonemapText: string;
  TextWidth: Integer;
begin
  R3D_Begin(Camera);
    R3D_DrawModel(@Sponza, Vector3Create(0, 0, 0), 1.0);
  R3D_End();

  // Draw light positions
  BeginMode3D(Camera);
    DrawSphere(R3D_GetLightPosition(Lights[0]), 0.5, WHITE);
    DrawSphere(R3D_GetLightPosition(Lights[1]), 0.5, WHITE);
  EndMode3D();

  // Display current tonemap mode
  Tonemap := R3D_GetTonemapMode();
  case Tonemap of
    R3D_TONEMAP_LINEAR: TonemapText := '< TONEMAP LINEAR >';
    R3D_TONEMAP_REINHARD: TonemapText := '< TONEMAP REINHARD >';
    R3D_TONEMAP_FILMIC: TonemapText := '< TONEMAP FILMIC >';
    R3D_TONEMAP_ACES: TonemapText := '< TONEMAP ACES >';
    R3D_TONEMAP_AGX: TonemapText := '< TONEMAP AGX >';
  end;

  TextWidth := MeasureText(PChar(TonemapText), 20);
  DrawText(PChar(TonemapText), GetScreenWidth - TextWidth - 10, 10, 20, LIME);

  DrawFPS(10, 10);
end;

procedure Close;
begin
  R3D_UnloadModel(@Sponza, True);
  R3D_UnloadSkybox(Skybox);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Sponza Example');
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
