program r3d_Sponza;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,{$ENDIF}
  Classes, SysUtils, raylib, rlgl, r3d, raymath;

var
  Sponza: TR3D_Model;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  Lights: array[0..1] of TR3D_Light;
  Sky: Boolean = False;

const
  RESOURCES_PATH = 'resources/';

function Init: PChar;
var
  i: Integer;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  // Configure default post process settings
  R3D_SetSSAO(True);
  R3D_SetSSAORadius(4.0);
  R3D_SetBloomMode(R3D_BLOOM_MIX);

  // Set default ambient color (when no skybox is activated)
  R3D_SetAmbientColor(GRAY);

  // Load Sponza scene
  Sponza := R3D_LoadModel(PChar(RESOURCES_PATH + 'sponza.glb'));

  // Load skybox (disabled by default)
  Skybox := R3D_LoadSkybox(PChar(RESOURCES_PATH + 'sky/skybox3.png'), CUBEMAP_LAYOUT_AUTO_DETECT);
  //R3D_EnableSkybox(Skybox);

  // Set scene bounds, useful if you use directional lights
  R3D_SetSceneBounds(Sponza.aabb);

  // Configure lights
  for i := 0 to 1 do
  begin
    Lights[i] := R3D_CreateLight(R3D_LIGHT_OMNI);

    if i = 0 then
      R3D_SetLightPosition(Lights[i], Vector3Create(10, 20, 0))
    else
      R3D_SetLightPosition(Lights[i], Vector3Create(-10, 20, 0));

    R3D_SetLightActive(Lights[i], True);
    R3D_SetLightEnergy(Lights[i], 1.0);
    R3D_SetShadowUpdateMode(Lights[i], R3D_SHADOW_UPDATE_MANUAL);
    R3D_EnableShadow(Lights[i], 4096);
  end;

  // Configure camera
  Camera.position := Vector3Create(0, 0, 0);
  Camera.target := Vector3Create(0, 0, -1);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60;
  Camera.projection := CAMERA_PERSPECTIVE;

  // Ready to go!
  DisableCursor();

  Result := '[r3d] - Sponza example';
end;

procedure Update(delta: Single);
var
  Tonemap: R3D_Tonemap;
begin
  // Update the camera via raylib's functions
  UpdateCamera(@Camera, CAMERA_FREE);

  // Skybox toggling
  if IsKeyPressed(KEY_ZERO) then
  begin
    if Sky then
      R3D_DisableSkybox()
    else
      R3D_EnableSkybox(Skybox);
    Sky := not Sky;
  end;

  // SSAO toggling
  if IsKeyPressed(KEY_ONE) then
    R3D_SetSSAO(not R3D_GetSSAO());

  // Fog toggling
  if IsKeyPressed(KEY_TWO) then
  begin
    if R3D_GetFogMode() = R3D_FOG_DISABLED then
      R3D_SetFogMode(R3D_FOG_EXP)
    else
      R3D_SetFogMode(R3D_FOG_DISABLED);
  end;

  // FXAA toggling
  if IsKeyPressed(KEY_THREE) then
  begin
    if R3D_HasState(R3D_FLAG_FXAA) then
      R3D_ClearState(R3D_FLAG_FXAA)
    else
      R3D_SetState(R3D_FLAG_FXAA);
  end;

  // Tonemapping setter
  if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
  begin
    Tonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((Tonemap + R3D_TONEMAP_COUNT - 1) mod R3D_TONEMAP_COUNT);
  end;

  if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) then
  begin
    Tonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((Tonemap + 1) mod R3D_TONEMAP_COUNT);
  end;
end;

procedure Draw;
var
  Tonemap: R3D_Tonemap;
  Text: String;
begin
  // Render R3D scene
  R3D_Begin(Camera);
    R3D_DrawModel(@Sponza, Vector3Create(0, 0, 0), 1.0);
  R3D_End();

  // 'Standard' raylib rendering to show where are the lights
  BeginMode3D(Camera);
    DrawSphere(R3D_GetLightPosition(Lights[0]), 0.5, WHITE);
    DrawSphere(R3D_GetLightPosition(Lights[1]), 0.5, WHITE);
  EndMode3D();

  // Indicates which tonemapping is used
  Tonemap := R3D_GetTonemapMode();

  case Tonemap of
    R3D_TONEMAP_LINEAR:
      Text := '< TONEMAP LINEAR >';
    R3D_TONEMAP_REINHARD:
      Text := '< TONEMAP REINHARD >';
    R3D_TONEMAP_FILMIC:
      Text := '< TONEMAP FILMIC >';
    R3D_TONEMAP_ACES:
      Text := '< TONEMAP ACES >';
    R3D_TONEMAP_AGX:
      Text := '< TONEMAP AGX >';
    else
      Text := '';
  end;

  if Text <> '' then
    DrawText(PChar(Text), GetScreenWidth() - MeasureText(PChar(Text), 20) - 10, 10, 20, LIME);

  // I think we understand what's going on here
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
