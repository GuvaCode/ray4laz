program r3d_sponza;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d, rlgl, math;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    sponza: TModel;
    skybox: TR3D_Skybox;
    camera: TCamera3D;
    lights: array[0..1] of TR3D_Light;
    sky: boolean;
  public
    procedure Init;
    procedure Update(delta: Single);
    procedure Draw;
    procedure Close;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - sponza example';

{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags
  Init;
  //SetTargetFPS(60); // Set our game to run at 60 frames-per-second
end;

procedure TRayApplication.DoRun;
begin

  while (not WindowShouldClose) do // Detect window close button or ESC key
  begin
    // Update your variables here
    Update(GetFrameTime);
    // Draw
    BeginDrawing();
      Draw;
    EndDrawing();
  end;

  // Stop program loop
  Terminate;
end;

procedure TRayApplication.Init;
var i: integer;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_NONE);
  SetTargetFPS(60);
  sky := false;
  R3D_SetSSAO(true);
  R3D_SetSSAORadius(4.0);
  R3D_SetBloomMode(R3D_BLOOM_SOFT_LIGHT);

  sponza := LoadModel('resources/sponza.glb');

  for i := 0 to sponza.materialCount -1 do
  begin
    sponza.materials[i].maps[MATERIAL_MAP_ALBEDO].color := WHITE;
    sponza.materials[i].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
    sponza.materials[i].maps[MATERIAL_MAP_ROUGHNESS].value := 1.0;
    sponza.materials[i].maps[MATERIAL_MAP_METALNESS].value := 1.0;

    GenTextureMipmaps(@sponza.materials[i].maps[MATERIAL_MAP_ALBEDO].texture);
    SetTextureFilter(sponza.materials[i].maps[MATERIAL_MAP_ALBEDO].texture, TEXTURE_FILTER_TRILINEAR);

    GenTextureMipmaps(@sponza.materials[i].maps[MATERIAL_MAP_NORMAL].texture);
    SetTextureFilter(sponza.materials[i].maps[MATERIAL_MAP_NORMAL].texture, TEXTURE_FILTER_TRILINEAR);

    // REVIEW: Issue with the model textures
    sponza.materials[i].maps[MATERIAL_MAP_ROUGHNESS].texture.id := rlGetTextureIdDefault();
  end;

  // NOTE: Toggle sky with 'T' key
  skybox := R3D_LoadSkybox('resources/sky/skybox3.png', CUBEMAP_LAYOUT_AUTO_DETECT);

  for i := 0 to 1 do
  begin
    lights[i] := R3D_CreateLight(R3D_LIGHT_SPOT);
    if i = 1 then
      R3D_SetLightPosition(lights[i],  Vector3Create(-10, 20, 0))
    else
      R3D_SetLightPosition(lights[i],  Vector3Create(10, 20, 0));

    R3D_SetLightTarget(lights[i], Vector3Create ( 0, 0, 0 ));
    R3D_SetLightActive(lights[i], true);

    R3D_SetShadowUpdateMode(lights[i], R3D_SHADOW_UPDATE_MANUAL);
    R3D_EnableShadow(lights[i], 4096);
  end;

  camera.Create(Vector3Create(0, 0, 0), Vector3Create(0, 0, -1), Vector3Create(0, 1, 0), 60, 0);

  DisableCursor();
end;

procedure TRayApplication.Update(delta: Single);
var fxaa: boolean; tonemap: R3D_Tonemap;
begin
  UpdateCamera(@camera, CAMERA_FREE);

  if (IsKeyPressed(KEY_T)) then
  begin
      if (sky) then R3D_DisableSkybox()
      else R3D_EnableSkybox(skybox);
      sky := not sky;
  end;

  if (IsKeyPressed(KEY_F)) then
  begin
    fxaa := R3D_HasState(R3D_FLAG_FXAA);
    if (fxaa) then R3D_ClearState(R3D_FLAG_FXAA)
    else R3D_SetState(R3D_FLAG_FXAA);
  end;

  if (IsKeyPressed(KEY_O)) then R3D_SetSSAO(not R3D_GetSSAO());

  if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT)) then
  begin
    tonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((tonemap + 5 - 1) mod 5);
  end;

  if (IsMouseButtonPressed(MOUSE_BUTTON_RIGHT)) then
  begin
    tonemap := R3D_GetTonemapMode();
    R3D_SetTonemapMode((tonemap + 1) mod 5);
  end;

end;

procedure TRayApplication.Draw;
var tonemap: R3D_Tonemap; txt: PChar;
begin
  R3D_Begin(camera);
      R3D_DrawModel(sponza, Vector3Create(0, 0, 0), 1.0);
  R3D_End();

  BeginMode3D(camera);
      DrawSphere(R3D_GetLightPosition(lights[0]), 0.5, WHITE);
      DrawSphere(R3D_GetLightPosition(lights[1]), 0.5, WHITE);
  EndMode3D();

  tonemap := R3D_GetTonemapMode();

  case tonemap of

    R3D_TONEMAP_LINEAR: begin
      txt := '< TONEMAP LINEAR >';
      DrawText(txt, GetScreenWidth() - MeasureText(txt, 20) - 10, 10, 20, LIME);
    end;

    R3D_TONEMAP_REINHARD: begin
      txt := '< TONEMAP REINHARD >';
      DrawText(txt, GetScreenWidth() - MeasureText(txt, 20) - 10, 10, 20, LIME);
    end;

    R3D_TONEMAP_FILMIC: begin
      txt := '< TONEMAP FILMIC >';
      DrawText(txt, GetScreenWidth() - MeasureText(txt, 20) - 10, 10, 20, LIME);
    end;

    R3D_TONEMAP_ACES: begin
      txt := '< TONEMAP ACES >';
      DrawText(txt, GetScreenWidth() - MeasureText(txt, 20) - 10, 10, 20, LIME);
    end;

    R3D_TONEMAP_AGX: begin
      txt := '< TONEMAP AGX >';
      DrawText(txt, GetScreenWidth() - MeasureText(txt, 20) - 10, 10, 20, LIME);
    end;

  end;

  DrawFPS(10, 10);
end;

procedure TRayApplication.Close;
begin
  UnloadModel(sponza);
  R3D_UnloadSkybox(skybox);
  R3D_Close();
end;

destructor TRayApplication.Destroy;
begin
  Close; // De-Initialization
  CloseWindow(); // Close window and OpenGL context

  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
  TraceLog(LOG_INFO, 'your first window is close and destroy');

  inherited Destroy;
end;

var
  Application: TRayApplication;
begin
  Application:=TRayApplication.Create(nil);
  Application.Title:=AppTitle;
  Application.Run;
  Application.Free;
end.

