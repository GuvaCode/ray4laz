program r3d_pbr;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d, raymath;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    model: TModel;
    skybox: TR3D_Skybox;
    camera: TCamera3D;
    modelScale: Single;
    procedure Init;
    procedure Update({%H-}delta: Single);
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - PBR example';

{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags
  Init;
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
end;

procedure TRayApplication.DoRun;
begin

  while (not WindowShouldClose) do // Detect window close button or ESC key
  begin
    // Update your variables here
    Update(getFrameTime);
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
    light: TR3D_Light;
begin
  modelScale := 1.0;

  R3D_Init(GetScreenWidth(), GetScreenHeight(), integer(R3D_FLAG_FXAA));
  SetTargetFPS(60);

  R3D_SetSSAO(true);
  R3D_SetSSAORadius(4.0);

  R3D_SetTonemapMode(R3D_TONEMAP_ACES);
  R3D_SetTonemapExposure(0.75);
  R3D_SetTonemapWhite(1.25);

  model := LoadModel('resources/pbr/musket.glb');

  model.transform := MatrixMultiply(model.transform, MatrixRotateY(PI / 2));

  for i := 0 to model.materialCount -1 do
  begin
    model.materials[i].maps[MATERIAL_MAP_ALBEDO].color := WHITE;
    model.materials[i].maps[MATERIAL_MAP_ROUGHNESS].value := 1.0;
    model.materials[i].maps[MATERIAL_MAP_METALNESS].value := 1.0;
    SetTextureFilter(model.materials[i].maps[MATERIAL_MAP_ALBEDO].texture, TEXTURE_FILTER_BILINEAR);
    SetTextureFilter(model.materials[i].maps[MATERIAL_MAP_ROUGHNESS].texture, TEXTURE_FILTER_BILINEAR);
    SetTextureFilter(model.materials[i].maps[MATERIAL_MAP_METALNESS].texture, TEXTURE_FILTER_BILINEAR);
    SetTextureFilter(model.materials[i].maps[MATERIAL_MAP_NORMAL].texture, TEXTURE_FILTER_BILINEAR);
  end;


  skybox := R3D_LoadSkybox('resources/sky/skybox2.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(skybox);

  camera.Create(Vector3Create( 0, 0, 50 ), Vector3Create(0, 0, 0), Vector3Create(0, 1, 0), 60 , CAMERA_PERSPECTIVE);

  light := R3D_CreateLight(R3D_LIGHT_DIR);
  R3D_SetLightDirection(light, Vector3Create( 0, -1, -1 ));
  R3D_SetLightActive(light, true);


end;

procedure TRayApplication.Update(delta: Single);
var pitch, yaw: single;
begin
  modelScale := Clamp(modelScale + GetMouseWheelMove() * 0.1, 0.25, 2.5);

  if (IsMouseButtonDown(MOUSE_BUTTON_LEFT)) then
  begin
    pitch := (GetMouseDelta().y * 0.005) / modelScale;
    yaw := (GetMouseDelta().x * 0.005) / modelScale;

    model.transform := MatrixMultiply(
    model.transform, MatrixRotateXYZ(Vector3Create( pitch, yaw, 0.0 )));

  end;
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);
  	R3D_DrawModel(model, Vector3Zero, modelScale);
  R3D_End();

  DrawFPS(10, 10);

  DrawText('Model made by TommyLingL', 10, GetScreenHeight() - 30, 20, LIME);
end;

procedure TRayApplication.Close;
begin
  UnloadModel(model);
  R3D_UnloadSkybox(skybox);
  R3D_Close();
end;

destructor TRayApplication.Destroy;
begin
  Close;
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

