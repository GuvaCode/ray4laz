program r3d_fog;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    sponza: TModel;
    camera: TCamera3D;
    procedure Init;
    procedure Update;
    procedure Draw;
    Procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - fog example';

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
    Update;
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
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  sponza := LoadModel('resources/sponza.glb');

  for i := 0 to sponza.materialCount -1 do
  begin
    sponza.materials[i].maps[MATERIAL_MAP_ALBEDO].color := WHITE;
    sponza.materials[i].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
    sponza.materials[i].maps[MATERIAL_MAP_ROUGHNESS].value := 1.0;
    sponza.materials[i].maps[MATERIAL_MAP_METALNESS].value := 1.0;
  end;

  R3D_SetFogMode(R3D_FOG_EXP);

  light := R3D_CreateLight(R3D_LIGHT_DIR);
  R3D_SetLightDirection(light, Vector3Create( 0, -1, 0 ));
  R3D_SetLightActive(light, true);

  camera.Create(Vector3Create(0,0,0), Vector3Create(0,0,-1), Vector3Create(0,1,0), 60);

  DisableCursor();

end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_FREE);
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);
      R3D_DrawModel(sponza, Vector3Create(0,0,0), 1.0);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure TRayApplication.Close;
begin
  UnloadModel(sponza);
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

