program r3d_instanced;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d, raymath;

const INSTANCE_COUNT = 1000;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    camera: TCamera3D;
    mesh: TMesh;
    material: TMaterial;
    transforms: array[0..INSTANCE_COUNT - 1] of TMatrix;
    colors: array[0..INSTANCE_COUNT - 1] of TColor;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - instanced example';

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
var i: Integer;
    translate, rotate, scale: TMatrix;
    light: TR3D_Light;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  mesh := GenMeshCube(1, 1, 1);

  material := LoadMaterialDefault();
  R3D_SetMaterialOcclusion(@material, nil, 1.0);
  R3D_SetMaterialRoughness(@material, nil, 0.5);
  R3D_SetMaterialMetalness(@material, nil, 0.5);
  //GenMeshTangents(&mesh);

  for i:=0 to INSTANCE_COUNT -1 do
  begin
   translate := MatrixTranslate(
          GetRandomValue(-50000, 50000) / 1000,
          GetRandomValue(-50000, 50000) / 1000,
          GetRandomValue(-50000, 50000) / 1000);

   rotate := MatrixRotateXYZ(Vector3Create(
          GetRandomValue(-314000, 314000) / 100000,
          GetRandomValue(-314000, 314000) / 100000,
          GetRandomValue(-314000, 314000) / 100000));

   scale := MatrixScale(
          GetRandomValue(100, 2000) / 1000,
          GetRandomValue(100, 2000) / 1000,
          GetRandomValue(100, 2000) / 1000);

      transforms[i] := MatrixMultiply(MatrixMultiply(scale, rotate), translate);
      colors[i] := ColorFromHSV(GetRandomValue(0, 360000) / 1000, 1.0, 1.0);
  end;

  camera.Create(Vector3Create(2,2,2), Vector3Create(0,0,0), Vector3Create(0,1,0), 60);

   light := R3D_CreateLight(R3D_LIGHT_DIR);

   R3D_SetLightDirection(light, Vector3Create( 0, -1, 0 ));
   R3D_SetLightActive(light, true);
   DisableCursor();

end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_FREE);
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);
      R3D_DrawMeshInstancedEx(mesh, material, transforms, colors, INSTANCE_COUNT);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure TRayApplication.Close;
begin
  UnloadMaterial(material);
  UnloadMesh(mesh);
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

