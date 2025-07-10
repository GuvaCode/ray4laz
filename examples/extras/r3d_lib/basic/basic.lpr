program basic;

{$mode objfpc}{$H+}

uses
 cthreads,
 Classes, SysUtils, CustApp, raylib, r3d, raymath;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    plane, sphere: TR3D_Mesh;
    material: TR3D_Material;
    camera: TCamera3D;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - basic example';

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
var light: TR3D_Light;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  plane := R3D_GenMeshPlane(1000, 1000, 1, 1, true);
  sphere := R3D_GenMeshSphere(0.5, 64, 64, true);
  material := R3D_GetDefaultMaterial();


  Camera.Create(Vector3Create(0,2,2), Vector3Create(0,0,0), Vector3Create(0,1,0),60,0);

  light := R3D_CreateLight(R3D_LIGHT_SPOT);

  R3D_LightLookAt(light, Vector3Create( 0, 10, 5 ), Vector3Create(0,0,0));

  R3D_SetLightActive(light, true);
  R3D_EnableShadow(light, 4096);
end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_ORBITAL);
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);
  R3D_DrawMesh(@plane, @material, MatrixTranslate(0, -0.5, 0));
  R3D_DrawMesh(@sphere, @material, MatrixIdentity());
  R3D_End();
end;

procedure TRayApplication.Close;
begin
  R3D_UnloadMesh(@plane);
  R3D_UnloadMesh(@sphere);
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

