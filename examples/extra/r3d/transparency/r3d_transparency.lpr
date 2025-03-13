program r3d_transparency;

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
    cube, plane, sphere: TModel;
    camera: TCamera;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  end;

  const AppTitle = '[r3d] - transparency example';

{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags
  Init;

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

destructor TRayApplication.Destroy;
begin
  Close;
  CloseWindow(); // Close window and OpenGL context

  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
  TraceLog(LOG_INFO, 'your first window is close and destroy');

  inherited Destroy;
end;

procedure TRayApplication.Init;
var light: TR3D_Light;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  // NOTE: This mode is already the default one, but the call is made here for example purposes.
  //       In this mode, R3D will attempt to detect when to perform deferred or forward rendering
  //       automatically based on the alpha of the albedo color or the format of the albedo texture.

  R3D_ApplyRenderMode(R3D_RENDER_AUTO_DETECT);

  cube := LoadModelFromMesh(GenMeshCube(1, 1, 1));
  cube.materials[0].maps[MATERIAL_MAP_ALBEDO].color := ColorCreate( 100, 100, 255, 100 );
  cube.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
  cube.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 0.2;
  cube.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0.2;

  plane := LoadModelFromMesh(GenMeshPlane(1000, 1000, 1, 1));
  plane.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
  plane.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 1.0;
  plane.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0.0;

  sphere := LoadModelFromMesh(GenMeshSphere(0.5, 64, 64));
  sphere.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
  sphere.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 0.25;
  sphere.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0.75;

  camera.Create(Vector3Create(0, 2, 2), Vector3Create(0, 0, 0), Vector3Create(0, 1, 0), 60, 0);

  light := R3D_CreateLight(R3D_LIGHT_SPOT);

  R3D_SetLightPosition(light, Vector3Create ( 0, 10, 5 ));
  R3D_SetLightTarget(light, Vector3Create ( 0,0,0 ));
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
    R3D_ApplyShadowCastMode(R3D_SHADOW_CAST_FRONT_FACES);

    R3D_DrawModel(plane, Vector3Create ( 0, -0.5, 0 ), 1.0);
    R3D_DrawModel(sphere, Vector3Create ( 0,0,0) , 1.0);

    R3D_ApplyShadowCastMode(R3D_SHADOW_CAST_DISABLED);

    R3D_DrawModel(cube, Vector3Create ( 0,0,0 ), 1.0);
  R3D_End();
  DrawFps(10,10);
end;

procedure TRayApplication.Close;
begin
  UnloadModel(plane);
  UnloadModel(sphere);
  R3D_Close();
end;

var
  Application: TRayApplication;
begin
  Application:=TRayApplication.Create(nil);
  Application.Title:=AppTitle;
  Application.Run;
  Application.Free;
end.

