program r3d_directional;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, raymath, r3d;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    plane: TModel;
    sphere: TMesh;
    material: TMaterial;
    camera: TCamera3D;
    transforms: PMatrix;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - directional example';

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

procedure TRayApplication.Init;
var x, z, index: integer;
   light: TR3D_Light;

begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  plane := LoadModelFromMesh(GenMeshPlane(1000, 1000, 1, 1));
  plane.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1;
  plane.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 1;
  plane.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0;

  sphere := GenMeshSphere(0.35, 16, 16);

  material := LoadMaterialDefault();
  material.maps[MATERIAL_MAP_OCCLUSION].value := 1;
  material.maps[MATERIAL_MAP_ROUGHNESS].value := 0.25;
  material.maps[MATERIAL_MAP_METALNESS].value := 0.75;

  camera.Create(Vector3Create(0, 2, 2), Vector3Create(0, 0, 0), Vector3Create(0, 1, 0), 60, 0);
  GetMem(transforms, 100 * 100 * SizeOf(TMatrix));

  for x := -50 to 49 do
  begin
    for z := -50 to 49 do
     begin
       index := (z + 50) * 100 + (x + 50);
       transforms[index] := MatrixTranslate(x * 2, 0, z * 2);
     end;
    end;

   light := R3D_CreateLight(R3D_LIGHT_DIR);

   R3D_SetLightDirection(light, Vector3Create( 0, -1, -1 ));
   R3D_SetShadowUpdateMode(light, R3D_SHADOW_UPDATE_MANUAL);
   R3D_SetShadowBias(light, 0.005);
   R3D_EnableShadow(light, 4096);
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
      R3D_DrawModel(plane, Vector3Create( 0, -0.5, 0 ), 1.0);
      R3D_DrawMeshInstanced(sphere, material, transforms, 100 * 100);
  R3D_End();

  DrawFPS(10, 10);
end;

procedure TRayApplication.Close;
begin

  UnloadModel(plane);
  UnloadMesh(sphere);

  UnloadMaterial(material);
  FreeMem(transforms);
  R3D_Close();

end;

destructor TRayApplication.Destroy;
begin
  Close;
  CloseWindow(); // Close window and OpenGL context

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

