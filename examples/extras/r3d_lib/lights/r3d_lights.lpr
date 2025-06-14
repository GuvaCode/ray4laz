program r3d_lights;

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
    plane: TModel;
    sphere: TMesh;
    material: TMaterial;
    camera: TCamera3D;
    transforms: PMatrix;
    lights: array[0..99] of TR3D_Light;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - lights example';

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
      DrawText('Press Space to view light shape' ,10, 30, 20, BLACK);
    EndDrawing();
  end;

  // Stop program loop
  Terminate;
end;

procedure TRayApplication.Init;
var x, z, index: integer;
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
        transforms[index] := MatrixTranslate(x , 0, z );
      end;
     end;

   for x := -5 to 4 do
   begin
     for z := -5 to 4 do
     begin
       index := (z + 5) * 10 + (x + 5);
       lights[index] := R3D_CreateLight(R3D_LIGHT_OMNI);
       R3D_SetLightPosition(lights[index], Vector3Create ( x * 10, 10, z * 10 ));
       R3D_SetLightColor(lights[index], ColorFromHSV(index / 100 * 360, 1.0, 1.0));
       R3D_SetLightRange(lights[index], 20.0);
       R3D_SetLightActive(lights[index], true);
     end;
   end;
  DisableCursor();

end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_ORBITAL);
end;

procedure TRayApplication.Draw;
var i: integer;
begin
  R3D_Begin(camera);
      R3D_DrawModel(plane, Vector3Create( 0, -0.5, 0 ), 1.0);
      R3D_DrawMeshInstanced(sphere, material, transforms, 100 * 100);
  R3D_End();

  DrawFPS(10, 10);

  if (IsKeyDown(KEY_SPACE)) then
  begin
   BeginMode3D(camera);
     for i :=0 to 99 do //(int i = 0; i < 100; i++) {
       R3D_DrawLightShape(lights[i]);
   EndMode3D();
  end;

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

