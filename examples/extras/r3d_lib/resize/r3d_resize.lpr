program r3d_resize;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d, rlGl, math;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    sphere: TModel;
    camera: TCamera;
    materials: array[0..4] of TMaterial;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  end;

  const AppTitle = '[r3d] - resize example';

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

destructor TRayApplication.Destroy;
begin
  close;
  CloseWindow(); // Close window and OpenGL context

  // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
  TraceLog(LOG_INFO, 'your first window is close and destroy');

  inherited Destroy;
end;

procedure TRayApplication.Init;
var i: integer;
    light: TR3D_Light;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_NONE);
  SetWindowState(FLAG_WINDOW_RESIZABLE);
  SetTargetFPS(60);

  sphere := LoadModelFromMesh(GenMeshSphere(0.5, 64, 64));
  UnloadMaterial(sphere.materials[0]);

  for i := 0 to 4 do
  begin
    materials[i] := LoadMaterialDefault();
    materials[i].maps[MATERIAL_MAP_ALBEDO].color := ColorFromHSV(i / 5 * 330, 1.0, 1.0);
    materials[i].maps[MATERIAL_MAP_OCCLUSION].value := 1;
    materials[i].maps[MATERIAL_MAP_ROUGHNESS].value := 1;
    materials[i].maps[MATERIAL_MAP_METALNESS].value := 0;
  end;

 camera.Create(Vector3Create(0, 2, 2), Vector3Create(0, 0, 0), Vector3Create(0, 1, 0), 60, 0);

 light := R3D_CreateLight(R3D_LIGHT_DIR);

 R3D_SetLightDirection(light, Vector3Create( 0, 0, -1 ));
 R3D_SetLightActive(light, true);

end;

procedure TRayApplication.Update;
var keep, linear: boolean;
begin
 UpdateCamera(@camera, CAMERA_ORBITAL);

 if IsKeyPressed(KEY_R) then
 begin
   keep := R3D_HasState(R3D_FLAG_ASPECT_KEEP);
   if (keep) then R3D_ClearState(R3D_FLAG_ASPECT_KEEP)
   else R3D_SetState(R3D_FLAG_ASPECT_KEEP);
 end;

 if (IsKeyPressed(KEY_F)) then
 begin
   linear := R3D_HasState(R3D_FLAG_BLIT_LINEAR);
   if (linear) then R3D_ClearState(R3D_FLAG_BLIT_LINEAR)
   else R3D_SetState(R3D_FLAG_BLIT_LINEAR);
 end;
end;

procedure TRayApplication.Draw;
var keep, linear: boolean; i: integer;
begin
 keep := R3D_HasState(R3D_FLAG_ASPECT_KEEP);
 linear := R3D_HasState(R3D_FLAG_BLIT_LINEAR);

 if (keep) then
 ClearBackground(BLACK);


 R3D_Begin(camera);
     rlPushMatrix();
     for i := 0 to 4 do
     begin
       sphere.materials[0] := materials[i];
       R3D_DrawModel(sphere, Vector3Create( i - 2, 0, 0 ), 1.0);
     end;
     rlPopMatrix();
 R3D_End();

 if keep then DrawText('(R)esize mode: KEEP', 10, 10, 20, BLACK) else
 DrawText('(R)esize mode: EXPAND', 10, 10, 20, BLACK);

 if linear then DrawText('(F)ilter mode: LINEAR', 10, 40, 20, BLACK) else
 DrawText('(F)ilter mode: NEAREST', 10, 40, 20, BLACK);
end;

procedure TRayApplication.Close;
begin
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

