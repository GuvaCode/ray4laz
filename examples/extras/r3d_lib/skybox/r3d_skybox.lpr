program r3d_skybox;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d, raymath;

const
  MATERIALS_ROWS = 7;
  MATERIALS_COLS = 7;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    sphere: TMesh;
    skybox: TR3D_Skybox;
    camera: TCamera;
      materials: array[0..MATERIALS_ROWS * MATERIALS_COLS - 1] of TMaterial;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Update;
    procedure Draw;
    procedure Close;
  end;

  const AppTitle = '[r3d] - skybox example';

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
    update;
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
  inherited Destroy;
end;

procedure TRayApplication.Init;
var x, y, i: integer;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), R3D_FLAG_NONE);
  SetTargetFPS(60);

  sphere := GenMeshSphere(0.5, 64, 64);

 for x := 0 to MATERIALS_ROWS - 1 do
   for y := 0 to MATERIALS_COLS - 1 do
     begin
       i := y * MATERIALS_ROWS + x;
       materials[i] := LoadMaterialDefault();
       R3D_SetMaterialOcclusion(@materials[i], nil, 1.0);
       R3D_SetMaterialMetalness(@materials[i], nil, x / MATERIALS_ROWS);
       R3D_SetMaterialRoughness(@materials[i], nil, y / MATERIALS_COLS);
       R3D_SetMaterialAlbedo(@materials[i], nil, ColorFromHSV((x/MATERIALS_ROWS) * 360, 1, 1));
     end;

  skybox := R3D_LoadSkybox('resources/sky/skybox1.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(skybox);

  camera.Create(Vector3Create(0, 0, 5), Vector3Create(0, 0, 0), Vector3Create(0, 1, 0), 60, 0);

  DisableCursor();

end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_FREE);
end;

procedure TRayApplication.Draw;
var x, y: integer;
begin
 R3D_Begin(camera);
 for x := 0 to 6 do
   for y := 0 to 6 do
     R3D_DrawMesh(sphere, materials[y * 7 + x], MatrixTranslate(x - 3, y - 3, 0.0));
  R3D_End();
end;

procedure TRayApplication.Close;
begin
  UnloadMesh(sphere);
  R3D_UnloadSkybox(skybox);
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

