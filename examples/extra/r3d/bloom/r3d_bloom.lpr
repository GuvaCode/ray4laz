program r3d_bloom;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, r3d;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    sphere: TModel;
    skybox: TR3D_Skybox;
    camera: TCamera3D;
    Materials: array[0..24] of TMaterial; // Массив из 25 материалов (5x5)
    procedure Init;
    procedure Draw;
    procedure Update;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - bloom example';

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
var y, x, i : integer;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);

  sphere := LoadModelFromMesh(GenMeshSphere(0.5, 64, 64));
  UnloadMaterial(sphere.materials[0]);

  for y := 0 to 4 do  //(int y = 0; y < 5; y++) {
      for x := 0 to 4 do //(int x = 0; x < 5; x++) {
        begin
          i := y * 5 + x;
          materials[i] := LoadMaterialDefault();
          materials[i].maps[MATERIAL_MAP_EMISSION].value := 1.0;
          materials[i].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
          materials[i].maps[MATERIAL_MAP_ROUGHNESS].value := x / 5;
          materials[i].maps[MATERIAL_MAP_METALNESS].value := y / 5;
          materials[i].maps[MATERIAL_MAP_ALBEDO].color := ColorFromHSV(x / 5.0 * 330, 1.0, 1.0);
          materials[i].maps[MATERIAL_MAP_EMISSION].color := materials[i].maps[MATERIAL_MAP_ALBEDO].color;
          materials[i].maps[MATERIAL_MAP_EMISSION].texture := materials[i].maps[MATERIAL_MAP_ALBEDO].texture;
        end;

  skybox := R3D_LoadSkybox('resources/sky/skybox1.png', CUBEMAP_LAYOUT_AUTO_DETECT);
  R3D_EnableSkybox(skybox);

  Camera.Create(Vector3Create(0,0,5), Vector3Create(0,0,0), Vector3Create(0,1,0), 60 ,0);

end;

procedure TRayApplication.Draw;
var
  x, y: Integer;
  position: TVector3;
begin
  R3D_Begin(camera);
  for y := -2 to 2 do
  begin
    for x := -2 to 2 do
    begin
      sphere.materials[0] := materials[(y + 2) * 5 + (x + 2)];
      position := Vector3Create(x * 1.1, y * 1.1, 0.0);
      R3D_DrawModel(sphere, position, 1.0);
    end;
  end;
  R3D_End;
  R3D_DrawBufferEmission(10, 10, 100, 100);
  R3D_DrawBufferBloom(120, 10, 100, 100);
end;

procedure TRayApplication.Update;
begin
  UpdateCamera(@camera, CAMERA_ORBITAL);
end;

procedure TRayApplication.Close;
begin
  UnloadModel(sphere);
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

