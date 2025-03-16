program r3d_sprite;

{$mode objfpc}{$H+}

uses
{$IFDEF LINUX} cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, math, r3d;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    plane: TModel;
    camera: TCamera;
    texture: TTexture2d;
    sprite: TR3D_Sprite;
    birdDirX: Single;
    birdPos: TVector3;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure Update(delta: Single);
    procedure Draw;
    procedure Close;
  end;

  const AppTitle = '[r3d] - sprite example';

{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  birdDirX := 1.0;
  birdPos.Create(0,0.5,0);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags
  Init;

end;

procedure TRayApplication.DoRun;
begin

  while (not WindowShouldClose) do // Detect window close button or ESC key
  begin
    // Update your variables here
    Update(GetFrameTime);
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

  plane := LoadModelFromMesh(GenMeshPlane(1000, 1000, 1, 1));
  plane.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1;
  plane.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 1;
  plane.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0;

  texture := LoadTexture('resources/spritesheet.png');
  sprite := R3D_LoadSprite(texture, 4, 1);

  camera := Camera3DCreate(Vector3Create(0,2,5), Vector3Create(0,0,0), Vector3Create(0,1,0), 60,0  );

  light := R3D_CreateLight(R3D_LIGHT_SPOT);

  R3D_SetLightPosition(light, Vector3Create( 0, 10, 10 ));
  R3D_SetLightTarget(light, Vector3Create(0, 0, 0));
  R3D_SetLightActive(light, true);

end;

procedure TRayApplication.Update(delta: Single);
var birdPosPrev: TVector3;
begin
  R3D_UpdateSprite(@sprite, 10 * delta);

  birdPosPrev := birdPos;

  birdPos.x := 2.0 * sin(GetTime());
  birdPos.y := 1.0 + cos(GetTime() * 4.0) * 0.5;
  birdDirX := IfThen(birdPos.x - birdPosPrev.x >= 0, 1, -1);
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);

  R3D_ApplyBillboardMode(R3D_BILLBOARD_DISABLED);
  R3D_DrawModel(plane, Vector3Create ( 0, -0.5, 0 ), 1.0);

  R3D_ApplyBillboardMode(R3D_BILLBOARD_Y_AXIS);
  R3D_DrawSpriteEx(sprite, birdPos, Vector2Create ( birdDirX, 1.0 ), 0.0);

  R3D_End();
end;

procedure TRayApplication.Close;
begin
  R3D_UnloadSprite(sprite);
  UnloadTexture(texture);
  UnloadModel(plane);
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

