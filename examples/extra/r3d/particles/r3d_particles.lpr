program r3d_particles;

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
    sphere: TMesh;
    material: TMaterial;
    skybox: TR3D_Skybox;
    camera: TCamera3D;
    curve: TR3D_InterpolationCurve;
    particles: TR3D_ParticleSystem;
    particlesAabb: TBoundingBox;
    procedure Init;
    procedure Update(delta: Single);
    procedure Draw;
    procedure Close;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = '[r3d] - particles example';

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
    Update(GetFrameTime);
    // Draw
    BeginDrawing();
      Draw;
    EndDrawing();
  end;

  // Stop program loop
  Terminate;
end;

procedure TRayApplication.Init;
begin
  R3D_Init(GetScreenWidth(), GetScreenHeight(), 0);
  SetTargetFPS(60);

  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  R3D_SetBackgroundColor(ColorCreate( 4, 4, 4, 255 ));
  R3D_SetAmbientColor(BLACK);

  sphere := GenMeshSphere(0.1, 16, 32);

   material := LoadMaterialDefault();
   R3D_SetMaterialEmission(@material, nil, ColorCreate( 255, 0, 0, 255 ), 1.0);

   curve := R3D_LoadInterpolationCurve(3);
   R3D_AddKeyframe(@curve, 0.0, 0.0);
   R3D_AddKeyframe(@curve, 0.5, 1.0);
   R3D_AddKeyframe(@curve, 1.0, 0.0);

   particles := R3D_LoadParticleSystem(2048);
   particles.initialVelocity := Vector3Create( 0, 10.0, 0 );
   particles.scaleOverLifetime := @curve;
   particles.spreadAngle := 45.0;
   particles.emissionRate := 2048;
   particles.lifetime := 2.0;

   particlesAabb := R3D_GetParticleSystemBoundingBox(@particles);

   camera.Create(Vector3Create(-7,7,-7), Vector3Create(0, 1, 0), Vector3Create(0, 1, 0), 60 , CAMERA_PERSPECTIVE);
end;

procedure TRayApplication.Update(delta: Single);
begin
  UpdateCamera(@camera, CAMERA_ORBITAL);
  R3D_UpdateParticleSystem(@particles, GetFrameTime());
end;

procedure TRayApplication.Draw;
begin
  R3D_Begin(camera);
    R3D_DrawParticleSystem(@particles, sphere, material);
  R3D_End();

  BeginMode3D(camera);
    DrawBoundingBox(particlesAabb, GREEN);
  EndMode3D();

  DrawFPS(10, 10);
end;

procedure TRayApplication.Close;
begin
  R3D_UnloadInterpolationCurve(curve);
  R3D_UnloadParticleSystem(@particles);

  UnloadMesh(sphere);
  UnloadMaterial(material);

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

