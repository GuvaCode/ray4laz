program r3d_particles;
{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, raylib, r3d, raymath;

var
  Sphere: TR3D_Mesh;
  Material: TR3D_Material;
  Skybox: TR3D_Skybox;
  Camera: TCamera3D;
  Curve: TR3D_InterpolationCurve;
  Particles: TR3D_ParticleSystem;

function Init: PChar;
var
  InitialVelocity: TVector3;
  BgColor: TColor;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, 0);
  SetTargetFPS(60);

  // Configure bloom and background
  R3D_SetBloomMode(R3D_BLOOM_ADDITIVE);
  BgColor := ColorCreate(4, 4, 4, 255);
  R3D_SetBackgroundColor(BgColor);
  R3D_SetAmbientColor(BLACK);

  // Create sphere mesh for particles
  Sphere := R3D_GenMeshSphere(0.1, 16, 32, True);

  // Configure material with emission
  Material := R3D_GetDefaultMaterial();
  Material.emission.color := RED;
  Material.emission.energy := 1.0;

  // Create interpolation curve for particle scaling
  Curve := R3D_LoadInterpolationCurve(3);
  R3D_AddKeyframe(@Curve, 0.0, 0.0);
  R3D_AddKeyframe(@Curve, 0.5, 1.0);
  R3D_AddKeyframe(@Curve, 1.0, 0.0);

  // Configure particle system
  Particles := R3D_LoadParticleSystem(2048);
  InitialVelocity := Vector3Create(0, 10.0, 0);
  Particles.initialVelocity := InitialVelocity;
  Particles.scaleOverLifetime := @Curve;
  Particles.spreadAngle := 45.0;
  Particles.emissionRate := 2048;
  Particles.lifetime := 2.0;

  R3D_CalculateParticleSystemBoundingBox(@Particles);

  // Setup camera
  Camera.position := Vector3Create(-7, 7, -7);
  Camera.target := Vector3Create(0, 1, 0);
  Camera.up := Vector3Create(0, 1, 0);
  Camera.fovy := 60.0;
  Camera.projection := CAMERA_PERSPECTIVE;

  Result := '[r3d] - Particles example';
end;

procedure Update(delta: Single);
begin
  UpdateCamera(@Camera, CAMERA_ORBITAL);
  R3D_UpdateParticleSystem(@Particles, GetFrameTime());
end;

procedure Draw;
begin
  R3D_Begin(Camera);
    R3D_DrawParticleSystem(@Particles, @Sphere, @Material);
  R3D_End();

  // Draw bounding box in 3D space
  BeginMode3D(Camera);
    DrawBoundingBox(Particles.aabb, GREEN);
  EndMode3D();

  DrawFPS(10, 10);
end;

procedure Close;
begin
  R3D_UnloadInterpolationCurve(Curve);
  R3D_UnloadParticleSystem(@Particles);
  R3D_UnloadMesh(@Sphere);
  R3D_UnloadMaterial(@Material);
  R3D_Close();
end;

begin
  InitWindow(800, 600, 'Particles Example');
  Init();

  while not WindowShouldClose() do
  begin
    Update(GetFrameTime());
    BeginDrawing();
      ClearBackground(BLACK);
      Draw();
    EndDrawing();
  end;

  Close();
  CloseWindow();
end.
