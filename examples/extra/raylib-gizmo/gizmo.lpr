program gizmo;

{$mode objfpc}{$H+}

uses
 {$IFNDEF WINDOWS}cthreads,{$ENDIF}
 Classes, SysUtils, CustApp, raylib, raymath, raygizmo;

type
  { TRayApplication }
  TRayApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  const AppTitle = 'raylib - basic window';
         CRATE_COUNT = 4;
  var crateTransforms: array [0..CRATE_COUNT -1] of TTransform;
      crateTexture: TTexture;
      crateModel: TModel;
      cam: TCamera;
      gizmoTypes: array[0..CRATE_COUNT -1] of integer;
{ TRayApplication }

constructor TRayApplication.Create(TheOwner: TComponent);
var i: integer;
begin
  inherited Create(TheOwner);

  InitWindow(800, 600, AppTitle); // for window settings, look at example - window flags

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  // Initialize the transforms with default values
  for  i:= 0 to CRATE_COUNT -1 do
  begin
    crateTransforms[i] := GizmoIdentity();
    crateTransforms[i].translation.x := -15.0 + 6.0 * i;  // Offset crates along the X-axis
  end;

  // Assign each crate a different gizmo type
  gizmoTypes[0] := GIZMO_TRANSLATE;
  gizmoTypes[1] := GIZMO_SCALE;
  gizmoTypes[2] := GIZMO_ROTATE;
  gizmoTypes[3] := GIZMO_ALL;

  // Load the crate texture.
  crateTexture := LoadTexture('resources/models/crate_texture.jpg');
  GenTextureMipmaps(@crateTexture);
  SetTextureFilter(crateTexture, TEXTURE_FILTER_TRILINEAR);

  // Load the crate model and apply the texture.
  crateModel := LoadModel('resources/models/crate_model.obj');
  crateModel.materials[0].maps[MATERIAL_MAP_ALBEDO].texture := crateTexture;

  // Setup the 3D camera.
  cam.fovy := 45.0;
  cam.position := Vector3Create( 7.5, 5.5, 5.0 );
  cam.target := Vector3Create( 0, 1.5, 0 );
  cam.up := Vector3Create( 0, 1, 0 );
  cam.projection := CAMERA_PERSPECTIVE;


end;

procedure TRayApplication.DoRun;
var i: integer;
begin

  while (not WindowShouldClose) do // Detect window close button or ESC key
  begin
    BeginDrawing();

      // Clear the background with a dark blue color.
      ClearBackground(ColorCreate( 0, 0, 25, 255 ));

      BeginMode3D(cam);

      // Draw the crates with their updated transforms
      for i := 0 to  CRATE_COUNT - 1 do
      begin
        crateModel.transform := GizmoToMatrix(crateTransforms[i]);
        DrawModel(crateModel, Vector3Zero(), 1.0, WHITE);
      end;

      // Draw the gizmos and handle user input
      for i := 0 to  CRATE_COUNT - 1 do
      DrawGizmo3D(gizmoTypes[i], @crateTransforms[i]);


      EndMode3D();
      EndDrawing();

  end;

  // Stop program loop
  Terminate;
end;

destructor TRayApplication.Destroy;
begin
  // Unload resources and clean up.
  UnloadTexture(crateTexture);
  UnloadModel(crateModel);
  // De-Initialization
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

