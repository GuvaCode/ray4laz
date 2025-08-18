program dofexample;

uses
  SysUtils, Math,
  raylib, rlgl, raymath, r3d;

const
  X_INSTANCES = 10;
  Y_INSTANCES = 10;
  INSTANCE_COUNT = X_INSTANCES * Y_INSTANCES;

var
  meshSphere: TR3D_Mesh;
  matDefault: TR3D_Material;
  camDefault: TCamera3D;
  instances: array[0..INSTANCE_COUNT-1] of TMatrix;
  instanceColors: array[0..INSTANCE_COUNT-1] of TColor;

function Init: PChar;
var
  light: TR3D_Light;
  spacing, offsetX, offsetZ: Single;
  x, y, idx: Integer;
begin
  R3D_Init(GetScreenWidth, GetScreenHeight, R3D_FLAG_FXAA);
  SetTargetFPS(60);

  // Enable and configure DOF
  R3D_SetDofMode(R3D_DOF_ENABLED);
  R3D_SetDofFocusPoint(2.0);
  R3D_SetDofFocusScale(3.0);
  R3D_SetDofMaxBlurSize(20.0);
  R3D_SetDofDebugMode(False);

  // Setup scene lighting
  light := R3D_CreateLight(R3D_LIGHT_DIR);
  R3D_SetLightDirection(light, Vector3Create(0, -1, 0));
  R3D_SetLightActive(light, True);

  // Load sphere mesh and material
  meshSphere := R3D_GenMeshSphere(0.2, 64, 64, True);
  matDefault := R3D_GetDefaultMaterial();

  // Generate instances
  spacing := 0.5;
  offsetX := (X_INSTANCES * spacing) / 2;
  offsetZ := (Y_INSTANCES * spacing) / 2;
  idx := 0;

  for x := 0 to X_INSTANCES - 1 do
  begin
    for y := 0 to Y_INSTANCES - 1 do
    begin
      instances[idx] := MatrixTranslate(x * spacing - offsetX, 0, y * spacing - offsetZ);
      instanceColors[idx] := ColorCreate(
        Random(256),
        Random(256),
        Random(256),
        255
      );
      Inc(idx);
    end;
  end;

  // Configure the camera
  camDefault.position := Vector3Create(0, 2, 2);
  camDefault.target := Vector3Create(0, 0, 0);
  camDefault.up := Vector3Create(0, 1, 0);
  camDefault.fovy := 60;

  Result := '[r3d] - DoF example';
end;

procedure Update(delta: Single);
var
  rotation: TMatrix;
  view: TVector3;
  mousePosition: TVector2;
  mouseWheel: Single;
  focusPoint, focusScale, maxBlurSize: Single;
  debugMode: Boolean;
begin
  // Rotate camera
  rotation := MatrixRotate(camDefault.up, 0.1 * delta);
  view := Vector3Subtract(camDefault.position, camDefault.target);
  view := Vector3Transform(view, rotation);
  camDefault.position := Vector3Add(camDefault.target, view);

  // Adjust DoF based on mouse position
  mousePosition := GetMousePosition;
  mouseWheel := GetMouseWheelMove;

  focusPoint := 0.5 + (5.0 - (mousePosition.y / GetScreenHeight) * 5.0);
  R3D_SetDofFocusPoint(focusPoint);

  focusScale := 0.5 + (5.0 - (mousePosition.x / GetScreenWidth) * 5.0);
  R3D_SetDofFocusScale(focusScale);

  if mouseWheel <> 0.0 then
  begin
    maxBlurSize := R3D_GetDofMaxBlurSize;
    maxBlurSize := maxBlurSize + mouseWheel * 0.1;
    R3D_SetDofMaxBlurSize(maxBlurSize);
  end;


  if IsKeyPressed(KEY_F1) then
    begin
      debugMode := R3D_GetDofDebugMode();
      R3D_SetDofDebugMode(not debugMode); // Toggle between true/false
    end;
end;

procedure Draw;
var
  dofText, fpsText: PChar;
begin
  // Render R3D scene
  R3D_Begin(camDefault);
    R3D_SetBackgroundColor(ColorCreate(0, 0, 0, 255));
    R3D_DrawMeshInstancedEx(@meshSphere, @matDefault, @instances[0], @instanceColors[0], INSTANCE_COUNT);
  R3D_End();

  dofText := TextFormat('Focus Point: %.2f'#10'Focus Scale: %.2f'#10'Max Blur Size: %.2f'#10'Debug Mode: %d',
    R3D_GetDofFocusPoint, R3D_GetDofFocusScale, R3D_GetDofMaxBlurSize, Integer(R3D_GetDofDebugMode));

  DrawText(dofText, 10, 30, 20, WHITE);

  // Print instructions
  DrawText('F1: Toggle Debug Mode'#10'Scroll: Adjust Max Blur Size'#10+
           'Mouse Left/Right: Shallow/Deep DoF'#10'Mouse Up/Down: Adjust Focus Point Depth',
           300, 10, 20, WHITE);

  // Draw FPS
  fpsText := TextFormat('FPS: %d', GetFPS);
  DrawText(fpsText, 10, 10, 20, WHITE);
end;

procedure Close;
begin
  R3D_UnloadMesh(@meshSphere);
  R3D_Close;
end;

begin
  InitWindow(800, 600, 'Pascal DOF Example');
  Init;

  while not WindowShouldClose do
  begin
    Update(GetFrameTime);
    BeginDrawing;
      ClearBackground(BLACK);
      Draw;
    EndDrawing;
  end;

  Close;
  CloseWindow;
end.
