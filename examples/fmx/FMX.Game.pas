unit FMX.Game;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Math, raylib, raymath;


//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------
const
  // Movement constants
  GRAVITY = 32.0;
  MAX_SPEED = 20.0;
  CROUCH_SPEED = 5.0;
  JUMP_FORCE = 12.0;
  MAX_ACCEL = 150.0;
  // Grounded drag
  FRICTION = 0.86;
  // Increasing air drag, increases strafing speed
  AIR_DRAG = 0.98;
  // Responsiveness for turning movement direction to looked direction
  CONTROL = 15.0;
  CROUCH_HEIGHT = 0.0;
  STAND_HEIGHT = 1.0;
  BOTTOM_HEIGHT = 0.5;
  NORMALIZE_INPUT = 0;

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------
type
  // Body structure
  PBody = ^TBody;

  TBody = record
    position: TVector3;
    velocity: TVector3;
    dir: TVector3;
    isGrounded: Boolean;
  end;

//----------------------------------------------------------------------------------
// Global Variables Definition
//----------------------------------------------------------------------------------
var
  sensitivity: TVector2;
  player: TBody;
  lookRotation: TVector2;
  headTimer: Single;
  walkLerp: Single;
  headLerp: Single;
  lean: TVector2;

procedure Run(Parent: THandle; Proc: TProc<THandle>);

implementation

//----------------------------------------------------------------------------------
// Module Functions Definition
//----------------------------------------------------------------------------------
// Update body considering current world state

procedure UpdateBody(body: PBody; rot: Single; side: Integer; forward_: Integer; jumpPressed: Boolean; crouchHold, sprintHold: Boolean);
var
  input: TVector2;
  delta: Single;
  front, right, desiredDir: TVector3;
  decel: Single;
  hvel: TVector3;
  hvelLength, speed, maxSpeed, accel: Single;
begin
  input := Vector2Create(side, -forward_);

  {$IFDEF NORMALIZE_INPUT}
  // Slow down diagonal movement
  if (side <> 0) and (forward <> 0) then
    input := Vector2Normalize(input);
  {$ENDIF}

  delta := GetFrameTime();

  if not body^.isGrounded then
    body^.velocity.y := body^.velocity.y - GRAVITY * delta;

  if body^.isGrounded and jumpPressed then
  begin
    body^.velocity.y := JUMP_FORCE;
    body^.isGrounded := false;

    // Sound can be played at this moment
    //SetSoundPitch(fxJump, 1.0 + (GetRandomValue(-100, 100)*0.001));
    //PlaySound(fxJump);
  end;

  front := Vector3Create(Sin(rot), 0.0, Cos(rot));
  right := Vector3Create(Cos(-rot), 0.0, Sin(-rot));

  desiredDir := Vector3Create(
    input.x * right.x + input.y * front.x,
    0.0,
    input.x * right.z + input.y * front.z
  );
  body^.dir := Vector3Lerp(body^.dir, desiredDir, CONTROL * delta);

  if body^.isGrounded then
    decel := FRICTION
  else
    decel := AIR_DRAG;

  hvel := Vector3Create(
    body^.velocity.x * decel,
    0.0,
    body^.velocity.z * decel
  );

  hvelLength := Vector3Length(hvel); // Magnitude
  if hvelLength < (MAX_SPEED * 0.01) then
    hvel := Vector3Create(0, 0, 0);

  // This is what creates strafing
  speed := Vector3DotProduct(hvel, body^.dir);

  // Whenever the amount of acceleration to add is clamped by the maximum acceleration constant,
  // a Player can make the speed faster by bringing the direction closer to horizontal velocity angle
  // More info here: https://youtu.be/v3zT3Z5apaM?t=165
  if crouchHold then
    maxSpeed := CROUCH_SPEED
  else if sprintHold then
    maxSpeed := MAX_SPEED * 2
  else
    maxSpeed := MAX_SPEED;

  accel := Clamp(maxSpeed - speed, 0.0, MAX_ACCEL * delta);
  hvel.x := hvel.x + body^.dir.x * accel;
  hvel.z := hvel.z + body^.dir.z * accel;

  body^.velocity.x := hvel.x;
  body^.velocity.z := hvel.z;

  body^.position.x := body^.position.x + body^.velocity.x * delta;
  body^.position.y := body^.position.y + body^.velocity.y * delta;
  body^.position.z := body^.position.z + body^.velocity.z * delta;

  // Fancy collision system against the floor
  if body^.position.y <= 0.0 then
  begin
    body^.position.y := 0.0;
    body^.velocity.y := 0.0;
    body^.isGrounded := true; // Enable jumping
  end;
end;

// Update camera for FPS behaviour
procedure UpdateCameraFPS(camera: PCamera);
var
  up, targetOffset, yaw, right, pitch: TVector3;
  maxAngleUp, maxAngleDown, pitchAngle: Single;
  headSin, headCos: Single;
  stepRotation, bobSide, bobUp: Single;
  bobbing: TVector3;
begin
  up := Vector3Create(0.0, 1.0, 0.0);
  targetOffset := Vector3Create(0.0, 0.0, -1.0);

  // Left and right
  yaw := Vector3RotateByAxisAngle(targetOffset, up, lookRotation.x);

  // Clamp view up
  maxAngleUp := Vector3Angle(up, yaw);
  maxAngleUp := maxAngleUp - 0.001; // Avoid numerical errors
  if -lookRotation.y > maxAngleUp then
    lookRotation.y := -maxAngleUp;

  // Clamp view down
  maxAngleDown := Vector3Angle(Vector3Negate(up), yaw);
  maxAngleDown := maxAngleDown *  - 1.0; // Downwards angle is negative
  maxAngleDown := maxAngleDown + 0.001; // Avoid numerical errors
  if -lookRotation.y < maxAngleDown then
    lookRotation.y := -maxAngleDown;

  // Up and down
  right := Vector3Normalize(Vector3CrossProduct(yaw, up));

  // Rotate view vector around right axis
  pitchAngle := -lookRotation.y - lean.y;
  pitchAngle := Clamp(pitchAngle, -PI / 2 + 0.0001, PI / 2 - 0.0001); // Clamp angle so it doesn't go past straight up or straight down
  pitch := Vector3RotateByAxisAngle(yaw, right, pitchAngle);

  // Head animation
  // Rotate up direction around forward axis
  headSin := Sin(headTimer * PI);
  headCos := Cos(headTimer * PI);
  stepRotation := 0.01;
  camera^.up := Vector3RotateByAxisAngle(up, pitch, headSin * stepRotation + lean.x);

  // Camera BOB
  bobSide := 0.1;
  bobUp := 0.15;
  bobbing := Vector3Scale(right, headSin * bobSide);
  bobbing.y := Abs(headCos * bobUp);

  camera^.position := Vector3Add(camera^.position, Vector3Scale(bobbing, walkLerp));
  camera^.target := Vector3Add(camera^.position, pitch);
end;

// Draw game level
procedure DrawLevel;
var
  floorExtent, x, y: Integer;
  tileSize: Single;
  tileColor1: TColor;
  towerSize, towerPos: TVector3;
  towerColor: TColor;
begin
  floorExtent := 25;
  tileSize := 5.0;
  tileColor1 := ColorCreate(150, 200, 200, 255);

  // Floor tiles
  for y := -floorExtent to floorExtent - 1 do
  begin
    for x := -floorExtent to floorExtent - 1 do
    begin
      if ((y and 1) <> 0) and ((x and 1) <> 0) then
      begin
        DrawPlane(Vector3Create(x * tileSize, 0.0, y * tileSize),
          Vector2Create(tileSize, tileSize), tileColor1);
      end
      else if ((y and 1) = 0) and ((x and 1) = 0) then
      begin
        DrawPlane(Vector3Create(x * tileSize, 0.0, y * tileSize),
          Vector2Create(tileSize, tileSize), LIGHTGRAY);
      end;
    end;
  end;

  towerSize := Vector3Create(16.0, 32.0, 16.0);
  towerColor := ColorCreate(150, 200, 200, 255);

  towerPos := Vector3Create(16.0, 16.0, 16.0);
  DrawCubeV(towerPos, towerSize, towerColor);
  DrawCubeWiresV(towerPos, towerSize, DARKBLUE);

  towerPos.x := towerPos.x *  - 1;
  DrawCubeV(towerPos, towerSize, towerColor);
  DrawCubeWiresV(towerPos, towerSize, DARKBLUE);

  towerPos.z := towerPos.z *  - 1;
  DrawCubeV(towerPos, towerSize, towerColor);
  DrawCubeWiresV(towerPos, towerSize, DARKBLUE);

  towerPos.x := towerPos.x *  - 1;
  DrawCubeV(towerPos, towerSize, towerColor);
  DrawCubeWiresV(towerPos, towerSize, DARKBLUE);

  // Red sun
  DrawSphere(Vector3Create(300.0, 300.0, 0.0), 100.0, ColorCreate(255, 0, 0, 255));
end;

procedure Run(Parent: THandle; Proc: TProc<THandle>);

//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------
begin

  // Main game loop
  TThread.CreateAnonymousThread(
    procedure
    var
      screenWidth, screenHeight: Integer;
      camera: TCamera;
      mouseDelta: TVector2;
      sideway, forward: Integer;
      crouching, sprinting: Boolean;
      delta: Single;
    begin
  // Initialization
  //--------------------------------------------------------------------------------------
      screenWidth := 800;
      screenHeight := 450;

      InitWindow(screenWidth, screenHeight, 'raylib [core] example - 3d camera fps');
      SetParent(HWND(GetWindowHandle), Parent);
      Proc(HWND(GetWindowHandle));
  //

  //ToggleFullscreen;
  // Initialize global variables
      sensitivity := Vector2Create(0.001, 0.001);
      player.position := Vector3Create(0, 0, 0);
      player.velocity := Vector3Create(0, 0, 0);
      player.dir := Vector3Create(0, 0, 0);
      player.isGrounded := true;
      lookRotation := Vector2Create(0, 0);
      headTimer := 0.0;
      walkLerp := 0.0;
      headLerp := STAND_HEIGHT;
      lean := Vector2Create(0, 0);

  // Initialize camera variables
  // NOTE: UpdateCameraFPS() takes care of the rest
      camera.fovy := 60.0;
      camera.projection := CAMERA_PERSPECTIVE;
      camera.position := Vector3Create(
        player.position.x,
        player.position.y + (BOTTOM_HEIGHT + headLerp),
        player.position.z
      );

      UpdateCameraFPS(@camera); // Update camera parameters

      //DisableCursor();        // Limit cursor to relative movement inside the window

      SetTargetFPS(75);       // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
      while not WindowShouldClose() do    // Detect window close button or ESC key
      begin
        // Update
        //----------------------------------------------------------------------------------
        mouseDelta := GetMouseDelta();
        lookRotation.x := lookRotation.x - mouseDelta.x * sensitivity.x;
        lookRotation.y := lookRotation.y + mouseDelta.y * sensitivity.y;

        sideway := Integer(IsKeyDown(KEY_D)) - Integer(IsKeyDown(KEY_A));
        forward := Integer(IsKeyDown(KEY_W)) - Integer(IsKeyDown(KEY_S));
        crouching := IsKeyDown(KEY_LEFT_CONTROL);
        sprinting := IsKeyDown(KEY_LEFT_SHIFT);
        UpdateBody(@player, lookRotation.x, sideway, forward, IsKeyDown(KEY_SPACE), crouching, sprinting);

        delta := GetFrameTime();
        headLerp := Lerp(headLerp, IfThen(crouching, CROUCH_HEIGHT, STAND_HEIGHT), 20.0 * delta);
        camera.position := Vector3Create(
          player.position.x,
          player.position.y + (BOTTOM_HEIGHT + headLerp),
          player.position.z
        );

        if player.isGrounded and ((forward <> 0) or (sideway <> 0)) then
        begin
          headTimer := headTimer + delta * 3.0;
          walkLerp := Lerp(walkLerp, 1.0, 10.0 * delta);
          camera.fovy := Lerp(camera.fovy, 55.0, 5.0 * delta);
        end
        else
        begin
          walkLerp := Lerp(walkLerp, 0.0, 10.0 * delta);
          camera.fovy := Lerp(camera.fovy, 60.0, 5.0 * delta);
        end;

        lean.x := Lerp(lean.x, sideway * 0.02, 10.0 * delta);
        lean.y := Lerp(lean.y, forward * 0.015, 10.0 * delta);

        UpdateCameraFPS(@camera);
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------
        BeginDrawing();

        ClearBackground(RAYWHITE);

        BeginMode3D(camera);
        DrawLevel();
        EndMode3D();

          // Draw info box
        DrawRectangle(5, 5, 330, 75, Fade(SKYBLUE, 0.5));
        DrawRectangleLines(5, 5, 330, 75, BLUE);

        DrawText('Camera controls:', 15, 15, 10, BLACK);
        DrawText('- Move keys: W, A, S, D, Space, Left-Ctrl', 15, 30, 10, BLACK);
        DrawText('- Look around: arrow keys or mouse', 15, 45, 10, BLACK);
        DrawText(PAnsiChar(TextFormat('- Velocity Len: (%06.3f)',
              Vector2Length(Vector2Create(player.velocity.x, player.velocity.z)))),
          15, 60, 10, BLACK);

        EndDrawing();
      //----------------------------------------------------------------------------------
      end;

      // De-Initialization
      //--------------------------------------------------------------------------------------
      CloseWindow();        // Close window and OpenGL context
      //--------------------------------------------------------------------------------------
    end).Start;
end;

end.

