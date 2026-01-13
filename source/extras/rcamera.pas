unit rcamera;

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I raylib.inc}

interface

uses
  raylib;

type
  // Camera projection types
  TCameraProjection = (
    CAMERA_PERSPECTIVE = 0,  // Perspective projection
    CAMERA_ORTHOGRAPHIC     // Orthographic projection
  );

  // Camera system modes
  TCameraMode = (
    CAMERA_CUSTOM = 0,       // Camera custom, controlled by user (UpdateCamera() does nothing)
    CAMERA_FREE,             // Camera free mode
    CAMERA_ORBITAL,          // Camera orbital, around target, zoom supported
    CAMERA_FIRST_PERSON,     // Camera first person
    CAMERA_THIRD_PERSON      // Camera third person
  );

// Returns the camera's forward vector (normalized)
function GetCameraForward(camera: PCamera3D): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraForward';

// Returns the camera's up vector (normalized)
// Note: The up vector might not be perpendicular to the forward vector
function GetCameraUp(camera: PCamera3D): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraUp';

// Returns the camera's right vector (normalized)
function GetCameraRight(camera: PCamera3D): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraRight';

// Camera movement functions
procedure CameraMoveForward(camera: PCamera3D; distance: Single; moveInWorldPlane: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraMoveForward';
procedure CameraMoveUp(camera: PCamera3D; distance: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraMoveUp';
procedure CameraMoveRight(camera: PCamera3D; distance: Single; moveInWorldPlane: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraMoveRight';
procedure CameraMoveToTarget(camera: PCamera3D; delta: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraMoveToTarget';

// Camera rotation functions
procedure CameraYaw(camera: PCamera3D; angle: Single; rotateAroundTarget: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraYaw';
procedure CameraPitch(camera: PCamera3D; angle: Single; lockView, rotateAroundTarget, rotateUp: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraPitch';
procedure CameraRoll(camera: PCamera3D; angle: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CameraRoll';

// Returns the camera view matrix
function GetCameraViewMatrix(camera: PCamera3D): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraViewMatrix';

// Returns the camera projection matrix
function GetCameraProjectionMatrix(camera: PCamera3D; aspect: Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraProjectionMatrix';

// Update camera position for selected mode
// Camera mode: CAMERA_FREE, CAMERA_FIRST_PERSON, CAMERA_THIRD_PERSON, CAMERA_ORBITAL or CUSTOM
procedure UpdateCamera(camera: PCamera3D; mode: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateCamera';

// Update camera movement, movement/rotation values should be provided by user
procedure UpdateCameraPro(camera: PCamera3D; movement, rotation: TVector3; zoom: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateCameraPro';

implementation

end.
