{**********************************************************************************************
*
*   raymath v1.5 - Math functions to work with Vector2, Vector3, Matrix and Quaternions
*
*
*   CONVENTIONS:
*
*     - Functions are always self-contained, no function use another raymath function inside,
*       required code is directly re-implemented inside
*     - Functions input parameters are always received by value (2 unavoidable exceptions)
*     - Functions use always a "result" anmed variable for return
*     - Functions are always defined inline
*     - Angles are always in radians (DEG2RAD/RAD2DEG macros provided for convenience)
*
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2015-2021 Ramon Santamaria (@raysan5)
*   Pascal header 2021 Gunko Vadim (@guvacode)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************}

unit raymath;

interface

uses raylib;

type
    PFloat3 = ^TFloat3;
    TFloat3 = record
     v: array[0..2] of single;
    end;

    PFloat16 = ^TFloat16;
    TFloat16 = record
     v: array[0..15] of single;
    end;

//----------------------------------------------------------------------------------
// Module Functions Definition - Utils math
//----------------------------------------------------------------------------------
function  Clamp(value, min, max : Single): Single; cdecl; external cDllName;   // Clamp float value
function  Lerp(start, end_, amount : Single): Single; cdecl; external cDllName; // Calculate linear interpolation between two floats
function  Normalize(value, start, end_: Single): Single; cdecl; external cDllName; // Normalize input value within input range
function  Remap(value, inputStart, inputEnd, outputStart, outputEnd : Single): Single; cdecl; external cDllName;// Remap input value within input range to output range

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector2 math
//----------------------------------------------------------------------------------
function  Vector2Zero: TVector2; cdecl; external cDllName;// Vector with components value 0.0
function  Vector2One: TVector2; cdecl; external cDllName;// Vector with components value 1.0
function  Vector2Add(v1, v2 : TVector2): TVector2; cdecl; external cDllName;// Add two vectors (v1 + v2)
function  Vector2AddValue(v: TVector2; add: Single): TVector2;cdecl; external cDllName; // Add vector and float value
function  Vector2Subtract(v1, v2 : TVector2): TVector2; cdecl; external cDllName;// Subtract two vectors (v1 - v2)
function  Vector2SubtractValue( v: TVector2; sub:Single): TVector2; cdecl; external cDllName;// Subtract vector by float value
function  Vector2Length(v : TVector2): Single; cdecl; external cDllName; // Calculate vector length
function  Vector2LengthSqr(v : TVector2): Single; cdecl; external cDllName; // Calculate vector square length
function  Vector2DotProduct(v1, v2 : TVector2): Single; cdecl; external cDllName; // Calculate two vectors dot product
function  Vector2Distance(v1, v2 : TVector2): Single; cdecl; external cDllName;// Calculate distance between two vectors
function  Vector2Angle(v1, v2 : TVector2): Single; cdecl; external cDllName;// Calculate angle from two vectors in X-axis
function  Vector2Scale(v : TVector2; scale : Single): TVector2; cdecl; external cDllName;// Scale vector (multiply by value)
function  Vector2Multiply(v1, v2 : TVector2): TVector2; cdecl; external cDllName;// Multiply vector by vector
function  Vector2Negate(v : TVector2): TVector2; cdecl; external cDllName;// Negate vector
function  Vector2Divide(v1, v2 : TVector2): TVector2; cdecl; external cDllName;// Divide vector by vector
function  Vector2Normalize(v : TVector2): TVector2; cdecl; external cDllName; // Normalize provided vector
function  Vector2Lerp(v1, v2 : TVector2; amount : Single): TVector2; cdecl; external cDllName;// Calculate linear interpolation between two vectors
function  Vector2Reflect(v, normal : TVector2): TVector2; cdecl; external cDllName;// Calculate reflected vector to normal
function  Vector2Rotate(v: TVector2; angle:Single) :TVector2; cdecl; external cDllName;// Rotate vector by angle
function  Vector2MoveTowards(v, target:Tvector2; maxDistance: Single): TVector2; cdecl; external cDllName; // Move Vector towards target

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector3 math
//----------------------------------------------------------------------------------
function  Vector3Zero(): TVector3; cdecl; external cDllName;// Vector with components value 0.0
function  Vector3One(): TVector3; cdecl; external cDllName;// Vector with components value 1.0
function  Vector3Add(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Add two vectors
function  Vector3AddValue(v: Tvector3; add: single): TVector3; cdecl; external cDllName; // Add vector and float value
function  Vector3Subtract(v1, v2 : TVector3): TVector3; cdecl; external cDllName; // Subtract two vectors
function  Vector3SubtractValue(v: TVector3; sub:Single): Tvector3;  cdecl; external cDllName; // Subtract two vectors
function  Vector3Scale(a: TVector3; scalar:Single): TVector3; cdecl; external cDllName;// Multiply vector by scalar
function  Vector3Multiply(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Multiply vector by vector
function  Vector3CrossProduct(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Calculate two vectors cross product
function  Vector3Perpendicular(v : TVector3): TVector3; cdecl; external cDllName;// Calculate one vector perpendicular vector
function  Vector3Length(v : TVector3): Single; cdecl; external cDllName;// Calculate vector length
function  Vector3LengthSqr(v: TVector3): Single; cdecl; external cDllName;// Calculate vector square length
function  Vector3DotProduct(v1, v2 : TVector3): Single; cdecl; external cDllName;// Calculate two vectors dot product
function  Vector3Distance(v1, v2 : TVector3): Single; cdecl; external cDllName;// Calculate distance between two vectors
function  Vector3Angle(v1, v2: TVector3): Single; cdecl; external cDllName;// Calculate angle between two vectors in XY and XZ
function  Vector3Negate(aV : TVector3): TVector3; cdecl; external cDllName;// Negate provided vector (invert direction)
function  Vector3Divide(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Divide vector by vector
function  Vector3Normalize(v : TVector3): TVector3; cdecl; external cDllName; // Normalize provided vector
procedure Vector3OrthoNormalize(v1, v2 : PVector3); cdecl; external cDllName;// Orthonormalize provided vectors
function  Vector3Transform(v : TVector3; mat : TMatrix): TVector3; cdecl; external cDllName;// Transforms a Vector3 by a given Matrix
function  Vector3RotateByQuaternion(v : TVector3; q : TQuaternion): TVector3; cdecl; external cDllName;// Transform a vector by quaternion rotation
function  Vector3Lerp(v1, v2 : TVector3; amount : Single): TVector3; cdecl; external cDllName;// Calculate linear interpolation between two vectors
function  Vector3Reflect(v, normal : TVector3): TVector3; cdecl; external cDllName;// Calculate reflected vector to normal
function  Vector3Min(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Return min value for each pair of components
function  Vector3Max(v1, v2 : TVector3): TVector3; cdecl; external cDllName;// Return max value for each pair of components
function  Vector3Barycenter(p, a, b, c : TVector3): TVector3; cdecl; external cDllName;// Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)
function  Vector3Unproject(source:TVector3; projection:TMatrix; view:TMatrix):TVector3; cdecl; external cDllName;// Projects a Vector3 from screen space into object space
function  Vector3ToFloatV(v : TVector3): TFloat3; cdecl; external cDllName;// Returns Vector3 as float array


//----------------------------------------------------------------------------------
// Module Functions Definition - Matrix math
//----------------------------------------------------------------------------------
function  MatrixDeterminant(mat : TMatrix) : Single; cdecl; external cDllName; // Compute matrix determinant
function  MatrixTrace(mat : TMatrix): Single; cdecl; external cDllName;// Returns the trace of the matrix (sum of the values along the diagonal)
function  MatrixTranspose(mat : TMatrix): TMatrix; cdecl; external cDllName;// Transposes provided matrix
function  MatrixInvert(mat : TMatrix): TMatrix; cdecl; external cDllName; // Invert provided matrix
function  MatrixNormalize(mat : TMatrix): TMatrix; cdecl; external cDllName;// Normalize provided matrix
function  MatrixIdentity: TMatrix; cdecl; external cDllName;// Returns identity matrix
function  MatrixAdd(left : TMatrix; right : TMatrix): TMatrix; cdecl; external cDllName;// Add two matrices
function  MatrixSubtract(left : TMatrix; right : TMatrix): TMatrix; cdecl; external cDllName;// Subtract two matrices (left - right)
function  MatrixMultiply(left : TMatrix; right : TMatrix): TMatrix; cdecl; external cDllName;// Returns two matrix multiplication
function  MatrixTranslate(x, y, z : Single): TMatrix; cdecl; external cDllName;// Returns translation matrix
function  MatrixRotate(axis : TVector3; angle : Single): TMatrix; cdecl; external cDllName;// Create rotation matrix from axis and angle
function  MatrixRotateX(angle : Single): TMatrix; cdecl; external cDllName;// Returns x-rotation matrix (angle in radians)
function  MatrixRotateY(angle : Single): TMatrix; cdecl; external cDllName;// Returns y-rotation matrix (angle in radians)
function  MatrixRotateZ(angle : Single): TMatrix; cdecl; external cDllName;// Returns z-rotation matrix (angle in radians)
function  MatrixRotateXYZ(ang : TVector3): TMatrix; cdecl; external cDllName;// Returns xyz-rotation matrix (angles in radians)
function  MatrixRotateZYX(ang : TVector3): TMatrix; cdecl; external cDllName;// Returns zyx-rotation matrix (angles in radians)
function  MatrixScale(x, y, z : Single): TMatrix; cdecl; external cDllName;// Returns scaling matrix
function  MatrixFrustum(left, right, bottom, top, near_, far_ : double): TMatrix; cdecl; external cDllName;// Returns perspective projection matrix
function  MatrixPerspective(fovy, aspect, near_, far_ : double): TMatrix; cdecl; external cDllName;// Returns perspective projection matrix
function  MatrixOrtho(left, right, bottom, top, near_, far_ : double): TMatrix; cdecl; external cDllName;// Returns orthographic projection matrix
function  MatrixLookAt(eye, target, up : TVector3): TMatrix; cdecl; external cDllName;// Returns camera look-at matrix (view matrix)
function  MatrixToFloatV(mat : TMatrix): TFloat16; cdecl; external cDllName; // Returns float array of matrix data

//----------------------------------------------------------------------------------
// Module Functions Definition - Quaternion math
//----------------------------------------------------------------------------------
function  QuaternionAdd(q1, q2: TQuaternion): TQuaternion; cdecl; external cDllName;// Add two quaternions
function  QuaternionAddValue(q: TQuaternion ; add: single): TQuaternion; cdecl; external cDllName;// Add quaternion and float value
function  QuaternionSubtract(q1, q2: TQuaternion): TQuaternion; cdecl; external cDllName;// Subtract two quaternions
function  QuaternionSubtractValue(q:TQuaternion; sub:single): TQuaternion; cdecl; external cDllName;// Subtract quaternion and float value
function  QuaternionIdentity(): TQuaternion; cdecl; external cDllName;// Returns identity quaternion
function  QuaternionLength(q : TQuaternion): Single; cdecl; external cDllName;// Computes the length of a quaternion
function  QuaternionNormalize(q : TQuaternion): TQuaternion; cdecl; external cDllName;// Normalize provided quaternion
function  QuaternionInvert(q : TQuaternion): TQuaternion; cdecl; external cDllName;// Invert provided quaternion
function  QuaternionMultiply(q1, q2 : TQuaternion): TQuaternion; cdecl; external cDllName;// Calculate two quaternion multiplication
function  QuaternionScale(q:TQuaternion;mul:single): TQuaternion; cdecl; external cDllName;// Scale quaternion by float value
function  QuaternionDivide( q1, q2: TQuaternion): TQuaternion; cdecl; external cDllName;// Divide two quaternions
function  QuaternionLerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external cDllName;// Calculate linear interpolation between two quaternions
function  QuaternionNlerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external cDllName;// Calculate slerp-optimized interpolation between two quaternions
function  QuaternionSlerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external cDllName;// Calculates spherical linear interpolation between two quaternions
function  QuaternionFromVector3ToVector3(from, to_ : TVector3): TQuaternion; cdecl; external cDllName;// Calculate quaternion based on the rotation from one vector to another
function  QuaternionFromMatrix(mat : TMatrix): TQuaternion; cdecl; external cDllName;// Returns a quaternion for a given rotation matrix
function  QuaternionToMatrix(q : TQuaternion): TMatrix; cdecl; external cDllName;// Returns a matrix for a given quaternion
function  QuaternionFromAxisAngle(axis : TVector3; angle : Single): TQuaternion; cdecl; external cDllName;// Returns rotation quaternion for an angle and axis
procedure QuaternionToAxisAngle(q : TQuaternion; uutAxis : PVector3; outAngle : PSingle); cdecl; external cDllName;// Returns the rotation angle and axis for a given quaternion
function  QuaternionFromEuler(pitch, yaw, roll : Single): TQuaternion; cdecl; external cDllName;// Returns the quaternion equivalent to Euler angles
function  QuaternionToEuler(q : TQuaternion): TVector3; cdecl; external cDllName;// Return the Euler angles equivalent to quaternion (roll, pitch, yaw)
function  QuaternionTransform(q : TQuaternion; mat : TMatrix): TQuaternion; cdecl; external cDllName;// Transform a quaternion given a transformation matrix
// Custom Misc Functions
function fMinf(a, b: Single):single;
function fMaxf(a, b: Single):single;
//sinf
//cosf
implementation

function fMinf(a, b: Single): single;
begin
   if a < b then
    Result := a
  else
    Result := b;
end;

function fMaxf(a, b: Single): single;
begin
   if a > b then
    Result := a
  else
    Result := b;
end;

end.
