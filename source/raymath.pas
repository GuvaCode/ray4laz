{**********************************************************************************************
*
*   raymath v2.0 - Math functions to work with Vector2, Vector3, Matrix and Quaternions
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
*   Copyright (c) 2015-2023 Ramon Santamaria (@raysan5)
*   Pascal header 2021-2023 Gunko Vadim (@guvacode)
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

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I raylib.inc}

interface

uses raylib;

type
    PFloat3 = ^TFloat3;
    TFloat3 = record
     v: array[0..2] of Single;
    end;

    PFloat16 = ^TFloat16;
    TFloat16 = record
     v: array[0..15] of Single;
    end;

//----------------------------------------------------------------------------------
// Module Functions Definition - Utils math
//----------------------------------------------------------------------------------
{ Clamp float value }
function  Clamp(value, min, max : Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Clamp';
{ Calculate linear interpolation between two floats }
function  Lerp(start, end_, amount : Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Lerp';
{ Normalize input value within input range }
function  Normalize(value, start, end_: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Normalize';
{ Remap input value within input range to output range }
function  Remap(value, inputStart, inputEnd, outputStart, outputEnd : Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Remap';
{ Wrap input value from min to max }
function  Wrap(value, min, max: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Wrap';
{ Check whether two given floats are almost equal }
function  FloatEquals(x, y: Single): longint; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'FloatEquals';

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector2 math
//----------------------------------------------------------------------------------
{ Vector with components value 0.0 }
function Vector2Zero: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Zero';
{ Vector with components value 1.0 }
function Vector2One: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2One';
{ Add two vectors (v1 + v2) }
function Vector2Add(v1, v2 : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Add';
{ Add vector and float value }
function Vector2AddValue(v: TVector2; add: Single): TVector2;cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2AddValue';
{ Subtract two vectors (v1 - v2) }
function Vector2Subtract(v1, v2 : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Subtract';
{ Subtract vector by float value }
function Vector2SubtractValue( v: TVector2; sub:Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2SubtractValue';
{ Calculate vector length }
function Vector2Length(v : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Length';
{ Calculate vector square length }
function Vector2LengthSqr(v : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2LengthSqr';
{ Calculate two vectors dot product }
function Vector2DotProduct(v1, v2 : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2DotProduct';
{ Calculate distance between two vectors }
function Vector2Distance(v1, v2 : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Distance';
{ Calculate square distance between two vectors }
function Vector2DistanceSqr(v1, v2 : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2DistanceSqr';
{ Calculate angle from two vectors in X-axis }
function Vector2Angle(v1, v2 : TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Angle';
{ Calculate angle defined by a two vectors line }
function Vector2LineAngle(start, end_: TVector2): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2LineAngle';
{ Scale vector (multiply by value) }
function Vector2Scale(v : TVector2; scale : Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Scale';
{ Multiply vector by vector }
function Vector2Multiply(v1, v2 : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Multiply';
{ Negate vector }
function Vector2Negate(v : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Negate';
{ Divide vector by vector }
function Vector2Divide(v1, v2 : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Divide';
{ Normalize provided vector }
function Vector2Normalize(v : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Normalize';
{ Calculate linear interpolation between two vectors }
function Vector2Lerp(v1, v2 : TVector2; amount : Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Lerp';
{ Calculate reflected vector to normal }
function Vector2Reflect(v, normal : TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Reflect';
{Get min value for each pair of components}
function Vector2Min(v1, v2: TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Min';
{Get max value for each pair of components}
function Vector2Max(v1, v2: TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Max';
{ Rotate vector by angle }
function Vector2Rotate(v: TVector2; angle:Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Rotate';
{ Move Vector towards target }
function Vector2MoveTowards(v, target: TVector2; maxDistance: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2MoveTowards';
{ Invert the given vector }
function Vector2Invert(v: TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Invert';
{ Clamp the components of the vector between min and max values specified by the given vectors }
function Vector2Clamp(v, min, max: TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Clamp';
{ Clamp the magnitude of the vector between two min and max values }
function Vector2ClampValue(v, min, max: TVector2): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2ClampValue';
{ Check whether two given vectors are almost equal }
function Vector2Equals(p, q: TVector2): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Equals';
{ Compute the direction of a refracted ray }
// v: normalized direction of the incoming ray
// n: normalized normal vector of the interface of two optical media
// r: ratio of the refractive index of the medium from where the ray comes
// to the refractive index of the medium on the other side of the surface
function Vector2Refract(v, n: TVector2; r: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector2Refract';

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector3 math
//----------------------------------------------------------------------------------
{ Vector with components value 0.0 }
function Vector3Zero(): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Zero';
{ Vector with components value 1.0 }
function Vector3One(): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3One';
{ Add two vectors }
function Vector3Add(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Add';
{ Add vector and float value }
function Vector3AddValue(v: Tvector3; add: single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3AddValue';
{ Subtract two vectors }
function Vector3Subtract(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Subtract';
{ Subtract vector by float value }
function Vector3SubtractValue(v: TVector3; sub:Single): Tvector3;  cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3SubtractValue';
{ Multiply vector by scalar }
function Vector3Scale(a: TVector3; scalar:Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Scale';
{ Multiply vector by vector }
function Vector3Multiply(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Multiply';
{ Calculate two vectors cross product }
function Vector3CrossProduct(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3CrossProduct';
{ Calculate one vector perpendicular vector }
function Vector3Perpendicular(v : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Perpendicular';
{ Calculate vector length }
function Vector3Length(v : TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Length';
{ Calculate vector square length }
function Vector3LengthSqr(v: TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3LengthSqr';
{ Calculate two vectors dot product }
function Vector3DotProduct(v1, v2 : TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3DotProduct';
{ Calculate distance between two vectors }
function Vector3Distance(v1, v2 : TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Distance';
{ Calculate square distance between two vectors }
function Vector3DistanceSqr(v1, v2 : TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3DistanceSqr';
{ Calculate angle between two vectors in XY and XZ }
function Vector3Angle(v1, v2: TVector3): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Angle';
{ Negate provided vector (invert direction) }
function Vector3Negate(aV : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Negate';
{ Divide vector by vector }
function Vector3Divide(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Divide';
{ Normalize provided vector }
function Vector3Normalize(v : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Normalize';
{ Calculate the projection of the vector v1 on to v2 }
function Vector3Project(v1, v2: TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Project';
{ Calculate the rejection of the vector v1 on to v2 }
function Vector3Reject(v1, v2: TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Reject';
{ Orthonormalize provided vectors }
procedure Vector3OrthoNormalize(v1, v2 : PVector3); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3OrthoNormalize';
{ Transforms a Vector3 by a given Matrix }
function Vector3Transform(v : TVector3; mat : TMatrix): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Transform';
{ Transform a vector by quaternion rotation }
function Vector3RotateByQuaternion(v : TVector3; q : TQuaternion): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3RotateByQuaternion';
{ Rotates a vector around an axis }
function Vector3RotateByAxisAngle(v : TVector3; axis: TVector3; angle: single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3RotateByAxisAngle';
{ Move Vector towards target }
function Vector3MoveTowards(v, target: TVector3; maxDistance: Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3MoveTowards';
{ Calculate linear interpolation between two vectors }
function Vector3Lerp(v1, v2 : TVector3; amount : Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Lerp';
{Calculate cubic hermite interpolation between two vectors and their tangents
 as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic}
function Vector3CubicHermite(v1, tangent1, v2, tangent2: TVector3; amount: Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3CubicHermite';
{ Calculate reflected vector to normal }
function Vector3Reflect(v, normal : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Reflect';
{ Return min value for each pair of components }
function Vector3Min(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Min';
{ Return max value for each pair of components }
function Vector3Max(v1, v2 : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Max';
{ Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c) }
function Vector3Barycenter(p, a, b, c : TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Barycenter';
{ Projects a Vector3 from screen space into object space }
function Vector3Unproject(source:TVector3; projection:TMatrix; view:TMatrix):TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Unproject';
{ Returns Vector3 as float array }
function Vector3ToFloatV(v : TVector3): TFloat3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3ToFloatV';
{ Invert the given vector }
function Vector3Invert(v: TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Invert';
{ Clamp the components of the vector between min and max values specified by the given vectors }
function Vector3Clamp(v, min, max: TVector3): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Clamp';
{ Clamp the magnitude of the vector between two values }
function Vector3ClampValue(v: TVector3; min, max: Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3ClampValue';
{ Check whether two given vectors are almost equal }
function Vector3Equals(p, q: TVector3): longint; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Equals';
{ Compute the direction of a refracted ray where v specifies the normalized direction of the incoming ray, n specifies the
  normalized normal vector of the interface of two optical media, and r specifies the ratio of the refractive index of the medium
  from where the ray comes to the refractive index of the medium on the other side of the surface }
function Vector3Refract(v, n: TVector3; r: Single): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector3Refract';

//----------------------------------------------------------------------------------
// Module Functions Definition - Vector4 math
//----------------------------------------------------------------------------------
function Vector4Zero(): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Zero';
function Vector4One(): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4One';
function Vector4Add(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Add';
function Vector4AddValue(v: TVector4; add: Single): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4AddValue';
function Vector4Subtract(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Subtract';
function Vector4SubtractValue(v: TVector4; add: Single): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4SubtractValue';
function Vector4Length(v: TVector4): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Length';
function Vector4LengthSqr(v: TVector4): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4LengthSqr';
function Vector4DotProduct(v1, v2: TVector4): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4DotProduct';
{ Calculate distance between two vectors }
function Vector4Distance(v1, v2: TVector4): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Distance';
{ Calculate square distance between two vectors }
function Vector4DistanceSqr(v1, v2: TVector4): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4DistanceSqr';
function Vector4Scale(v: TVector4; scale: Single): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Scale';
{ Multiply vector by vector }
function Vector4Multiply(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Multiply';
{ Negate vector }
function Vector4Negate(v: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Negate';
{ Divide vector by vector }
function Vector4Divide(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Divide';
{ Normalize provided vector }
function Vector4Normalize(v: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Normalize';
{ Get min value for each pair of components }
function Vector4Min(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Min';
{ Get max value for each pair of components }
function Vector4Max(v1, v2: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Max';
{ Calculate linear interpolation between two vectors }
function Vector4Lerp(v1, v2: TVector4; amount: Single): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Lerp';
{ Move Vector towards target }
function Vector4MoveTowards(v, target: TVector4; maxDistance: Single): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4MoveTowards';
{ Invert the given vector }
function Vector4Invert(v: TVector4): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Invert';
{ Check whether two given vectors are almost equal }
function Vector4Equals(p, q: TVector4): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Vector4Equals';

//----------------------------------------------------------------------------------
// Module Functions Definition - Matrix math
//----------------------------------------------------------------------------------
{ Compute matrix determinant }
function MatrixDeterminant(mat : TMatrix) : Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixDeterminant';
{ Returns the trace of the matrix (sum of the values along the diagonal) }
function MatrixTrace(mat : TMatrix): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixTrace';
{ Transposes provided matrix }
function MatrixTranspose(mat : TMatrix): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixTranspose';
{ Invert provided matrix }
function MatrixInvert(mat : TMatrix): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixInvert';
{ Returns identity matrix }
function MatrixIdentity: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixIdentity';
{ Add two matrices }
function MatrixAdd(left : TMatrix; right : TMatrix): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixAdd';
{ Subtract two matrices (left - right) }
function MatrixSubtract(left : TMatrix; right : TMatrix): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixSubtract';
{ Returns two matrix multiplication }
function MatrixMultiply(left : TMatrix; right : TMatrix): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixMultiply';
{ Returns translation matrix }
function MatrixTranslate(x, y, z : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixTranslate';
{ Create rotation matrix from axis and angle }
function MatrixRotate(axis : TVector3; angle : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotate';
{ Returns x-rotation matrix (angle in radians) }
function MatrixRotateX(angle : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotateX';
{ Returns y-rotation matrix (angle in radians) }
function MatrixRotateY(angle : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotateY';
{ Returns z-rotation matrix (angle in radians) }
function MatrixRotateZ(angle : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotateZ';
{ Returns xyz-rotation matrix (angles in radians) }
function MatrixRotateXYZ(angle : TVector3): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotateXYZ';
{ Returns zyx-rotation matrix (angles in radians) }
function MatrixRotateZYX(angle : TVector3): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixRotateZYX';
{ Returns scaling matrix }
function MatrixScale(x, y, z : Single): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixScale';
{ Returns perspective projection matrix }
function MatrixFrustum(left, right, bottom, top, nearPlane, farPlane : double): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixFrustum';
{ Returns perspective projection matrix }
function MatrixPerspective(fovY, aspect, nearPlane, farPlane : double): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixPerspective';
{ Returns orthographic projection matrix }
function MatrixOrtho(left, right, bottom, top, nearPlane, farPlane : double): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixOrtho';
{ Returns camera look-at matrix (view matrix) }
function MatrixLookAt(eye, target, up : TVector3): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixLookAt';
{ Returns float array of matrix data }
function MatrixToFloatV(mat : TMatrix): TFloat16; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MatrixToFloatV';

//----------------------------------------------------------------------------------
// Module Functions Definition - Quaternion math
//----------------------------------------------------------------------------------
{ Add two quaternions }
function QuaternionAdd(q1, q2: TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionAdd';
{ Add quaternion and float value }
function QuaternionAddValue(q: TQuaternion ; add: single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionAddValue';
{ Subtract two quaternions }
function QuaternionSubtract(q1, q2: TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionSubtract';
{ Subtract quaternion and float value }
function QuaternionSubtractValue(q:TQuaternion; sub:single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionSubtractValue';
{ Returns identity quaternion }
function QuaternionIdentity(): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionIdentity';
{ Computes the length of a quaternion }
function QuaternionLength(q : TQuaternion): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionLength';
{ Normalize provided quaternion }
function QuaternionNormalize(q : TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionNormalize';
{ Invert provided quaternion }
function QuaternionInvert(q : TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionInvert';
{ Calculate two quaternion multiplication }
function QuaternionMultiply(q1, q2 : TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionMultiply';
{ Scale quaternion by float value }
function QuaternionScale(q:TQuaternion;mul:single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionScale';
{ Divide two quaternions }
function QuaternionDivide(q1, q2: TQuaternion): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionDivide';
{ Calculate linear interpolation between two quaternions }
function QuaternionLerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionLerp';
{ Calculate slerp-optimized interpolation between two quaternions }
function QuaternionNlerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionNlerp';
{ Calculates spherical linear interpolation between two quaternions }
function QuaternionSlerp(q1, q2 : TQuaternion; amount : Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionSlerp';
{ Calculate quaternion cubic spline interpolation using Cubic Hermite Spline algorithm
 as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic}
function QuaternionCubicHermiteSpline(q1, outTangent1, q2, inTangent2: TQuaternion; t: Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionCubicHermiteSpline';
{ Calculate quaternion based on the rotation from one vector to another }
function QuaternionFromVector3ToVector3(from, to_ : TVector3): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionFromVector3ToVector3';
{ Returns a quaternion for a given rotation matrix }
function QuaternionFromMatrix(mat : TMatrix): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionFromMatrix';
{ Returns a matrix for a given quaternion }
function QuaternionToMatrix(q : TQuaternion): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionToMatrix';
{ Returns rotation quaternion for an angle and axis }
function QuaternionFromAxisAngle(axis : TVector3; angle : Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionFromAxisAngle';
{ Returns the rotation angle and axis for a given quaternion }
procedure QuaternionToAxisAngle(q : TQuaternion; uutAxis : PVector3; outAngle : PSingle); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionToAxisAngle';
{ Returns the quaternion equivalent to Euler angles }
function QuaternionFromEuler(pitch, yaw, roll : Single): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionFromEuler';
{ Return the Euler angles equivalent to quaternion (roll, pitch, yaw) }
function QuaternionToEuler(q : TQuaternion): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionToEuler';
{ Transform a quaternion given a transformation matrix }
function QuaternionTransform(q : TQuaternion; mat : TMatrix): TQuaternion; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionTransform';
{ Check whether two given quaternions are almost equal }
function QuaternionEquals(p, q: TQuaternion): longint; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'QuaternionEquals';

// Custom Misc Functions
function fMinf(a, b: Single):single;
function fMaxf(a, b: Single):single;

implementation
uses Math;

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

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.
