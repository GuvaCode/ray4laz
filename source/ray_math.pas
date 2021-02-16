{*********************************************************************************************
*
*   raymath  - Math functions to work with Vector3, Matrix and Quaternions
*
*   CONFIGURATION:
*
*   #define RAYMATH_IMPLEMENTATION
*       Generates the implementation of the library into the included file.
*       If not defined, the library is in header only mode and can be included in other headers
*       or source files without problems. But only ONE file should hold the implementation.
*
*   #define RAYMATH_HEADER_ONLY
*       Define static inline functions code, so #include header suffices for use.
*       This may use up lots of memory.
*
*   #define RAYMATH_STANDALONE
*       Avoid raylib.h header inclusion in this file.
*       Vector3 and Matrix data types are defined internally in raymath module.
*
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2015-2021 Ramon Santamaria (@raysan5)
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
unit ray_math;

interface

uses ray_header;

type
    TFloat3 = record
        f1 : Single;
        f2 : Single;
        f3 : Single;
    end;

    TFloat16 = record
        f1 : single;
        f2 : single;
        f3 : single;
        f4 : single;
        f5 : single;
        f6 : single;
        f7 : single;
        f8 : single;
        f9 : single;
        f10 : single;
        f11 : single;
        f12 : single;
        f13 : single;
        f14 : single;
        f15 : single;
        f16 : single;
    end;

function  Clamp(aValue, aMin, aMax : Single): Single; cdecl; external cDllName;
function  Lerp(aStart, aEnd, aAmount : Single): Single; cdecl; external cDllName;
function  Vector2Zero(): TVector2; cdecl; external cDllName;
function  Vector2One(): TVector2; cdecl; external cDllName;
function  Vector2Add(aV1, aV2 : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Subtract(aV1, aV2 : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Length(aV : TVector2): Single; cdecl; external cDllName;
function  Vector2DotProduct(aV1, aV2 : TVector2): Single; cdecl; external cDllName;
function  Vector2Distance(aV1, aV2 : TVector2): Single; cdecl; external cDllName;
function  Vector2Angle(aV1, aV2 : TVector2): Single; cdecl; external cDllName;
function  Vector2Scale(aV : TVector2; aScale : Single): TVector2; cdecl; external cDllName;
function  Vector2MultiplyV(aV1, aV2 : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Negate(aV : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Divide(aV : TVector2; aDiv : Single): TVector2; cdecl; external cDllName;
function  Vector2DivideV(aV1, aV2 : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Normalize(aV : TVector2): TVector2; cdecl; external cDllName;
function  Vector2Lerp(aV1, aV2 : TVector2; aAmount : Single): TVector2; cdecl; external cDllName;
function  Vector3Zero(): TVector3; cdecl; external cDllName;
function  Vector3One(): TVector3; cdecl; external cDllName;
function  Vector3Add(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Subtract(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Multiply(aV : TVector3; aScalar : Single): TVector3; cdecl; external cDllName;
function  Vector3MultiplyV(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3CrossProduct(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Perpendicular(aV : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Length(aV : TVector3): Single; cdecl; external cDllName;
function  Vector3DotProduct(aV1, aV2 : TVector3): Single; cdecl; external cDllName;
function  Vector3Distance(aV1, aV2 : TVector3): Single; cdecl; external cDllName;
function  Vector3Scale(aV : TVector3; aScale : Single): TVector3; cdecl; external cDllName;
function  Vector3Negate(aV : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Divide(aV : TVector3; aDiv : Single): TVector3; cdecl; external cDllName;
function  Vector3DivideV(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Normalize(aV : TVector3): TVector3; cdecl; external cDllName;
procedure Vector3OrthoNormalize(aV1, aV2 : PVector3); cdecl; external cDllName;
function  Vector3Transform(aV : TVector3; aMat : TMatrix): TVector3; cdecl; external cDllName;
function  Vector3RotateByQuaternion(aV : TVector3; aQ : TQuaternion): TVector3; cdecl; external cDllName;
function  Vector3Lerp(aV1, aV2 : TVector3; aAmount : Single): TVector3; cdecl; external cDllName;
function  Vector3Reflect(aV, aNormal : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Min(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Max(aV1, aV2 : TVector3): TVector3; cdecl; external cDllName;
function  Vector3Barycenter(aP, aA, aB, aC : TVector3): TVector3; cdecl; external cDllName;
function  Vector3ToFloatV(aV : TVector3): TFloat3; cdecl; external cDllName;
function  MatrixDeterminant(amat : TMatrix) : Single; cdecl; external cDllName;
function  MatrixTrace(mat : TMatrix): Single; cdecl; external cDllName;
function  MatrixTranspose(amat : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixInvert(amat : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixNormalize(amat : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixIdentity(): TMatrix; cdecl; external cDllName;
function  MatrixAdd(aleft : TMatrix; aright : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixSubtract(aleft : TMatrix; aright : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixTranslate(aX, aY, aZ : Single): TMatrix; cdecl; external cDllName;
function  MatrixRotate(aAxis : TVector3; aAngle : Single): TMatrix; cdecl; external cDllName;
function  MatrixRotateXYZ(aAng : TVector3): TMatrix; cdecl; external cDllName;
function  MatrixRotateX(aAngle : Single): TMatrix; cdecl; external cDllName;
function  MatrixRotateY(aAngle : Single): TMatrix; cdecl; external cDllName;
function  MatrixRotateZ(aAngle : Single): TMatrix; cdecl; external cDllName;
function  MatrixScale(aX, aY, aZ : Single): TMatrix; cdecl; external cDllName;
function  MatrixMultiply(aLeft : TMatrix; aRight : TMatrix): TMatrix; cdecl; external cDllName;
function  MatrixFrustum(aLeft, aRight, aBottom, aTop, aNear, aFar : Double): TMatrix; cdecl; external cDllName;
function  MatrixPerspective(aFovy, aAspect, aNear, aFar : Double): TMatrix; cdecl; external cDllName;
function  MatrixOrtho(aLeft, aRight, aBottom, aTop, aNear, aFar : Double): TMatrix; cdecl; external cDllName;
function  MatrixLookAt(aEye, aTarget, aUp : TVector3): TMatrix; cdecl; external cDllName;
function  MatrixToFloatV(aMat : TMatrix): TFloat16; cdecl; external cDllName;
function  QuaternionIdentity(): TQuaternion; cdecl; external cDllName;
function  QuaternionLength(aQ : TQuaternion): Single; cdecl; external cDllName;
function  QuaternionNormalize(aQ : TQuaternion): TQuaternion; cdecl; external cDllName;
function  QuaternionInvert(aQ : TQuaternion): TQuaternion; cdecl; external cDllName;
function  QuaternionMultiply(aQ1, aQ2 : TQuaternion): TQuaternion; cdecl; external cDllName;
function  QuaternionLerp(aQ1, aQ2 : TQuaternion; aAmount : Single): TQuaternion; cdecl; external cDllName;
function  QuaternionNlerp(aQ1, aQ2 : TQuaternion; aAmount : Single): TQuaternion; cdecl; external cDllName;
function  QuaternionSlerp(aQ1, aQ2 : TQuaternion; aAmount : Single): TQuaternion; cdecl; external cDllName;
function  QuaternionFromVector3ToVector3(aFrom, aTo : TVector3): TQuaternion; cdecl; external cDllName;
function  QuaternionFromMatrix(aMat : TMatrix): TQuaternion; cdecl; external cDllName;
function  QuaternionToMatrix(aQ : TQuaternion): TMatrix; cdecl; external cDllName;
function  QuaternionFromAxisAngle(aAxis : TVector3; aAngle : Single): TQuaternion; cdecl; external cDllName;
procedure QuaternionToAxisAngle(aQ : TQuaternion; aOutAxis : PVector3; aOutAngle : PSingle); cdecl; external cDllName;
function  QuaternionFromEuler(aRoll, aPitch, aYaw : Single): TQuaternion; cdecl; external cDllName;
function  QuaternionToEuler(aQ : TQuaternion): TVector3; cdecl; external cDllName;
function  QuaternionTransform(aQ : TQuaternion; aMat : TMatrix): TQuaternion; cdecl; external cDllName;

implementation

end.
