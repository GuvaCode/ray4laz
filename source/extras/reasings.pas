(*******************************************************************************************
*
*   reasings - raylib easings library, based on Robert Penner library
*
*   Useful easing functions for values animation
*
*   This header uses:
*       #define REASINGS_STATIC_INLINE      // Inlines all functions code, so it runs faster.
*                                           // This requires lots of memory on system.
*   How to use:
*   The four inputs t,b,c,d are defined as follows:
*   t = current time (in any unit measure, but same unit as duration)
*   b = starting value to interpolate
*   c = the total change in value of b that needs to occur
*   d = total time it should take to complete (duration)
*
*   Example:
*
*   int currentTime = 0;
*   int duration = 100;
*   float startPositionX = 0.0f;
*   float finalPositionX = 30.0f;
*   float currentPositionX = startPositionX;
*
*   while (currentPositionX < finalPositionX)
*   {
*       currentPositionX = EaseSineIn(currentTime, startPositionX, finalPositionX - startPositionX, duration);
*       currentTime++;
*   }
*
*   A port of Robert Penner's easing equations to C (http://robertpenner.com/easing/)
*
*   Robert Penner License
*   ---------------------------------------------------------------------------------
*   Open source under the BSD License.
*
*   Copyright (c) 2001 Robert Penner. All rights reserved.
*
*   Redistribution and use in source and binary forms, with or without modification,
*   are permitted provided that the following conditions are met:
*
*       - Redistributions of source code must retain the above copyright notice,
*         this list of conditions and the following disclaimer.
*       - Redistributions in binary form must reproduce the above copyright notice,
*         this list of conditions and the following disclaimer in the documentation
*         and/or other materials provided with the distribution.
*       - Neither the name of the author nor the names of contributors may be used
*         to endorse or promote products derived from this software without specific
*         prior written permission.
*
*   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
*   IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
*   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
*   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
*   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
*   OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
*   OF THE POSSIBILITY OF SUCH DAMAGE.
*   ---------------------------------------------------------------------------------
*
*   Copyright (c) 2015-2022 Ramon Santamaria (@raysan5)
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
*   ---------------------------------------------------------------------------------
*
*   Copyright (c) 2022-2022 Turborium (@turborium)
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
**********************************************************************************************)

// WARNING:
// This file is not part of Raylib, it is a Pascal translation of one of the add-on modules.
// Accordingly, this file is supplied under the license of the original.
// This file has not been properly tested.

unit reasings;

interface

// Linear Easing functions
function EaseLinearNone(T, B, C, D: Single): Single; // Ease: Linear
function EaseLinearIn(T, B, C, D: Single): Single; // Ease: Linear In
function EaseLinearOut(T, B, C, D: Single): Single; // Ease: Linear Out
function EaseLinearInOut(T, B, C, D: Single): Single; // Ease: Linear In Out

// Sine Easing functions
function EaseSineIn(T, B, C, D: Single): Single; // Ease: Sine In
function EaseSineOut(T, B, C, D: Single): Single; // Ease: Sine Out
function EaseSineInOut(T, B, C, D: Single): Single; // Ease: Sine In Out

// Circular Easing functions
function EaseCircIn(T, B, C, D: Single): Single; // Ease: Circular In
function EaseCircOut(T, B, C, D: Single): Single; // Ease: Circular Out
function EaseCircInOut(T, B, C, D: Single): Single; // Ease: Circular In Out

// Cubic Easing functions
function EaseCubicIn(T, B, C, D: Single): Single; // Ease: Cubic In
function EaseCubicOut(T, B, C, D: Single): Single; // Ease: Cubic Out
function EaseCubicInOut(T, B, C, D: Single): Single; // Ease: Cubic In Out

// Quadratic Easing functions
function EaseQuadIn(T, B, C, D: Single): Single; // Ease: Quadratic In
function EaseQuadOut(T, B, C, D: Single): Single; // Ease: Quadratic Out
function EaseQuadInOut(T, B, C, D: Single): Single; //Ease: Quadratic In Out

// Exponential Easing functions
function EaseExpoIn(T, B, C, D: Single): Single; // Ease: Exponential In
function EaseExpoOut(T, B, C, D: Single): Single; // Ease: Exponential Out
function EaseExpoInOut(T, B, C, D: Single): Single; // Ease: Exponential In Out

// Back Easing functions
function EaseBackIn(T, B, C, D: Single): Single; // Ease: Back In
function EaseBackOut(T, B, C, D: Single): Single; // Ease: Back Out
function EaseBackInOut(T, B, C, D: Single): Single; // Ease: Back In Out

// Bounce Easing functions
function EaseBounceOut(T, B, C, D: Single): Single; // Ease: Bounce Out
function EaseBounceIn(T, B, C, D: Single): Single; // Ease: Bounce In
function EaseBounceInOut(T, B, C, D: Single): Single; // Ease: Bounce In Out

// Elastic Easing functions
function EaseElasticIn(T, B, C, D: Single): Single; //  Ease: Elastic In
function EaseElasticOut(T, B, C, D: Single): Single; // Ease: Elastic Out
function EaseElasticInOut(T, B, C, D: Single): Single; // Ease: Elastic In Out

implementation

uses
  Math;

// Linear Easing functions
function EaseLinearNone(T, B, C, D: Single): Single;
begin
  Result := C * T / D + B;
end;

function EaseLinearIn(T, B, C, D: Single): Single;
begin
  Result := C * T / D + B;
end;

function EaseLinearOut(T, B, C, D: Single): Single;
begin
  Result := C * T / D + B;
end;

function EaseLinearInOut(T, B, C, D: Single): Single;
begin
  Result := C * T / D + B;
end;

// Sine Easing functions
function EaseSineIn(T, B, C, D: Single): Single;
begin
  Result := -C * Cos(T / D * (PI / 2.0)) + C + B;
end;

function EaseSineOut(T, B, C, D: Single): Single;
begin
  Result := C * Sin(T / D * (PI / 2.0)) + B;
end;

function EaseSineInOut(T, B, C, D: Single): Single;
begin
  Result := -C / 2.0 * (Cos(PI * T / D) - 1.0) + B;
end;

// Circular Easing functions
function EaseCircIn(T, B, C, D: Single): Single;
begin
  T := T / D;
  Result := -C * (Sqrt(1.0 - T * T) - 1.0) + B;
end;

function EaseCircOut(T, B, C, D: Single): Single;
begin
  T := T / D - 1.0;
  Result := C * Sqrt(1.0 - T * T) + B;
end;

function EaseCircInOut(T, B, C, D: Single): Single;
begin
  T := T / (D / 2.0);
  if T < 1.0 then
    exit(-C / 2.0 * (Sqrt(1.0 - T * T) - 1.0) + B);
  T := T - 2.0;
  exit(C / 2.0 * (Sqrt(1.0 - T * T) + 1.0) + B);
end;

// Cubic Easing functions
function EaseCubicIn(T, B, C, D: Single): Single;
begin
  T := T / D;
  exit(C * T * T * T + B);
end;

function EaseCubicOut(T, B, C, D: Single): Single;
begin
  T := T / D - 1.0;
  exit(C * (T * T * T + 1.0) + B);
end;

function EaseCubicInOut(T, B, C, D: Single): Single;
begin
  T := T / (D / 2.0);
  if T < 1.0 then
    exit(C / 2.0 * T * T * T + B);
  T := T - 2.0;
  exit(C / 2.0 * (T * T * T + 2.0) + B);
end;

// Quadratic Easing functions
function EaseQuadIn(T, B, C, D: Single): Single;
begin
  T := T / D;
  exit(C * T * T + B);
end;

function EaseQuadOut(T, B, C, D: Single): Single;
begin
  T := T / D;
  exit(-C * T * (T - 2.0) + B);
end;

function EaseQuadInOut(T, B, C, D: Single): Single;
begin
  T := T / (D / 2.0);
  if T < 1.0 then
    exit(((C / 2.0) * (T * T)) + B);
  exit(-C / 2.0 * (((T - 1.0) * (T - 3.0)) - 1.0) + B);
end;

// Exponential Easing functions
function EaseExpoIn(T, B, C, D: Single): Single;
begin
  if T = 0.0 then
    exit(B)
  else
    exit(C * Power(2.0, 10.0 * (T / D - 1.0)) + B);
end;

function EaseExpoOut(T, B, C, D: Single): Single;
begin
  if T = D then
    exit(B + C)
  else
    exit(C * (-Power(2.0, -10.0 * T / D) + 1.0) + B);
end;

function EaseExpoInOut(T, B, C, D: Single): Single;
begin
  if T = 0.0 then
    exit(B);
  if T = D then
    exit(B + C);
  T := T / (D / 2.0);
  if T < 1.0 then
    exit(C / 2.0 * Power(2.0, 10.0 * (T - 1.0)) + B);

  exit(C / 2.0 * (-Power(2.0, -10.0 * (T - 1.0)) + 2.0) + B);
end;

// Back Easing functions
function EaseBackIn(T, B, C, D: Single): Single;
var
  S, PostFix: Single;
begin
  S := 1.70158;
  T := T / D;
  PostFix := T;
  exit(C * (PostFix) * T * ((S + 1.0) * T - S) + B);
end;

function EaseBackOut(T, B, C, D: Single): Single;
var
  S: Single;
begin
  S := 1.70158;
  T := T / D - 1.0;
  exit(C * (T * T * ((S + 1.0) * T + S) + 1.0) + B);
end;

function EaseBackInOut(T, B, C, D: Single): Single;
var
  S, PostFix: Single;
begin
  S := 1.70158;
  T := T / (D / 2.0);
  if T < 1.0 then
  begin
    S := S * 1.525;
    exit(C / 2.0 * (T * T * ((S + 1.0) * T - S)) + B);
  end;

  T := T - 2.0;
  PostFix := T;
  S := S * 1.525;
  exit(C / 2.0 * ((PostFix) * T * ((S + 1.0) * T + S) + 2.0) + B);
end;

// Bounce Easing functions
function EaseBounceOut(T, B, C, D: Single): Single;
var
  PostFix: Single;
begin
  T := T / D;
  if T < (1.0 / 2.75) then
  begin
    exit(C * (7.5625 * T * T) + B);
  end
  else if T < (2.0 / 2.75) then
  begin
    T := T - (1.5 / 2.75);
    PostFix := T;
    exit(C * (7.5625 * (PostFix) * T + 0.75) + B);
  end
  else if T < (2.5/2.75) then
  begin
    T := T - (2.25 / 2.75);
    PostFix := T;
    exit(C * (7.5625 * (PostFix) * T + 0.9375) + B);
  end
  else
  begin
    T := T - (2.625 /2.75);
    PostFix := T;
    exit(C * (7.5625 * (PostFix) * T + 0.984375) + B);
  end;
end;

function EaseBounceIn(T, B, C, D: Single): Single;
begin
  exit(C - EaseBounceOut(D - T, 0.0, C, D) + B);
end;

function EaseBounceInOut(T, B, C, D: Single): Single;
begin
  if T < D / 2.0 then
    exit(EaseBounceIn(T * 2.0, 0.0, C, D) * 0.5 + B)
  else
    exit(EaseBounceOut(T * 2.0 - D, 0.0, C, D) * 0.5 + C * 0.5 + B);
end;

// Elastic Easing functions
function EaseElasticIn(T, B, C, D: Single): Single;
var
  P, A, S, PostFix: Single;
begin
  if t = 0.0 then
    exit(B);
  T := T / D;
  if T = 1.0 then
    exit(B + C);
  P := D * 0.3;
  A := c;
  S := P / 4.0;
  T := T - 1.0;
  PostFix := A * Power(2.0, 10.0 * T);
  exit(-(PostFix * Sin((T * D - S) * (2.0 * PI) / P)) + B);
end;

function EaseElasticOut(T, B, C, D: Single): Single;
var
  P, A, S: Single;
begin
  if T = 0.0 then
    exit(B);
  T := T / D;
  if T = 1.0 then
    exit(B + C);
  P := D  *0.3;
  A := C;
  S := P / 4.0;
  exit(A * Power(2.0, -10.0 * T) * Sin((T * D - S) * (2.0 * PI) / P) + C + B);
end;

function EaseElasticInOut(T, B, C, D: Single): Single;
var
  P, A, S, PostFix: Single;
begin
  if T = 0.0 then
    exit(B);
  T := T / (D / 2.0);
  if T = 2.0 then
    exit(B + C);
  P := D * (0.3 * 1.5);
  A := C;
  S := P / 4.0;

  if T < 1.0 then
  begin
    T := T - 1.0;
    PostFix := A * Power(2.0, 10.0 * T);
    exit(-0.5 * (PostFix * Sin((T * D - S) * (2.0 * PI) / P)) + B);
  end;

  T := T - 1.0;
  PostFix := A * Power(2.0, -10.0 * T);

  exit(PostFix * Sin((T * D - S) * (2.0 * PI) / P) * 0.5 + C + B);
end;

end.
