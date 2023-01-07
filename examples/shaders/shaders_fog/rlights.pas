{**********************************************************************************************
*
*   raylib.lights - Some useful functions to deal with lights data
*
*   CONFIGURATION:
*
*   #define RLIGHTS_IMPLEMENTATION
*       Generates the implementation of the library into the included file.
*       If not defined, the library is in header only mode and can be included in other headers
*       or source files without problems. But only ONE file should hold the implementation.
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2017-2020 Victor Fisac (@victorfisac) and Ramon Santamaria (@raysan5)
*   Pascal conversion (c) 2021 Gunko Vadim (@guvacode)
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
unit rlights;

{$mode ObjFPC}{$H+}

interface

uses
  raylib, sysutils;

const
  MAX_LIGHTS = 4;         // Max dynamic lights supported by shader

type
  TLight = record
    type_: Integer;
    enabled: Boolean;
    position: TVector3;
    target: TVector3;
    color: TColorB;
    attenuation: Single;

    // Shader locations
    enabledLoc: Integer;
    typeLoc: Integer;
    positionLoc: Integer;
    targetLoc: Integer;
    colorLoc: Integer;
    attenuationLoc: Integer;
  end;

 type
  PLightType = ^TLightType;
  TLightType = Integer;
   const
     LIGHT_DIRECTIONAL = 0;
     LIGHT_POINT = 1;

 function CreateLight(type_:Integer; position, target: TVector3; color: TColorB; shader: TShader): TLight; // Create a light and get shader locations
 procedure UpdateLightValues(shader: TShader; light: TLight); // Send light properties to shader

implementation
var lightsCount: longint = 0;    // Current amount of created lights

function CreateLight(type_: Integer; position, target: TVector3; color: TColorB; shader: TShader): TLight;
var light:TLight;
begin
  if (lightsCount < MAX_LIGHTS) then
   begin
       light.enabled := true;
       light.type_ := type_;
       light.position := position;
       light.target := target;
       light.color := color;

       // NOTE: Lighting shader naming must be the provided ones
       light.enabledLoc := GetShaderLocation(shader, TextFormat('lights[%i].enabled', lightsCount));
       light.typeLoc := GetShaderLocation(shader, TextFormat('lights[%i].type', lightsCount));
       light.positionLoc := GetShaderLocation(shader, TextFormat('lights[%i].position', lightsCount));
       light.targetLoc := GetShaderLocation(shader, TextFormat('lights[%i].target', lightsCount));
       light.colorLoc := GetShaderLocation(shader, TextFormat('lights[%i].color', lightsCount));

       UpdateLightValues(shader, light);

       Inc(lightsCount);
   end;
   result := light;
end;


// Send light properties to shader
// NOTE: Light shader locations should be available
procedure UpdateLightValues(shader: TShader; light: TLight);
var   position, target: array [0..2] of single;
      color: array [0..3] of single;
begin
  // Send to shader light enabled state and type
  SetShaderValue(shader, light.enabledLoc, @light.enabled, SHADER_UNIFORM_INT);
  SetShaderValue(shader, light.typeLoc, @light.type_, SHADER_UNIFORM_INT);
  // Send to shader light position values
  position[0]:= light.position.x;
  position[1]:= light.position.y;
  position[2]:= light.position.z;
  SetShaderValue(shader, light.positionLoc, @position, SHADER_UNIFORM_VEC3);
  // Send to shader light target position values
  target[0]:= light.target.x;
  target[1]:= light.target.y;
  target[2]:= light.target.z;
  SetShaderValue(shader, light.targetLoc, @target, SHADER_UNIFORM_VEC3);
  // Send to shader light color values
  color[0]:= light.color.r/255;
  color[1]:= light.color.g/255;
  color[2]:= light.color.b/255;
  color[3]:= light.color.a/255;
  SetShaderValue(shader, light.colorLoc, @color, SHADER_UNIFORM_VEC4);
end;

end.

