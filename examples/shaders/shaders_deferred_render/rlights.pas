unit rlights;

interface

{$MINENUMSIZE 4}

uses
  raylib;

//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------
const
  MAX_LIGHTS = 4; // Max dynamic lights supported by shader

//----------------------------------------------------------------------------------
// Types and Structures Definition
//----------------------------------------------------------------------------------

type TLightType = (LIGHT_DIRECTIONAL, LIGHT_POINT);

// Light data
type TLight = record
  LightType: TLightType;
  Enabled: Boolean;
  Position: TVector3;
  Target: TVector3;
  Color: TColor;
  Attenuation: Single;

  // Shader locations
  EnabledLoc: Integer;
  TypeLoc: Integer;
  PositionLoc: Integer;
  TargetLoc: Integer;
  ColorLoc: Integer;
  AttenuationLoc: Integer;
end;

//----------------------------------------------------------------------------------
// Global Variables Definition
//----------------------------------------------------------------------------------
var LightsCount: Integer = 0; // Current amount of created lights

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

// Create a light and get shader locations
function CreateLight(LightType: TLightType; Position: TVector3; Target: TVector3; Color: TColor; Shader: TShader): TLight;
// Send light properties to shader
procedure UpdateLightValues(Shader: TShader; Light: TLight);

implementation

function CreateLight(LightType: TLightType; Position: TVector3; Target: TVector3; Color: TColor; Shader: TShader): TLight;
begin
  Result := Default(TLight);

  if LightsCount < MAX_LIGHTS then
  begin
    Result.Enabled := True;
    Result.LightType := LightType;
    Result.Position := Position;
    Result.Target := Target;
    Result.Color := Color;

    // NOTE: Lighting shader naming must be the provided ones
    Result.EnabledLoc := GetShaderLocation(Shader, TextFormat('lights[%i].enabled', LightsCount));
    Result.TypeLoc := GetShaderLocation(Shader, TextFormat('lights[%i].type', LightsCount));
    Result.PositionLoc := GetShaderLocation(Shader, TextFormat('lights[%i].position', LightsCount));
    Result.TargetLoc := GetShaderLocation(Shader, TextFormat('lights[%i].targe', LightsCount));
    Result.ColorLoc := GetShaderLocation(Shader, TextFormat('lights[%i].color', LightsCount));

    UpdateLightValues(Shader, Result);

    Inc(LightsCount);
  end;
end;

procedure UpdateLightValues(Shader: TShader; Light: TLight);
var
  Position, Target, Color: array of Single;
begin
  // Send to shader light enabled state and type
  SetShaderValue(Shader, Light.EnabledLoc, @Light.Enabled, SHADER_UNIFORM_INT);
  SetShaderValue(Shader, Light.TypeLoc, @Light.LightType, SHADER_UNIFORM_INT);

  // Send to shader light position values
  Position := [Light.Position.X, Light.Position.Y, Light.Position.Z];
  SetShaderValue(Shader, Light.PositionLoc, @Position[0], SHADER_UNIFORM_VEC3);

  // Send to shader light target position values
  Target := [Light.Target.X, Light.Target.Y, Light.Target.Z];
  SetShaderValue(Shader, Light.TargetLoc, @Target[0], SHADER_UNIFORM_VEC3);

  // Send to shader light color values
  Color := [Light.Color.R / 255, Light.Color.G / 255, Light.Color.B / 255, Light.Color.A / 255];
  SetShaderValue(Shader, Light.ColorLoc, @Color[0], SHADER_UNIFORM_VEC4);
end;

end.
