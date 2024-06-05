unit raylights;
{$mode objfpc}{$H+}


interface

uses
  raylib, raymath, rlgl, cmem, math;

type
  PRLG_LightType = ^TRLG_LightType;
  TRLG_LightType =  Integer;
  const
    RLG_DIRECTIONAL = TRLG_LightType(0);
    RLG_OMNILIGHT   = TRLG_LightType(1);
    RLG_SPOTLIGHT   = TRLG_LightType(2);

type
  PRLG_GlobalLightLocs = ^TRLG_GlobalLightLocs;
  TRLG_GlobalLightLocs = record
    colSpecular: Integer;
    colAmbient: Integer;
    viewPos: Integer;
    shininess: Integer;
    useSpecularMap: Integer;
    useNormalMap: Integer;
  end;

  PRLG_GlobalLight = ^TRLG_GlobalLight;
  TRLG_GlobalLight = record
    colSpecular: TVector3;
    colAmbient: TVector3;
    viewPos: TVector3;
    shininess: Single;
    useSpecularMap: Integer;
    useNormalMap: Integer;
  end;

  PRLG_ShadowMap = ^TRLG_ShadowMap;
  TRLG_ShadowMap = record
    depth: TTexture2D;
    id: LongWord;
    width, height: Integer;
  end;

  PRLG_LightLocs = ^TRLG_LightLocs;
  TRLG_LightLocs = record
    shadowMap: Integer;
    matrix: Integer;
    position: Integer;
    direction: Integer;
    diffuse: Integer;
    specular: Integer;
    innerCutOff: Integer;
    outerCutOff: Integer;
    constant: Integer;
    linear: Integer;
    quadratic: Integer;
    shadowMapTxlSz: Integer;
    depthBias: Integer;
    type_: Integer;
    shadow: Integer;
    enabled: Integer;
   end;

   PRLG_Light = ^TRLG_Light;
   TRLG_Light = record
     shadowMap: TRLG_ShadowMap;
     position: TVector3;
     direction: TVector3;
     diffuse: TVector3;
     specular: TVector3;
     innerCutOff: Single;
     outerCutOff: Single;
     constant: Single;
     linear: Single;
     quadratic: Single;
     shadowMapTxlSz: Single;
     depthBias: Single;
     type_: Integer;
     shadow: Integer;
     enabled: Integer;
   end;

   PRLG_ShadowMapShaderData = ^TRLG_ShadowMapShaderData;
   TRLG_ShadowMapShaderData = record
     near, far: Single;
     locNear, locFar: Integer;
   end;

   PRLG_Core = ^TRLG_Core;
   TRLG_Core = record
     locsGlobalLight: TRLG_GlobalLightLocs;
     globalLight: TRLG_GlobalLight;
     locsLights: PRLG_LightLocs;
     lights: PRLG_Light;
     lightCount: Integer;
     shadowMapShaderData: TRLG_ShadowMapShaderData;
     lightShader: TShader;
     depthShader: TShader;
     shadowMapShader: TShader;
   end;

const
  //static const char rlgLightVS[] = GLSL_VERSION_DEF
  //Ln = +#10+#13+;

  rlgLightVs =
    '#version 330' +#10+#13+
    'in vec3 vertexPosition;' +#10+#13+
    'in vec2 vertexTexCoord;' +#10+#13+
    'in vec4 vertexTangent;' +#10+#13+
    'in vec3 vertexNormal;' +#10+#13+
    'in vec4 vertexColor;' +#10+#13+

    'uniform lowp int useNormalMap;' +#10+#13+
    'uniform mat4 matNormal;' +#10+#13+
    'uniform mat4 matModel;' +#10+#13+
    'uniform mat4 mvp;' +#10+#13+


    'out vec3 fragPosition;' +#10+#13+
    'out vec2 fragTexCoord;' +#10+#13+
    'out vec3 fragNormal;' +#10+#13+
    'out vec4 fragColor;' +#10+#13+
    'out mat3 TBN;' +#10+#13+


    'void main()' +#10+#13+
    '{' +#10+#13+
        'fragPosition = vec3(matModel*vec4(vertexPosition, 1.0));' +#10+#13+
        'fragNormal = (matNormal*vec4(vertexNormal, 0.0)).xyz;' +#10+#13+

        'fragTexCoord = vertexTexCoord;' +#10+#13+
        'fragColor = vertexColor;' +#10+#13+

        'if (useNormalMap != 0)' +#10+#13+
        '{' +#10+#13+
            'vec3 T = normalize(vec3(matModel*vec4(vertexTangent.xyz, 0.0)));' +#10+#13+
            'vec3 B = cross(fragNormal, T)*vertexTangent.w;' +#10+#13+
            'TBN = mat3(T, B, fragNormal);' +#10+#13+
        '}' +#10+#13+

        'gl_Position = mvp*vec4(vertexPosition, 1.0);' +#10+#13+
    '}';

  rlgLightFS =
      '#version 330' +#10+#13+
      '#define TEX texture' +#10+#13+
      //'#define NUM_LIGHTS %i\n' +#10+#13+
      '#define NUM_LIGHTS         %s' + #10+#13+
      '#define DIRECTIONAL_LIGHT  0' +#10+#13+
      '#define OMNI_LIGHT         1' +#10+#13+
      '#define SPOT_LIGHT         2' +#10+#13+
      'in vec3 fragPosition;' +#10+#13+
      'in vec2 fragTexCoord;' +#10+#13+
      'in vec3 fragNormal;' +#10+#13+
      'in vec4 fragColor;' +#10+#13+
      'in mat3 TBN;' +#10+#13+
      'out vec4 _;' +#10+#13+
      'struct Light {' +#10+#13+
          'sampler2D shadowMap;' +#10+#13+     ///< Sampler for the shadow map texture
          'mat4 matrix;' +#10+#13+             ///< Transformation matrix for the light
          'vec3 position;' +#10+#13+           ///< Position of the light in world coordinates
          'vec3 direction;' +#10+#13+          ///< Direction vector of the light (for directional and spotlights)
          'vec3 diffuse;'+#10+#13+             ///< Diffuse color of the light
          'vec3 specular;' +#10+#13+           ///< Specular color of the light
          'float innerCutOff;' +#10+#13+       ///< Inner cutoff angle for spotlights (cosine of the angle)
          'float outerCutOff;'+#10+#13+        ///< Outer cutoff angle for spotlights (cosine of the angle)
          'float constant;'+#10+#13+           ///< Constant attenuation factor
          'float linear;'+#10+#13+             ///< Linear attenuation factor
          'float quadratic;' +#10+#13+         ///< Quadratic attenuation factor
          'float shadowMapTxlSz;'+#10+#13+     ///< Texel size of the shadow map
          'float depthBias;'+#10+#13+          ///< Bias value to avoid self-shadowing artifacts
          'lowp int type;' +#10+#13+           ///< Type of the light (e.g., point, directional, spotlight)
          'lowp int shadow;'+#10+#13+          ///< Indicates if the light casts shadows (1 for true, 0 for false)
          'lowp int enabled;' +#10+#13+        ///< Indicates if the light is active (1 for true, 0 for false)
      '};' +#10+#13+
      'uniform Light lights[NUM_LIGHTS];' +#10+#13+
      'uniform lowp int useSpecularMap;' +#10+#13+
      'uniform lowp int useNormalMap;' +#10+#13+
      'uniform sampler2D texture0;' +#10+#13+   // diffuse
      'uniform sampler2D texture1;' +#10+#13+   // specular
      'uniform sampler2D texture2;' +#10+#13+   // normal
      'uniform vec3 colSpecular;' +#10+#13+     // sent by rlights
      'uniform vec4 colDiffuse;' +#10+#13+      // sent by raylib
      'uniform vec3 colAmbient;' +#10+#13+      // sent by rlights
      'uniform float shininess;' +#10+#13+
      'uniform vec3 viewPos;' +#10+#13+
      'float ShadowCalc(int i)' +#10+#13+
      '{' + #10+#13+
          'vec4 p = lights[i].matrix*vec4(fragPosition, 1.0);' +#10+#13+  ///< TODO: Optimize this (avoid repeated calculations)
          'vec3 projCoords = p.xyz/p.w;' +#10+#13+
          'projCoords = projCoords*0.5 + 0.5;' +#10+#13+
          'projCoords.z -= lights[i].depthBias;' +#10+#13+ ///< * distance(lights[i].position, fragPosition) * X
          'if (projCoords.z > 1.0 || projCoords.x > 1.0 || projCoords.y > 1.0)' +#10+#13+
          '{' +#10+#13+
              'return 1.0;' +#10+#13+
          '}' +#10+#13+
          'float depth = projCoords.z;' +#10+#13+
          'float shadow = 0.0;' +#10+#13+
          // NOTE: You can increase iterations to improve PCF quality
          'for (int x = -1; x <= 1; x++)' +#10+#13+
          '{' +#10+#13+
              'for (int y = -1; y <= 1; y++)' +#10+#13+
              '{' +#10+#13+
                  'float pcfDepth = TEX(lights[i].shadowMap, projCoords.xy + vec2(x, y)*lights[i].shadowMapTxlSz).r; ' +#10+#13+
                  'shadow += step(depth, pcfDepth);' +#10+#13+
              '}' +#10+#13+
          '}' +#10+#13+
          'return shadow/9.0;' +#10+#13+
      '}' +#10+#13+
      'void main()' +#10+#13+
      '{' +#10+#13+
          // get texture samples
          'vec3 diffSample = TEX(texture0, fragTexCoord).rgb*colDiffuse.rgb*fragColor.rgb;' +#10+#13+
          'vec3 specSample = (useSpecularMap != 0) ? TEX(texture1, fragTexCoord).rgb*colSpecular : colSpecular;' +#10+#13+
          // ambient
          'vec3 ambientColor = colAmbient*diffSample;' +#10+#13+
          // compute normals
          'vec3 normal;' +#10+#13+
          'if (useNormalMap == 0) normal = normalize(fragNormal);' +#10+#13+
          'else normal = normalize(TBN*(TEX(texture2, fragTexCoord).rgb*2.0 - 1.0));' +#10+#13+
          // compute current view dir for this frag
          'vec3 viewDir = normalize(viewPos - fragPosition);' +#10+#13+
          // process lights
          'vec3 finalColor = vec3(0.0);' +#10+#13+
          'for (int i = 0; i < NUM_LIGHTS; i++)' +#10+#13+
          '{' +#10+#13+
              'if (lights[i].enabled != 0)' +#10+#13+
              '{' +#10+#13+
                  // get lightDir
                  'vec3 lightDir = (lights[i].type != DIRECTIONAL_LIGHT)' +#10+#13+
                      '? normalize(lights[i].position - fragPosition)' +#10+#13+
                      ': normalize(-lights[i].direction);' +#10+#13+
                  // diffuse
                  'float diff = max(dot(normal, lightDir), 0.0);' +#10+#13+
                  'vec3 diffuse = lights[i].diffuse*diffSample*diff;' +#10+#13+
                  // specular (Blinn-Phong)
                  'vec3 halfwayDir = normalize(lightDir + viewDir);' +#10+#13+
                  'float spec = pow(max(dot(normal, halfwayDir), 0.0), shininess);' +#10+#13+
                  'vec3 specular = lights[i].specular*specSample*spec;' +#10+#13+
                  // spotlight
                  'float intensity = 1.0;' +#10+#13+
                  'if (lights[i].type == SPOT_LIGHT)' +#10+#13+
                  '{' +#10+#13+
                      'float theta = dot(lightDir, normalize(-lights[i].direction));' +#10+#13+
                      'float epsilon = (lights[i].innerCutOff - lights[i].outerCutOff);' +#10+#13+
                      'intensity = smoothstep(0.0, 1.0, (theta - lights[i].outerCutOff) / epsilon);' +#10+#13+
                  '}' +#10+#13+
                  // attenuation
                  'float distance    = length(lights[i].position - fragPosition);' +#10+#13+
                  'float attenuation = 1.0/(lights[i].constant + lights[i].linear*distance + lights[i].quadratic*(distance*distance));' +#10+#13+
                  // shadow
                  'float shadow = (lights[i].shadow != 0) ? ShadowCalc(i) : 1.0;' +#10+#13+
                  // add final light color
                  'finalColor += (diffuse + specular)*intensity*attenuation*shadow;' +#10+#13+
              '}' +#10+#13+
          '}' +#10+#13+
          '_ = vec4 (ambientColor + finalColor, 1.0);' +#10+#13+
      '}';

  rlgDepthVS =
      '#version 330' +#10+#13+
      'in vec3 vertexPosition;' +#10+#13+
      'uniform mat4 mvp;' +#10+#13+
      'void main()' +#10+#13+
      '{' +#10+#13+
          'gl_Position = mvp*vec4(vertexPosition, 1.0);' +#10+#13+
      '}';

  rlgDepthFS =
      '#version 330' +#10+#13+
      'out vec4 _;' +#10+#13+
      'void main()' +#10+#13+
      '{' +#10+#13+
      '_ = vec4 (gl_FragCoord.z);' +#10+#13+
      '}';

  rlgShadowMapFS =
      '#version 330' +#10+#13+
      '#define TEX texture' +#10+#13+
      'in vec2 fragTexCoord;' +#10+#13+
      'uniform sampler2D texture0;' +#10+#13+
      'uniform float near;' +#10+#13+
      'uniform float far;' +#10+#13+
      'void main()' +#10+#13+
      '{' +#10+#13+
          'float depth = TEX(texture0, vec2(fragTexCoord.x, 1.0 - fragTexCoord.y)).r;' +#10+#13+
          'depth = (2.0*near*far)/(far + near - (depth*2.0 - 1.0)*(far - near));' +#10+#13+
          'gl_FragColor = vec4(vec3(depth/far), 1.0);' +#10+#13+
      '}';


procedure RLG_Init(count: LongWord);
procedure RLG_Close();

function RLG_GetLightShader: PShader;
function RLG_GetDepthShader: PShader;

procedure RLG_SetViewPosition(x, y, z: Single);
procedure RLG_SetViewPositionV(position: TVector3);
function RLG_GetViewPosition(): TVector3;

procedure RLG_EnableSpecularMap();
procedure RLG_DisableSpecularMap();
function RLG_IsSpecularMapEnabled: Boolean;

procedure RLG_EnableNormalMap();
procedure RLG_DisableNormalMap();
function RLG_IsNormalMapEnabled: Boolean;

procedure RLG_SetShininess(value: Single);
function RLG_GetShininess: Single;

procedure RLG_SetSpecular(r, g, b: Single);
procedure RLG_SetSpecularV(color: TVector3);
procedure RLG_SetSpecularC(color: TColorB);
function RLG_GetSpecular: TVector3;
function RLG_GetSpecularC: TColorB;

procedure RLG_SetAmbient(r, g, b: Single);
procedure RLG_SetAmbientV(color: TVector3);
procedure RLG_SetAmbientC(color: TColorB);
function RLG_GetAmbient: TVector3;
function RLG_GetAmbientC: TColorB;

function RLG_GetLightcount: LongWord;

procedure RLG_ToggleLight(light: LongWord);
procedure RLG_EnableLight(light: LongWord);
procedure RLG_DisableLight(light: LongWord);
function RLG_IsLightEnabled(light: LongWord): Boolean;

procedure RLG_SetLightType(light: LongWord; type_: TRLG_LightType);
function RLG_GetLightType(light: LongWord): TRLG_LightType;

procedure RLG_SetLightPosition(light: LongWord; x, y, z: Single);
procedure RLG_SetLightPositionV(light: LongWord; position: TVector3);
function RLG_GetLightPosition(light: LongWord): TVector3;

procedure RLG_SetLightDirection(light: LongWord; x, y, z: Single);
procedure RLG_SetLightDirectionV(light: LongWord; direction: TVector3);
function RLG_GetLightDirection(light: LongWord): TVector3;

procedure RLG_SetLightTarget(light: LongWord; x, y, z: Single);
procedure RLG_SetLightTargetV(light: LongWord; targetPosition: TVector3);
function RLG_GetLightTarget(light: LongWord): TVector3;

procedure RLG_SetLightDiffuse(light: LongWord; r, g, b: Single);
procedure RLG_SetLightDiffuseV(light: LongWord; color: TVector3);
procedure RLG_SetLightDiffuseC(light: LongWord; color: TColorB);
function RLG_GetLightDiffuse(light: LongWord): TVector3;
function RLG_GetLightDiffuseC(light: LongWord): TColorB;

procedure RLG_SetLightSpecular(light: LongWord; r, g, b: Single);
procedure RLG_SetLightSpecularV(light: LongWord; color: TVector3);
procedure RLG_SetLightSpecularC(light: LongWord; color: TcolorB);
function RLG_GetLightSpecular(light: LongWord): TVector3;
function RLG_GetLightSpecularC(light: LongWord): TColorB;

procedure RLG_SetLightInnerCutOff(light: LongWord; degrees: Single);
function RLG_GetLightInnerCutoff(light: LongWord): Single;

procedure RLG_SetLightOuterCutOff(light: LongWord; degrees: Single);
function RLG_GetLightOuterCutoff(light: LongWord): Single;

procedure RLG_SetLightAttenuation(light: LongWord; constant, linear, quadratic: Single);
procedure RLG_GetLightAttenuation(light: LongWord; constant, linear, quadratic: PSingle);

procedure RLG_SetLightAttenuationQuadratic(light: LongWord; quadratic: Single);
procedure RLG_SetLightAttenuationConstant(light: LongWord; constant: Single);
procedure RLG_SetLightAttenuationLinear(light: LongWord; linear: Single);

procedure RLG_EnableLightShadow(light: LongWord; shadowMapResolution: Integer);
procedure RLG_DisableLightShadow(light: LongWord);
function RLG_IsLightShadowEnabled(light: LongWord): Boolean;

procedure RLG_SetLightShadowBias(light: LongWord; value: Single);
function RLG_GetLightShadowBias(light: LongWord): Single;

procedure RLG_BeginShadowCast(light: LongWord);
procedure RLG_EndShadowCast();

procedure RLG_ClearShadowMap();

procedure RLG_DrawShadowMap(light: LongWord; x, y, w, h: Integer);
procedure RLG_DrawShadowMapEx(light: LongWord; x, y, w, h: Integer; near, far: Single);

procedure RLG_CastMesh(mesh: TMesh; material: TMaterial; transform: TMatrix);
procedure RLG_CastModel(model: TModel; position: TVector3; scale: Single);
procedure RLG_CastModelEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3);

procedure RLG_DrawMesh(mesh: TMesh; material: TMaterial; transform: TMatrix);
procedure RLG_DrawModel(model: TModel; position: TVector3; scale: Single; tint: TColorB);
procedure RLG_DrawModelEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB);


var  RLG: TRLG_Core;

implementation
uses SysUtils;

function IntToBool(const AnInt: Integer): Boolean;
begin
  if AnInt = 0 then Result := False
  else Result := True;
end;

procedure RLG_Init(count: LongWord);
var fmtFrag: String; i: Integer;
     light: PRLG_Light; locs: PRLG_LightLocs;
begin
  fmtFrag := '';
  if (RLG.lights <> nil) then
  begin
    TraceLog(LOG_ERROR, 'You are trying to initialize rlights when it has already been initialized.');
    Exit;
  end;
  // NOTE: The limit of 99 is because we measure the size of `rlgLightFS` with '%s'
  if (count > 98) then
  begin
    TraceLog(LOG_WARNING, 'The limit of lights supported by rlights is 99.' +
    'The number of lights has therefore been adjusted to this value.');
    count := 98;
  end;
  // Format frag shader with lights count
  FmtStr(fmtFrag,rlgLightFS,[IntToStr(count)]);
  // Load shader and get locations
  RLG.lightShader := LoadShaderFromMemory(rlgLightVS, PChar(fmtFrag));
  // Retrieving global shader locations
  RLG.locsGlobalLight.useSpecularMap := GetShaderLocation(RLG.lightShader, 'useSpecularMap');
  RLG.locsGlobalLight.useNormalMap := GetShaderLocation(RLG.lightShader, 'useNormalMap');
  RLG.locsGlobalLight.colSpecular := GetShaderLocation(RLG.lightShader, 'colSpecular');
  RLG.locsGlobalLight.colAmbient := GetShaderLocation(RLG.lightShader, 'colAmbient');
  RLG.locsGlobalLight.shininess := GetShaderLocation(RLG.lightShader, 'shininess');
  RLG.locsGlobalLight.viewPos := GetShaderLocation(RLG.lightShader, 'viewPos');
  // Define default global uniforms
  RLG.globalLight := Default(TRLG_GlobalLight);
  RLG.globalLight.colSpecular := Vector3Create ( 1.0, 1.0, 1.0 );
  RLG.globalLight.colAmbient := Vector3Create ( 0.1, 0.1, 0.1 );
  RLG.globalLight.shininess := 32.0;
  // Send default globals uniforms (no need to send zero-values)
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colSpecular, @RLG.globalLight.colSpecular, SHADER_UNIFORM_VEC3);
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colAmbient, @RLG.globalLight.colAmbient, SHADER_UNIFORM_VEC3);
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.shininess, @RLG.globalLight.shininess, SHADER_UNIFORM_FLOAT);

  // Allocation and initialization of the desired number of lights
   RLG.lights := malloc(Count*SizeOf(PRLG_Light)* 99);
   RLG.locsLights :=malloc(sizeOf(PRLG_LightLocs)* 99);

   for i := 0 to count do
   begin
     light := @RLG.lights[i];
     locs := @RLG.locsLights[i];
     light^.shadowMap      := Default(TRLG_ShadowMap);
     light^.position       := Vector3Create ( 0, 0, 0 );
     light^.direction      := Vector3Create ( 0, 0, 0 );
     light^.diffuse        := Vector3Create ( 1.0, 1.0, 1.0);
     light^.specular       := Vector3Create ( 1.0, 1.0, 1.0);
     light^.innerCutOff    := -1.0;
     light^.outerCutOff    := -1.0;
     light^.constant       := 1.0;
     light^.linear         := 0.0;
     light^.quadratic      := 0.0;
     light^.shadowMapTxlSz := 0.0;
     light^.depthBias      := 0.0;
     light^.type_          := RLG_DIRECTIONAL;
     light^.shadow         := 0;
     light^.enabled        := 0;

     locs^.shadowMap      := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].shadowMap', i));
     locs^.matrix         := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].matrix', i));
     locs^.position       := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].position', i));
     locs^.direction      := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].direction', i));
     locs^.diffuse        := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].diffuse', i));
     locs^.specular       := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].specular', i));
     locs^.innerCutOff    := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].innerCutOff', i));
     locs^.outerCutOff    := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].outerCutOff', i));
     locs^.constant       := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].constant', i));
     locs^.linear         := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].linear', i));
     locs^.quadratic      := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].quadratic', i));
     locs^.shadowMapTxlSz := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].shadowMapTxlSz', i));
     locs^.depthBias      := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].depthBias', i));
     locs^.type_          := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].type', i));
     locs^.shadow         := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].shadow', i));
     locs^.enabled        := GetShaderLocation(RLG.lightShader, TextFormat('lights[%i].enabled', i));

     SetShaderValue(RLG.lightShader, locs^.diffuse, @light^.diffuse, SHADER_UNIFORM_VEC3);
     SetShaderValue(RLG.lightShader, locs^.specular, @light^.specular, SHADER_UNIFORM_VEC3);
     SetShaderValue(RLG.lightShader, locs^.innerCutOff, @light^.innerCutOff, SHADER_UNIFORM_FLOAT);
     SetShaderValue(RLG.lightShader, locs^.outerCutOff, @light^.outerCutOff, SHADER_UNIFORM_FLOAT);
     SetShaderValue(RLG.lightShader, locs^.constant, @light^.constant, SHADER_UNIFORM_FLOAT);
  end;
  // Set light count
  RLG.lightCount := count;
  // Load depth shader (used for shadow casting)
  RLG.depthShader := LoadShaderFromMemory(rlgDepthVS, rlgDepthFS);
  // Load shadow map shader (used to render shadow maps)
  RLG.shadowMapShader := LoadShaderFromMemory(nil, rlgShadowMapFS);
  RLG.shadowMapShaderData.near := 0.1;
  RLG.shadowMapShaderData.far := 100.0;
  RLG.shadowMapShaderData.locNear := GetShaderLocation(RLG.shadowMapShader, 'near');
  RLG.shadowMapShaderData.locFar := GetShaderLocation(RLG.shadowMapShader, 'far');

  SetShaderValue(RLG.shadowMapShader, RLG.shadowMapShaderData.locNear,
  @RLG.shadowMapShaderData.near, SHADER_UNIFORM_FLOAT);
  SetShaderValue(RLG.shadowMapShader, RLG.shadowMapShaderData.locFar,
  @RLG.shadowMapShaderData.far, SHADER_UNIFORM_FLOAT);
end;

procedure RLG_Close();
var i: Integer;
begin
  if IsShaderReady(RLG.lightShader) then
  UnloadShader(RLG.lightShader);

  if IsShaderReady(RLG.depthShader) then
  UnloadShader(RLG.depthShader);

  if IsShaderReady(RLG.shadowMapShader) then
  UnloadShader(RLG.shadowMapShader);

  if (RLG.lights > nil) then
  begin
    for i:= 0 to RLG.lightCount do
    if (RLG.lights[i].shadowMap.id > 0) then
    begin
      rlUnloadTexture(RLG.lights[i].shadowMap.depth.id);
      rlUnloadFramebuffer(RLG.lights[i].shadowMap.id);
    end;
      //free(RLG.lights);
      //RLG.lights := nil;
  end;

  if (RLG.locsLights > nil) then
  begin
    //free(RLG.locsLights);
    //RLG.locsLights := nil;
  end;

  RLG.lightCount := 0;
end;

function RLG_GetLightShader: PShader;
begin
  if IsShaderReady(RLG.lightShader) then
  result := @RLG.lightShader else
  result := @Default(TShader);
end;

function RLG_GetDepthShader: PShader;
begin
  if IsShaderReady(RLG.depthShader) then
  result := @RLG.depthShader else
  result := @Default(TShader);
end;

procedure RLG_SetViewPosition(x, y, z: Single);
begin
  RLG_SetViewPositionV(Vector3Create(x, y, z));
end;

procedure RLG_SetViewPositionV(position: TVector3);
begin
  RLG.globalLight.viewPos := position;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.viewPos,
  @RLG.globalLight.viewPos, SHADER_UNIFORM_VEC3);
end;

function RLG_GetViewPosition(): TVector3;
begin
  result := RLG.globalLight.viewPos;
end;

procedure RLG_EnableSpecularMap();
var v: Integer;
begin
  v := 1;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.useSpecularMap, @v, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableSpecularMap();
var v: Integer;
begin
  v := 0;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.useSpecularMap, @v, SHADER_UNIFORM_INT);
end;

function RLG_IsSpecularMapEnabled: Boolean;
begin
  result := IntToBool(RLG.globalLight.useSpecularMap);
end;

procedure RLG_EnableNormalMap();
var v: Integer;
begin
  v := 1;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.useNormalMap, @v, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableNormalMap();
var v: Integer;
begin
  v := 0;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.useNormalMap, @v, SHADER_UNIFORM_INT);
end;

function RLG_IsNormalMapEnabled: Boolean;
begin
  result := IntToBool(RLG.globalLight.useNormalMap);
end;

procedure RLG_SetShininess(value: Single);
begin
  RLG.globalLight.shininess := value;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.shininess,
  @RLG.globalLight.shininess, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetShininess: Single;
begin
  result := RLG.globalLight.shininess;
end;

procedure RLG_SetSpecular(r, g, b: Single);
begin
  RLG.globalLight.colSpecular := Vector3Create(r, g, b);
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colSpecular,
  @RLG.globalLight.colSpecular, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetSpecularV(color: TVector3);
begin
  RLG.globalLight.colSpecular := color;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colSpecular,
  @RLG.globalLight.colSpecular, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetSpecularC(color: TColorB);
begin
  RLG.globalLight.colSpecular := Vector3Create(color.r*(1.0/255),  color.g*(1.0/255),  color.b*(1.0/255));
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colSpecular,
  @RLG.globalLight.colSpecular, SHADER_UNIFORM_VEC3);
end;

function RLG_GetSpecular: TVector3;
begin
  result := RLG.globalLight.colSpecular;
end;

function RLG_GetSpecularC: TColorB;
begin
  result := ColorCreate(
      Round(255*RLG.globalLight.colSpecular.x),
      Round(255*RLG.globalLight.colSpecular.y),
      Round(255*RLG.globalLight.colSpecular.z),255);
end;

procedure RLG_SetAmbient(r, g, b: Single);
begin
  RLG.globalLight.colAmbient := Vector3Create(r, g, b);
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colAmbient,
  @RLG.globalLight.colAmbient, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetAmbientV(color: TVector3);
begin
  RLG.globalLight.colAmbient := color;
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colAmbient,
  @RLG.globalLight.colAmbient, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetAmbientC(color: TColorB);
begin
  RLG.globalLight.colAmbient := Vector3Create(color.r*(1.0/255), color.g*(1.0/255), color.b*(1.0/255));
  SetShaderValue(RLG.lightShader, RLG.locsGlobalLight.colAmbient,
  @RLG.globalLight.colAmbient, SHADER_UNIFORM_VEC3);
end;

function RLG_GetAmbient: TVector3;
begin
  result := RLG.globalLight.colAmbient;
end;

function RLG_GetAmbientC: TColorB;
begin
  result := ColorCreate(
    Round(255*RLG.globalLight.colAmbient.x),
    Round(255*RLG.globalLight.colAmbient.y),
    Round(255*RLG.globalLight.colAmbient.z),255);
end;

function RLG_GetLightcount: LongWord;
begin
  result := RLG.lightCount;
end;

procedure RLG_ToggleLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_ToggleLight'' exceeds allocated number. [%i/%i]', light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].enabled := not RLG.lights[light].enabled;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].enabled,
  @RLG.lights[light].enabled, SHADER_UNIFORM_INT);
end;

procedure RLG_EnableLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_EnableLight'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].enabled := 1;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].enabled,
  @RLG.lights[light].enabled, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_DisableLight'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].enabled := 0;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].enabled,
  @RLG.lights[light].enabled, SHADER_UNIFORM_INT);
end;

function RLG_IsLightEnabled(light: LongWord): Boolean;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_IsLightEnabled'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := false;
  end else
  result := IntToBool(RLG.lights[light].enabled);
end;

procedure RLG_SetLightType(light: LongWord; type_: TRLG_LightType);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightType'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].type_ := type_;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].type_,
  @RLG.lights[light].type_, SHADER_UNIFORM_INT);
end;

function RLG_GetLightType(light: LongWord): TRLG_LightType;
begin
  if (light >= RLG.lightCount) then
  begin
  TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightType'' exceeds allocated number. [%i/%i]',
  light, RLG.lightCount));
  result := TRLG_LightType(0);
  end else
  result := TRLG_LightType(RLG.lights[light].type_);
end;

procedure RLG_SetLightPosition(light: LongWord; x, y, z: Single);
begin
  RLG_SetLightPositionV(light, Vector3Create(x, y, z));
end;

procedure RLG_SetLightPositionV(light: LongWord; position: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightPosition'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].position := position;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].position,
  @RLG.lights[light].position, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightPosition(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightPosition'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := Vector3Create(0, 0, 0);
  end else
  result := RLG.lights[light].position;
end;

procedure RLG_SetLightDirection(light: LongWord; x, y, z: Single);
begin
  RLG_SetLightDirectionV(light, Vector3Create(x, y, z));
end;

procedure RLG_SetLightDirectionV(light: LongWord; direction: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightDirection'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].direction := direction;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].direction,
  @RLG.lights[light].direction, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightDirection(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
      TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightDirection'' exceeds allocated number. [%i/%i]',
      light, RLG.lightCount));
      result := Vector3Create(0, 0, 0);
  end else
  result := RLG.lights[light].direction;
end;

procedure RLG_SetLightTarget(light: LongWord; x, y, z: Single);
begin
  RLG_SetLightTargetV(light, Vector3Create(x, y, z));
end;

procedure RLG_SetLightTargetV(light: LongWord; targetPosition: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightTarget'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].direction := Vector3Normalize(Vector3Subtract(
  targetPosition, RLG.lights[light].position));

  SetShaderValue(RLG.lightShader, RLG.locsLights[light].direction,
  @RLG.lights[light].direction, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightTarget(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightTarget'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := Vector3Create(0, 0, 0);
  end else
  result :=Vector3Add(RLG.lights[light].position, RLG.lights[light].direction);
end;

procedure RLG_SetLightDiffuse(light: LongWord; r, g, b: Single);
begin
  RLG_SetLightDiffuseV(light, Vector3Create(r, g, b));
end;

procedure RLG_SetLightDiffuseV(light: LongWord; color: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightDiffuse'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].diffuse := color;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].diffuse,
  @RLG.lights[light].diffuse, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetLightDiffuseC(light: LongWord; color: TColorB);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightDiffuseC'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].diffuse := Vector3Create(color.r*(1.0/255), color.g*(1.0/255), color.b*(1.0/255));
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].diffuse,
  @RLG.lights[light].diffuse, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightDiffuse(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightDiffuse'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := Vector3Create(0, 0, 0);
  end else
  result := RLG.lights[light].diffuse;
end;

function RLG_GetLightDiffuseC(light: LongWord): TColorB;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightDiffuseC'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := BLANK;
  end else
  result := ColorCreate(
      Trunc(255*RLG.lights[light].diffuse.x),
      Trunc(255*RLG.lights[light].diffuse.y),
      Trunc(255*RLG.lights[light].diffuse.z),255);
end;

procedure RLG_SetLightSpecular(light: LongWord; r, g, b: Single);
begin
  RLG_SetLightSpecularV(light, Vector3Create(r, g, b));
end;

procedure RLG_SetLightSpecularV(light: LongWord; color: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightSpecular'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].specular := color;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].specular,
  @RLG.lights[light].specular, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetLightSpecularC(light: LongWord; color: TcolorB);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightSpecularC'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].specular := Vector3Create(color.r*(1.0/255), color.g*(1.0/255), color.b*(1.0/255));
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].specular,
  @RLG.lights[light].specular, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightSpecular(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightSpecular'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := Vector3Create(0, 0, 0);
  end else
  result := RLG.lights[light].specular;
end;

function RLG_GetLightSpecularC(light: LongWord): TColorB;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightSpecularC'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := BLANK;
  end else
  result := ColorCreate(
      Round(255*RLG.lights[light].diffuse.x),
      Round(255*RLG.lights[light].diffuse.y),
      Round(255*RLG.lights[light].diffuse.z),255);
end;

procedure RLG_SetLightInnerCutOff(light: LongWord; degrees: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightInnerCutOff'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].innerCutOff := cos(degrees*DEG2RAD);
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].innerCutOff,
  @RLG.lights[light].innerCutOff, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetLightInnerCutoff(light: LongWord): Single;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightInnerCutoff'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := 0;
  end else
  result := RLG.lights[light].innerCutOff;
end;

procedure RLG_SetLightOuterCutOff(light: LongWord; degrees: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightOuterCutOff'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].outerCutOff := cos(degrees*DEG2RAD);
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].outerCutOff,
  @RLG.lights[light].outerCutOff, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetLightOuterCutoff(light: LongWord): Single;
begin
  if (light >= RLG.lightCount) then
  begin
   TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightOuterCutoff'' exceeds allocated number. [%i/%i]',
   light, RLG.lightCount));
   result := 0;
  end else
  result := RLG.lights[light].outerCutOff;
end;

procedure RLG_SetLightAttenuation(light: LongWord; constant, linear,
  quadratic: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightAttenuation'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].constant := constant;
  RLG.lights[light].linear := linear;
  RLG.lights[light].quadratic := quadratic;

  SetShaderValue(RLG.lightShader, RLG.locsLights[light].constant,
  @RLG.lights[light].constant, SHADER_UNIFORM_FLOAT);

  SetShaderValue(RLG.lightShader, RLG.locsLights[light].linear,
  @RLG.lights[light].linear, SHADER_UNIFORM_FLOAT);

  SetShaderValue(RLG.lightShader, RLG.locsLights[light].quadratic,
  @RLG.lights[light].quadratic, SHADER_UNIFORM_FLOAT);
end;

procedure RLG_GetLightAttenuation(light: LongWord; constant, linear,
  quadratic: PSingle);
begin
  constant := Default(PSingle);
  linear := Default(PSingle);
  quadratic := Default(PSingle);

  if (light >= RLG.lightCount) then
  begin
  TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_GetLightAttenuation'' exceeds allocated number. [%i/%i]',
  light, RLG.lightCount));
  Exit;
  end;
  constant := @RLG.lights[light].constant;
  linear := @RLG.lights[light].linear;
  quadratic := @RLG.lights[light].linear;
end;

procedure RLG_SetLightAttenuationQuadratic(light: LongWord; quadratic: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightAttenuationQuadratic'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].quadratic := quadratic;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].quadratic,
  @RLG.lights[light].quadratic, SHADER_UNIFORM_FLOAT);
end;

procedure RLG_SetLightAttenuationConstant(light: LongWord; constant: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightAttenuationConstant'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].constant := constant;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].constant,
  @RLG.lights[light].constant, SHADER_UNIFORM_FLOAT);
end;

procedure RLG_SetLightAttenuationLinear(light: LongWord; linear: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_SetLightAttenuationLinear'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].linear := linear;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].linear,
  @RLG.lights[light].linear, SHADER_UNIFORM_FLOAT);
end;

procedure RLG_EnableLightShadow(light: LongWord; shadowMapResolution: Integer);
var l: PRLG_Light;
    sm: PRLG_ShadowMap;
    texelSize: Single;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_EnableLightShadow'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;

  l := @RLG.lights[light];

  if (l^.type_ <> RLG_SPOTLIGHT) then
  begin
    TraceLog(LOG_WARNING, 'Shadow mapping currently works fully only with spotlights.', light, RLG.lightCount);
  end;

  if (l^.shadowMap.width <> shadowMapResolution)  then ///< TODO: Review for CSM
  begin
      if (l^.shadowMap.id > 0) then
      begin
          rlUnloadTexture(l^.shadowMap.depth.id);
          rlUnloadFramebuffer(l^.shadowMap.id);
      end;

      sm := @l^.shadowMap;

      sm^.id := rlLoadFramebuffer();//(shadowMapResolution, shadowMapResolution);
      sm^.width := shadowMapResolution;
      sm^.height := shadowMapResolution;
      rlEnableFramebuffer(sm^.id);

      sm^.depth.id := rlLoadTextureDepth(shadowMapResolution, shadowMapResolution, false);
      sm^.depth.width := shadowMapResolution;
      sm^.depth.height := shadowMapResolution;
      sm^.depth.format := 19;
      sm^.depth.mipmaps := 1;

      rlTextureParameters(sm^.depth.id, RL_TEXTURE_WRAP_S, RL_TEXTURE_WRAP_CLAMP);
      rlTextureParameters(sm^.depth.id, RL_TEXTURE_WRAP_T, RL_TEXTURE_WRAP_CLAMP);
      rlFramebufferAttach(sm^.id, sm^.depth.id, RL_ATTACHMENT_DEPTH, RL_ATTACHMENT_TEXTURE2D, 0);

      // REVIEW: Should this value be modifiable by the user?
      texelSize := 1.0/shadowMapResolution;
      SetShaderValue(RLG.lightShader, RLG.locsLights[light].shadowMapTxlSz,
      @texelSize, SHADER_UNIFORM_FLOAT);

      // NOTE: This is a rough approximation, other factors may affect this variable.
      //       A better approach would be to calculate the bias in the shader,
      //       taking into account factors such as the distance between the
      //       light and the fragment position.
      //l^.depthBias := 0.1*shadowMapResolution*tan(acosf(l^.outerCutOff));
      l^.depthBias := 0.1*shadowMapResolution*tan(ArcCos(l^.outerCutOff));
      SetShaderValue(RLG.lightShader, RLG.locsLights[light].depthBias,
      @l^.depthBias, SHADER_UNIFORM_FLOAT);
  end;

  l^.shadow := 1;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].shadow,
  @RLG.lights[light].shadow, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableLightShadow(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_DisableLightShadow'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    Exit;
  end;
  RLG.lights[light].shadow := 0;
  SetShaderValue(RLG.lightShader, RLG.locsLights[light].shadow,
  @RLG.lights[light].shadow, SHADER_UNIFORM_INT);
end;

function RLG_IsLightShadowEnabled(light: LongWord): Boolean;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, TextFormat('Light ID specified to ''RLG_IsLightShadowEnabled'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount));
    result := false;
  end else
  result :=IntToBool(RLG.lights[light].shadow);
end;

procedure RLG_SetLightShadowBias(light: LongWord; value: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, 'Light ID specified to ''RLG_SetLightShadowBias'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount);
    Exit;
  end;
  RLG.lights[light].depthBias := value;
   SetShaderValue(RLG.lightShader, RLG.locsLights[light].depthBias,
   @value, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetLightShadowBias(light: LongWord): Single;
begin
  if (light >= RLG.lightCount) then
  begin
      TraceLog(LOG_ERROR, 'Light ID specified to ''RLG_SetLightShadowBias'' exceeds allocated number. [%i/%i]',
      light, RLG.lightCount);
      result := 0;
  end else
  result := RLG.lights[light].depthBias;
end;

procedure RLG_BeginShadowCast(light: LongWord);
var l: PRLG_Light;
    bound: Double;
    matView, viewProj: TMatrix;
const NEAR = 0.01;
      FAR = 1000;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, 'Light ID specified to ''RLG_BeginShadowCast'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount);
    Exit;
  end;

  l := @RLG.lights[light];

  if (not IntToBool(l^.shadow)) then
  begin
    TraceLog(LOG_ERROR, 'Light does not support shadow casting. Light ID: [%i]', light);
    Exit;
  end;

  rlDrawRenderBatchActive();
  rlEnableFramebuffer(l^.shadowMap.id);

  rlViewport(0, 0, l^.shadowMap.width, l^.shadowMap.height);

  rlMatrixMode(RL_PROJECTION);
  rlPushMatrix();
  rlLoadIdentity();

//#   define NEAR .01     // TODO: replace with rlGetCullDistanceNear()
//#   define FAR 1000.    // TODO: replace with rlGetCullDistanceFar()

  // TODO: Review for CSM (aspect ratio ?)
  // NOTE: acos(outerCutoff) works only with spotlight   ArcCos
 // bound := NEAR*tan(acosf(l^.outerCutOff));
 bound := NEAR*tan(ArcCos(l^.outerCutOff));
 rlFrustum(-bound, bound, -bound, bound, NEAR, FAR);

  rlMatrixMode(RL_MODELVIEW);
  rlLoadIdentity();

  matView := MatrixLookAt(l^.position, Vector3Add(l^.position, l^.direction), Vector3Create(0, 1, 0));
  rlMultMatrixf(MatrixToFloatV(matView).v);

  rlEnableDepthTest();
  rlDisableColorBlend();

  viewProj := MatrixMultiply(matView, rlGetMatrixProjection());
  SetShaderValueMatrix(RLG.lightShader, RLG.locsLights[light].matrix, viewProj);
end;

procedure RLG_EndShadowCast();
begin
  rlEnableColorBlend();

  rlDrawRenderBatchActive();
  rlDisableFramebuffer();

  rlViewport(0, 0, GetScreenWidth(), GetScreenHeight());

  rlMatrixMode(RL_PROJECTION);
  rlPopMatrix();

  rlMatrixMode(RL_MODELVIEW);
  rlLoadIdentity();
end;

procedure RLG_ClearShadowMap();
begin
  rlClearColor(255, 255, 255, 255);
  rlClearScreenBuffers();
end;

procedure RLG_DrawShadowMap(light: LongWord; x, y, w, h: Integer);
begin
  RLG_DrawShadowMapEx(light, x, y, w, h, 0.1, 100.0);
end;

procedure RLG_DrawShadowMapEx(light: LongWord; x, y, w, h: Integer; near,
  far: Single);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_ERROR, 'Light ID specified to ''RLG_DrawShadowMap'' exceeds allocated number. [%i/%i]',
    light, RLG.lightCount);
    Exit;
  end;

  if (near <> RLG.shadowMapShaderData.near) then
  begin
    RLG.shadowMapShaderData.near := near;
    SetShaderValue(RLG.shadowMapShader, RLG.shadowMapShaderData.locNear,
    @RLG.shadowMapShaderData.near, SHADER_UNIFORM_FLOAT);
  end;


  if (far <> RLG.shadowMapShaderData.far) then
  begin
    RLG.shadowMapShaderData.far := far;
    SetShaderValue(RLG.shadowMapShader, RLG.shadowMapShaderData.locFar,
    @RLG.shadowMapShaderData.far, SHADER_UNIFORM_FLOAT);
  end;


  BeginShaderMode(RLG.shadowMapShader);
  rlBegin(RL_QUADS);

      rlSetTexture(RLG.lights[light].shadowMap.depth.id);

      rlTexCoord2f(0, 0); rlVertex2i(x, y);
      rlTexCoord2f(0, 1); rlVertex2i(x, y + h);
      rlTexCoord2f(1, 1); rlVertex2i(x + w, y + h);
      rlTexCoord2f(1, 0); rlVertex2i(x + w, y);

      rlSetTexture(rlGetTextureIdDefault());

  rlEnd();
  EndShaderMode();
end;

procedure RLG_CastMesh(mesh: TMesh; material: TMaterial; transform: TMatrix);
var matModel, matView, matModelView, matProjection, matModelViewProjection: TMatrix;
    eye, eyeCount: Integer;
begin
  // Bind shader program
   rlEnableShader(RLG.depthShader.id);

   // Get a copy of current matrices to work with,
   // just in case stereo render is required, and we need to modify them
   // NOTE: At this point the modelview matrix just contains the view matrix (camera)
   // That's because BeginMode3D() sets it and there is no model-drawing function
   // that modifies it, all use rlPushMatrix() and rlPopMatrix()
   matModel := MatrixIdentity();
   matView := rlGetMatrixModelview();
   matModelView := MatrixIdentity();
   matProjection := rlGetMatrixProjection();

   // Accumulate several model transformations:
   //    transform: model transformation provided (includes DrawModel() params combined with model.transform)
   //    rlGetMatrixTransform(): rlgl internal transform matrix due to push/pop matrix stack
   matModel := MatrixMultiply(transform, rlGetMatrixTransform());

   // Get model-view matrix
   matModelView := MatrixMultiply(matModel, matView);

   // Try binding vertex array objects (VAO) or use VBOs if not possible
   if not rlEnableVertexArray(mesh.vaoId) then
   begin
       // Bind mesh VBO data: vertex position (shader-location = 0)
       rlEnableVertexBuffer(mesh.vboId[0]);
       rlSetVertexAttribute(RLG.depthShader.locs[SHADER_LOC_VERTEX_POSITION], 3, RL_FLOAT, IntToBool(0), 0, 0);
       rlEnableVertexAttribute(RLG.depthShader.locs[SHADER_LOC_VERTEX_POSITION]);

       // If vertex indices exist, bine the VBO containing the indices
       if (mesh.indices > nil) then rlEnableVertexBufferElement(mesh.vboId[6]);
   end;
   ////IfThen(IsKeyPressed(KEY_SPACE), 20.0, playerVel.y - 0.8)
   eyeCount := IfThen(rlIsStereoRenderEnabled(), 2, 1);
   //eyeCount := rlIsStereoRenderEnabled() ? 2 : 1;

   for eye := 0 to eyeCount do // (int eye = 0; eye < eyeCount; eye++)
   begin
     // Calculate model-view-projection matrix (MVP)
     matModelViewProjection := MatrixIdentity();
     if (eyeCount = 1) then matModelViewProjection := MatrixMultiply(matModelView, matProjection)
     else
     begin
       // Setup current eye viewport (half screen width)
       rlViewport(eye*rlGetFramebufferWidth() div 2, 0, rlGetFramebufferWidth() div 2, rlGetFramebufferHeight());
       matModelViewProjection := MatrixMultiply(MatrixMultiply(matModelView, rlGetMatrixViewOffsetStereo(eye)), rlGetMatrixProjectionStereo(eye));
     end;

     // Send combined model-view-projection matrix to shader
     rlSetUniformMatrix(RLG.depthShader.locs[SHADER_LOC_MATRIX_MVP], matModelViewProjection);

     // Draw mesh
     if (mesh.indices > nil) then rlDrawVertexArrayElements(0, mesh.triangleCount*3, nil)
     else rlDrawVertexArray(0, mesh.vertexCount);
   end;

   // Disable all possible vertex array objects (or VBOs)
   rlDisableVertexArray();
   rlDisableVertexBuffer();
   rlDisableVertexBufferElement();

   // Disable shader program
   rlDisableShader();

   // Restore rlgl internal modelview and projection matrices
   rlSetMatrixModelview(matView);
   rlSetMatrixProjection(matProjection);
end;

procedure RLG_CastModel(model: TModel; position: TVector3; scale: Single);
var vScale, rotationAxis: TVector3;
begin
  vScale := Vector3Create(scale, scale, scale);
  rotationAxis := Vector3Create(0.0, 1.0, 0.0);
  RLG_CastModelEx(model, position, rotationAxis, 0.0, vScale);
end;

procedure RLG_CastModelEx(model: TModel; position, rotationAxis: TVector3;
  rotationAngle: Single; scale: TVector3);
var matScale, matRotation, matTranslation, matTransform: TMatrix;
    i: Integer;
begin
  matScale := MatrixScale(scale.x, scale.y, scale.z);
  matRotation := MatrixRotate(rotationAxis, rotationAngle*DEG2RAD);
  matTranslation := MatrixTranslate(position.x, position.y, position.z);

  matTransform := MatrixMultiply(MatrixMultiply(matScale, matRotation), matTranslation);
  model.transform := MatrixMultiply(model.transform, matTransform);

  for i :=0 to model.meshCount -1 do // (int i = 0; i < model.meshCount; i++)
  RLG_CastMesh(model.meshes[i], model.materials[model.meshMaterial[i]], model.transform);

end;

procedure RLG_DrawMesh(mesh: TMesh; material: TMaterial; transform: TMatrix);
var values: array[0..3] of Single;
    value: array[0..3] of Single;
    matModel, matView, matModelView, matProjection, matModelViewProjection: TMatrix;
    i, j, eye, eyeCount: Integer;
begin
  // Bind shader program
  rlEnableShader(RLG.lightShader.id);

  // Send required data to shader (matrices, values)
  //-----------------------------------------------------
  // Upload to shader globalLight.colDiffuse
  if (RLG.lightShader.locs[SHADER_LOC_COLOR_DIFFUSE] <> -1) then
  begin
    values[0] := material.maps[MATERIAL_MAP_DIFFUSE].color.r/255.0;
    values[1] := material.maps[MATERIAL_MAP_DIFFUSE].color.g/255.0;
    values[2] := material.maps[MATERIAL_MAP_DIFFUSE].color.b/255.0;
    values[3] := material.maps[MATERIAL_MAP_DIFFUSE].color.a/255.0;
    rlSetUniform(RLG.lightShader.locs[SHADER_LOC_COLOR_DIFFUSE], @values, SHADER_UNIFORM_VEC4, 1);
  end;

  // Upload to shader globalLight.colSpecular (if location available)
  if (RLG.lightShader.locs[SHADER_LOC_COLOR_SPECULAR] <> -1) then
  begin
    values[0] := material.maps[MATERIAL_MAP_SPECULAR].color.r/255.0;
    values[1] := material.maps[MATERIAL_MAP_SPECULAR].color.g/255.0;
    values[2] := material.maps[MATERIAL_MAP_SPECULAR].color.b/255.0;
    values[3] := material.maps[MATERIAL_MAP_SPECULAR].color.a/255.0;
     rlSetUniform(RLG.lightShader.locs[SHADER_LOC_COLOR_SPECULAR], @values, SHADER_UNIFORM_VEC4, 1);
  end;

  // Get a copy of current matrices to work with,
  // just in case stereo render is required, and we need to modify them
  // NOTE: At this point the modelview matrix just contains the view matrix (camera)
  // That's because BeginMode3D() sets it and there is no model-drawing function
  // that modifies it, all use rlPushMatrix() and rlPopMatrix()
  matModel := MatrixIdentity();
  matView := rlGetMatrixModelview();
  matModelView := MatrixIdentity();
  matProjection := rlGetMatrixProjection();

  // Upload view and projection matrices (if locations available)
  if (RLG.lightShader.locs[SHADER_LOC_MATRIX_VIEW] <> -1) then
  rlSetUniformMatrix(RLG.lightShader.locs[SHADER_LOC_MATRIX_VIEW], matView);

  if (RLG.lightShader.locs[SHADER_LOC_MATRIX_PROJECTION] <> -1) then
  rlSetUniformMatrix(RLG.lightShader.locs[SHADER_LOC_MATRIX_PROJECTION], matProjection);

  // Model transformation matrix is sent to shader uniform location: SHADER_LOC_MATRIX_MODEL
  if (RLG.lightShader.locs[SHADER_LOC_MATRIX_MODEL] <> -1) then
  rlSetUniformMatrix(RLG.lightShader.locs[SHADER_LOC_MATRIX_MODEL], transform);

  // Accumulate several model transformations:
  //    transform: model transformation provided (includes DrawModel() params combined with model.transform)
  //    rlGetMatrixTransform(): rlgl internal transform matrix due to push/pop matrix stack
  matModel := MatrixMultiply(transform, rlGetMatrixTransform());

  // Get model-view matrix
  matModelView := MatrixMultiply(matModel, matView);

  // Upload model normal matrix (if locations available)
  if (RLG.lightShader.locs[SHADER_LOC_MATRIX_NORMAL] <> -1) then
  rlSetUniformMatrix(RLG.lightShader.locs[SHADER_LOC_MATRIX_NORMAL], MatrixTranspose(MatrixInvert(matModel)));
  //-----------------------------------------------------

  // Bind active texture maps (if available)
  for i := 0 to 11 do //(int i = 0; i < 11; i++)
  begin
      if (material.maps[i].texture.id > 0) then
      begin
          // Select current shader texture slot
          rlActiveTextureSlot(i);
          // Enable texture for active slot
          if ((i = MATERIAL_MAP_IRRADIANCE) or (i = MATERIAL_MAP_PREFILTER) or (i = MATERIAL_MAP_CUBEMAP)) then
              rlEnableTextureCubemap(material.maps[i].texture.id)
          else rlEnableTexture(material.maps[i].texture.id);
          rlSetUniform(RLG.lightShader.locs[SHADER_LOC_MAP_DIFFUSE + i], @i, SHADER_UNIFORM_INT, 1);
      end;
  end;

  // Bind depth textures for shadow mapping
  for i := 0 to RLG.lightCount do //(int i = 0; i < RLG.lightCount; i++)
  begin
      if IntToBool(RLG.lights[i].shadow) then
      begin
          j := 11 + i;
          rlActiveTextureSlot(j);
          rlEnableTexture(RLG.lights[i].shadowMap.depth.id);
          rlSetUniform(RLG.locsLights[i].shadowMap, @j, SHADER_UNIFORM_INT, 1);
      end;
  end;

  // Try binding vertex array objects (VAO) or use VBOs if not possible
  // WARNING: UploadMesh() enables all vertex attributes available in mesh and sets default attribute values
  // for shader expected vertex attributes that are not provided by the mesh (i.e. colors)
  // This could be a dangerous approach because different meshes with different shaders can enable/disable some attributes
  if (not rlEnableVertexArray(mesh.vaoId)) then
  begin
      // Bind mesh VBO data: vertex position (shader-location = 0)
      rlEnableVertexBuffer(mesh.vboId[0]);
      rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_POSITION], 3, RL_FLOAT, IntToBool(0), 0, 0);
      rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_POSITION]);

      // Bind mesh VBO data: vertex texcoords (shader-location = 1)
      rlEnableVertexBuffer(mesh.vboId[1]);
      rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TEXCOORD01], 2, RL_FLOAT, IntToBool(0), 0, 0);
      rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TEXCOORD01]);

      if (RLG.lightShader.locs[SHADER_LOC_VERTEX_NORMAL] <> -1) then
      begin
          // Bind mesh VBO data: vertex normals (shader-location = 2)
          rlEnableVertexBuffer(mesh.vboId[2]);
          rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_NORMAL], 3, RL_FLOAT, IntToBool(0), 0, 0);
          rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_NORMAL]);
      end;

      // Bind mesh VBO data: vertex colors (shader-location = 3, if available)
      if (RLG.lightShader.locs[SHADER_LOC_VERTEX_COLOR] <> -1) then
      begin
          if (mesh.vboId[3] <> 0) then
          begin
              rlEnableVertexBuffer(mesh.vboId[3]);
              rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_COLOR], 4, RL_UNSIGNED_BYTE, IntToBool(1), 0, 0);
              rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_COLOR]);
          end
          else
          begin
              // Set default value for defined vertex attribute in shader but not provided by mesh
              // WARNING: It could result in GPU undefined behaviour
              //float value[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
               value[0] := 1.0;
               value[1] := 1.0;
               value[2] := 1.0;
               value[3] := 1.0;
              rlSetVertexAttributeDefault(RLG.lightShader.locs[SHADER_LOC_VERTEX_COLOR], @value, SHADER_ATTRIB_VEC4, 4);
              rlDisableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_COLOR]);
          end;
      end;

      // Bind mesh VBO data: vertex tangents (shader-location = 4, if available)
      if (RLG.lightShader.locs[SHADER_LOC_VERTEX_TANGENT] <> -1) then
      begin
          rlEnableVertexBuffer(mesh.vboId[4]);
          rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TANGENT], 4, RL_FLOAT, IntToBool(0), 0, 0);
          rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TANGENT]);
      end;

      // Bind mesh VBO data: vertex texcoords2 (shader-location = 5, if available)
      if (RLG.lightShader.locs[SHADER_LOC_VERTEX_TEXCOORD02] <> -1) then
      begin
          rlEnableVertexBuffer(mesh.vboId[5]);
          rlSetVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TEXCOORD02], 2, RL_FLOAT, IntToBool(0), 0, 0);
          rlEnableVertexAttribute(RLG.lightShader.locs[SHADER_LOC_VERTEX_TEXCOORD02]);
      end;

      if (mesh.indices <> nil) then rlEnableVertexBufferElement(mesh.vboId[6]);
  end;

  eyeCount := 1;
  if (rlIsStereoRenderEnabled()) then eyeCount := 2;

  for eye := 0 to eyeCount do //(int eye = 0; eye < eyeCount; eye++)
  begin
      // Calculate model-view-projection matrix (MVP)
      matModelViewProjection := MatrixIdentity();
      if (eyeCount = 1) then matModelViewProjection := MatrixMultiply(matModelView, matProjection)
      else
      begin
          // Setup current eye viewport (half screen width)
          rlViewport(eye*rlGetFramebufferWidth() div 2, 0, rlGetFramebufferWidth() div 2, rlGetFramebufferHeight());
          matModelViewProjection := MatrixMultiply(MatrixMultiply(matModelView, rlGetMatrixViewOffsetStereo(eye)), rlGetMatrixProjectionStereo(eye));
      end;

      // Send combined model-view-projection matrix to shader
      rlSetUniformMatrix(RLG.lightShader.locs[SHADER_LOC_MATRIX_MVP], matModelViewProjection);

      // Draw mesh
      if (mesh.indices <> nil) then rlDrawVertexArrayElements(0, mesh.triangleCount*3, nil)
      else rlDrawVertexArray(0, mesh.vertexCount);
  end;

  // Unbind all bound texture maps
  for i:=0 to 11 do //(int i = 0; i < 11; i++)
  begin
      if (material.maps[i].texture.id > 0)  then
      begin
          // Select current shader texture slot
          rlActiveTextureSlot(i);

          // Disable texture for active slot
          if ((i = MATERIAL_MAP_IRRADIANCE) or
              (i = MATERIAL_MAP_PREFILTER) or
              (i = MATERIAL_MAP_CUBEMAP)) then rlDisableTextureCubemap()
          else rlDisableTexture();
      end;
  end;

  // Unbind depth textures
  for i:=0 to RLG.lightCount do //(int i = 0; i < RLG.lightCount; i++)
  begin
      if IntToBool(RLG.lights[i].shadow) then
      begin
          rlActiveTextureSlot(11 + i);
          rlDisableTexture();
      end;
  end;

  // Disable all possible vertex array objects (or VBOs)
  rlDisableVertexArray();
  rlDisableVertexBuffer();
  rlDisableVertexBufferElement();

  // Disable shader program
  rlDisableShader();

  // Restore rlgl internal modelview and projection matrices
  rlSetMatrixModelview(matView);
  rlSetMatrixProjection(matProjection);

end;

procedure RLG_DrawModel(model: TModel; position: TVector3; scale: Single;
  tint: TColorB);
var vScale,rotationAxis: TVector3;
begin
  vScale := Vector3Create( scale, scale, scale );
  rotationAxis := Vector3Create( 0.0, 1.0, 0.0 );

  RLG_DrawModelEx(model, position, rotationAxis, 0.0, vScale, tint);
end;

procedure RLG_DrawModelEx(model: TModel; position, rotationAxis: TVector3;
  rotationAngle: Single; scale: TVector3; tint: TColorB);
var i: Integer; color, colorTint: TColorB;
    matScale, matRotation, matTranslation, matTransform: TMatrix;
begin
  matScale := MatrixScale(scale.x, scale.y, scale.z);
  matRotation := MatrixRotate(rotationAxis, rotationAngle*DEG2RAD);
  matTranslation := MatrixTranslate(position.x, position.y, position.z);

  matTransform := MatrixMultiply(MatrixMultiply(matScale, matRotation), matTranslation);
  model.transform := MatrixMultiply(model.transform, matTransform);

  for i:=0 to model.meshCount - 1 do //(int i = 0; i < model.meshCount; i++)
  begin
    color := model.materials[model.meshMaterial[i]].maps[MATERIAL_MAP_DIFFUSE].color;

    colorTint := WHITE;
    colorTint.r := (color.r*tint.r) div 255;// Round((color.r*tint.r)/255);
    colorTint.g := (color.g*tint.g) div 255;
    colorTint.b := (color.b*tint.b) div 255;
    colorTint.a := (color.a*tint.a) div 255;

      model.materials[model.meshMaterial[i]].maps[MATERIAL_MAP_DIFFUSE].color := colorTint;
      RLG_DrawMesh(model.meshes[i], model.materials[model.meshMaterial[i]], model.transform);
      model.materials[model.meshMaterial[i]].maps[MATERIAL_MAP_DIFFUSE].color := color;
  end;
end;





end.

