unit raylights;
{$mode objfpc}{$H+}


interface

uses
  raylib, cmem;

type
  PRLG_LightType = ^TRLG_LightType;
  TRLG_LightType =  Integer;
  const
    RLG_DIRECTIONAL = TRLG_LightType(0);
    RLG_OMNILIGHT   = TRLG_LightType(1);
    RLG_SPOTLIGHT   = TRLG_LightType(2);

type
  PRLG_MaterialLocs = ^TRLG_MaterialLocs;
  TRLG_MaterialLocs = record
    useSpecularMap: Integer;
    useNormalMap  : Integer;
    shininess     : Integer;
    mSpecular     : Integer;
    ambient       : Integer;
  end;

  PRLG_Material = ^TRLG_Material;
  TRLG_Material = record
    useSpecularMap: Integer;
    useNormalMap: Integer;
    shininess: Single;
    mSpecular: Single;
    ambient: TVector3;
  end;

  PRLG_LightLocs = ^TRLG_LightLocs;
  TRLG_LightLocs = record
    position: Integer;
    direction: Integer;
    diffuse: Integer;
    specular: Integer;
    innerCutOff: Integer;
    outerCutOff: Integer;
    constant: Integer;
    linear: Integer;
    quadratic: Integer;
    type_: Integer;
    active: Integer;
  end;

  PRLG_Light = ^TRLG_Light;
  TRLG_Light = record
    position: TVector3;
    direction: TVector3;
    diffuse: TVector3;
    specular: TVector3;
    innerCutOff: Single;
    outerCutOff: Single;
    constant: Single;
    linear: Single;
    quadratic: Single;
    type_: Integer;
    active: Integer;
  end;

 TRLG_Core = record
   locsMaterial: TRLG_MaterialLocs;
   material: TRLG_Material;
   locsLights: PRLG_LightLocs;
   lights: PRLG_Light;
   lightCount: Integer;
   viewPos: TVector3;
   locViewPos: Integer;
   shader: TShader;
 end;

 procedure RLG_Init(count: LongWord);
 procedure RLG_Close;
 function RLG_GetShader: PShader;
 procedure RLG_SetViewPosition(x, y, z: Single);
 procedure RLG_SetViewPositionV(position: TVector3);

 procedure RLG_EnableSpecularMap();
 procedure RLG_DisableSpecularMap();
 function RLG_IsSpecularMapEnabled: Boolean;

 procedure RLG_EnableNormalMap();
 procedure RLG_DisableNormalMap();
 function RLG_IsNormalMapEnabled: Boolean;

 procedure RLG_SetShininess(value: Single);
 function RLG_GetShininess: Single;

 procedure RLG_SetSpecular(value: Single);
 function RLG_GetSpecular: Single;

 procedure RLG_SetAmbient(r, g, b: Single);
 procedure RLG_SetAmbientV(color: TVector3);
 procedure RLG_SetAmbientC(color: TColorB);
 function RLG_GetAmbient: TVector3;
 function RLG_GetAmbientC: TColorB;

 function RLG_GetLightcount():LongWord;

 procedure RLG_ToggleLight(light: LongWord);
 procedure RLG_EnableLight(light: LongWord);
 procedure RLG_DisableLight(light: LongWord);
 function RLG_IsLightEnabled(light: LongWord): Boolean;

 procedure RLG_SetLightType(light: LongWord; type_ : TRLG_LightType);
 function RLG_GetLightType(light: LongWord): TRLG_LightType;

 procedure RLG_SetLightPosition(light: LongWord; x, y, z: Single);
 procedure RLG_SetLightPositionV(light: LongWord; position: TVector3);
 function RLG_GetLightPosition(light: LongWord): TVector3;

 procedure RLG_SetLightDirection(light: LongWord; x, y, z: Single);
 procedure RLG_SetLightDirectionV(light: LongWord; direction: TVector3);
 function RLG_GetLightDirection(light: LongWord): TVector3;


const rlgLightVS =
     '#version 110' +#10+#13+
     'attribute vec3 vertexPosition;' +#10+#13+
     'attribute vec2 vertexTexCoord;' +#10+#13+
     'attribute vec4 vertexTangent;' +#10+#13+
     'attribute vec3 vertexNormal;' +#10+#13+
     'uniform lowp int useNormalMap;' +#10+#13+
     'uniform mat4 matNormal;' +#10+#13+
     'uniform mat4 matModel;' +#10+#13+
     'uniform mat4 mvp;' +#10+#13+
     'varying vec3 fragPosition;' +#10+#13+
     'varying vec2 fragTexCoord;' +#10+#13+
     'varying vec3 fragNormal;' +#10+#13+
     'varying mat3 TBN;' +#10+#13+
     'void main()' +#10+#13+
     '{' +#10+#13+
         'fragPosition = vec3(matModel*vec4(vertexPosition, 1.0));' +#10+#13+
         'fragNormal = (matNormal*vec4(vertexNormal, 0.0)).xyz;' +#10+#13+
         'fragTexCoord = vertexTexCoord;' +#10+#13+
         'if (useNormalMap != 0)' +#10+#13+
         '{' +#10+#13+
             'vec3 T = normalize(vec3(matModel*vec4(vertexTangent.xyz, 0.0)));' +#10+#13+
             'vec3 B = cross(fragNormal, T)*vertexTangent.w;' +#10+#13+
             'TBN = mat3(T, B, fragNormal);' +#10+#13+
         '}' +#10+#13+
         'gl_Position = mvp*vec4(vertexPosition, 1.0);' +#10+#13+
     '}';



 // const rlgLightFS0 =



  const rlgLightFS =
       '#version 100' +#10+#13+
      '#define NUM_LIGHT %s ' + #10+#13+
      '#define DIRECTIONAL_LIGHT  0' +#10+#13+
      '#define OMNI_LIGHT         1' +#10+#13+
      '#define SPOT_LIGHT         2' +#10+#13+
      'precision mediump float;' +#10+#13+
      'varying vec3 fragPosition;' +#10+#13+
      'varying vec2 fragTexCoord;' +#10+#13+
      'varying vec3 fragNormal;' +#10+#13+
      'varying mat3 TBN;' +#10+#13+
      'struct Light {' +#10+#13+
          'vec3 position;' +#10+#13+
          'vec3 direction;' +#10+#13+
          'vec3 diffuse;' +#10+#13+
          'vec3 specular;' +#10+#13+
          'float innerCutOff;' +#10+#13+
          'float outerCutOff;' +#10+#13+
          'float constant;' +#10+#13+
          'float linear;' +#10+#13+
          'float quadratic;' +#10+#13+
          'lowp int type;' +#10+#13+
          'lowp int active;' +#10+#13+
      '};' +#10+#13+
      'uniform Light lights[NUM_LIGHT];' +#10+#13+
      'uniform lowp int useSpecularMap;' +#10+#13+
      'uniform lowp int useNormalMap;' +#10+#13+
      'uniform sampler2D texture0;' +#10+#13+   // diffuse
      'uniform sampler2D texture1;' +#10+#13+   // specular
      'uniform sampler2D texture2;' +#10+#13+   // normal
      'uniform float shininess;' +#10+#13+
      'uniform float mSpecular;' +#10+#13+
      'uniform vec3 ambient;' +#10+#13+
      'uniform vec3 viewPos;' +#10+#13+
      'void main()' +#10+#13+
      '{' +#10+#13+
          // get texture samples
          'vec3 diffSample = texture2D(texture0, fragTexCoord).rgb;' +#10+#13+
          'vec3 specSample = (useSpecularMap != 0) ? texture2D(texture1, fragTexCoord).rgb : vec3(1.0);' +#10+#13+
          // ambient
          'vec3 ambientColor = diffSample*ambient;' +#10+#13+
          // compute normals
          'vec3 normal;' +#10+#13+
          'if (useNormalMap == 0) normal = normalize(fragNormal);' +#10+#13+
          'else normal = normalize(TBN*(texture2D(texture2, fragTexCoord).rgb*2.0 - 1.0));' +#10+#13+
          // compute current view dir for this frag
          'vec3 viewDir = normalize(viewPos - fragPosition);' +#10+#13+
          // process lights
          'vec3 finalColor = vec3(0.0);' +#10+#13+
          'for (int i = 0; i < NUM_LIGHT; i++)' +#10+#13+
          '{' +#10+#13+
              'if (lights[i].active != 0)' +#10+#13+
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
                  'vec3 specular = lights[i].specular*specSample*spec*mSpecular;' +#10+#13+
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
                  // add final light color
                  'finalColor += (diffuse + specular)*intensity*attenuation;' +#10+#13+
              '}' +#10+#13+
          '}' +#10+#13+
          'gl_FragColor = vec4(ambientColor + finalColor, 1.0);' +#10+#13+
      '}';

 var   RLG: TRLG_Core;

implementation
uses SysUtils;

function IntToBool(const AnInt: Integer): Boolean;
begin
   if AnInt = 0 then Result := False
                else Result := True;
end;

procedure RLG_Init(count: LongWord);
var fmtFrag: AnsiString;
   // structMat: TRLG_Material;
    light: PRLG_Light; locs: PRLG_LightLocs;
    i: integer;
begin
  if (RLG.lights > nil) then
  begin
    TraceLog(LOG_WARNING, 'You are trying to initialize rlights when it has already been initialized.');
    exit;
  end;

  // NOTE: The limit of 99 is because we measure the size of `rlgLightFS` with '%s'
  if (count > 99) then
  begin
        TraceLog(LOG_WARNING, 'The limit of lights supported by rlights is 99.');
        TraceLog(LOG_WARNING,'The number of lights has therefore been adjusted to this value.');
        count := 99;
  end;
  //structMat := Default(TRLG_Material);

  // Format frag shader with lights count
 // fmtFrag := MemAlloc(SizeOf(rlgLightFS)* count);
 // snprintf(fmtFrag, sizeof(rlgLightFS), rlgLightFS, count);
 //   '#version 100' +#10+#13;
 //    '#define NUM_LIGHT %i' +#10+#13+
  //fmtFrag := PChar('#version 100' +#10+#13+ '#define NUM_LIGHT ' + IntToStr(Count) + #10+#13 + rlgLightFS);
  //FmtStr (S,'For some nice examples of fomatting see %s.',['Format']);
  //fmtFrag := TextFormat(rlgLightFS, count);
  // Load shader and get locations

  FmtStr(fmtFrag,rlgLightFS,[IntToStr(count)], Default(TFormatSettings));

  writeln(rlgLightVS);
  //RLG.shader := LoadShaderFromMemory(rlgLightVS, rlgLightFS);
   RLG.shader := LoadShader('test.vs','fert.fs');
  //free(fmtFrag);
  exit;
  // Retrieving viewpos loc and set default value
  RLG.locViewPos := GetShaderLocation(RLG.shader, 'viewPos');
  RLG.viewPos := Vector3Create(0,0,0);

  // Retrieving global shader locations
  RLG.locsMaterial.useSpecularMap := GetShaderLocation(RLG.shader, 'useSpecularMap');
  RLG.locsMaterial.useNormalMap := GetShaderLocation(RLG.shader, 'useNormalMap');
  RLG.locsMaterial.shininess := GetShaderLocation(RLG.shader, 'shininess');
  RLG.locsMaterial.mSpecular := GetShaderLocation(RLG.shader, 'mSpecular');
  RLG.locsMaterial.ambient := GetShaderLocation(RLG.shader, 'ambient');

  // Define default global uniforms
  RLG.material := Default(TRLG_Material);
  RLG.material.shininess := 32.0;
  RLG.material.mSpecular := 1.0;
  RLG.material.ambient := Vector3Create(0.1, 0.1, 0.1);

  // Send default globals uniforms (no need to send zero-values)
  SetShaderValue(RLG.shader, RLG.locsMaterial.shininess, @RLG.material.shininess, SHADER_UNIFORM_FLOAT);
  SetShaderValue(RLG.shader, RLG.locsMaterial.mSpecular, @RLG.material.mSpecular, SHADER_UNIFORM_FLOAT);
  SetShaderValue(RLG.shader, RLG.locsMaterial.ambient, @RLG.material.ambient, SHADER_UNIFORM_VEC3);

  // Allocation and initialization of the desired number of lights
   RLG.lights := malloc(count*sizeof(PRLG_Light));
   RLG.locsLights := malloc(count*sizeof(PRLG_LightLocs));
  //RLG.locsLights = (struct RLG_LightLocs*)malloc(count*sizeof(struct RLG_LightLocs));

  for i :=0 to count -1 do //(int i = 0; i < count; i++)
  begin
    light := @RLG.lights[i];
    locs := @RLG.locsLights[i];
    light^.position := Vector3Create(0,0,0);
    light^.direction := Vector3Create(0,0,0);
    light^.diffuse :=  Vector3Create(1.0,1.0,1.0);
    light^.specular := Vector3Create(1.0,1.0,1.0);
    light^.innerCutOff  := -1.0;
    light^.outerCutOff  := -1.0;
    light^.constant := 1.0;
    light^.linear := 0.0;
    light^.quadratic := 0.0;
    light^.active := 0;


    locs^.position := GetShaderLocation(RLG.shader, TextFormat('lights[%i].position', i));
    locs^.direction := GetShaderLocation(RLG.shader, TextFormat('lights[%i].direction', i));
    locs^.diffuse := GetShaderLocation(RLG.shader, TextFormat('lights[%i].diffuse', i));
    locs^.specular := GetShaderLocation(RLG.shader, TextFormat('lights[%i].specular', i));
    locs^.innerCutOff := GetShaderLocation(RLG.shader, TextFormat('lights[%i].innerCutOff', i));
    locs^.outerCutOff := GetShaderLocation(RLG.shader, TextFormat('lights[%i].outerCutOff', i));
    locs^.constant := GetShaderLocation(RLG.shader, TextFormat('lights[%i].constant', i));
    locs^.linear := GetShaderLocation(RLG.shader, TextFormat('lights[%i].linear', i));
    locs^.quadratic := GetShaderLocation(RLG.shader, TextFormat('lights[%i].quadratic', i));
    locs^.type_ := GetShaderLocation(RLG.shader, TextFormat('lights[%i].type', i));
    locs^.active := GetShaderLocation(RLG.shader, TextFormat('lights[%i].active', i));

    SetShaderValue(RLG.shader, locs^.position, @light^.position, SHADER_UNIFORM_VEC3);
    SetShaderValue(RLG.shader, locs^.direction, @light^.direction, SHADER_UNIFORM_VEC3);
    SetShaderValue(RLG.shader, locs^.diffuse, @light^.diffuse, SHADER_UNIFORM_VEC3);
    SetShaderValue(RLG.shader, locs^.specular, @light^.specular, SHADER_UNIFORM_VEC3);
    SetShaderValue(RLG.shader, locs^.innerCutOff, @light^.innerCutOff, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, locs^.outerCutOff, @light^.outerCutOff, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, locs^.constant, @light^.constant, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, locs^.linear, @light^.linear, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, locs^.quadratic, @light^.quadratic, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, locs^.type_, @light^.type_, SHADER_UNIFORM_INT);
    SetShaderValue(RLG.shader, locs^.active, @light^.active, SHADER_UNIFORM_INT);

  end;
      RLG.lightCount := count;
end;

procedure RLG_Close;
begin
  if (IsShaderReady(RLG.shader)) then
  begin
    UnloadShader(RLG.shader);
    //RLG.shader = (Shader) { 0 };
  end;

  if (RLG.lights > nil) then
  begin
    free(RLG.lights);
    RLG.lights := nil;
  end;

  if (RLG.locsLights > nil) then
  begin
    free(RLG.locsLights);
    RLG.locsLights := nil;
  end;
  RLG.lightCount := 0;
end;

function RLG_GetShader: PShader;
begin
      if (IsShaderReady(RLG.shader)) then
      result := @RLG.shader
      else
      result := nil;
end;

procedure RLG_SetViewPosition(x, y, z: Single);
begin
  RLG_SetViewPositionV(Vector3Create( x, y, z ));
end;

procedure RLG_SetViewPositionV(position: TVector3);
begin
  RLG.viewPos := position;
  SetShaderValue(RLG.shader, RLG.locViewPos, @RLG.viewPos, SHADER_UNIFORM_VEC3);
end;

procedure RLG_EnableSpecularMap();
var v: integer;
begin
  v := 1;
  SetShaderValue(RLG.shader, RLG.locsMaterial.useSpecularMap, @v, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableSpecularMap();
var v: integer;
begin
  v := 0;
  SetShaderValue(RLG.shader, RLG.locsMaterial.useSpecularMap, @v, SHADER_UNIFORM_INT);
end;

function RLG_IsSpecularMapEnabled: Boolean;
begin
  result := IntToBool(RLG.material.useSpecularMap);
end;

procedure RLG_EnableNormalMap();
var v: integer;
begin
  v := 1;
  SetShaderValue(RLG.shader, RLG.locsMaterial.useNormalMap, @v, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableNormalMap();
var v: integer;
begin
  v := 0;
  SetShaderValue(RLG.shader, RLG.locsMaterial.useNormalMap, @v, SHADER_UNIFORM_INT);
end;

function RLG_IsNormalMapEnabled: Boolean;
begin
  result := IntToBool(RLG.material.useNormalMap);
end;

procedure RLG_SetShininess(value: Single);
begin
  RLG.material.shininess := value;
  SetShaderValue(RLG.shader, RLG.locsMaterial.shininess,
      @RLG.material.shininess, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetShininess: Single;
begin
  result := RLG.material.shininess;
end;

procedure RLG_SetSpecular(value: Single);
begin
  RLG.material.mSpecular := value;
  SetShaderValue(RLG.shader, RLG.locsMaterial.mSpecular,
      @RLG.material.mSpecular, SHADER_UNIFORM_FLOAT);
end;

function RLG_GetSpecular: Single;
begin
  result := RLG.material.mSpecular;
end;

procedure RLG_SetAmbient(r, g, b: Single);
begin
  RLG.material.ambient := Vector3Create( r, g, b );
  SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
  @RLG.material.ambient, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetAmbientV(color: TVector3);
begin
  RLG.material.ambient := color;
  SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
      @RLG.material.ambient, SHADER_UNIFORM_VEC3);
end;

procedure RLG_SetAmbientC(color: TColorB);
begin
  RLG.material.ambient := Vector3Create(color.r*(1 div 255), color.g*(1 div 255), color.b*(1 div 255));
  SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
  @RLG.material.ambient, SHADER_UNIFORM_VEC3);
end;

function RLG_GetAmbient: TVector3;
begin
  result := RLG.material.ambient;
end;

function RLG_GetAmbientC: TColorB;
begin
  result := ColorCreate(Trunc(255*RLG.material.ambient.x),
                        Trunc(255*RLG.material.ambient.y),
                        Trunc(255*RLG.material.ambient.z), 255);
end;

function RLG_GetLightcount(): LongWord;
begin
 result := RLG.lightCount;
end;

procedure RLG_ToggleLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_ToggleLight'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    exit;                                                                          //todo fix textformat
  end;

  RLG.lights[light].active := not RLG.lights[light].active;
  SetShaderValue(RLG.shader, RLG.locsLights[light].active,
  @RLG.lights[light].active, SHADER_UNIFORM_INT);
end;

procedure RLG_EnableLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_EnableLight'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    exit;                                               //fixme
  end;

  RLG.lights[light].active := 1;
  SetShaderValue(RLG.shader, RLG.locsLights[light].active,
  @RLG.lights[light].active, SHADER_UNIFORM_INT);
end;

procedure RLG_DisableLight(light: LongWord);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_DisableLight'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    Exit;                                                                                      /// fixMe
  end;
  RLG.lights[light].active := 0;
  SetShaderValue(RLG.shader, RLG.locsLights[light].active,
  @RLG.lights[light].active, SHADER_UNIFORM_INT);
end;

function RLG_IsLightEnabled(light: LongWord): Boolean;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_IsLightEnabled'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    result := false;                                                                             //fixme
  end;
  result := IntToBool(RLG.lights[light].active);
end;

procedure RLG_SetLightType(light: LongWord; type_: TRLG_LightType);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_SetLightType'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    Exit;
  end;
  RLG.lights[light].type_ := type_;
  SetShaderValue(RLG.shader, RLG.locsLights[light].type_,
  @RLG.lights[light].type_, SHADER_UNIFORM_INT);

end;

function RLG_GetLightType(light: LongWord): TRLG_LightType;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_GetLightType'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    result := TRLG_LightType(0);
  end else
  result := TRLG_LightType(RLG.lights[light].type_);
end;

procedure RLG_SetLightPosition(light: LongWord; x, y, z: Single);
begin
  RLG_SetLightPositionV(light, Vector3Create( x, y, z ));
end;

procedure RLG_SetLightPositionV(light: LongWord; position: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_SetLightPosition'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    Exit;                                                           //fime
  end;
  RLG.lights[light].position := position;
  SetShaderValue(RLG.shader, RLG.locsLights[light].position,
  @RLG.lights[light].position, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightPosition(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_GetLightPosition'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    result := Vector3Create(0, 0, 0);
  end else
   result := RLG.lights[light].position;
end;

procedure RLG_SetLightDirection(light: LongWord; x, y, z: Single);
begin
  RLG_SetLightDirectionV(light, Vector3Create( x, y, z ));
end;

procedure RLG_SetLightDirectionV(light: LongWord; direction: TVector3);
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_SetLightDirection'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    Exit;
  end;
  RLG.lights[light].direction := direction;
  SetShaderValue(RLG.shader, RLG.locsLights[light].direction,
  @RLG.lights[light].direction, SHADER_UNIFORM_VEC3);
end;

function RLG_GetLightDirection(light: LongWord): TVector3;
begin
  if (light >= RLG.lightCount) then
  begin
    TraceLog(LOG_WARNING, 'Light ID specified to ''RLG_GetLightDirection'' exceeds allocated number. [%i/%i]', light, RLG.lightCount);
    result := Vector3Create(0, 0, 0);
  end else
  result := RLG.lights[light].direction;
end;


end.

