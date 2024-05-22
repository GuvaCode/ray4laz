#ifndef RLIGHTS_H
#define RLIGHTS_H

#include <raylib.h>

typedef enum {
    RLG_DIRECTIONAL,
    RLG_OMNILIGHT,
    RLG_SPOTLIGHT
} RLG_LightType;

#if defined(__cplusplus)
extern "C" {
#endif

void RLG_Init(unsigned int count);
void RLG_Close(void);

const Shader* RLG_GetShader(void);

void RLG_SetViewPosition(float x, float y, float z);
void RLG_SetViewPositionV(Vector3 position);

void RLG_EnableSpecularMap(void);
void RLG_DisableSpecularMap(void);
bool RLG_IsSpecularMapEnabled(void);

void RLG_EnableNormalMap(void);
void RLG_DisableNormalMap(void);
bool RLG_IsNormalMapEnabled(void);

void RLG_SetShininess(float value);
float RLG_GetShininess(void);

void RLG_SetSpecular(float value);
float RLG_GetSpecular(void);

void RLG_SetAmbient(float r, float g, float b);
void RLG_SetAmbientV(Vector3 color);
void RLG_SetAmbientC(Color color);
Vector3 RLG_GetAmbient(void);
Color RLG_GetAmbientC(void);

unsigned int RLG_GetLightcount(void);

void RLG_ToggleLight(unsigned int light);
void RLG_EnableLight(unsigned int light);
void RLG_DisableLight(unsigned int light);
bool RLG_IsLightEnabled(unsigned int light);

void RLG_SetLightType(unsigned int light, RLG_LightType type);
RLG_LightType RLG_GetLightType(unsigned int light);

void RLG_SetLightPosition(unsigned int light, float x, float y, float z);
void RLG_SetLightPositionV(unsigned int light, Vector3 position);
Vector3 RLG_GetLightPosition(unsigned int light);

void RLG_SetLightDirection(unsigned int light, float x, float y, float z);
void RLG_SetLightDirectionV(unsigned int light, Vector3 direction);
Vector3 RLG_GetLightDirection(unsigned int light);

void RLG_SetLightTarget(unsigned int light, float x, float y, float z);
void RLG_SetLightTargetV(unsigned int light, Vector3 targetPosition);
Vector3 RLG_GetLightTarget(unsigned int light);

void RLG_SetLightDiffuse(unsigned int light, float r, float g, float b);
void RLG_SetLightDiffuseV(unsigned int light, Vector3 color);
void RLG_SetLightDiffuseC(unsigned int light, Color color);
Vector3 RLG_GetLightDiffuse(unsigned int light);
Color RLG_GetLightDiffuseC(unsigned int light);

void RLG_SetLightSpecular(unsigned int light, float r, float g, float b);
void RLG_SetLightSpecularV(unsigned int light, Vector3 color);
void RLG_SetLightSpecularC(unsigned int light, Color color);
Vector3 RLG_GetLightSpecular(unsigned int light);
Color RLG_GetLightSpecularC(unsigned int light);

void RLG_SetLightInnerCutOff(unsigned int light, float degrees);
float RLG_GetLightInnerCutoff(unsigned int light);

void RLG_SetLightOuterCutOff(unsigned int light, float degrees);
float RLG_GetLightOuterCutoff(unsigned int light);

void RLG_SetLightAttenuation(unsigned int light, float constant, float linear, float quadratic);
void RLG_GetLightAttenuation(unsigned int light, float* constant, float* linear, float* quadratic);

void RLG_SetLightAttenuationQuadratic(unsigned int light, float quadratic);
void RLG_SetLightAttenuationConstant(unsigned int light, float constant);
void RLG_SetLightAttenuationLinear(unsigned int light, float linear);

#if defined(__cplusplus)
}
#endif

#ifdef RLIGHTS_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <raymath.h>

/* Shader */

/*
 * Here we directly use the TBN matrix to transform the normals in the fragment shader
 * rather than the inverse of TBN in the vertex shader for simplicity, since we cannot
 * transmit a varying array containing all the transformed light positions in the tangent
 * space, we should thus transform them in the fragment shader. Therefore, transforming
 * only the normals directly in the fragment shader remains more optimized in this
 * specific case.
 */

static const char rlgLightVS[] =
    "#version 100\n"

    "attribute vec3 vertexPosition;"
    "attribute vec2 vertexTexCoord;"
    "attribute vec4 vertexTangent;"
    "attribute vec3 vertexNormal;"

    "uniform lowp int useNormalMap;"
    "uniform mat4 matNormal;"
    "uniform mat4 matModel;"
    "uniform mat4 mvp;"

    "varying vec3 fragPosition;"
    "varying vec2 fragTexCoord;"
    "varying vec3 fragNormal;"
    "varying mat3 TBN;"

    "void main()"
    "{"
        "fragPosition = vec3(matModel*vec4(vertexPosition, 1.0));"
        "fragNormal = (matNormal*vec4(vertexNormal, 0.0)).xyz;"
        "fragTexCoord = vertexTexCoord;"

        "if (useNormalMap != 0)"
        "{"
            "vec3 T = normalize(vec3(matModel*vec4(vertexTangent.xyz, 0.0)));"
            "vec3 B = cross(fragNormal, T)*vertexTangent.w;"
            "TBN = mat3(T, B, fragNormal);"
        "}"

        "gl_Position = mvp*vec4(vertexPosition, 1.0);"
    "}";

static const char rlgLightFS[] =
    "#version 100\n"
    "#define NUM_LIGHT %i\n"

    "#define DIRECTIONAL_LIGHT  0\n"
    "#define OMNI_LIGHT         1\n"
    "#define SPOT_LIGHT         2\n"

    "precision mediump float;"

    "varying vec3 fragPosition;"
    "varying vec2 fragTexCoord;"
    "varying vec3 fragNormal;"
    "varying mat3 TBN;"

    "struct Light {"
        "vec3 position;"
        "vec3 direction;"
        "vec3 diffuse;"
        "vec3 specular;"
        "float innerCutOff;"
        "float outerCutOff;"
        "float constant;"
        "float linear;"
        "float quadratic;"
        "lowp int type;"
        "lowp int active;"
    "};"

    "uniform Light lights[NUM_LIGHT];"

    "uniform lowp int useSpecularMap;"
    "uniform lowp int useNormalMap;"

    "uniform sampler2D texture0;"   // diffuse
    "uniform sampler2D texture1;"   // specular
    "uniform sampler2D texture2;"   // normal

    "uniform float shininess;"
    "uniform float mSpecular;"
    "uniform vec3 ambient;"

    "uniform vec3 viewPos;"

    "void main()"
    "{"
        // get texture samples
        "vec3 diffSample = texture2D(texture0, fragTexCoord).rgb;"
        "vec3 specSample = (useSpecularMap != 0) ? texture2D(texture1, fragTexCoord).rgb : vec3(1.0);"

        // ambient
        "vec3 ambientColor = diffSample*ambient;"

        // compute normals
        "vec3 normal;"
        "if (useNormalMap == 0) normal = normalize(fragNormal);"
        "else normal = normalize(TBN*(texture2D(texture2, fragTexCoord).rgb*2.0 - 1.0));"

        // compute current view dir for this frag
        "vec3 viewDir = normalize(viewPos - fragPosition);"

        // process lights
        "vec3 finalColor = vec3(0.0);"
        "for (int i = 0; i < NUM_LIGHT; i++)"
        "{"
            "if (lights[i].active != 0)"
            "{"
                // get lightDir
                "vec3 lightDir = (lights[i].type != DIRECTIONAL_LIGHT)"
                    "? normalize(lights[i].position - fragPosition)"
                    ": normalize(-lights[i].direction);"

                // diffuse
                "float diff = max(dot(normal, lightDir), 0.0);"
                "vec3 diffuse = lights[i].diffuse*diffSample*diff;"

                // specular (Blinn-Phong)
                "vec3 halfwayDir = normalize(lightDir + viewDir);"
                "float spec = pow(max(dot(normal, halfwayDir), 0.0), shininess);"
                "vec3 specular = lights[i].specular*specSample*spec*mSpecular;"

                // spotlight
                "float intensity = 1.0;"
                "if (lights[i].type == SPOT_LIGHT)"
                "{"
                    "float theta = dot(lightDir, normalize(-lights[i].direction));"
                    "float epsilon = (lights[i].innerCutOff - lights[i].outerCutOff);"
                    "intensity = smoothstep(0.0, 1.0, (theta - lights[i].outerCutOff) / epsilon);"
                "}"

                // attenuation
                "float distance    = length(lights[i].position - fragPosition);"
                "float attenuation = 1.0/(lights[i].constant + lights[i].linear*distance + lights[i].quadratic*(distance*distance));"

                // add final light color
                "finalColor += (diffuse + specular)*intensity*attenuation;"
            "}"
        "}"

        "gl_FragColor = vec4(ambientColor + finalColor, 1.0);"
    "}";


/* Types definitions */

struct RLG_MaterialLocs
{
    int useSpecularMap;
    int useNormalMap;
    int shininess;
    int mSpecular;
    int ambient;
};

struct RLG_Material
{
    int useSpecularMap;
    int useNormalMap;
    float shininess;
    float mSpecular;
    Vector3 ambient;
};

struct RLG_LightLocs
{
    int position;
    int direction;
    int diffuse;
    int specular;
    int innerCutOff;
    int outerCutOff;
    int constant;
    int linear;
    int quadratic;
    int type;
    int active;
};

struct RLG_Light
{
    Vector3 position;
    Vector3 direction;
    Vector3 diffuse;
    Vector3 specular;
    float innerCutOff;
    float outerCutOff;
    float constant;
    float linear;
    float quadratic;
    int type;
    int active;
};

static struct RLG_Core
{
    struct RLG_MaterialLocs locsMaterial;
    struct RLG_Material material;

    struct RLG_LightLocs *locsLights;
    struct RLG_Light *lights;
    int lightCount;

    Vector3 viewPos;
    int locViewPos;

    Shader shader;
}
RLG = { 0 };


/* Public API */

void RLG_Init(unsigned int count)
{
    if (RLG.lights != NULL)
    {
        TraceLog(LOG_WARNING, "You are trying to initialize rlights when it has already been initialized.");
        return;
    }

    // NOTE: The limit of 99 is because we measure the size of `rlgLightFS` with '%s'
    if (count > 99)
    {
        TraceLog(LOG_WARNING, "The limit of lights supported by rlights is 99."
                              "The number of lights has therefore been adjusted to this value.");
        count = 99;
    }

    // Format frag shader with lights count
    char *fmtFrag = (char*)malloc(sizeof(rlgLightFS));
    snprintf(fmtFrag, sizeof(rlgLightFS), rlgLightFS, count);

    // Load shader and get locations
    RLG.shader = LoadShaderFromMemory(rlgLightVS, fmtFrag);
    free(fmtFrag);

    // Retrieving viewpos loc and set default value
    RLG.locViewPos = GetShaderLocation(RLG.shader, "viewPos");
    RLG.viewPos = (Vector3) { 0 };

    // Retrieving global shader locations
    RLG.locsMaterial.useSpecularMap = GetShaderLocation(RLG.shader, "useSpecularMap");
    RLG.locsMaterial.useNormalMap = GetShaderLocation(RLG.shader, "useNormalMap");
    RLG.locsMaterial.shininess = GetShaderLocation(RLG.shader, "shininess");
    RLG.locsMaterial.mSpecular = GetShaderLocation(RLG.shader, "mSpecular");
    RLG.locsMaterial.ambient = GetShaderLocation(RLG.shader, "ambient");

    // Define default global uniforms
    RLG.material = (struct RLG_Material) { 0 };
    RLG.material.shininess = 32.0f;
    RLG.material.mSpecular = 1.0f;
    RLG.material.ambient = (Vector3) { 0.1f, 0.1f, 0.1f };

    // Send default globals uniforms (no need to send zero-values)
    SetShaderValue(RLG.shader, RLG.locsMaterial.shininess, &RLG.material.shininess, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, RLG.locsMaterial.mSpecular, &RLG.material.mSpecular, SHADER_UNIFORM_FLOAT);
    SetShaderValue(RLG.shader, RLG.locsMaterial.ambient, &RLG.material.ambient, SHADER_UNIFORM_VEC3);

    // Allocation and initialization of the desired number of lights
    RLG.lights = (struct RLG_Light*)malloc(count*sizeof(struct RLG_Light));
    RLG.locsLights = (struct RLG_LightLocs*)malloc(count*sizeof(struct RLG_LightLocs));
    for (int i = 0; i < count; i++)
    {
        struct RLG_Light *light = &RLG.lights[i];
        struct RLG_LightLocs *locs = &RLG.locsLights[i];

        *light = (struct RLG_Light) {
            .position = (Vector3) { 0 },
            .direction = (Vector3) { 0 },
            .diffuse = (Vector3) { 1.0f, 1.0f, 1.0f },
            .specular = (Vector3) { 1.0f, 1.0f, 1.0f },
            .innerCutOff = -1.0f,
            .outerCutOff = -1.0f,
            .constant = 1.0f,
            .linear = 0.0f,
            .quadratic = 0.0f,
            .active = 0
        };

        locs->position = GetShaderLocation(RLG.shader, TextFormat("lights[%i].position", i));
        locs->direction = GetShaderLocation(RLG.shader, TextFormat("lights[%i].direction", i));
        locs->diffuse = GetShaderLocation(RLG.shader, TextFormat("lights[%i].diffuse", i));
        locs->specular = GetShaderLocation(RLG.shader, TextFormat("lights[%i].specular", i));
        locs->innerCutOff = GetShaderLocation(RLG.shader, TextFormat("lights[%i].innerCutOff", i));
        locs->outerCutOff = GetShaderLocation(RLG.shader, TextFormat("lights[%i].outerCutOff", i));
        locs->constant = GetShaderLocation(RLG.shader, TextFormat("lights[%i].constant", i));
        locs->linear = GetShaderLocation(RLG.shader, TextFormat("lights[%i].linear", i));
        locs->quadratic = GetShaderLocation(RLG.shader, TextFormat("lights[%i].quadratic", i));
        locs->type = GetShaderLocation(RLG.shader, TextFormat("lights[%i].type", i));
        locs->active = GetShaderLocation(RLG.shader, TextFormat("lights[%i].active", i));

        SetShaderValue(RLG.shader, locs->position, &light->position, SHADER_UNIFORM_VEC3);
        SetShaderValue(RLG.shader, locs->direction, &light->direction, SHADER_UNIFORM_VEC3);
        SetShaderValue(RLG.shader, locs->diffuse, &light->diffuse, SHADER_UNIFORM_VEC3);
        SetShaderValue(RLG.shader, locs->specular, &light->specular, SHADER_UNIFORM_VEC3);
        SetShaderValue(RLG.shader, locs->innerCutOff, &light->innerCutOff, SHADER_UNIFORM_FLOAT);
        SetShaderValue(RLG.shader, locs->outerCutOff, &light->outerCutOff, SHADER_UNIFORM_FLOAT);
        SetShaderValue(RLG.shader, locs->constant, &light->constant, SHADER_UNIFORM_FLOAT);
        SetShaderValue(RLG.shader, locs->linear, &light->linear, SHADER_UNIFORM_FLOAT);
        SetShaderValue(RLG.shader, locs->quadratic, &light->quadratic, SHADER_UNIFORM_FLOAT);
        SetShaderValue(RLG.shader, locs->type, &light->type, SHADER_UNIFORM_INT);
        SetShaderValue(RLG.shader, locs->active, &light->active, SHADER_UNIFORM_INT);
    }

    RLG.lightCount = count;
}

void RLG_Close(void)
{
    if (IsShaderReady(RLG.shader))
    {
        UnloadShader(RLG.shader);
        RLG.shader = (Shader) { 0 };
    }

    if (RLG.lights != NULL)
    {
        free(RLG.lights);
        RLG.lights = NULL;
    }

    if (RLG.locsLights != NULL)
    {
        free(RLG.locsLights);
        RLG.locsLights = NULL;
    }

    RLG.lightCount = 0;
}

const Shader* RLG_GetShader(void)
{
    if (IsShaderReady(RLG.shader))
    {
        return &RLG.shader;
    }

    return NULL;
}

void RLG_SetViewPosition(float x, float y, float z)
{
    RLG_SetViewPositionV((Vector3) { x, y, z });
}

void RLG_SetViewPositionV(Vector3 position)
{
    RLG.viewPos = position;
    SetShaderValue(RLG.shader, RLG.locViewPos,
        &RLG.viewPos, SHADER_UNIFORM_VEC3);
}

void RLG_EnableSpecularMap(void)
{
    const int v = 1;
    SetShaderValue(RLG.shader, RLG.locsMaterial.useSpecularMap, &v, SHADER_UNIFORM_INT);
}

void RLG_DisableSpecularMap(void)
{
    const int v = 0;
    SetShaderValue(RLG.shader, RLG.locsMaterial.useSpecularMap, &v, SHADER_UNIFORM_INT);
}

bool RLG_IsSpecularMapEnabled(void)
{
    return RLG.material.useSpecularMap;
}

void RLG_EnableNormalMap(void)
{
    const int v = 1;
    SetShaderValue(RLG.shader, RLG.locsMaterial.useNormalMap, &v, SHADER_UNIFORM_INT);
}

void RLG_DisableNormalMap(void)
{
    const int v = 0;
    SetShaderValue(RLG.shader, RLG.locsMaterial.useNormalMap, &v, SHADER_UNIFORM_INT);
}

bool RLG_IsNormalMapEnabled(void)
{
    return RLG.material.useNormalMap;
}

void RLG_SetShininess(float value)
{
    RLG.material.shininess = value;
    SetShaderValue(RLG.shader, RLG.locsMaterial.shininess,
        &RLG.material.shininess, SHADER_UNIFORM_FLOAT);
}

float RLG_GetShininess(void)
{
    return RLG.material.shininess;
}

void RLG_SetSpecular(float value)
{
    RLG.material.mSpecular = value;
    SetShaderValue(RLG.shader, RLG.locsMaterial.mSpecular,
        &RLG.material.mSpecular, SHADER_UNIFORM_FLOAT);
}

float RLG_GetSpecular(void)
{
    return RLG.material.mSpecular;
}

void RLG_SetAmbient(float r, float g, float b)
{
    RLG.material.ambient = (Vector3) { r, g, b };
    SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
        &RLG.material.ambient, SHADER_UNIFORM_VEC3);
}

void RLG_SetAmbientV(Vector3 color)
{
    RLG.material.ambient = color;
    SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
        &RLG.material.ambient, SHADER_UNIFORM_VEC3);
}

void RLG_SetAmbientC(Color color)
{
    RLG.material.ambient = (Vector3) {
        (float)color.r*(1.0f/255),
        (float)color.g*(1.0f/255),
        (float)color.b*(1.0f/255)
    };

    SetShaderValue(RLG.shader, RLG.locsMaterial.ambient,
        &RLG.material.ambient, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetAmbient(void)
{
    return RLG.material.ambient;
}

Color RLG_GetAmbientC(void)
{
    return (Color) {
        (unsigned char)(255*RLG.material.ambient.x),
        (unsigned char)(255*RLG.material.ambient.y),
        (unsigned char)(255*RLG.material.ambient.z),
        255
    };
}

unsigned int RLG_GetLightcount(void)
{
    return RLG.lightCount;
}

void RLG_ToggleLight(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_ToggleLight' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].active = !RLG.lights[light].active;
    SetShaderValue(RLG.shader, RLG.locsLights[light].active,
        &RLG.lights[light].active, SHADER_UNIFORM_INT);
}

void RLG_EnableLight(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_EnableLight' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].active = 1;
    SetShaderValue(RLG.shader, RLG.locsLights[light].active,
        &RLG.lights[light].active, SHADER_UNIFORM_INT);
}

void RLG_DisableLight(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_DisableLight' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].active = 0;
    SetShaderValue(RLG.shader, RLG.locsLights[light].active,
        &RLG.lights[light].active, SHADER_UNIFORM_INT);
}

bool RLG_IsLightEnabled(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_IsLightEnabled' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return false;
    }

    return (bool)RLG.lights[light].active;
}

void RLG_SetLightType(unsigned int light, RLG_LightType type)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightType' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].type = (int)type;
    SetShaderValue(RLG.shader, RLG.locsLights[light].type,
        &RLG.lights[light].type, SHADER_UNIFORM_INT);
}

RLG_LightType RLG_GetLightType(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightType' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (RLG_LightType)0;
    }

    return (RLG_LightType)RLG.lights[light].type;
}

void RLG_SetLightPosition(unsigned int light, float x, float y, float z)
{
    RLG_SetLightPositionV(light, (Vector3) { x, y, z });
}

void RLG_SetLightPositionV(unsigned int light, Vector3 position)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightPosition' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].position = position;
    SetShaderValue(RLG.shader, RLG.locsLights[light].position,
        &RLG.lights[light].position, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetLightPosition(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightPosition' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (Vector3) { 0 };
    }

    return RLG.lights[light].position;
}

void RLG_SetLightDirection(unsigned int light, float x, float y, float z)
{
    RLG_SetLightDirectionV(light, (Vector3) { x, y, z });
}

void RLG_SetLightDirectionV(unsigned int light, Vector3 direction)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightDirection' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].direction = direction;
    SetShaderValue(RLG.shader, RLG.locsLights[light].direction,
        &RLG.lights[light].direction, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetLightDirection(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightDirection' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (Vector3) { 0 };
    }

    return RLG.lights[light].direction;
}

void RLG_SetLightTarget(unsigned int light, float x, float y, float z)
{
    RLG_SetLightTargetV(light, (Vector3) { x, y, z });
}

void RLG_SetLightTargetV(unsigned int light, Vector3 targetPosition)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightTarget' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].direction = Vector3Normalize(Vector3Subtract(
        targetPosition, RLG.lights[light].position));

    SetShaderValue(RLG.shader, RLG.locsLights[light].direction,
        &RLG.lights[light].direction, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetLightTarget(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightTarget' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (Vector3) { 0 };
    }

    return Vector3Add(
        RLG.lights[light].position,
        RLG.lights[light].direction);
}

void RLG_SetLightDiffuse(unsigned int light, float r, float g, float b)
{
    RLG_SetLightDiffuseV(light, (Vector3) { r, g, b });
}

void RLG_SetLightDiffuseV(unsigned int light, Vector3 color)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightDiffuse' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].diffuse = color;
    SetShaderValue(RLG.shader, RLG.locsLights[light].diffuse,
        &RLG.lights[light].diffuse, SHADER_UNIFORM_VEC3);
}

void RLG_SetLightDiffuseC(unsigned int light, Color color)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightDiffuseC' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].diffuse = (Vector3) {
        (float)color.r*(1.0f/255),
        (float)color.g*(1.0f/255),
        (float)color.b*(1.0f/255)
    };

    SetShaderValue(RLG.shader, RLG.locsLights[light].diffuse,
        &RLG.lights[light].diffuse, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetLightDiffuse(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightDiffuse' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (Vector3) { 0 };
    }

    return RLG.lights[light].diffuse;
}

Color RLG_GetLightDiffuseC(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightDiffuseC' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return BLANK;
    }

    return (Color) {
        (unsigned char)(255*RLG.lights[light].diffuse.x),
        (unsigned char)(255*RLG.lights[light].diffuse.y),
        (unsigned char)(255*RLG.lights[light].diffuse.z),
        255
    };
}

void RLG_SetLightSpecular(unsigned int light, float r, float g, float b)
{
    RLG_SetLightSpecularV(light, (Vector3) { r, g, b });
}

void RLG_SetLightSpecularV(unsigned int light, Vector3 color)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightSpecular' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].specular = color;
    SetShaderValue(RLG.shader, RLG.locsLights[light].specular,
        &RLG.lights[light].specular, SHADER_UNIFORM_VEC3);
}

void RLG_SetLightSpecularC(unsigned int light, Color color)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightSpecularC' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].specular = (Vector3) {
        (float)color.r*(1.0f/255),
        (float)color.g*(1.0f/255),
        (float)color.b*(1.0f/255)
    };

    SetShaderValue(RLG.shader, RLG.locsLights[light].specular,
        &RLG.lights[light].specular, SHADER_UNIFORM_VEC3);
}

Vector3 RLG_GetLightSpecular(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightSpecular' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return (Vector3) { 0 };
    }

    return RLG.lights[light].specular;
}

Color RLG_GetLightSpecularC(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightSpecularC' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return BLANK;
    }

    return (Color) {
        (unsigned char)(255*RLG.lights[light].diffuse.x),
        (unsigned char)(255*RLG.lights[light].diffuse.y),
        (unsigned char)(255*RLG.lights[light].diffuse.z),
        255
    };
}

void RLG_SetLightInnerCutOff(unsigned int light, float degrees)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightInnerCutOff' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].innerCutOff = cosf(degrees*DEG2RAD);
    SetShaderValue(RLG.shader, RLG.locsLights[light].innerCutOff,
        &RLG.lights[light].innerCutOff, SHADER_UNIFORM_FLOAT);
}

float RLG_GetLightInnerCutoff(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightInnerCutoff' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return 0;
    }

    return RLG.lights[light].innerCutOff;
}

void RLG_SetLightOuterCutOff(unsigned int light, float degrees)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightOuterCutOff' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].outerCutOff = cosf(degrees*DEG2RAD);
    SetShaderValue(RLG.shader, RLG.locsLights[light].outerCutOff,
        &RLG.lights[light].outerCutOff, SHADER_UNIFORM_FLOAT);
}

float RLG_GetLightOuterCutoff(unsigned int light)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightOuterCutoff' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return 0;
    }

    return RLG.lights[light].outerCutOff;
}

void RLG_SetLightAttenuation(unsigned int light, float constant, float linear, float quadratic)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightAttenuation' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].constant = constant;
    RLG.lights[light].linear = linear;
    RLG.lights[light].quadratic = quadratic;

    SetShaderValue(RLG.shader, RLG.locsLights[light].constant,
        &RLG.lights[light].constant, SHADER_UNIFORM_FLOAT);

    SetShaderValue(RLG.shader, RLG.locsLights[light].linear,
        &RLG.lights[light].linear, SHADER_UNIFORM_FLOAT);

    SetShaderValue(RLG.shader, RLG.locsLights[light].quadratic,
        &RLG.lights[light].quadratic, SHADER_UNIFORM_FLOAT);
}

void RLG_GetLightAttenuation(unsigned int light, float* constant, float* linear, float* quadratic)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_GetLightAttenuation' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    if (constant) *constant = RLG.lights[light].constant;
    if (linear) *linear = RLG.lights[light].linear;
    if (quadratic) *quadratic = RLG.lights[light].linear;
}

void RLG_SetLightAttenuationQuadratic(unsigned int light, float quadratic)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightAttenuationQuadratic' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].quadratic = quadratic;
    SetShaderValue(RLG.shader, RLG.locsLights[light].quadratic,
        &RLG.lights[light].quadratic, SHADER_UNIFORM_FLOAT);
}

void RLG_SetLightAttenuationConstant(unsigned int light, float constant)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightAttenuationConstant' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].constant = constant;
    SetShaderValue(RLG.shader, RLG.locsLights[light].constant,
        &RLG.lights[light].constant, SHADER_UNIFORM_FLOAT);
}

void RLG_SetLightAttenuationLinear(unsigned int light, float linear)
{
    if (light >= RLG.lightCount)
    {
        TraceLog(LOG_WARNING, "Light ID specified to 'RLG_SetLightAttenuationLinear' exceeds allocated number. [%i/%i]", light, RLG.lightCount);
        return;
    }

    RLG.lights[light].linear = linear;
    SetShaderValue(RLG.shader, RLG.locsLights[light].linear,
        &RLG.lights[light].linear, SHADER_UNIFORM_FLOAT);
}

#endif //RLIGHTS_IMPLEMENTATION
#endif //RLIGHTS_H