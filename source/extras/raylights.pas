unit raylights;
{$mode objfpc}{$H+}


interface

{$IFDEF LINUX}
  {$IFNDEF RAY_DYNAMIC}
    {$DEFINE RAY_STATIC}
  {$ENDIF}
{$ENDIF}

{$IFNDEF RAY_STATIC}
const
  cDllName = {$IFDEF WINDOWS} 'raylib.dll' {$IFEND}
             {$IFDEF DARWIN} 'libraylib.dylib' {$IFEND}
             {$IFDEF LINUX} 'libraylib.so' {$IFEND};
{$ENDIF}

uses
  raylib;

type
  //Enum representing different types of lights.
  PRLG_LightType = ^TRLG_LightType;
  TRLG_LightType = Integer;
  const
    RLG_DIRLIGHT    = TRLG_LightType(0); // Enum representing a directional light type.
    RLG_OMNILIGHT   = TRLG_LightType(1); // Enum representing an omnilight type.
    RLG_SPOTLIGHT   = TRLG_LightType(2); // Enum representing a spotlight type.

type
  PRLG_LightProperty = ^TRLG_LightProperty;
  TRLG_LightProperty = Integer;
  const
    RLG_LIGHT_POSITION               = TRLG_LightProperty(0);  // Position of the light.
    RLG_LIGHT_DIRECTION              = TRLG_LightProperty(1);  // Direction of the light.
    RLG_LIGHT_COLOR                  = TRLG_LightProperty(2);  // Diffuse color of the light.
    RLG_LIGHT_ENERGY                 = TRLG_LightProperty(3);  // Energy factor of the light.
    RLG_LIGHT_SPECULAR               = TRLG_LightProperty(4);  // Specular tint color of the light.
    RLG_LIGHT_SIZE                   = TRLG_LightProperty(5);  // Light size, affects fade and shadow blur (spotlight, omnilight only).
    RLG_LIGHT_INNER_CUTOFF           = TRLG_LightProperty(6);  // Inner cutoff angle of a spotlight.
    RLG_LIGHT_OUTER_CUTOFF           = TRLG_LightProperty(7);  // Outer cutoff angle of a spotlight.
    RLG_LIGHT_ATTENUATION_CLQ        = TRLG_LightProperty(8);  // Attenuation coefficients (constant, linear, quadratic) of the light.
    RLG_LIGHT_ATTENUATION_CONSTANT   = TRLG_LightProperty(9);  // Constant attenuation coefficient of the light.
    RLG_LIGHT_ATTENUATION_LINEAR     = TRLG_LightProperty(10); // Linear attenuation coefficient of the light.
    RLG_LIGHT_ATTENUATION_QUADRATIC  = TRLG_LightProperty(11); // Quadratic attenuation coefficient of the light.

type
  PRLG_ShaderLocationIndex = ^TRLG_ShaderLocationIndex;
  TRLG_ShaderLocationIndex = Integer;
  const
    RLG_LOC_VERTEX_POSITION          = TRLG_ShaderLocationIndex(0);
    RLG_LOC_VERTEX_TEXCOORD01        = TRLG_ShaderLocationIndex(1);
    RLG_LOC_VERTEX_TEXCOORD02        = TRLG_ShaderLocationIndex(2);
    RLG_LOC_VERTEX_NORMAL            = TRLG_ShaderLocationIndex(3);
    RLG_LOC_VERTEX_TANGENT           = TRLG_ShaderLocationIndex(4);
    RLG_LOC_VERTEX_COLOR             = TRLG_ShaderLocationIndex(5);
    RLG_LOC_MATRIX_MVP               = TRLG_ShaderLocationIndex(6);
    RLG_LOC_MATRIX_VIEW              = TRLG_ShaderLocationIndex(7);
    RLG_LOC_MATRIX_PROJECTION        = TRLG_ShaderLocationIndex(8);
    RLG_LOC_MATRIX_MODEL             = TRLG_ShaderLocationIndex(9);
    RLG_LOC_MATRIX_NORMAL            = TRLG_ShaderLocationIndex(10);
    RLG_LOC_VECTOR_VIEW              = TRLG_ShaderLocationIndex(11);
    RLG_LOC_COLOR_DIFFUSE            = TRLG_ShaderLocationIndex(12);
    RLG_LOC_COLOR_SPECULAR           = TRLG_ShaderLocationIndex(13);
    RLG_LOC_COLOR_AMBIENT            = TRLG_ShaderLocationIndex(14);
    RLG_LOC_MAP_ALBEDO               = TRLG_ShaderLocationIndex(15);
    RLG_LOC_MAP_METALNESS            = TRLG_ShaderLocationIndex(16);
    RLG_LOC_MAP_NORMAL               = TRLG_ShaderLocationIndex(17);
    RLG_LOC_MAP_ROUGHNESS            = TRLG_ShaderLocationIndex(18);
    RLG_LOC_MAP_OCCLUSION            = TRLG_ShaderLocationIndex(19);
    RLG_LOC_MAP_EMISSION             = TRLG_ShaderLocationIndex(20);
    RLG_LOC_MAP_HEIGHT               = TRLG_ShaderLocationIndex(21);
    RLG_LOC_MAP_CUBEMAP              = TRLG_ShaderLocationIndex(22);
    RLG_LOC_MAP_IRRADIANCE           = TRLG_ShaderLocationIndex(23);
    RLG_LOC_MAP_PREFILTER            = TRLG_ShaderLocationIndex(24);
    RLG_LOC_MAP_BRDF                 = TRLG_ShaderLocationIndex(25);
    RLG_LOC_COLOR_EMISSION           = TRLG_ShaderLocationIndex(26);
    RLG_LOC_METALNESS_SCALE          = TRLG_ShaderLocationIndex(27);
    RLG_LOC_ROUGHNESS_SCALE          = TRLG_ShaderLocationIndex(28);
    RLG_LOC_AO_LIGHT_AFFECT          = TRLG_ShaderLocationIndex(29);
    RLG_LOC_HEIGHT_SCALE             = TRLG_ShaderLocationIndex(30);
    RLG_COUNT_LOCS                   = TRLG_ShaderLocationIndex(31);


  {Structure representing a skybox with associated textures and buffers.

  This structure contains the textures and buffer IDs necessary for rendering
  a skybox. It includes the cubemap texture, the irradiance texture, vertex
  buffer object (VBO) IDs, vertex array object (VAO) ID, and a flag indicating
  whether the skybox is in high dynamic range (HDR).}
type
  PRLG_Skybox = ^TRLG_Skybox;
  TRLG_Skybox = record
    cubemap:  TTextureCubemap;    // The cubemap texture representing the skybox.
    irradiance: TTextureCubemap;  // The irradiance cubemap texture for diffuse lighting.
    vboPostionsID: Integer;       // The ID of the vertex buffer object for positions.
    vboIndicesID: Integer;        // The ID of the vertex buffer object for indices.
    vaoID: Integer;               // The ID of the vertex array object.
    isHDR: Boolean;               // Flag indicating if the skybox is HDR (high dynamic range).
  end;


type
  TRLG_Context = Pointer;
  TRLG_DrawFunc = Pointer;

function RLG_CreateContext(lightCount: LongWord): TRLG_Context;  cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'RLG_CreateContext';




implementation
uses SysUtils;


end.

