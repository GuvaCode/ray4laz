{**********************************************************************************************
*
*   rlgl v5.0 - A multi-OpenGL abstraction layer with an immediate-mode style API
*
*   An abstraction layer for multiple OpenGL versions (1.1, 2.1, 3.3 Core, ES 2.0)
*   that provides a pseudo-OpenGL 1.1 immediate-mode style API (rlVertex, rlTranslate, rlRotate...)
*
*   When chosing an OpenGL backend different than OpenGL 1.1, some internal buffer are
*   initialized on rlglInit() to accumulate vertex data.
*
*   When an internal state change is required all the stored vertex data is renderer in batch,
*   additioanlly, rlDrawRenderBatchActive() could be called to force flushing of the batch.
*
*   Some additional resources are also loaded for convenience, here the complete list:
*      - Default batch (RLGL.defaultBatch): RenderBatch system to accumulate vertex data
*      - Default texture (RLGL.defaultTextureId): 1x1 white pixel R8G8B8A8
*      - Default shader (RLGL.State.defaultShaderId, RLGL.State.defaultShaderLocs)
*
*   Internal buffer (and additional resources) must be manually unloaded calling rlglClose().
*
*
*   DEPENDENCIES:
*
*      - OpenGL libraries (depending on platform and OpenGL version selected)
*      - GLAD OpenGL extensions loading library (only for OpenGL 3.3 Core)
*
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2014-2023 Ramon Santamaria (@raysan5)
*   Pascal header 2022-2023 Gunko Vadim (@guvacode)
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


unit rlgl;

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I raylib.inc}


interface

uses raylib;

// Include configuration file
{$I raylib.inc}

// OpenGL 2.1 uses most of OpenGL 3.3 Core functionality
// WARNING: Specific parts are checked with #if defines
{$if defined(GRAPHICS_API_OPENGL_21)}
    {$define GRAPHICS_API_OPENGL_33}
{$endif}

// OpenGL 4.3 uses OpenGL 3.3 Core functionality
{$if defined(GRAPHICS_API_OPENGL_43)}
    {$define GRAPHICS_API_OPENGL_33}
{$endif}


const
  (* Default internal render batch elements limits *)
  {$ifndef RL_DEFAULT_BATCH_BUFFER_ELEMENTS}
     {$if defined (GRAPHICS_API_OPENGL_ES2)}
          // We reduce memory sizes for embedded systems (RPI and HTML5)
          // NOTE: On HTML5 (emscripten) this is allocated on heap,
          // by default it's only 16MB!...just take care...
          RL_DEFAULT_BATCH_BUFFER_ELEMENTS = 2048;
      {$else}
          // This is the maximum amount of elements (quads) per batch
          // NOTE: Be careful with text, every letter maps to a quad
          RL_DEFAULT_BATCH_BUFFER_ELEMENTS = 8192;
      {$endif}
  {$endif}

  RL_DEFAULT_BATCH_BUFFERS = 1;              // Default number of batch buffers (multi-buffering)
  RL_DEFAULT_BATCH_DRAWCALLS = 256;          // Default number of batch draw calls (by state changes: mode, texture)
  RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS = 4;    // Maximum number of textures units that can be activated on batch drawing (SetShaderValueTexture())
  RL_MAX_MATRIX_STACK_SIZE = 32;             // Maximum size of Matrix stack
  RL_MAX_SHADER_LOCATIONS = 32;              // Maximum number of shader locations supported
  RL_CULL_DISTANCE_NEAR = 0.01;              // Default near cull distance
  RL_CULL_DISTANCE_FAR = 1000.0;             // Default far cull distance

const
  (* Texture parameters (equivalent to OpenGL defines) *)
  RL_TEXTURE_WRAP_S = $2802;                        // GL_TEXTURE_WRAP_S
  RL_TEXTURE_WRAP_T = $2803;                        // GL_TEXTURE_WRAP_T
  RL_TEXTURE_MAG_FILTER = $2800;                    // GL_TEXTURE_MAG_FILTER
  RL_TEXTURE_MIN_FILTER = $2801;                    // GL_TEXTURE_MIN_FILTER

  RL_TEXTURE_FILTER_NEAREST = $2600;                // GL_NEAREST
  RL_TEXTURE_FILTER_LINEAR = $2601;                 // GL_LINEAR
  RL_TEXTURE_FILTER_MIP_NEAREST = $2700;            // GL_NEAREST_MIPMAP_NEAREST
  RL_TEXTURE_FILTER_NEAREST_MIP_LINEAR = $2702;     // GL_NEAREST_MIPMAP_LINEAR
  RL_TEXTURE_FILTER_LINEAR_MIP_NEAREST = $2701;     // GL_LINEAR_MIPMAP_NEAREST
  RL_TEXTURE_FILTER_MIP_LINEAR = $2703;             // GL_LINEAR_MIPMAP_LINEAR
  RL_TEXTURE_FILTER_ANISOTROPIC = $3000;            // Anisotropic filter (custom identifier)
  RL_TEXTURE_MIPMAP_BIAS_RATIO = $4000;             // Texture mipmap bias (percentage ratio)

  RL_TEXTURE_WRAP_REPEAT = $2901;                   // GL_REPEAT
  RL_TEXTURE_WRAP_CLAMP = $812F;                    // GL_CLAMP_TO_EDGE
  RL_TEXTURE_WRAP_MIRROR_REPEAT = $8370;            // GL_MIRRORED_REPEAT
  RL_TEXTURE_WRAP_MIRROR_CLAMP = $8742;             // GL_MIRROR_CLAMP_EXT

  (* Matrix modes (equivalent to OpenGL) *)
type
  PrlMatrixMode = ^TrlMatrixMode;
  TrlMatrixMode = Integer;
const
  RL_MODELVIEW  = TrlMatrixMode($1700); // GL_MODELVIEW
  RL_PROJECTION = TrlMatrixMode($1701); // GL_PROJECTION
  RL_TEXTURE    = TrlMatrixMode($1702); // GL_TEXTURE

  (* Primitive assembly draw modes *)
type
  PrlDrawMode = ^TrlDrawMode;
  TrlDrawMode = Integer;
const
  RL_LINES     = TrlDrawMode($0001); // GL_LINES
  RL_TRIANGLES = TrlDrawMode($0004); // GL_TRIANGLES
  RL_QUADS     = TrlDrawMode($0007); // GL_QUADS


  (* GL equivalent data types *)
  RL_UNSIGNED_BYTE = $1401;                         // GL_UNSIGNED_BYTE
  RL_FLOAT = $1406;                                 // GL_FLOAT

  (* Buffer usage hint *)
  RL_STREAM_DRAW = $88E0;                           // GL_STREAM_DRAW
  RL_STREAM_READ = $88E1;                           // GL_STREAM_READ
  RL_STREAM_COPY = $88E2;                           // GL_STREAM_COPY
  RL_STATIC_DRAW = $88E4;                           // GL_STATIC_DRAW
  RL_STATIC_READ = $88E5;                           // GL_STATIC_READ
  RL_STATIC_COPY = $88E6;                           // GL_STATIC_COPY
  RL_DYNAMIC_DRAW = $88E8;                          // GL_DYNAMIC_DRAW
  RL_DYNAMIC_READ = $88E9;                          // GL_DYNAMIC_READ
  RL_DYNAMIC_COPY = $88EA;                          // GL_DYNAMIC_COPY

  (* GL Shader type *)
  RL_FRAGMENT_SHADER = $8B30;                       // GL_FRAGMENT_SHADER
  RL_VERTEX_SHADER = $8B31;                         // GL_VERTEX_SHADER
  RL_COMPUTE_SHADER = $91B9;                        // GL_COMPUTE_SHADER

  (* GL blending factors *)
  RL_ZERO = 0;                                      // GL_ZERO
  RL_ONE = 1;                                       // GL_ONE
  RL_SRC_COLOR = $0300;                             // GL_SRC_COLOR
  RL_ONE_MINUS_SRC_COLOR = $0301;                   // GL_ONE_MINUS_SRC_COLOR
  RL_SRC_ALPHA = 0302;                              // GL_SRC_ALPHA
  RL_ONE_MINUS_SRC_ALPHA = $0303;                   // GL_ONE_MINUS_SRC_ALPHA
  RL_DST_ALPHA = $0304;                             // GL_DST_ALPHA
  RL_ONE_MINUS_DST_ALPHA = $0305;                   // GL_ONE_MINUS_DST_ALPHA
  RL_DST_COLOR = $0306;                             // GL_DST_COLOR
  RL_ONE_MINUS_DST_COLOR = $0307;                   // GL_ONE_MINUS_DST_COLOR
  RL_SRC_ALPHA_SATURATE = $0308;                    // GL_SRC_ALPHA_SATURATE
  RL_CONSTANT_COLOR = $8001;                        // GL_CONSTANT_COLOR
  RL_ONE_MINUS_CONSTANT_COLOR = $8002;              // GL_ONE_MINUS_CONSTANT_COLOR
  RL_CONSTANT_ALPHA = $8003;                        // GL_CONSTANT_ALPHA
  RL_ONE_MINUS_CONSTANT_ALPHA = $8004;              // GL_ONE_MINUS_CONSTANT_ALPHA

  (* GL blending functions/equations *)
  RL_FUNC_ADD = $8006;                              // GL_FUNC_ADD
  RL_MIN = $8007;                                   // GL_MIN
  RL_MAX = $8008;                                   // GL_MAX
  RL_FUNC_SUBTRACT = $800A;                         // GL_FUNC_SUBTRACT
  RL_FUNC_REVERSE_SUBTRACT = $800B;                 // GL_FUNC_REVERSE_SUBTRACT
  RL_BLEND_EQUATION = $8009;                        // GL_BLEND_EQUATION
  RL_BLEND_EQUATION_RGB = $8009;                    // GL_BLEND_EQUATION_RGB   // (Same as BLEND_EQUATION)
  RL_BLEND_EQUATION_ALPHA = $883D;                  // GL_BLEND_EQUATION_ALPHA
  RL_BLEND_DST_RGB = $80C8;                         // GL_BLEND_DST_RGB
  RL_BLEND_SRC_RGB = $80C9;                         // GL_BLEND_SRC_RGB
  RL_BLEND_DST_ALPHA = $80CA;                       // GL_BLEND_DST_ALPHA
  RL_BLEND_SRC_ALPHA = $80CB;                       // GL_BLEND_SRC_ALPHA
  RL_BLEND_COLOR = $8005;                           // GL_BLEND_COLOR

  RL_READ_FRAMEBUFFER = $8CA8;                       // GL_READ_FRAMEBUFFER
  RL_DRAW_FRAMEBUFFER = $8CA9;                       // GL_DRAW_FRAMEBUFFER

  RL_DEFAULT_SHADER_ATTRIB_LOCATION_POSITION = 0;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD = 1;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_NORMAL = 2;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_COLOR = 3;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_TANGENT = 4;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_TEXCOORD2 = 5;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_INDICES = 6;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEIDS = 7;
  RL_DEFAULT_SHADER_ATTRIB_LOCATION_BONEWEIGHTS = 8;


type
  // Dynamic vertex buffers (position + texcoords + colors + indices arrays)
  PrlVertexBuffer = ^TrlVertexBuffer;
  TrlVertexBuffer = record
      elementCount : Integer;          // Number of elements in the buffer (QUADS)
      vertices : PSingle;              // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      texcoords : PSingle;             // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      normals : PSingle;               // Vertex normal (XYZ - 3 components per vertex) (shader-location = 2)
      colors : PByte;                  // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      {$if defined(GRAPHICS_API_OPENGL_ES2)}
      indices : PDword;                // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
      {$else}
      indices : PLongWord;             // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
      {$endif}
      vaoId : LongWord;                // OpenGL Vertex Array Object id
      vboId : array[0..4] of LongWord; // OpenGL Vertex Buffer Objects id (5 types of vertex data)
  end;

type
  (* Draw call type *)
  // NOTE: Only texture changes register a new draw, other state-change-related elements are not
  // used at this moment (vaoId, shaderId, matrices), raylib just forces a batch draw call if any
  // of those state-change happens (this is done in core module)
  PrlDrawCall = ^TrlDrawCall;
  TrlDrawCall = record
    mode : Integer;             // Drawing mode: LINES, TRIANGLES, QUADS
    vertexCount : Integer;      // Number of vertex of the draw
    vertexAlignment : Integer;  // Number of vertex required for index alignment (LINES, TRIANGLES)
    textureId : LongWord;       // Texture id to be used on the draw -> Use to create new draw call if changes
  end;

type
  (* rlRenderBatch type *)
  PrlRenderBatch = ^TrlRenderBatch;
  TrlRenderBatch = record
    bufferCount : Integer;    // Number of vertex buffers (multi-buffering support)
    currentBuffer : Integer;  // Current buffer tracking in case of multi-buffering
    vertexBuffer : PrlVertexBuffer; // Dynamic buffer(s) for vertex data
    draws : PrlDrawCall;      // Draw calls array, depends on textureId
    drawCounter : Integer;    // Draw calls counter
    currentDepth : Single;    // Current depth value for next draw
  end;

type
  (* OpenGL version *)
  PrlGlVersion = ^TrlGlVersion;
  TrlGlVersion =  Integer;
  const
    OPENGL_11 = TrlGlVersion(1);    // OpenGL 1.1
    OPENGL_21 = TrlGlVersion(2);    // OpenGL 2.1 (GLSL 120)
    OPENGL_33 = TrlGlVersion(3);    // OpenGL 3.3 (GLSL 330)
    OPENGL_43 = TrlGlVersion(4);    // OpenGL 4.3 (using GLSL 330)
    OPENGL_ES_20 = TrlGlVersion(5); // OpenGL ES 2.0 (GLSL 100)

type
  (* Trace log level *)
  // NOTE: Organized by priority level
  PrlTraceLogLevel = ^TrlTraceLogLevel;
  TrlTraceLogLevel =  Integer;
  const
    RL_LOG_ALL     = TrlTraceLogLevel(0); // Display all logs
    RL_LOG_TRACE   = TrlTraceLogLevel(1); // Trace logging, intended for internal use only
    RL_LOG_DEBUG   = TrlTraceLogLevel(2); // Debug logging, used for internal debugging, it should be disabled on release builds
    RL_LOG_INFO    = TrlTraceLogLevel(3); // Info logging, used for program execution info
    RL_LOG_WARNING = TrlTraceLogLevel(4); // Warning logging, used on recoverable failures
    RL_LOG_ERROR   = TrlTraceLogLevel(5); // Error logging, used on unrecoverable failures
    RL_LOG_FATAL   = TrlTraceLogLevel(6); // Fatal logging, used to abort program: exit(EXIT_FAILURE)
    RL_LOG_NONE    = TrlTraceLogLevel(7); // Disable logging


type
  (* Texture pixel formats *)
  // NOTE: Support depends on OpenGL version
  PrlPixelFormat = ^TrlPixelFormat;
  TrlPixelFormat =  Integer;
  const
    RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE    = TrlPixelFormat(1);  // 8 bit per pixel (no alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA   = TrlPixelFormat(2);  // 8*2 bpp (2 channels)
    RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5       = TrlPixelFormat(3);  // 16 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8       = TrlPixelFormat(4);  // 24 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1     = TrlPixelFormat(5);  // 16 bpp (1 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4     = TrlPixelFormat(6);  // 16 bpp (4 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8     = TrlPixelFormat(7);  // 32 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R32          = TrlPixelFormat(8);  // 32 bpp (1 channel - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32    = TrlPixelFormat(9);  // 32*3 bpp (3 channels - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = TrlPixelFormat(10); // 32*4 bpp (4 channels - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16          = TrlPixelFormat(11); // 16 bpp (1 channel - half float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16    = TrlPixelFormat(12); // 16*3 bpp (3 channels - half float)
    RL_PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 = TrlPixelFormat(13); // 16*4 bpp (4 channels - half float)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGB       = TrlPixelFormat(14); // 4 bpp (no alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA      = TrlPixelFormat(15); // 4 bpp (1 bit alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA      = TrlPixelFormat(16); // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA      = TrlPixelFormat(17); // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC1_RGB       = TrlPixelFormat(18); // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_RGB       = TrlPixelFormat(19); // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA  = TrlPixelFormat(20); // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGB       = TrlPixelFormat(21); // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA      = TrlPixelFormat(22); // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA  = TrlPixelFormat(23); // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA  = TrlPixelFormat(24); // 2 bpp

type
  (* Texture parameters: filter mode *)
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  PrlTextureFilter = ^TrlTextureFilter;
  TrlTextureFilter =  Integer;
  const
    RL_TEXTURE_FILTER_POINT           = TrlTextureFilter(0); // No filter, just pixel approximation
    RL_TEXTURE_FILTER_BILINEAR        = TrlTextureFilter(1); // Linear filtering
    RL_TEXTURE_FILTER_TRILINEAR       = TrlTextureFilter(2); // Trilinear filtering (linear with mipmaps)
    RL_TEXTURE_FILTER_ANISOTROPIC_4X  = TrlTextureFilter(3); // Anisotropic filtering 4x
    RL_TEXTURE_FILTER_ANISOTROPIC_8X  = TrlTextureFilter(4); // Anisotropic filtering 8x
    RL_TEXTURE_FILTER_ANISOTROPIC_16X = TrlTextureFilter(5); // Anisotropic filtering 16x

type
  (* Color blending modes (pre-defined) *)
  PrlBlendMode = ^TrlBlendMode;
  TrlBlendMode =  Integer;
  const
    RL_BLEND_ALPHA             = TrlBlendMode(0); // Blend textures considering alpha (default)
    RL_BLEND_ADDITIVE          = TrlBlendMode(1); // Blend textures adding colors
    RL_BLEND_MULTIPLIED        = TrlBlendMode(2); // Blend textures multiplying colors
    RL_BLEND_ADD_COLORS        = TrlBlendMode(3); // Blend textures adding colors (alternative)
    RL_BLEND_SUBTRACT_COLORS   = TrlBlendMode(4); // Blend textures subtracting colors (alternative)
    RL_BLEND_ALPHA_PREMULTIPLY = TrlBlendMode(5); // Blend premultiplied textures considering alpha
    RL_BLEND_CUSTOM            = TrlBlendMode(6); // Blend textures using custom src/dst factors (use rlSetBlendFactors())
    RL_BLEND_CUSTOM_SEPARATE   = TrlBlendMode(7); // Blend textures using custom src/dst factors (use rlSetBlendFactorsSeparate())

type
  (* Shader location point type *)
  PrlShaderLocationIndex = ^TrlShaderLocationIndex;
  TrlShaderLocationIndex =  Integer;
  const
    RL_SHADER_LOC_VERTEX_POSITION   = TrlShaderLocationIndex(0);  // Shader location: vertex attribute: position
    RL_SHADER_LOC_VERTEX_TEXCOORD01 = TrlShaderLocationIndex(1);  // Shader location: vertex attribute: texcoord01
    RL_SHADER_LOC_VERTEX_TEXCOORD02 = TrlShaderLocationIndex(2);  // Shader location: vertex attribute: texcoord02
    RL_SHADER_LOC_VERTEX_NORMAL     = TrlShaderLocationIndex(3);  // Shader location: vertex attribute: normal
    RL_SHADER_LOC_VERTEX_TANGENT    = TrlShaderLocationIndex(4);  // Shader location: vertex attribute: tangent
    RL_SHADER_LOC_VERTEX_COLOR      = TrlShaderLocationIndex(5);  // Shader location: vertex attribute: color
    RL_SHADER_LOC_MATRIX_MVP        = TrlShaderLocationIndex(6);  // Shader location: matrix uniform: model-view-projection
    RL_SHADER_LOC_MATRIX_VIEW       = TrlShaderLocationIndex(7);  // Shader location: matrix uniform: view (camera transform)
    RL_SHADER_LOC_MATRIX_PROJECTION = TrlShaderLocationIndex(8);  // Shader location: matrix uniform: projection
    RL_SHADER_LOC_MATRIX_MODEL      = TrlShaderLocationIndex(9);  // Shader location: matrix uniform: model (transform)
    RL_SHADER_LOC_MATRIX_NORMAL     = TrlShaderLocationIndex(10); // Shader location: matrix uniform: normal
    RL_SHADER_LOC_VECTOR_VIEW       = TrlShaderLocationIndex(11); // Shader location: vector uniform: view
    RL_SHADER_LOC_COLOR_DIFFUSE     = TrlShaderLocationIndex(12); // Shader location: vector uniform: diffuse color
    RL_SHADER_LOC_COLOR_SPECULAR    = TrlShaderLocationIndex(13); // Shader location: vector uniform: specular color
    RL_SHADER_LOC_COLOR_AMBIENT     = TrlShaderLocationIndex(14); // Shader location: vector uniform: ambient color
    RL_SHADER_LOC_MAP_ALBEDO        = TrlShaderLocationIndex(15); // Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)
    RL_SHADER_LOC_MAP_METALNESS     = TrlShaderLocationIndex(16); // Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)
    RL_SHADER_LOC_MAP_NORMAL        = TrlShaderLocationIndex(17); // Shader location: sampler2d texture: normal
    RL_SHADER_LOC_MAP_ROUGHNESS     = TrlShaderLocationIndex(18); // Shader location: sampler2d texture: roughness
    RL_SHADER_LOC_MAP_OCCLUSION     = TrlShaderLocationIndex(19); // Shader location: sampler2d texture: occlusion
    RL_SHADER_LOC_MAP_EMISSION      = TrlShaderLocationIndex(20); // Shader location: sampler2d texture: emission
    RL_SHADER_LOC_MAP_HEIGHT        = TrlShaderLocationIndex(21); // Shader location: sampler2d texture: height
    RL_SHADER_LOC_MAP_CUBEMAP       = TrlShaderLocationIndex(22); // Shader location: samplerCube texture: cubemap
    RL_SHADER_LOC_MAP_IRRADIANCE    = TrlShaderLocationIndex(23); // Shader location: samplerCube texture: irradiance
    RL_SHADER_LOC_MAP_PREFILTER     = TrlShaderLocationIndex(24); // Shader location: samplerCube texture: prefilter
    RL_SHADER_LOC_MAP_BRDF          = TrlShaderLocationIndex(25); // Shader location: sampler2d texture: brdf

    RL_SHADER_LOC_MAP_DIFFUSE  = RL_SHADER_LOC_MAP_ALBEDO;
    RL_SHADER_LOC_MAP_SPECULAR = RL_SHADER_LOC_MAP_METALNESS;


type
  (* Shader uniform data type *)
  PrlShaderUniformDataType = ^TrlShaderUniformDataType;
  TrlShaderUniformDataType =  Integer;
  const
    RL_SHADER_UNIFORM_FLOAT     = TrlShaderUniformDataType(0);  // Shader uniform type: float
    RL_SHADER_UNIFORM_VEC2      = TrlShaderUniformDataType(1);  // Shader uniform type: vec2 (2 float)
    RL_SHADER_UNIFORM_VEC3      = TrlShaderUniformDataType(2);  // Shader uniform type: vec3 (3 float)
    RL_SHADER_UNIFORM_VEC4      = TrlShaderUniformDataType(3);  // Shader uniform type: vec4 (4 float)
    RL_SHADER_UNIFORM_INT       = TrlShaderUniformDataType(4);  // Shader uniform type: int
    RL_SHADER_UNIFORM_IVEC2     = TrlShaderUniformDataType(5);  // Shader uniform type: ivec2 (2 int)
    RL_SHADER_UNIFORM_IVEC3     = TrlShaderUniformDataType(6);  // Shader uniform type: ivec3 (3 int)
    RL_SHADER_UNIFORM_IVEC4     = TrlShaderUniformDataType(7);  // Shader uniform type: ivec4 (4 int)
    RL_SHADER_UNIFORM_UINT      = TrlShaderUniformDataType(8);  // Shader uniform type: unsigned int
    RL_SHADER_UNIFORM_UIVEC2    = TrlShaderUniformDataType(9);  // Shader uniform type: uivec2 (2 unsigned int)
    RL_SHADER_UNIFORM_UIVEC3    = TrlShaderUniformDataType(10); // Shader uniform type: uivec3 (3 unsigned int)
    RL_SHADER_UNIFORM_UIVEC4    = TrlShaderUniformDataType(11); // Shader uniform type: uivec4 (4 unsigned int)
    RL_SHADER_UNIFORM_SAMPLER2D = TrlShaderUniformDataType(12); // Shader uniform type: sampler2d


type
  (* Shader attribute data types *)
  PrlShaderAttributeDataType = ^TrlShaderAttributeDataType;
  TrlShaderAttributeDataType =  Integer;
  const
    RL_SHADER_ATTRIB_FLOAT = TrlShaderAttributeDataType(0); // Shader attribute type: float
    RL_SHADER_ATTRIB_VEC2  = TrlShaderAttributeDataType(1); // Shader attribute type: vec2 (2 float)
    RL_SHADER_ATTRIB_VEC3  = TrlShaderAttributeDataType(2); // Shader attribute type: vec3 (3 float)
    RL_SHADER_ATTRIB_VEC4  = TrlShaderAttributeDataType(3); // Shader attribute type: vec4 (4 float)

type
  (* Framebuffer attachment type *)
  // NOTE: By default up to 8 color channels defined but it can be more
  PrlFramebufferAttachType = ^TrlFramebufferAttachType;
  TrlFramebufferAttachType =  Integer;
  const
    RL_ATTACHMENT_COLOR_CHANNEL0 = TrlFramebufferAttachType(0);   // Framebuffer attachmment type: color 0
    RL_ATTACHMENT_COLOR_CHANNEL1 = TrlFramebufferAttachType(1);   // Framebuffer attachmment type: color 1
    RL_ATTACHMENT_COLOR_CHANNEL2 = TrlFramebufferAttachType(2);   // Framebuffer attachmment type: color 2
    RL_ATTACHMENT_COLOR_CHANNEL3 = TrlFramebufferAttachType(3);   // Framebuffer attachmment type: color 3
    RL_ATTACHMENT_COLOR_CHANNEL4 = TrlFramebufferAttachType(4);   // Framebuffer attachmment type: color 4
    RL_ATTACHMENT_COLOR_CHANNEL5 = TrlFramebufferAttachType(5);   // Framebuffer attachmment type: color 5
    RL_ATTACHMENT_COLOR_CHANNEL6 = TrlFramebufferAttachType(6);   // Framebuffer attachmment type: color 6
    RL_ATTACHMENT_COLOR_CHANNEL7 = TrlFramebufferAttachType(7);   // Framebuffer attachmment type: color 7
    RL_ATTACHMENT_DEPTH          = TrlFramebufferAttachType(100); // Framebuffer attachmment type: depth
    RL_ATTACHMENT_STENCIL        = TrlFramebufferAttachType(200); // Framebuffer attachmment type: stencil

type
  (* Framebuffer texture attachment type *)
  PrlFramebufferAttachTextureType = ^TrlFramebufferAttachTextureType;
  TrlFramebufferAttachTextureType =  Integer;
  const
    RL_ATTACHMENT_CUBEMAP_POSITIVE_X = TrlFramebufferAttachTextureType(0);   // Framebuffer texture attachment type: cubemap, +X side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_X = TrlFramebufferAttachTextureType(1);   // Framebuffer texture attachment type: cubemap, -X side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Y = TrlFramebufferAttachTextureType(2);   // Framebuffer texture attachment type: cubemap, +Y side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Y = TrlFramebufferAttachTextureType(3);   // Framebuffer texture attachment type: cubemap, -Y side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Z = TrlFramebufferAttachTextureType(4);   // Framebuffer texture attachment type: cubemap, +Z side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Z = TrlFramebufferAttachTextureType(5);   // Framebuffer texture attachment type: cubemap, -Z side
    RL_ATTACHMENT_TEXTURE2D          = TrlFramebufferAttachTextureType(100); // Framebuffer texture attachment type: texture2d
    RL_ATTACHMENT_RENDERBUFFER       = TrlFramebufferAttachTextureType(200); // Framebuffer texture attachment type: renderbuffer

type
  (* Face culling mode *)
  PrlCullMode = ^TrlCullMode;
  TrlCullMode = Integer;
  const
    RL_CULL_FACE_FRONT = TrlCullMode(0);
    RL_CULL_FACE_BACK  = TrlCullMode(1);


//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------

{Choose the current matrix to be transformed}
procedure rlMatrixMode(mode: TrlMatrixMode); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlMatrixMode';
{Push the current matrix to stack}
procedure rlPushMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlPushMatrix';
{Pop lattest inserted matrix from stack}
procedure rlPopMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlPopMatrix';
{Reset current matrix to identity matrix}
procedure rlLoadIdentity; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadIdentity';
{Multiply the current matrix by a translation matrix}
procedure rlTranslatef(x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlTranslatef';
{Multiply the current matrix by a rotation matrix}
procedure rlRotatef(angle, x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlRotatef';
{Multiply the current matrix by a scaling matrix}
procedure rlScalef(x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlScalef';
{Multiply the current matrix by another matrix}
procedure rlMultMatrixf(const matf: PSingle); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlMultMatrixf';
procedure rlFrustum(left, right, bottom, top, znear, zfar: Double); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlFrustum';
procedure rlOrtho(left, right, bottom, top, znear, zfar: Double); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlOrtho';
{Set the viewport area}
procedure rlViewport(x, y, width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlViewport';
{Set clip planes distances}
procedure rlSetClipPlanes(nearPlane, farPlane: Double); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetClipPlanes';
{Get cull plane distance near}
function rlGetCullDistanceNear(): Double; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetCullDistanceNear';
{Get cull plane distance far}
function rlGetCullDistanceFar(): Double; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetCullDistanceFar';

//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------

{Initialize drawing mode (how to organize vertex)}
procedure rlBegin(mode: TrlDrawMode); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlBegin';
{Finish vertex providing}
procedure rlEnd; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnd';
{Define one vertex (position) - 2 int}
procedure rlVertex2i(x, y: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlVertex2i';
{Define one vertex (position) - 2 float}
procedure rlVertex2f(x, y: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlVertex2f';
{Define one vertex (position) - 3 float}
procedure rlVertex3f(x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlVertex3f';
{Define one vertex (texture coordinate) - 2 float}
procedure rlTexCoord2f(x, y: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlTexCoord2f';
{Define one vertex (normal) - 3 float}
procedure rlNormal3f(x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlNormal3f';
{Define one vertex (color) - 4 byte}
procedure rlColor4ub(r, g, b, a: Byte); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlColor4ub';
{Define one vertex (color) - 3 float}
procedure rlColor3f(x, y, z: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlColor3f';
{Define one vertex (color) - 4 float}
procedure rlColor4f(x, y, z, w: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlColor4f';

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL style functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer,
// some of them are direct wrappers over OpenGL calls, some others are custom
//------------------------------------------------------------------------------------

(* Vertex buffers state *)

{Enable vertex array (VAO, if supported)}
function rlEnableVertexArray(vaoId: LongWord): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableVertexArray';
{Disable vertex array (VAO, if supported)}
procedure rlDisableVertexArray; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableVertexArray';
{Enable vertex buffer (VBO)}
procedure rlEnableVertexBuffer(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableVertexBuffer';
{Disable vertex buffer (VBO)}
procedure rlDisableVertexBuffer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableVertexBuffer';
{Enable vertex buffer element (VBO element)}
procedure rlEnableVertexBufferElement(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableVertexBufferElement';
{Disable vertex buffer element (VBO element)}
procedure rlDisableVertexBufferElement; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableVertexBufferElement';
{Enable vertex attribute index}
procedure rlEnableVertexAttribute(index: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableVertexAttribute';
{Disable vertex attribute index}
procedure rlDisableVertexAttribute(index: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableVertexAttribute';
{$if defined(GRAPHICS_API_OPENGL_11)}
{Enable attribute state pointer}
procedure rlEnableStatePointer(vertexAttribType: Integer; buffer: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableStatePointer';
{Disable attribute state pointer}
procedure rlDisableStatePointer(vertexAttribType: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableStatePointer';
{$endif}

(* Textures state *)

{Select and active a texture slot}
procedure rlActiveTextureSlot(slot: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlActiveTextureSlot';
{Enable texture}
procedure rlEnableTexture(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableTexture';
{Disable texture}
procedure rlDisableTexture; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableTexture';
{Enable texture cubemap}
procedure rlEnableTextureCubemap(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableTextureCubemap';
{Disable texture cubemap}
procedure rlDisableTextureCubemap; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableTextureCubemap';
{Set texture parameters (filter, wrap)}
procedure rlTextureParameters(id: LongWord; param, value: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlTextureParameters';
{Set cubemap parameters (filter, wrap)}
procedure rlCubemapParameters(id: LongWord; param, value: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlTextureParameters';

(* Shader state *)

{Enable shader program}
procedure rlEnableShader(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableShader';
{Disable shader program}
procedure rlDisableShader; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableShader';

(* Framebuffer state *)

{Enable render texture (fbo)}
procedure rlEnableFramebuffer(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableFramebuffer';
{Disable render texture (fbo), return to default framebuffer}
procedure rlDisableFramebuffer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableFramebuffer';
{Get the currently active render texture (fbo), 0 for default framebuffer}
function rlGetActiveFramebuffer(): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetActiveFramebuffer';
{Activate multiple draw color buffers}
procedure rlActiveDrawBuffers(count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlActiveDrawBuffers';
{Blit active framebuffer to main framebuffer}
procedure rlBlitFramebuffer(srcX, srcY, srcWidth, srcHeight, dstX, dstY, dstWidth, dstHeight, bufferMask: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlBlitFramebuffer';
{Bind framebuffer (FBO)}
procedure rlBindFramebuffer(target, framebuffer: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlBindFramebuffer';

(* General render state *)

{Enable color blending}
procedure rlEnableColorBlend; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableColorBlend';
{Disable color blending}
procedure rlDisableColorBlend; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableColorBlend';
{Enable depth test}
procedure rlEnableDepthTest; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableDepthTest';
{Disable depth test}
procedure rlDisableDepthTest; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableDepthTest';
{Enable depth write}
procedure rlEnableDepthMask; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableDepthMask';
{Disable depth write}
procedure rlDisableDepthMask; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableDepthMask';
{Enable backface culling}
procedure rlEnableBackfaceCulling; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableBackfaceCulling';
{Disable backface culling}
procedure rlDisableBackfaceCulling; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableBackfaceCulling';
{Color mask control}
procedure rlColorMask(r, g, b, a: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlColorMask';
{Set face culling mode}
procedure rlSetCullFace(mode: TrlCullMode); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetCullFace';
{Enable scissor test}
procedure rlEnableScissorTest; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableScissorTest';
{Disable scissor test}
procedure rlDisableScissorTest; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableScissorTest';
{Scissor test}
procedure rlScissor(x, y, width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlScissor';
{Enable wire mode}
procedure rlEnableWireMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableWireMode';
{Enable point mode}
procedure rlEnablePointMode(); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableWireMode';
{Disable wire (and point) mode}
procedure rlDisableWireMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableWireMode';
{Set the line drawing width}
procedure rlSetLineWidth(width: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetLineWidth';
{Get the line drawing width}
function rlGetLineWidth: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetLineWidth';
{Enable line aliasing}
procedure rlEnableSmoothLines; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableSmoothLines';
{Disable line aliasing}
procedure rlDisableSmoothLines; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableSmoothLines';
{Enable stereo rendering}
procedure rlEnableStereoRender; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlEnableStereoRender';
{Disable stereo rendering}
procedure rlDisableStereoRender; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDisableStereoRender';
{Check if stereo render is enabled}
function rlIsStereoRenderEnabled: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlIsStereoRenderEnabled';
{Clear color buffer with color}
procedure rlClearColor(r, g, b, a: Byte); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlClearColor';
{Clear used screen buffers (color and depth)}
procedure rlClearScreenBuffers; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlClearScreenBuffers';
{Check and log OpenGL error codes}
procedure rlCheckErrors; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlCheckErrors';
{Set blending mode}
procedure rlSetBlendMode(mode: TrlBlendMode); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetBlendMode';
{Set blending mode factor and equation (using OpenGL factors)}
procedure rlSetBlendFactors(glSrcFactor, glDstFactor, glEquation: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetBlendFactors';
{Set blending mode factors and equations separately (using OpenGL factors)}
procedure rlSetBlendFactorsSeparate(glSrcRGB, glDstRGB, glSrcAlpha, glDstAlpha, glEqRGB, glEqAlpha: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetBlendFactorsSeparate';

//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------

(* rlgl initialization functions *)

{Initialize rlgl (buffers, shaders, textures, states)}
procedure rlglInit(width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlglInit';
{De-inititialize rlgl (buffers, shaders, textures)}
procedure rlglClose; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlglClose';
{Load OpenGL extensions (loader function required)}
procedure rlLoadExtensions(loader: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadExtensions';
{Get current OpenGL version}
function rlGetVersion: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetVersion';
{Set current framebuffer width}
procedure rlSetFramebufferWidth(width: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetFramebufferWidth';
{Get default framebuffer width}
function rlGetFramebufferWidth: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetFramebufferWidth';
{Set current framebuffer height}
procedure rlSetFramebufferHeight(height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetFramebufferHeight';
{Get default framebuffer height}
function rlGetFramebufferHeight: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetFramebufferHeight';
{Get default texture id}
function rlGetTextureIdDefault: LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetTextureIdDefault';
{Get default shader id}
function rlGetShaderIdDefault: LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetShaderIdDefault';
{Get default shader locations}
function rlGetShaderLocsDefault: PInteger; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetShaderLocsDefault';

(* Render batch management *)
// NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
// but this render batch API is exposed in case of custom batches are required

{Load a render batch system}
function rlLoadRenderBatch(numBuffers, bufferElements: Integer): TrlRenderBatch; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadRenderBatch';
{Unload render batch system}
procedure rlUnloadRenderBatch(batch: TrlRenderBatch); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadRenderBatch';
{Draw render batch data (Update->Draw->Reset)}
procedure rlDrawRenderBatch(batch: PrlRenderBatch); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawRenderBatch';
{Set the active render batch for rlgl (NULL for default internal)}
procedure rlSetRenderBatchActive(batch: PrlRenderBatch); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetRenderBatchActive';
{Update and draw internal render batch}
procedure rlDrawRenderBatchActive; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawRenderBatchActive';
{Check internal buffer overflow for a given number of vertex}
function rlCheckRenderBatchLimit(vCount: Integer): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlCheckRenderBatchLimit';
{Set current texture for render batch and check buffers limits}
procedure rlSetTexture(id: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetTexture';

//------------------------------------------------------------------------------------------------------------------------

(* Vertex buffers management *)

{Load vertex array (vao) if supported  }
function rlLoadVertexArray: LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadVertexArray';
{Load a vertex buffer attribute}
function rlLoadVertexBuffer(const buffer: Pointer; size: Integer; dynamic_: Boolean): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadVertexBuffer';
{Load a new attributes element buffer}
function rlLoadVertexBufferElement(const buffer: Pointer; size: Integer; dynamic_: Boolean): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadVertexBufferElement';
{Update GPU buffer with new data}
procedure rlUpdateVertexBuffer(bufferId: LongWord; const data: Pointer; dataSize, offset: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUpdateVertexBuffer';
{Update vertex buffer elements with new data}
procedure rlUpdateVertexBufferElements(id: LongWord; const data: Pointer; dataSize, offset: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUpdateVertexBufferElements';
{Unload vertex array (vao)}
procedure rlUnloadVertexArray(vaoId: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadVertexArray';
{Unload vertex buffer object}
procedure rlUnloadVertexBuffer(vboId: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadVertexBuffer';
{Set vertex attribute data configuration}
procedure rlSetVertexAttribute(index: LongWord; compSize, type_: Integer; normalized: Boolean; stride, offset: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetVertexAttribute';
{Set vertex attribute data divisor}
procedure rlSetVertexAttributeDivisor(index: LongWord; divisor: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetVertexAttributeDivisor';
{Set vertex attribute default value, when attribute to provided}
procedure rlSetVertexAttributeDefault(locIndex: Integer; value:pointer; attribType, count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetVertexAttributeDefault';
{Draw vertex array (currently active vao)}
procedure rlDrawVertexArray(offset, count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawVertexArray';
{Draw vertex array elements}
procedure rlDrawVertexArrayElements(offset, count: Integer; const buffer: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawVertexArrayElements';
{Draw vertex array (currently active vao) with instancing}
procedure rlDrawVertexArrayInstanced(offset, count, instances: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawVertexArrayInstanced';
{Draw vertex array (currently active vao) with instancing}
procedure rlDrawVertexArrayElementsInstanced(offset, count: Integer; const buffer: Pointer; instances: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlDrawVertexArrayElementsInstanced';

(* Textures management *)

{Load texture in GPU}
function rlLoadTexture(const data: Pointer; width, height: Integer; format: TrlPixelFormat; mipmapCount: Integer): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadTexture';
{Load depth texture/renderbuffer (to be attached to fbo)}
function rlLoadTextureDepth(width, height: Integer; useRenderBuffer: boolean): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadTextureDepth';
{Load texture cubemap}
function rlLoadTextureCubemap(const data: Pointer; size: Integer; format: TrlPixelFormat; mipmapCount: integer): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadTextureCubemap';
{Update GPU texture with new data}
procedure rlUpdateTexture(id: LongWord; offsetX, offsetY, width, height: Integer; format: TrlPixelFormat; data: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUpdateTexture';
{Get OpenGL internal formats}
procedure rlGetGlTextureFormats(format: TrlPixelFormat; glInternalFormat, glFormat, glType: PLongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetGlTextureFormats';
{Get name string for pixel format}
function rlGetPixelFormatName(format: TrlPixelFormat): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetPixelFormatName';
{Unload texture from GPU memory}
procedure rlUnloadTexture(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadTexture';
{Generate mipmap data for selected texture}
procedure rlGenTextureMipmaps(id: LongWord; width, height: Integer; format: TrlPixelFormat; mipmaps: PInteger); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGenTextureMipmaps';
{Read texture pixel data}
function rlReadTexturePixels(id: LongWord; width, height: Integer; format: TrlPixelFormat): Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlReadTexturePixels';
{Read screen pixel data (color buffer)}
function rlReadScreenPixels(width, height: Integer): PByte; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlReadScreenPixels';


(* Framebuffer management (fbo) *)

{Load an empty framebuffer}
function rlLoadFramebuffer(): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadFramebuffer';
{Attach texture/renderbuffer to a framebuffer}
procedure rlFramebufferAttach(fboId, texId: LongWord; attachType: TrlFramebufferAttachType; texType: TrlFramebufferAttachTextureType; mipLevel: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlFramebufferAttach';
{Verify framebuffer is complete}
function rlFramebufferComplete(id: LongWord): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlFramebufferComplete';
{Delete framebuffer from GPU}
procedure rlUnloadFramebuffer(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadFramebuffer';

(* Shaders management *)

{Load shader from code strings}
function rlLoadShaderCode(vsCode, fsCode: PChar): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadShaderCode';
{Compile custom shader and return shader id (type: GL_VERTEX_SHADER,GL_FRAGMENT_SHADER)}
function rlCompileShader(shaderCode: PChar; type_: Integer): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlCompileShader';
{Load custom shader program}
function rlLoadShaderProgram(vShaderId, fShaderId: LongWord): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadShaderProgram';
{Unload shader program}
procedure rlUnloadShaderProgram(id: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadShaderProgram';
{Get shader location uniform}
function rlGetLocationUniform(shaderId: LongWord; uniformName: PChar): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetLocationUniform';
{Get shader location attribute}
function rlGetLocationAttrib(shaderId: LongWord; attribName: PChar): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetLocationAttrib';
{Set shader value uniform}
procedure rlSetUniform(locIndex: Integer; value: Pointer; uniformType: TrlShaderUniformDataType; count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetUniform';
{Set shader value matrix}
procedure rlSetUniformMatrix(locIndex: Integer; mat: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetUniformMatrix';
{Set shader value matrices}
procedure rlSetUniformMatrices(locIndex: Integer; const mat: PMatrix; count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetUniformMatrices';
{Set shader value sampler}
procedure rlSetUniformSampler(locIndex: Integer; textureId: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetUniformSampler';
{Set shader currently active (id and locations)}
procedure rlSetShader(id: LongWord; locs: PInteger); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetShader';


(* Compute shader management *)

{Load compute shader program}
function rlLoadComputeShaderProgram(shaderId: LongWord): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadComputeShaderProgram';
{Dispatch compute shader (equivalent to *draw* for graphics pilepine)}
procedure rlComputeShaderDispatch(groupX, groupY, groupZ: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlComputeShaderDispatch';

(* Shader buffer storage object management (ssbo) *)

{Load shader storage buffer object (SSBO)}
function rlLoadShaderBuffer(size: LongWord; data: Pointer; usageHint: LongWord): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadShaderBuffer';
{Unload shader storage buffer object (SSBO)}
procedure rlUnloadShaderBuffer(ssboId: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUnloadShaderBuffer';
{Update SSBO buffer data}
procedure rlUpdateShaderBuffer(id: LongWord; data: Pointer; dataSize, offset: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlUpdateShaderBuffer';
{Bind SSBO buffer}
procedure rlBindShaderBuffer(id, index: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlBindShaderBuffer';
{Read SSBO buffer data (GPU->CPU)}
procedure rlReadShaderBuffer(id: LongWord; dest: Pointer; count, offset: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlReadShaderBuffer';
{Copy SSBO data between buffers}
procedure rlCopyShaderBuffer(destId, srcId, destOffset, srcOffset, count: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlCopyShaderBuffer';
{Get SSBO buffer size}
function rlGetShaderBufferSize(id: LongWord): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetShaderBufferSize';

(* Buffer management *)

{Bind image texture}
procedure rlBindImageTexture(id, index: Integer; format: TrlPixelFormat; readonly: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlBindImageTexture';

(* Matrix state management *)

{Get internal modelview matrix}
function rlGetMatrixModelview: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetMatrixModelview';
{Get internal projection matrix}
function rlGetMatrixProjection: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetMatrixProjection';
{Get internal accumulated transform matrix}
function rlGetMatrixTransform: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetMatrixTransform';
{Get internal projection matrix for stereo render (selected eye)}
function rlGetMatrixProjectionStereo(eye: Integer): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetMatrixProjectionStereo';
{Get internal view offset matrix for stereo render (selected eye)}
function rlGetMatrixViewOffsetStereo(eye: Integer): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlGetMatrixViewOffsetStereo';
{Set a custom projection matrix (replaces internal projection matrix)}
procedure rlSetMatrixProjection(proj: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetMatrixProjection';
{Set a custom modelview matrix (replaces internal modelview matrix)}
procedure rlSetMatrixModelview(view: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetMatrixModelview';
{Set eyes projection matrices for stereo rendering}
procedure rlSetMatrixProjectionStereo(right, left: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetMatrixProjectionStereo';
{Set eyes view offsets matrices for stereo rendering}
procedure rlSetMatrixViewOffsetStereo(right, left: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlSetMatrixViewOffsetStereo';

(* Quick and dirty cube/quad buffers load->draw->unload *)

{Load and draw a cube}
procedure rlLoadDrawCube; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadDrawCube';
{Load and draw a quad}
procedure rlLoadDrawQuad; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'rlLoadDrawQuad';

implementation
uses Math;

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end.
