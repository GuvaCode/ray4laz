{**********************************************************************************************
*
*   rlgl v4.2 - A multi-OpenGL abstraction layer with an immediate-mode style API
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
*   Copyright (c) 2014-2021 Ramon Santamaria (@raysan5)
*   Pascal header 2022 Gunko Vadim (@guvacode)
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
  RL_MODELVIEW = $1700;                             // GL_MODELVIEW
  RL_PROJECTION = $1701;                            // GL_PROJECTION
  RL_TEXTURE = $1702;                               // GL_TEXTURE }

  (* Primitive assembly draw modes *)
  RL_LINES = $0001;                                 // GL_LINES
  RL_TRIANGLES = $0004;                             // GL_TRIANGLES
  RL_QUADS = $0007;                                 // GL_QUADS

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


type
  // Dynamic vertex buffers (position + texcoords + colors + indices arrays)
  PrlVertexBuffer = ^TrlVertexBuffer;
  TrlVertexBuffer = record
      elementCount : Integer;   // Number of elements in the buffer (QUADS)
      vertices : PSingle;       // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      texcoords : PSingle;      // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      colors : PByte;           // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      {$if defined(GRAPHICS_API_OPENGL_ES2)}
      indices : PDword;         // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
      {$else}
      indices : PLongWord;      // Vertex indices (in case vertex data comes indexed) (6 indices per quad)
      {$endif}
      vaoId : LongWord;         // OpenGL Vertex Array Object id
      vboId : array[0..3] of LongWord;// OpenGL Vertex Buffer Objects id (4 types of vertex data)
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
    OPENGL_11 = 1;            // OpenGL 1.1
    OPENGL_21 = 2;            // OpenGL 2.1 (GLSL 120)
    OPENGL_33 = 3;            // OpenGL 3.3 (GLSL 330)
    OPENGL_43 = 4;            // OpenGL 4.3 (using GLSL 330)
    OPENGL_ES_20 = 5;         // OpenGL ES 2.0 (GLSL 100)

type
  (* Trace log level *)
  // NOTE: Organized by priority level
  PrlTraceLogLevel = ^TrlTraceLogLevel;
  TrlTraceLogLevel =  Integer;
  const
    RL_LOG_ALL = 0;         // Display all logs
    RL_LOG_TRACE = 1;       // Trace logging, intended for internal use only
    RL_LOG_DEBUG = 2;       // Debug logging, used for internal debugging, it should be disabled on release builds
    RL_LOG_INFO = 3;        // Info logging, used for program execution info
    RL_LOG_WARNING = 4;     // Warning logging, used on recoverable failures
    RL_LOG_ERROR = 5;       // Error logging, used on unrecoverable failures
    RL_LOG_FATAL = 6;       // Fatal logging, used to abort program: exit(EXIT_FAILURE)
    RL_LOG_NONE = 7;        // Disable logging


type
  (* Texture pixel formats *)
  // NOTE: Support depends on OpenGL version
  PrlPixelFormat = ^TrlPixelFormat;
  TrlPixelFormat =  Integer;
  const
    RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1;         // 8 bit per pixel (no alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2;        // 8*2 bpp (2 channels)
    RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3;            // 16 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4;            // 24 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5;          // 16 bpp (1 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6;          // 16 bpp (4 bit alpha)
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7;          // 32 bpp
    RL_PIXELFORMAT_UNCOMPRESSED_R32 = 8;               // 32 bpp (1 channel - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9;         // 32*3 bpp (3 channels - float)
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10;     // 32*4 bpp (4 channels - float)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGB = 11;           // 4 bpp (no alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12;          // 4 bpp (1 bit alpha)
    RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13;          // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14;          // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC1_RGB = 15;           // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_RGB = 16;           // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17;      // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGB = 18;           // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19;          // 4 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20;      // 8 bpp
    RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21;      // 2 bpp

type
  (* Texture parameters: filter mode *)
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  PrlTextureFilter = ^TrlTextureFilter;
  TrlTextureFilter =  Integer;
  const
    RL_TEXTURE_FILTER_POINT = 0;            // No filter, just pixel approximation
    RL_TEXTURE_FILTER_BILINEAR = 1;         // Linear filtering
    RL_TEXTURE_FILTER_TRILINEAR = 2;        // Trilinear filtering (linear with mipmaps)
    RL_TEXTURE_FILTER_ANISOTROPIC_4X = 3;   // Anisotropic filtering 4x
    RL_TEXTURE_FILTER_ANISOTROPIC_8X = 4;   // Anisotropic filtering 8x
    RL_TEXTURE_FILTER_ANISOTROPIC_16X = 5;  // Anisotropic filtering 16x


type
  (* Color blending modes (pre-defined) *)
  PrlBlendMode = ^TrlBlendMode;
  TrlBlendMode =  Integer;
  const
    RL_BLEND_ALPHA = 0;              // Blend textures considering alpha (default)
    RL_BLEND_ADDITIVE = 1;           // Blend textures adding colors
    RL_BLEND_MULTIPLIED = 2;         // Blend textures multiplying colors
    RL_BLEND_ADD_COLORS = 3;         // Blend textures adding colors (alternative)
    RL_BLEND_SUBTRACT_COLORS = 4;    // Blend textures subtracting colors (alternative)
    RL_BLEND_ALPHA_PREMULTIPLY = 5;  // Blend premultiplied textures considering alpha
    RL_BLEND_CUSTOM = 6;             // Blend textures using custom src/dst factors (use rlSetBlendFactors())

type
  (* Shader location point type *)
  PrlShaderLocationIndex = ^TrlShaderLocationIndex;
  TrlShaderLocationIndex =  Integer;
  const
    RL_SHADER_LOC_VERTEX_POSITION = 0;     // Shader location: vertex attribute: position
    RL_SHADER_LOC_VERTEX_TEXCOORD01 = 1;   // Shader location: vertex attribute: texcoord01
    RL_SHADER_LOC_VERTEX_TEXCOORD02 = 2;   // Shader location: vertex attribute: texcoord02
    RL_SHADER_LOC_VERTEX_NORMAL = 3;       // Shader location: vertex attribute: normal
    RL_SHADER_LOC_VERTEX_TANGENT = 4;      // Shader location: vertex attribute: tangent
    RL_SHADER_LOC_VERTEX_COLOR = 5;        // Shader location: vertex attribute: color
    RL_SHADER_LOC_MATRIX_MVP = 6;          // Shader location: matrix uniform: model-view-projection
    RL_SHADER_LOC_MATRIX_VIEW = 7;         // Shader location: matrix uniform: view (camera transform)
    RL_SHADER_LOC_MATRIX_PROJECTION = 8;   // Shader location: matrix uniform: projection
    RL_SHADER_LOC_MATRIX_MODEL = 9;        // Shader location: matrix uniform: model (transform)
    RL_SHADER_LOC_MATRIX_NORMAL = 10;      // Shader location: matrix uniform: normal
    RL_SHADER_LOC_VECTOR_VIEW = 11;        // Shader location: vector uniform: view
    RL_SHADER_LOC_COLOR_DIFFUSE = 12;      // Shader location: vector uniform: diffuse color
    RL_SHADER_LOC_COLOR_SPECULAR = 13;     // Shader location: vector uniform: specular color
    RL_SHADER_LOC_COLOR_AMBIENT = 14;      // Shader location: vector uniform: ambient color
    RL_SHADER_LOC_MAP_ALBEDO = 15;         // Shader location: sampler2d texture: albedo (same as: RL_SHADER_LOC_MAP_DIFFUSE)
    RL_SHADER_LOC_MAP_METALNESS = 16;      // Shader location: sampler2d texture: metalness (same as: RL_SHADER_LOC_MAP_SPECULAR)
    RL_SHADER_LOC_MAP_NORMAL = 17;         // Shader location: sampler2d texture: normal
    RL_SHADER_LOC_MAP_ROUGHNESS = 18;      // Shader location: sampler2d texture: roughness
    RL_SHADER_LOC_MAP_OCCLUSION = 19;      // Shader location: sampler2d texture: occlusion
    RL_SHADER_LOC_MAP_EMISSION = 20;       // Shader location: sampler2d texture: emission
    RL_SHADER_LOC_MAP_HEIGHT = 21;         // Shader location: sampler2d texture: height
    RL_SHADER_LOC_MAP_CUBEMAP = 22;        // Shader location: samplerCube texture: cubemap
    RL_SHADER_LOC_MAP_IRRADIANCE = 23;     // Shader location: samplerCube texture: irradiance
    RL_SHADER_LOC_MAP_PREFILTER = 24;      // Shader location: samplerCube texture: prefilter
    RL_SHADER_LOC_MAP_BRDF = 25;           // Shader location: sampler2d texture: brdf

    RL_SHADER_LOC_MAP_DIFFUSE = RL_SHADER_LOC_MAP_ALBEDO;
    RL_SHADER_LOC_MAP_SPECULAR = RL_SHADER_LOC_MAP_METALNESS;


type
  (* Shader uniform data type *)
  PrlShaderUniformDataType = ^TrlShaderUniformDataType;
  TrlShaderUniformDataType =  Integer;
  const
    RL_SHADER_UNIFORM_FLOAT = 0;      // Shader uniform type: float
    RL_SHADER_UNIFORM_VEC2 = 1;       // Shader uniform type: vec2 (2 float)
    RL_SHADER_UNIFORM_VEC3 = 2;       // Shader uniform type: vec3 (3 float)
    RL_SHADER_UNIFORM_VEC4 = 3;       // Shader uniform type: vec4 (4 float)
    RL_SHADER_UNIFORM_INT = 4;        // Shader uniform type: int
    RL_SHADER_UNIFORM_IVEC2 = 5;      // Shader uniform type: ivec2 (2 int)
    RL_SHADER_UNIFORM_IVEC3 = 6;      // Shader uniform type: ivec3 (3 int)
    RL_SHADER_UNIFORM_IVEC4 = 7;      // Shader uniform type: ivec4 (4 int)
    RL_SHADER_UNIFORM_SAMPLER2D = 8;  // Shader uniform type: sampler2d


type
  (* Shader attribute data types *)
  PrlShaderAttributeDataType = ^TrlShaderAttributeDataType;
  TrlShaderAttributeDataType =  Integer;
  const
    RL_SHADER_ATTRIB_FLOAT = 0;  // Shader attribute type: float
    RL_SHADER_ATTRIB_VEC2  = 1;  // Shader attribute type: vec2 (2 float)
    RL_SHADER_ATTRIB_VEC3  = 2;  // Shader attribute type: vec3 (3 float)
    RL_SHADER_ATTRIB_VEC4  = 3;  // Shader attribute type: vec4 (4 float)

type
  (* Framebuffer attachment type *)
  // NOTE: By default up to 8 color channels defined but it can be more
  PrlFramebufferAttachType = ^TrlFramebufferAttachType;
  TrlFramebufferAttachType =  Integer;
  const
    RL_ATTACHMENT_COLOR_CHANNEL0 = 0;  // Framebuffer attachmment type: color 0
    RL_ATTACHMENT_COLOR_CHANNEL1 = 1;  // Framebuffer attachmment type: color 1
    RL_ATTACHMENT_COLOR_CHANNEL2 = 2;  // Framebuffer attachmment type: color 2
    RL_ATTACHMENT_COLOR_CHANNEL3 = 3;  // Framebuffer attachmment type: color 3
    RL_ATTACHMENT_COLOR_CHANNEL4 = 4;  // Framebuffer attachmment type: color 4
    RL_ATTACHMENT_COLOR_CHANNEL5 = 5;  // Framebuffer attachmment type: color 5
    RL_ATTACHMENT_COLOR_CHANNEL6 = 6;  // Framebuffer attachmment type: color 6
    RL_ATTACHMENT_COLOR_CHANNEL7 = 7;  // Framebuffer attachmment type: color 7
    RL_ATTACHMENT_DEPTH = 100;         // Framebuffer attachmment type: depth
    RL_ATTACHMENT_STENCIL = 200;       // Framebuffer attachmment type: stencil

type
  (* Framebuffer texture attachment type *)
  PrlFramebufferAttachTextureType = ^TrlFramebufferAttachTextureType;
  TrlFramebufferAttachTextureType =  Integer;
  const
    RL_ATTACHMENT_CUBEMAP_POSITIVE_X = 0;  // Framebuffer texture attachment type: cubemap, +X side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_X = 1;  // Framebuffer texture attachment type: cubemap, -X side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Y = 2;  // Framebuffer texture attachment type: cubemap, +Y side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Y = 3;  // Framebuffer texture attachment type: cubemap, -Y side
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Z = 4;  // Framebuffer texture attachment type: cubemap, +Z side
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Z = 5;  // Framebuffer texture attachment type: cubemap, -Z side
    RL_ATTACHMENT_TEXTURE2D = 100;         // Framebuffer texture attachment type: texture2d
    RL_ATTACHMENT_RENDERBUFFER = 200;      // Framebuffer texture attachment type: renderbuffer

//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------

{Choose the current matrix to be transformed}
procedure rlMatrixMode(mode: Integer); cdecl; external cDllName;
{Push the current matrix to stack}
procedure rlPushMatrix; cdecl; external cDllName;
{Pop lattest inserted matrix from stack}
procedure rlPopMatrix; cdecl; external cDllName;
{Reset current matrix to identity matrix}
procedure rlLoadIdentity; cdecl; external cDllName;
{Multiply the current matrix by a translation matrix}
procedure rlTranslatef(x, y, z: Single); cdecl; external cDllName;
{Multiply the current matrix by a rotation matrix}
procedure rlRotatef(angle, x, y, z: Single); cdecl; external cDllName;
{Multiply the current matrix by a scaling matrix}
procedure rlScalef(x, y, z: Single); cdecl; external cDllName;
{Multiply the current matrix by another matrix}
procedure rlMultMatrixf(matf: PSingle); cdecl; external cDllName;
procedure rlFrustum(left, right, bottom, top, znear, zfar: Double); cdecl; external cDllName;
procedure rlOrtho(left, right, bottom, top, znear, zfar: Double); cdecl; external cDllName;
{Set the viewport area}
procedure rlViewport(x, y, width, height: Integer); cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------

{Initialize drawing mode (how to organize vertex)}
procedure rlBegin(mode: Integer); cdecl; external cDllName;
{Finish vertex providing}
procedure rlEnd; cdecl; external cDllName;
{Define one vertex (position) - 2 int}
procedure rlVertex2i(x, y: Integer); cdecl; external cDllName;
{Define one vertex (position) - 2 float}
procedure rlVertex2f(x, y: Single); cdecl; external cDllName;
{Define one vertex (position) - 3 float}
procedure rlVertex3f(x, y, z: Single); cdecl; external cDllName;
{Define one vertex (texture coordinate) - 2 float}
procedure rlTexCoord2f(x, y: Single); cdecl; external cDllName;
{Define one vertex (normal) - 3 float}
procedure rlNormal3f(x, y, z: Single); cdecl; external cDllName;
{Define one vertex (color) - 4 byte}
procedure rlColor4ub(r, g, b, a: Byte); cdecl; external cDllName;
{Define one vertex (color) - 3 float}
procedure rlColor3f(x, y, z: Single); cdecl; external cDllName;
{Define one vertex (color) - 4 float}
procedure rlColor4f(x, y, z, w: Single); cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL style functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer,
// some of them are direct wrappers over OpenGL calls, some others are custom
//------------------------------------------------------------------------------------

(* Vertex buffers state *)

{Enable vertex array (VAO, if supported)}
function rlEnableVertexArray(vaoId: LongWord): Boolean; cdecl; external cDllName;
{Disable vertex array (VAO, if supported)}
procedure rlDisableVertexArray; cdecl; external cDllName;
{Enable vertex buffer (VBO)}
procedure rlEnableVertexBuffer(id: LongWord); cdecl; external cDllName;
{Disable vertex buffer (VBO)}
procedure rlDisableVertexBuffer; cdecl; external cDllName;
{Enable vertex buffer element (VBO element)}
procedure rlEnableVertexBufferElement(id: LongWord); cdecl; external cDllName;
{Disable vertex buffer element (VBO element)}
procedure rlDisableVertexBufferElement; cdecl; external cDllName;
{Enable vertex attribute index}
procedure rlEnableVertexAttribute(index: LongWord); cdecl; external cDllName;
{Disable vertex attribute index}
procedure rlDisableVertexAttribute(index: LongWord); cdecl; external cDllName;
{$if defined(GRAPHICS_API_OPENGL_11)}
{Enable attribute state pointer}
procedure rlEnableStatePointer(vertexAttribType: Integer; buffer: Pointer); cdecl; external cDllName;
{Disable attribute state pointer}
procedure rlDisableStatePointer(vertexAttribType: Integer); cdecl; external cDllName;
{$endif}

(* Textures state *)

{Select and active a texture slot}
procedure rlActiveTextureSlot(slot: Integer); cdecl; external cDllName;
{Enable texture}
procedure rlEnableTexture(id: LongWord); cdecl; external cDllName;
{Disable texture}
procedure rlDisableTexture; cdecl; external cDllName;
{Enable texture cubemap}
procedure rlEnableTextureCubemap(id: LongWord); cdecl; external cDllName;
{Disable texture cubemap}
procedure rlDisableTextureCubemap; cdecl; external cDllName;
{Set texture parameters (filter, wrap)}
procedure rlTextureParameters(id: LongWord; param, value: Integer); cdecl; external cDllName;

(* Shader state *)

{Enable shader program}
procedure rlEnableShader(id: LongWord); cdecl; external cDllName;
{Disable shader program}
procedure rlDisableShader; cdecl; external cDllName;

(* Framebuffer state *)

{Enable render texture (fbo)}
procedure rlEnableFramebuffer(id: LongWord); cdecl; external cDllName;
{Disable render texture (fbo), return to default framebuffer}
procedure rlDisableFramebuffer; cdecl; external cDllName;
{Activate multiple draw color buffers}
procedure rlActiveDrawBuffers(count: Integer); cdecl; external cDllName;

(* General render state *)

{Enable color blending}
procedure rlEnableColorBlend; cdecl; external cDllName;
{Disable color blending}
procedure rlDisableColorBlend; cdecl; external cDllName;
{Enable depth test}
procedure rlEnableDepthTest; cdecl; external cDllName;
{Disable depth test}
procedure rlDisableDepthTest; cdecl; external cDllName;
{Enable depth write}
procedure rlEnableDepthMask; cdecl; external cDllName;
{Disable depth write}
procedure rlDisableDepthMask; cdecl; external cDllName;
{Enable backface culling}
procedure rlEnableBackfaceCulling; cdecl; external cDllName;
{Disable backface culling}
procedure rlDisableBackfaceCulling; cdecl; external cDllName;
{Enable scissor test}
procedure rlEnableScissorTest; cdecl; external cDllName;
{Disable scissor test}
procedure rlDisableScissorTest; cdecl; external cDllName;
{Scissor test}
procedure rlScissor(x, y, width, height: Integer); cdecl; external cDllName;
{Enable wire mode}
procedure rlEnableWireMode; cdecl; external cDllName;
{Disable wire mode}
procedure rlDisableWireMode; cdecl; external cDllName;
{Set the line drawing width}
procedure rlSetLineWidth(width: Single); cdecl; external cDllName;
{Get the line drawing width}
function rlGetLineWidth: Single; cdecl; external cDllName;
{Enable line aliasing}
procedure rlEnableSmoothLines; cdecl; external cDllName;
{Disable line aliasing}
procedure rlDisableSmoothLines; cdecl; external cDllName;
{Enable stereo rendering}
procedure rlEnableStereoRender; cdecl; external cDllName;
{Disable stereo rendering}
procedure rlDisableStereoRender; cdecl; external cDllName;
{Check if stereo render is enabled}
function rlIsStereoRenderEnabled: Boolean; cdecl; external cDllName;
{Clear color buffer with color}
procedure rlClearColor(r, g, b, a: Byte); cdecl; external cDllName;
{Clear used screen buffers (color and depth)}
procedure rlClearScreenBuffers; cdecl; external cDllName;
{Check and log OpenGL error codes}
procedure rlCheckErrors; cdecl; external cDllName;
{Set blending mode}
procedure rlSetBlendMode(mode: Integer); cdecl; external cDllName;
{Set blending mode factor and equation (using OpenGL factors)}
procedure rlSetBlendFactors(glSrcFactor, glDstFactor, glEquation: Integer); cdecl; external cDllName;


//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------

(* rlgl initialization functions *)
procedure rlglInit(width:longint; height:longint); cdecl; external cDllName;// Initialize rlgl (buffers, shaders, textures, states)
procedure rlglClose; cdecl; external cDllName;// De-inititialize rlgl (buffers, shaders, textures)
procedure rlLoadExtensions(loader:pointer); cdecl; external cDllName;// Load OpenGL extensions (loader function required)
function rlGetVersion:longint; cdecl; external cDllName;// Get current OpenGL version
procedure rlSetFramebufferWidth(width:longint); cdecl; external cDllName;// Set current framebuffer width
function rlGetFramebufferWidth:longint; cdecl; external cDllName;// Get default framebuffer width
procedure rlSetFramebufferHeight(height:longint); cdecl; external cDllName;// Set current framebuffer height
function rlGetFramebufferHeight:longint; cdecl; external cDllName;// Get default framebuffer height
function rlGetTextureIdDefault:dword; cdecl; external cDllName;// Get default texture id
function rlGetShaderIdDefault:dword; cdecl; external cDllName;// Get default shader id
function rlGetShaderLocsDefault:Plongint; cdecl; external cDllName;// Get default shader locations

(* Render batch management *)
// NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
// but this render batch API is exposed in case of custom batches are required
function rlLoadRenderBatch(numBuffers:longint; bufferElements:longint):TrlRenderBatch; cdecl; external cDllName;// Load a render batch system
procedure rlUnloadRenderBatch(batch:TrlRenderBatch); cdecl; external cDllName;// Unload render batch system
procedure rlDrawRenderBatch(batch:PrlRenderBatch); cdecl; external cDllName;// Draw render batch data (Update->Draw->Reset)
procedure rlSetRenderBatchActive(batch:PrlRenderBatch); cdecl; external cDllName;// Set the active render batch for rlgl (NULL for default internal)
procedure rlDrawRenderBatchActive; cdecl; external cDllName;// Update and draw internal render batch
function rlCheckRenderBatchLimit(vCount:longint):boolean; cdecl; external cDllName;// Check internal buffer overflow for a given number of vertex
procedure rlSetTexture(id:dword); cdecl; external cDllName;// Set current texture for render batch and check buffers limits

//------------------------------------------------------------------------------------------------------------------------

(* Vertex buffers management *)
function rlLoadVertexArray:dword; cdecl; external cDllName;// Load vertex array (vao) if supported
function rlLoadVertexBuffer(const buffer:pointer; size:longint; dynamic_:boolean):dword; cdecl; external cDllName;// Load a vertex buffer attribute
function rlLoadVertexBufferElement(const buffer:pointer; size:longint; dynamic_:boolean):dword; cdecl; external cDllName;// Load a new attributes element buffer
procedure rlUpdateVertexBuffer(bufferId:dword;const data:pointer; dataSize:longint; offset:longint); cdecl; external cDllName;// Update GPU buffer with new data
procedure rlUpdateVertexBufferElements(id:dword; const data:pointer; dataSize:longint; offset:longint); cdecl; external cDllName;// Update vertex buffer elements with new data
procedure rlUnloadVertexArray(vaoId:dword); cdecl; external cDllName;
procedure rlUnloadVertexBuffer(vboId:dword); cdecl; external cDllName;
procedure rlSetVertexAttribute(index:dword; compSize:longint; type_:longint; normalized:boolean; stride:longint;const pointer_:pointer); cdecl; external cDllName;
procedure rlSetVertexAttributeDivisor(index:dword; divisor:longint); cdecl; external cDllName;
procedure rlSetVertexAttributeDefault(locIndex:longint; value:pointer; attribType:longint; count:longint); cdecl; external cDllName;// Set vertex attribute default value
procedure rlDrawVertexArray(offset:longint; count:longint); cdecl; external cDllName;
procedure rlDrawVertexArrayElements(offset:longint; count:longint;const buffer:pointer); cdecl; external cDllName;
procedure rlDrawVertexArrayInstanced(offset:longint; count:longint; instances:longint); cdecl; external cDllName;
procedure rlDrawVertexArrayElementsInstanced(offset:longint; count:longint;const buffer:pointer; instances:longint); cdecl; external cDllName;

(* Textures management *)
function rlLoadTexture(const data:pointer; width:longint; height:longint; format:longint; mipmapCount:longint):dword; cdecl; external cDllName;// Load texture in GPU
function rlLoadTextureDepth(width:longint; height:longint; useRenderBuffer:boolean):dword; cdecl; external cDllName;// Load depth texture/renderbuffer (to be attached to fbo)
function rlLoadTextureCubemap(const data:pointer; size:longint; format:longint):dword; cdecl; external cDllName;// Load texture cubemap
procedure rlUpdateTexture(id:dword; offsetX:longint; offsetY:longint; width:longint; height:longint;format:longint; data:pointer); cdecl; external cDllName;// Update GPU texture with new data
procedure rlGetGlTextureFormats(format:longint; glInternalFormat:PDword; glFormat:PDword; glType:PDword); cdecl; external cDllName;// Get OpenGL internal formats
function rlGetPixelFormatName(format:dword):Pchar; cdecl; external cDllName;// Get name string for pixel format
procedure rlUnloadTexture(id:dword); cdecl; external cDllName;// Unload texture from GPU memory
procedure rlGenTextureMipmaps(id:dword; width:longint; height:longint; format:longint; mipmaps:Plongint); cdecl; external cDllName;// Generate mipmap data for selected texture
function rlReadTexturePixels(id:dword; width:longint; height:longint; format:longint):pointer; cdecl; external cDllName;// Read texture pixel data
function rlReadScreenPixels(width:longint; height:longint):Pbyte; cdecl; external cDllName;// Read screen pixel data (color buffer)

(* Framebuffer management (fbo) *)
function rlLoadFramebuffer(width:longint; height:longint):dword; cdecl; external cDllName;// Load an empty framebuffer
procedure rlFramebufferAttach(fboId:dword; texId:dword; attachType:longint; texType:longint; mipLevel:longint); cdecl; external cDllName;// Attach texture/renderbuffer to a framebuffer
function rlFramebufferComplete(id:dword):boolean; cdecl; external cDllName;// Verify framebuffer is complete
procedure rlUnloadFramebuffer(id:dword); cdecl; external cDllName;// Delete framebuffer from GPU

(* Shaders management *)
function rlLoadShaderCode(vsCode:Pchar; fsCode:Pchar):dword; cdecl; external cDllName;// Load shader from code strings
function rlCompileShader(shaderCode:Pchar; _type:longint):dword; cdecl; external cDllName;// Compile custom shader and return shader id (type: GL_VERTEX_SHADER,GL_FRAGMENT_SHADER)
function rlLoadShaderProgram(vShaderId:dword; fShaderId:dword):dword; cdecl; external cDllName;// Load custom shader program
procedure rlUnloadShaderProgram(id:dword); cdecl; external cDllName;// Unload shader program }
function rlGetLocationUniform(shaderId:dword; uniformName:Pchar):longint; cdecl; external cDllName;// Get shader location uniform
function rlGetLocationAttrib(shaderId:dword; attribName:Pchar):longint; cdecl; external cDllName;// Get shader location attribute
procedure rlSetUniform(locIndex:longint; value:pointer; uniformType:longint; count:longint); cdecl; external cDllName;// Set shader value uniform
procedure rlSetUniformMatrix(locIndex:longint; mat:TMatrix); cdecl; external cDllName;// Set shader value matrix
procedure rlSetUniformSampler(locIndex:longint; textureId:dword); cdecl; external cDllName;// Set shader value sampler
procedure rlSetShader(id:dword; locs:Plongint); cdecl; external cDllName; // Set shader currently active (id and locations)

//{$if defined(GRAPHICS_API_OPENGL_43)}
// Compute shader management
function rlLoadComputeShaderProgram(shaderId: LongWord): LongWord; cdecl; external cDllName;
procedure rlComputeShaderDispatch(groupX:dword; groupY:dword; groupZ:dword); cdecl; external cDllName;

// Shader buffer storage object management (ssbo)
function rlLoadShaderBuffer(size: LongWord; data:pointer; usageHint: LongWord): LongWord; cdecl; external cDllName;// Load shader storage buffer object (SSBO)
procedure rlUnloadShaderBuffer(ssboId: LongWord); cdecl; external cDllName;
procedure rlUpdateShaderBuffer(id: LongWord; data: pointer; dataSize: LongWord; offset: LongWord); cdecl; external cDllName;// Update SSBO buffer data
procedure rlBindShaderBuffer(id: LongWord; index: LongWord); cdecl; external cDllName; // Bind SSBO buffer
procedure rlReadShaderBuffer(id: LongWord; dest: pointer; count: LongWord; offset: LongWord); cdecl; external cDllName;
procedure rlCopyShaderBuffer(destId, srcId, destOffset, srcOffset, count: LongWord); cdecl; external cDllName; // Copy SSBO data between buffers
function rlGetShaderBufferSize(id: LongWord): LongWord; cdecl; external cDllName;

// Buffer management
//procedure rlCopyBuffersElements(destId:dword; srcId:dword; destOffset:qword; srcOffset:qword; count:qword); cdecl; external cDllName;
procedure rlBindImageTexture(id: LongWord; index: LongWord; format: LongWord; readonly: Integer); cdecl; external cDllName;
//{$endif}

(* Matrix state management *)
function rlGetMatrixModelview:TMatrix; cdecl; external cDllName;// Get internal modelview matrix
function rlGetMatrixProjection:TMatrix; cdecl; external cDllName;// Get internal projection matrix
function rlGetMatrixTransform:TMatrix; cdecl; external cDllName;// Get internal accumulated transform matrix
function rlGetMatrixProjectionStereo(eye:longint):TMatrix; cdecl; external cDllName;// Get internal projection matrix for stereo render (selected eye)
function rlGetMatrixViewOffsetStereo(eye:longint):TMatrix; cdecl; external cDllName;// Get internal view offset matrix for stereo render (selected eye)
procedure rlSetMatrixProjection(proj:TMatrix); cdecl; external cDllName;// Set a custom projection matrix (replaces internal projection matrix)
procedure rlSetMatrixModelview(view:TMatrix); cdecl; external cDllName;// Set a custom modelview matrix (replaces internal modelview matrix)
procedure rlSetMatrixProjectionStereo(right:TMatrix; left:TMatrix); cdecl; external cDllName;// Set eyes projection matrices for stereo rendering
procedure rlSetMatrixViewOffsetStereo(right:TMatrix; left:TMatrix); cdecl; external cDllName;// Set eyes view offsets matrices for stereo rendering

(* Quick and dirty cube/quad buffers load->draw->unload *)
procedure rlLoadDrawCube; cdecl; external cDllName;// Load and draw a cube
procedure rlLoadDrawQuad; cdecl; external cDllName;// Load and draw a quad

implementation

end.
