{**********************************************************************************************
*
*   rlgl v4.0 - A multi-OpenGL abstraction layer with an immediate-mode style API
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
*   Pascal header 2021 Gunko Vadim (@guvacode)
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

const
      {$IFnDEF Arm}
      RL_DEFAULT_BATCH_BUFFER_ELEMENTS = 8192;
      {$Else}
      RL_DEFAULT_BATCH_BUFFER_ELEMENTS = 2048;
      {$ENDIF}
      RL_DEFAULT_BATCH_BUFFERS = 1;
      RL_DEFAULT_BATCH_DRAWCALLS = 256;
      RL_DEFAULT_BATCH_MAX_TEXTURE_UNITS = 4;
      RL_MAX_MATRIX_STACK_SIZE = 32;
      RL_MAX_SHADER_LOCATIONS = 32;
      RL_CULL_DISTANCE_NEAR = 0.01;      { Default near cull distance }
      RL_CULL_DISTANCE_FAR = 1000.0;      { Default far cull distance }

const
  RL_TEXTURE_WRAP_S = $2802;      { GL_TEXTURE_WRAP_S }
  RL_TEXTURE_WRAP_T = $2803;      { GL_TEXTURE_WRAP_T }
  RL_TEXTURE_MAG_FILTER = $2800;      { GL_TEXTURE_MAG_FILTER }
  RL_TEXTURE_MIN_FILTER = $2801;      { GL_TEXTURE_MIN_FILTER }
  RL_TEXTURE_FILTER_NEAREST = $2600;      { GL_NEAREST }
  RL_TEXTURE_FILTER_LINEAR = $2601;      { GL_LINEAR }
  RL_TEXTURE_FILTER_MIP_NEAREST = $2700;      { GL_NEAREST_MIPMAP_NEAREST }
  RL_TEXTURE_FILTER_NEAREST_MIP_LINEAR = $2702;      { GL_NEAREST_MIPMAP_LINEAR }
  RL_TEXTURE_FILTER_LINEAR_MIP_NEAREST = $2701;      { GL_LINEAR_MIPMAP_NEAREST }
  RL_TEXTURE_FILTER_MIP_LINEAR = $2703;      { GL_LINEAR_MIPMAP_LINEAR }
  RL_TEXTURE_FILTER_ANISOTROPIC = $3000;      { Anisotropic filter (custom identifier) }
  RL_TEXTURE_WRAP_REPEAT = $2901;      { GL_REPEAT }
  RL_TEXTURE_WRAP_CLAMP = $812F;      { GL_CLAMP_TO_EDGE }
  RL_TEXTURE_WRAP_MIRROR_REPEAT = $8370;      { GL_MIRRORED_REPEAT }
  RL_TEXTURE_WRAP_MIRROR_CLAMP = $8742;      { GL_MIRROR_CLAMP_EXT }
{ Matrix modes (equivalent to OpenGL) }
  RL_MODELVIEW = $1700;      { GL_MODELVIEW }
  RL_PROJECTION = $1701;      { GL_PROJECTION }
  RL_TEXTURE = $1702;      { GL_TEXTURE }
{ Primitive assembly draw modes }
  RL_LINES = $0001;      { GL_LINES }
  RL_TRIANGLES = $0004;      { GL_TRIANGLES }
  RL_QUADS = $0007;      { GL_QUADS }
{ GL equivalent data types }
  RL_UNSIGNED_BYTE = $1401;      { GL_UNSIGNED_BYTE }
  RL_FLOAT = $1406;      { GL_FLOAT }

type
  PrlGlVersion = ^TrlGlVersion;
  TrlGlVersion =  Longint;
  Const
    OPENGL_11 = 1;
    OPENGL_21 = 2;
    OPENGL_33 = 3;
    OPENGL_43 = 4;
    OPENGL_ES_20 = 5;

type
  PrlFramebufferAttachType = ^TrlFramebufferAttachType;
  TrlFramebufferAttachType =  Longint;
  Const
    RL_ATTACHMENT_COLOR_CHANNEL0 = 0;
    RL_ATTACHMENT_COLOR_CHANNEL1 = 1;
    RL_ATTACHMENT_COLOR_CHANNEL2 = 2;
    RL_ATTACHMENT_COLOR_CHANNEL3 = 3;
    RL_ATTACHMENT_COLOR_CHANNEL4 = 4;
    RL_ATTACHMENT_COLOR_CHANNEL5 = 5;
    RL_ATTACHMENT_COLOR_CHANNEL6 = 6;
    RL_ATTACHMENT_COLOR_CHANNEL7 = 7;
    RL_ATTACHMENT_DEPTH = 100;
    RL_ATTACHMENT_STENCIL = 200;

type
  PrlFramebufferAttachTextureType = ^TrlFramebufferAttachTextureType;
  TrlFramebufferAttachTextureType =  Longint;
  Const
    RL_ATTACHMENT_CUBEMAP_POSITIVE_X = 0;
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_X = 1;
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Y = 2;
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Y = 3;
    RL_ATTACHMENT_CUBEMAP_POSITIVE_Z = 4;
    RL_ATTACHMENT_CUBEMAP_NEGATIVE_Z = 5;
    RL_ATTACHMENT_TEXTURE2D = 100;
    RL_ATTACHMENT_RENDERBUFFER = 200;

type
  PrlVertexBuffer = ^TrlVertexBuffer;
  TrlVertexBuffer = record
      elementCount : longint;
     { vCounter : longint;
      tcCounter : longint;
      cCounter : longint; }
      vertices : Psingle;
      texcoords : Psingle;
      colors : Pbyte;
      {$IFnDEF Arm}
      indices : Pdword; // open gl 1.1 to 3.0
      {$Else}
      indices : Pword;//GRAPHICS_API_OPENGL_ES2
      {$ENDIF}
      vaoId : dword;
      vboId : array[0..3] of dword;
    end;

PrlDrawCall = ^TrlDrawCall;
TrlDrawCall = record
    mode : longint;
    vertexCount : longint;
    vertexAlignment : longint;
    textureId : dword;
  end;

PrlRenderBatch = ^TrlRenderBatch;
TrlRenderBatch = record
    bufferCount : longint;
    currentBuffer : longint;
    vertexBuffer : PrlVertexBuffer;
    draws : PrlDrawCall;
    drawCounter : longint;
    currentDepth : single;
  end;

{type
  PMatrix = ^TMatrix;
  TMatrix = record
      m0 : single;
      m4 : single;
      m8 : single;
      m12 : single;
      m1 : single;
      m5 : single;
      m9 : single;
      m13 : single;
      m2 : single;
      m6 : single;
      m10 : single;
      m14 : single;
      m3 : single;
      m7 : single;
      m11 : single;
      m15 : single;
    end;}

type
  PrlTraceLogLevel = ^TrlTraceLogLevel;
  TrlTraceLogLevel =  Longint;
  Const
    RL_LOG_ALL = 0;
    RL_LOG_TRACE = 1;
    RL_LOG_DEBUG = 2;
    RL_LOG_INFO = 3;
    RL_LOG_WARNING = 4;
    RL_LOG_ERROR = 5;
    RL_LOG_FATAL = 6;
    RL_LOG_NONE = 7;

type
  PrlPixelFormat = ^TrlPixelFormat;
  TrlPixelFormat =  Longint;
  Const
    RL_PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1;
    RL_PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2;
    RL_PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3;
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4;
    RL_PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5;
    RL_PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6;
    RL_PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7;
    RL_PIXELFORMAT_UNCOMPRESSED_R32 = 8;
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9;
    RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10;
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGB = 11;
    RL_PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12;
    RL_PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13;
    RL_PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14;
    RL_PIXELFORMAT_COMPRESSED_ETC1_RGB = 15;
    RL_PIXELFORMAT_COMPRESSED_ETC2_RGB = 16;
    RL_PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17;
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGB = 18;
    RL_PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19;
    RL_PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20;
    RL_PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21;

type
  PrlTextureFilter = ^TrlTextureFilter;
  TrlTextureFilter =  Longint;
  Const
    RL_TEXTURE_FILTER_POINT = 0;
    RL_TEXTURE_FILTER_BILINEAR = 1;
    RL_TEXTURE_FILTER_TRILINEAR = 2;
    RL_TEXTURE_FILTER_ANISOTROPIC_4X = 3;
    RL_TEXTURE_FILTER_ANISOTROPIC_8X = 4;
    RL_TEXTURE_FILTER_ANISOTROPIC_16X = 5;

type
  PrlBlendMode = ^TrlBlendMode;
  TrlBlendMode =  Longint;
  Const
    RL_BLEND_ALPHA = 0;
    RL_BLEND_ADDITIVE = 1;
    RL_BLEND_MULTIPLIED = 2;
    RL_BLEND_ADD_COLORS = 3;
    RL_BLEND_SUBTRACT_COLORS = 4;
    RL_BLEND_CUSTOM = 5;

  type
    PrlShaderLocationIndex = ^TrlShaderLocationIndex;
    TrlShaderLocationIndex =  Longint;
    Const
      RL_SHADER_LOC_VERTEX_POSITION = 0;
      RL_SHADER_LOC_VERTEX_TEXCOORD01 = 1;
      RL_SHADER_LOC_VERTEX_TEXCOORD02 = 2;
      RL_SHADER_LOC_VERTEX_NORMAL = 3;
      RL_SHADER_LOC_VERTEX_TANGENT = 4;
      RL_SHADER_LOC_VERTEX_COLOR = 5;
      RL_SHADER_LOC_MATRIX_MVP = 6;
      RL_SHADER_LOC_MATRIX_VIEW = 7;
      RL_SHADER_LOC_MATRIX_PROJECTION = 8;
      RL_SHADER_LOC_MATRIX_MODEL = 9;
      RL_SHADER_LOC_MATRIX_NORMAL = 10;
      RL_SHADER_LOC_VECTOR_VIEW = 11;
      RL_SHADER_LOC_COLOR_DIFFUSE = 12;
      RL_SHADER_LOC_COLOR_SPECULAR = 13;
      RL_SHADER_LOC_COLOR_AMBIENT = 14;
      RL_SHADER_LOC_MAP_ALBEDO = 15;
      RL_SHADER_LOC_MAP_METALNESS = 16;
      RL_SHADER_LOC_MAP_NORMAL = 17;
      RL_SHADER_LOC_MAP_ROUGHNESS = 18;
      RL_SHADER_LOC_MAP_OCCLUSION = 19;
      RL_SHADER_LOC_MAP_EMISSION = 20;
      RL_SHADER_LOC_MAP_HEIGHT = 21;
      RL_SHADER_LOC_MAP_CUBEMAP = 22;
      RL_SHADER_LOC_MAP_IRRADIANCE = 23;
      RL_SHADER_LOC_MAP_PREFILTER = 24;
      RL_SHADER_LOC_MAP_BRDF = 25;

     RL_SHADER_LOC_MAP_DIFFUSE = RL_SHADER_LOC_MAP_ALBEDO;
     RL_SHADER_LOC_MAP_SPECULAR = RL_SHADER_LOC_MAP_METALNESS;

(* Shader uniform data type *)
type
  PrlShaderUniformDataType = ^TrlShaderUniformDataType;
  TrlShaderUniformDataType =  Longint;
  Const
    RL_SHADER_UNIFORM_FLOAT     = 0; // Shader uniform type: float
    RL_SHADER_UNIFORM_VEC2      = 1; // Shader uniform type: vec2 (2 float)
    RL_SHADER_UNIFORM_VEC3      = 2; // Shader uniform type: vec3 (3 float)
    RL_SHADER_UNIFORM_VEC4      = 3; // Shader uniform type: vec4 (4 float)
    RL_SHADER_UNIFORM_INT       = 4; // Shader uniform type: int
    RL_SHADER_UNIFORM_IVEC2     = 5; // Shader uniform type: ivec2 (2 int)
    RL_SHADER_UNIFORM_IVEC3     = 6; // Shader uniform type: ivec3 (3 int)
    RL_SHADER_UNIFORM_IVEC4     = 7; // Shader uniform type: ivec4 (4 int)
    RL_SHADER_UNIFORM_SAMPLER2D = 8; // Shader uniform type: sampler2d

(* Shader attribute data types *)
type
  PrlShaderAttributeDataType = ^TrlShaderAttributeDataType;
  TrlShaderAttributeDataType =  Longint;
  Const
    RL_SHADER_ATTRIB_FLOAT = 0; // Shader attribute type: float
    RL_SHADER_ATTRIB_VEC2  = 1; // Shader attribute type: vec2 (2 float)
    RL_SHADER_ATTRIB_VEC3  = 2; // Shader attribute type: vec3 (3 float)
    RL_SHADER_ATTRIB_VEC4  = 3; // Shader attribute type: vec4 (4 float)

//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------
procedure rlMatrixMode(mode: longint);cdecl;external cDllName;// Choose the current matrix to be transformed
procedure rlPushMatrix;cdecl;external cDllName;// Push the current matrix to stack
procedure rlPopMatrix;cdecl;external cDllName;// Pop lattest inserted matrix from stack
procedure rlLoadIdentity;cdecl;external cDllName;// Reset current matrix to identity matrix
procedure rlTranslatef(x:single; y:single; z:single);cdecl;external cDllName;// Multiply the current matrix by a translation matrix
procedure rlRotatef(angle:single; x:single; y:single; z:single);cdecl;external cDllName;// Multiply the current matrix by a rotation matrix
procedure rlScalef(x:single; y:single; z:single);cdecl;external cDllName;// Multiply the current matrix by a scaling matrix
procedure rlMultMatrixf(matf:Psingle);cdecl;external cDllName;// Multiply the current matrix by another matrix
procedure rlFrustum(left:double; right:double; bottom:double; top:double; znear:double;zfar:double);cdecl;external cDllName;
procedure rlOrtho(left:double; right:double; bottom:double; top:double; znear:double;zfar:double);cdecl;external cDllName;
procedure rlViewport(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName;// Set the viewport area
//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------

procedure rlBegin(mode:longint);cdecl;external cDllName;// Initialize drawing mode (how to organize vertex)
procedure rlEnd;cdecl;external cDllName;// Finish vertex providing
procedure rlVertex2i(x:longint; y:longint);cdecl;external cDllName;// Define one vertex (position) - 2 int
procedure rlVertex2f(x:single; y:single);cdecl;external cDllName;// Define one vertex (position) - 2 float
procedure rlVertex3f(x:single; y:single; z:single);cdecl;external cDllName;// Define one vertex (position) - 3 float
procedure rlTexCoord2f(x:single; y:single);cdecl;external cDllName;// Define one vertex (texture coordinate) - 2 float
procedure rlNormal3f(x:single; y:single; z:single);cdecl;external cDllName;// Define one vertex (normal) - 3 float
procedure rlColor4ub(r:byte; g:byte; b:byte; a:byte);cdecl;external cDllName;// Define one vertex (color) - 4 byte
procedure rlColor3f(x:single; y:single; z:single);cdecl;external cDllName;// Define one vertex (color) - 3 float
procedure rlColor4f(x:single; y:single; z:single; w:single);cdecl;external cDllName;// Define one vertex (color) - 4 float

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL style functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer,
// some of them are direct wrappers over OpenGL calls, some others are custom
//------------------------------------------------------------------------------------

(* Vertex buffers state *)
function rlEnableVertexArray(vaoId:dword):boolean;cdecl;external cDllName;// Enable vertex array (VAO, if supported)
procedure rlDisableVertexArray;cdecl;external cDllName;// Disable vertex array (VAO, if supported)
procedure rlEnableVertexBuffer(id:dword);cdecl;external cDllName;// Enable vertex buffer (VBO)
procedure rlDisableVertexBuffer;cdecl;external cDllName;// Disable vertex buffer (VBO)
procedure rlEnableVertexBufferElement(id:dword);cdecl;external cDllName;// Enable vertex buffer element (VBO element)
procedure rlDisableVertexBufferElement;cdecl;external cDllName;// Disable vertex buffer element (VBO element)
procedure rlEnableVertexAttribute(index:dword);cdecl;external cDllName;// Enable vertex attribute index
procedure rlDisableVertexAttribute(index:dword);cdecl;external cDllName;// Disable vertex attribute index
{$if defined(GRAPHICS_API_OPENGL_11)}
procedure rlEnableStatePointer(vertexAttribType:longint; buffer:pointer);cdecl;external cDllName;// Enable attribute state pointer
procedure rlDisableStatePointer(vertexAttribType:longint);cdecl;external cDllName;// Disable attribute state pointer
{$endif}

(* Textures state *)
procedure rlActiveTextureSlot(slot:longint);cdecl;external cDllName;// Select and active a texture slot
procedure rlEnableTexture(id:dword);cdecl;external cDllName;// Enable texture
procedure rlDisableTexture;cdecl;external cDllName;// Disable texture
procedure rlEnableTextureCubemap(id:dword);cdecl;external cDllName;// Enable texture cubemap
procedure rlDisableTextureCubemap;cdecl;external cDllName;// Disable texture cubemap
procedure rlTextureParameters(id:dword; param:longint; value:longint);cdecl;external cDllName;// Set texture parameters (filter, wrap)

(* Shader state *)
procedure rlEnableShader(id:dword);cdecl;external cDllName;// Enable shader program
procedure rlDisableShader;cdecl;external cDllName;// Disable shader program

(* Framebuffer state *)
procedure rlEnableFramebuffer(id:dword);cdecl;external cDllName;// Enable render texture (fbo)
procedure rlDisableFramebuffer;cdecl;external cDllName;// Disable render texture (fbo), return to default framebuffer
procedure rlActiveDrawBuffers(count:longint);cdecl;external cDllName;// Activate multiple draw color buffers

(* General render state *)
procedure rlEnableColorBlend;cdecl;external cDllName;// Enable color blending
procedure rlDisableColorBlend;cdecl;external cDllName;// Disable color blending
procedure rlEnableDepthTest;cdecl;external cDllName;// Enable depth test
procedure rlDisableDepthTest;cdecl;external cDllName;// Disable depth test
procedure rlEnableDepthMask;cdecl;external cDllName;// Enable depth write
procedure rlDisableDepthMask;cdecl;external cDllName;// Disable depth write
procedure rlEnableBackfaceCulling;cdecl;external cDllName;// Enable backface culling
procedure rlDisableBackfaceCulling;cdecl;external cDllName;// Disable backface culling
procedure rlEnableScissorTest;cdecl;external cDllName;// Enable scissor test
procedure rlDisableScissorTest;cdecl;external cDllName;// Disable scissor test
procedure rlScissor(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName;// Scissor test
procedure rlEnableWireMode;cdecl;external cDllName;// Enable wire mode
procedure rlDisableWireMode;cdecl;external cDllName;// Disable wire mode
procedure rlSetLineWidth(width:single);cdecl;external cDllName;// Set the line drawing width
function rlGetLineWidth:single;cdecl;external cDllName;// Get the line drawing width
procedure rlEnableSmoothLines;cdecl;external cDllName;// Enable line aliasing
procedure rlDisableSmoothLines;cdecl;external cDllName;// Disable line aliasing
procedure rlEnableStereoRender;cdecl;external cDllName; // Enable stereo rendering
procedure rlDisableStereoRender;cdecl;external cDllName;// Disable stereo rendering
function rlIsStereoRenderEnabled:boolean;cdecl;external cDllName;// Check if stereo render is enabled
procedure rlClearColor(r:byte; g:byte; b:byte; a:byte);cdecl;external cDllName;// Clear color buffer with color
procedure rlClearScreenBuffers;cdecl;external cDllName;// Clear used screen buffers (color and depth)
procedure rlCheckErrors;cdecl;external cDllName;// Check and log OpenGL error codes
procedure rlSetBlendMode(mode:longint);cdecl;external cDllName;// Set blending mode
procedure rlSetBlendFactors(glSrcFactor:longint; glDstFactor:longint; glEquation:longint);cdecl;external cDllName; // Set blending mode factor and equation (using OpenGL factors)



//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------

(* rlgl initialization functions *)
procedure rlglInit(width:longint; height:longint);cdecl;external cDllName;// Initialize rlgl (buffers, shaders, textures, states)
procedure rlglClose;cdecl;external cDllName;// De-inititialize rlgl (buffers, shaders, textures)
procedure rlLoadExtensions(loader:pointer);cdecl;external cDllName;// Load OpenGL extensions (loader function required)
function rlGetVersion:longint;cdecl;external cDllName;// Get current OpenGL version
function rlGetFramebufferWidth:longint;cdecl;external cDllName;// Get default framebuffer width
function rlGetFramebufferHeight:longint;cdecl;external cDllName;// Get default framebuffer height
function rlGetTextureIdDefault:dword;cdecl;external cDllName;// Get default texture id
function rlGetShaderIdDefault:dword;cdecl;external cDllName;// Get default shader id
function rlGetShaderLocsDefault:Plongint;cdecl;external cDllName;// Get default shader locations

(* Render batch management *)
// NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
// but this render batch API is exposed in case of custom batches are required
function rlLoadRenderBatch(numBuffers:longint; bufferElements:longint):TrlRenderBatch;cdecl;external cDllName;// Load a render batch system
procedure rlUnloadRenderBatch(batch:TrlRenderBatch);cdecl;external cDllName;// Unload render batch system
procedure rlDrawRenderBatch(batch:PrlRenderBatch);cdecl;external cDllName;// Draw render batch data (Update->Draw->Reset)
procedure rlSetRenderBatchActive(batch:PrlRenderBatch);cdecl;external cDllName;// Set the active render batch for rlgl (NULL for default internal)
procedure rlDrawRenderBatchActive;cdecl;external cDllName;// Update and draw internal render batch
function rlCheckRenderBatchLimit(vCount:longint):boolean;cdecl;external cDllName;// Check internal buffer overflow for a given number of vertex
procedure rlSetTexture(id:dword);cdecl;external cDllName;// Set current texture for render batch and check buffers limits

//------------------------------------------------------------------------------------------------------------------------

(* Vertex buffers management *)
function rlLoadVertexArray:dword;cdecl;external cDllName;// Load vertex array (vao) if supported
function rlLoadVertexBuffer(buffer:pointer; size:longint; dynamic_:boolean):dword;cdecl;external cDllName;// Load a vertex buffer attribute
function rlLoadVertexBufferElement(buffer:pointer; size:longint; dynamic_:boolean):dword;cdecl;external cDllName;// Load a new attributes element buffer
procedure rlUpdateVertexBuffer(bufferId:dword; data:pointer; dataSize:longint; offset:longint);cdecl;external cDllName;// Update GPU buffer with new data
procedure rlUnloadVertexArray(vaoId:dword);cdecl;external cDllName;
procedure rlUnloadVertexBuffer(vboId:dword);cdecl;external cDllName;
procedure rlSetVertexAttribute(index:dword; compSize:longint; type_:longint; normalized:boolean; stride:longint;pointer_:pointer);cdecl;external cDllName;
procedure rlSetVertexAttributeDivisor(index:dword; divisor:longint);cdecl;external cDllName;
procedure rlSetVertexAttributeDefault(locIndex:longint; value:pointer; attribType:longint; count:longint);cdecl;external cDllName;// Set vertex attribute default value
procedure rlDrawVertexArray(offset:longint; count:longint);cdecl;external cDllName;
procedure rlDrawVertexArrayElements(offset:longint; count:longint; buffer:pointer);cdecl;external cDllName;
procedure rlDrawVertexArrayInstanced(offset:longint; count:longint; instances:longint);cdecl;external cDllName;
procedure rlDrawVertexArrayElementsInstanced(offset:longint; count:longint; buffer:pointer; instances:longint);cdecl;external cDllName;

(* Textures management *)
function rlLoadTexture(data:pointer; width:longint; height:longint; format:longint; mipmapCount:longint):dword;cdecl;external cDllName;// Load texture in GPU
function rlLoadTextureDepth(width:longint; height:longint; useRenderBuffer:boolean):dword;cdecl;external cDllName;// Load depth texture/renderbuffer (to be attached to fbo)
function rlLoadTextureCubemap(data:pointer; size:longint; format:longint):dword;cdecl;external cDllName;// Load texture cubemap
procedure rlUpdateTexture(id:dword; offsetX:longint; offsetY:longint; width:longint; height:longint;format:longint; data:pointer);cdecl;external cDllName;// Update GPU texture with new data
procedure rlGetGlTextureFormats(format:longint; glInternalFormat:Plongint; glFormat:Plongint; glType:Plongint);cdecl;external cDllName;// Get OpenGL internal formats
function rlGetPixelFormatName(format:dword):Pchar;cdecl;external cDllName;// Get name string for pixel format
procedure rlUnloadTexture(id:dword);cdecl;external cDllName;// Unload texture from GPU memory
procedure rlGenTextureMipmaps(id:dword; width:longint; height:longint; format:longint; mipmaps:Plongint);cdecl;external cDllName;// Generate mipmap data for selected texture
function rlReadTexturePixels(id:dword; width:longint; height:longint; format:longint):pointer;cdecl;external cDllName;// Read texture pixel data
function rlReadScreenPixels(width:longint; height:longint):Pbyte;cdecl;external cDllName;// Read screen pixel data (color buffer)

(* Framebuffer management (fbo) *)
function rlLoadFramebuffer(width:longint; height:longint):dword;cdecl;external cDllName;// Load an empty framebuffer
procedure rlFramebufferAttach(fboId:dword; texId:dword; attachType:longint; texType:longint; mipLevel:longint);cdecl;external cDllName;// Attach texture/renderbuffer to a framebuffer
function rlFramebufferComplete(id:dword):boolean;cdecl;external cDllName;// Verify framebuffer is complete
procedure rlUnloadFramebuffer(id:dword);cdecl;external cDllName;// Delete framebuffer from GPU

(* Shaders management *)
function rlLoadShaderCode(vsCode:Pchar; fsCode:Pchar):dword;cdecl;external cDllName;// Load shader from code strings
function rlCompileShader(shaderCode:Pchar; _type:longint):dword;cdecl;external cDllName;// Compile custom shader and return shader id (type: GL_VERTEX_SHADER,GL_FRAGMENT_SHADER)
function rlLoadShaderProgram(vShaderId:dword; fShaderId:dword):dword;cdecl;external cDllName;// Load custom shader program
procedure rlUnloadShaderProgram(id:dword);cdecl;external cDllName;// Unload shader program }
function rlGetLocationUniform(shaderId:dword; uniformName:Pchar):longint;cdecl;external cDllName;// Get shader location uniform
function rlGetLocationAttrib(shaderId:dword; attribName:Pchar):longint;cdecl;external cDllName;// Get shader location attribute
procedure rlSetUniform(locIndex:longint; value:pointer; uniformType:longint; count:longint);cdecl;external cDllName;// Set shader value uniform
procedure rlSetUniformMatrix(locIndex:longint; mat:TMatrix);cdecl;external cDllName;// Set shader value matrix
procedure rlSetUniformSampler(locIndex:longint; textureId:dword);cdecl;external cDllName;// Set shader value sampler
procedure rlSetShader(id:dword; locs:Plongint);cdecl;external cDllName; // Set shader currently active (id and locations)

//{$if defined(GRAPHICS_API_OPENGL_43)}
// Compute shader management
function rlLoadComputeShaderProgram(shaderId:dword):longint;cdecl;external cDllName;
procedure rlComputeShaderDispatch(groupX:dword; groupY:dword; groupZ:dword);cdecl;external cDllName;

// Shader buffer storage object management (ssbo)
function rlLoadShaderBuffer(size: qWord; data:pointer; usageHint:longint):longint;cdecl;external cDllName;
procedure rlUnloadShaderBuffer(ssboId:dword);cdecl;external cDllName;
procedure rlUpdateShaderBufferElements(id:dword; data:pointer; dataSize:qword; offset:qword);cdecl;external cDllName;
function rlGetShaderBufferSize(id:dword):qword;cdecl;external cDllName;
procedure rlReadShaderBufferElements(id:dword; dest:pointer; count:qword; offset:qword);cdecl;external cDllName;
procedure rlBindShaderBuffer(id:dword; index:dword);cdecl;external cDllName;
// Buffer management
procedure rlCopyBuffersElements(destId:dword; srcId:dword; destOffset:qword; srcOffset:qword; count:qword);cdecl;external cDllName;
procedure rlBindImageTexture(id:dword; index:dword; format:dword; readonly:longint);cdecl;external cDllName;
//{$endif}

(* Matrix state management *)
function rlGetMatrixModelview:TMatrix;cdecl;external cDllName;// Get internal modelview matrix
function rlGetMatrixProjection:TMatrix;cdecl;external cDllName;// Get internal projection matrix
function rlGetMatrixTransform:TMatrix;cdecl;external cDllName;// Get internal accumulated transform matrix
function rlGetMatrixProjectionStereo(eye:longint):TMatrix;cdecl;external cDllName;// Get internal projection matrix for stereo render (selected eye)
function rlGetMatrixViewOffsetStereo(eye:longint):TMatrix;cdecl;external cDllName;// Get internal view offset matrix for stereo render (selected eye)
procedure rlSetMatrixProjection(proj:TMatrix);cdecl;external cDllName;// Set a custom projection matrix (replaces internal projection matrix)
procedure rlSetMatrixModelview(view:TMatrix);cdecl;external cDllName;// Set a custom modelview matrix (replaces internal modelview matrix)
procedure rlSetMatrixProjectionStereo(right:TMatrix; left:TMatrix);cdecl;external cDllName;// Set eyes projection matrices for stereo rendering
procedure rlSetMatrixViewOffsetStereo(right:TMatrix; left:TMatrix);cdecl;external cDllName;// Set eyes view offsets matrices for stereo rendering

(* Quick and dirty cube/quad buffers load->draw->unload *)
procedure rlLoadDrawCube;cdecl;external cDllName;// Load and draw a cube
procedure rlLoadDrawQuad;cdecl;external cDllName;// Load and draw a quad

implementation

end.
