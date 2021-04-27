{
***********************************************************************************************
*
*   rlgl - raylib OpenGL abstraction layer
*
*   rlgl is a wrapper for multiple OpenGL versions (1.1, 2.1, 3.3 Core, ES 2.0) to
*   pseudo-OpenGL 1.1 style functions (rlVertex, rlTranslate, rlRotate...).
*
*   When chosing an OpenGL version greater than OpenGL 1.1, rlgl stores vertex data on internal
*   VBO buffers (and VAOs if available). It requires calling 3 functions:
*       rlglInit()  - Initialize internal buffers and auxiliary resources
*       rlglDraw()  - Process internal buffers and send required draw calls
*       rlglClose() - De-initialize internal buffers data and other auxiliar resources
}
unit ray_rlgl;

{$mode objfpc}{$H+}

interface

uses ray_header;

type
PVertexBuffer = ^TVertexBuffer;
TVertexBuffer = record
    elementsCount : longint;
    vCounter : longint;
    tcCounter : longint;
    cCounter : longint;
    vertices : Psingle;
    texcoords : Psingle;
    colors : Pbyte;
    indices : Pdword;

  //  if defined(GRAPHICS_API_OPENGL_ES2)
  //  indices : Pword;
  //  todo defined seting
    vaoId : dword;
    vboId : array[0..3] of dword;
  end;

PDrawCall = ^TDrawCall;
TDrawCall = record
    mode : longint;
    vertexCount : longint;
    vertexAlignment : longint;
    textureId : dword;
  end;

PRenderBatch = ^TRenderBatch;
TRenderBatch = record
    buffersCount : longint;
    currentBuffer : longint;
    vertexBuffer : PVertexBuffer;
    draws : PDrawCall;
    drawsCounter : longint;
    currentDepth : single;
  end;

//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------
procedure rlMatrixMode(aMode : Integer); cdecl; external cDllName;// Choose the current matrix to be transformed
procedure rlPopMatrix;cdecl;external cDllName;// Push the current matrix to stack
procedure rlLoadIdentity;cdecl;external cDllName;// Reset current matrix to identity matrix
procedure rlTranslatef(x:single; y:single; z:single);cdecl;external cDllName;
procedure rlRotatef(angleDeg:single; x:single; y:single; z:single);cdecl;external cDllName;
procedure rlScalef(x:single; y:single; z:single);cdecl;external cDllName;
procedure rlMultMatrixf(var matf:single);cdecl;external cDllName;
procedure rlFrustum(left:double; right:double; bottom:double; top:double; znear:double;zfar:double);cdecl;external cDllName;
procedure rlOrtho(left:double; right:double; bottom:double; top:double; znear:double;zfar:double);cdecl;external cDllName;
procedure rlViewport(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName;

//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------

procedure rlBegin(mode:longint);cdecl;external cDllName;
procedure rlEnd;cdecl;external cDllName;
procedure rlVertex2i(x:longint; y:longint);cdecl;external cDllName;
procedure rlVertex2f(x:single; y:single);cdecl;external cDllName;
procedure rlVertex3f(x:single; y:single; z:single);cdecl;external cDllName;
procedure rlTexCoord2f(x:single; y:single);cdecl;external cDllName;
procedure rlNormal3f(x:single; y:single; z:single);cdecl;external cDllName;
procedure rlColor4ub(r:byte; g:byte; b:byte; a:byte);cdecl;external cDllName;
procedure rlColor3f(x:single; y:single; z:single);cdecl;external cDllName;
procedure rlColor4f(x:single; y:single; z:single; w:single);cdecl;external cDllName;

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL style functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer,
// some of them are direct wrappers over OpenGL calls, some others are custom
//------------------------------------------------------------------------------------

// Vertex buffers state
function rlEnableVertexArray(vaoId:dword):boolean;cdecl;external cDllName;
procedure rlDisableVertexArray;cdecl;external cDllName;
procedure rlEnableVertexBuffer(id:dword);cdecl;external cDllName;
procedure rlDisableVertexBuffer;cdecl;external cDllName;
procedure rlEnableVertexBufferElement(id:dword);cdecl;external cDllName;
procedure rlDisableVertexBufferElement;cdecl;external cDllName;
procedure rlEnableVertexAttribute(index:dword);cdecl;external cDllName;
procedure rlDisableVertexAttribute(index:dword);cdecl;external cDllName;
{$if defined(GRAPHICS_API_OPENGL_11)}
procedure rlEnableStatePointer(vertexAttribType:longint; buffer:pointer);cdecl;external cDllName;
procedure rlDisableStatePointer(vertexAttribType:longint);cdecl;external cDllName;
{$endif}

// Textures state
procedure rlActiveTextureSlot(slot:longint);cdecl;external cDllName;
procedure rlEnableTexture(id:dword);cdecl;external cDllName;
procedure rlDisableTexture;cdecl;external cDllName;
procedure rlEnableTextureCubemap(id:dword);cdecl;external cDllName;
procedure rlDisableTextureCubemap;cdecl;external cDllName;
procedure rlTextureParameters(id:dword; param:longint; value:longint);cdecl;external cDllName;

// Shader state
procedure rlEnableShader(id:dword);cdecl;external cDllName;
procedure rlDisableShader;cdecl;external cDllName;

// Framebuffer state
procedure rlEnableFramebuffer(id:dword);cdecl;external cDllName;
procedure rlDisableFramebuffer;cdecl;external cDllName;


// General render state
procedure rlEnableDepthTest;cdecl;external  cDllName;
procedure rlDisableDepthTest;cdecl;external cDllName;
procedure rlEnableDepthMask;cdecl;external  cDllName;
procedure rlDisableDepthMask;cdecl;external cDllName;
procedure rlEnableBackfaceCulling;cdecl;external cDllName;
procedure rlDisableBackfaceCulling;cdecl;external cDllName;
procedure rlEnableScissorTest;cdecl;external cDllName;
procedure rlDisableScissorTest;cdecl;external cDllName;
procedure rlScissor(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName;
procedure rlEnableWireMode;cdecl;external cDllName;
procedure rlDisableWireMode;cdecl;external cDllName;
procedure rlSetLineWidth(width:single);cdecl;external cDllName;
function rlGetLineWidth:single;cdecl;external cDllName;
procedure rlEnableSmoothLines;cdecl;external cDllName;
procedure rlDisableSmoothLines;cdecl;external cDllName;
procedure rlEnableStereoRender;cdecl;external cDllName;
procedure rlDisableStereoRender;cdecl;external cDllName;
function rlIsStereoRenderEnabled:boolean;cdecl;external cDllName;

procedure rlClearColor(r:byte; g:byte; b:byte; a:byte);cdecl;external cDllName;
procedure rlClearScreenBuffers;cdecl;external cDllName;
procedure rlCheckErrors;cdecl;external cDllName;
procedure rlSetBlendMode(mode:longint);cdecl;external cDllName;
procedure rlSetBlendFactors(glSrcFactor:longint; glDstFactor:longint; glEquation:longint);cdecl;external cDllName;

//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------
// rlgl initialization functions
procedure rlglInit(width:longint; height:longint);cdecl;external cDllName;
procedure rlglClose;cdecl;external cDllName;
procedure rlLoadExtensions(loader:pointer);cdecl;external cDllName;
function rlGetVersion:longint;cdecl;external cDllName;
function rlGetFramebufferWidth:longint;cdecl;external cDllName;
function rlGetFramebufferHeight:longint;cdecl;external cDllName;

function rlGetShaderDefault:TShader;cdecl;external cDllName;
function rlGetTextureDefault:TTexture2D;cdecl;external cDllName;

// Render batch management
// NOTE: rlgl provides a default render batch to behave like OpenGL 1.1 immediate mode
// but this render batch API is exposed in case of custom batches are required
function rlLoadRenderBatch(numBuffers:longint; bufferElements:longint):TRenderBatch;cdecl;external cDllName;
procedure rlUnloadRenderBatch(batch:TRenderBatch);cdecl;external cDllName;
procedure rlDrawRenderBatch(var batch:TRenderBatch);cdecl;external cDllName;
procedure rlSetRenderBatchActive(var batch:TRenderBatch);cdecl;external cDllName;
procedure rlDrawRenderBatchActive;cdecl;external cDllName;
function rlCheckRenderBatchLimit(vCount:longint):boolean;cdecl;external cDllName;
procedure rlSetTexture(id:dword);cdecl;external cDllName;

//------------------------------------------------------------------------------------------------------------------------

// Vertex buffers management
function rlLoadVertexArray:dword;cdecl;external cDllName;
function rlLoadVertexBuffer(buffer:pointer; size:longint; _dynamic:boolean):dword;cdecl;external cDllName;
function rlLoadVertexBufferElement(buffer:pointer; size:longint; _dynamic:boolean):dword;cdecl;external cDllName;
procedure rlUpdateVertexBuffer(bufferId:longint; data:pointer; dataSize:longint; offset:longint);cdecl;external cDllName;
procedure rlUnloadVertexArray(vaoId:dword);cdecl;external cDllName;
procedure rlUnloadVertexBuffer(vboId:dword);cdecl;external cDllName;
procedure rlSetVertexAttribute(index:dword; compSize:longint; _type:longint; normalized:boolean; stride:longint;_pointer:pointer);cdecl;external cDllName;
procedure rlSetVertexAttributeDivisor(index:dword; divisor:longint);cdecl;external cDllName;
procedure rlSetVertexAttributeDefault(locIndex:longint; value:pointer; attribType:longint; count:longint);cdecl;external cDllName;
procedure rlDrawVertexArray(offset:longint; count:longint);cdecl;external cDllName;
procedure rlDrawVertexArrayElements(offset:longint; count:longint; buffer:pointer);cdecl;external cDllName;
procedure rlDrawVertexArrayInstanced(offset:longint; count:longint; instances:longint);cdecl;external cDllName;
procedure rlDrawVertexArrayElementsInstanced(offset:longint; count:longint; buffer:pointer; instances:longint);cdecl;external cDllName;

// Textures management
function rlLoadTexture(data:pointer; width:longint; height:longint; format:longint; mipmapCount:longint):dword;cdecl;external cDllName;
function rlLoadTextureDepth(width:longint; height:longint; useRenderBuffer:boolean):dword;cdecl;external cDllName;
function rlLoadTextureCubemap(data:pointer; size:longint; format:longint):dword;cdecl;external cDllName;

procedure rlUpdateTexture(id:dword; offsetX:longint; offsetY:longint; width:longint; height:longint;format:longint; data:pointer);cdecl;external cDllName;
procedure rlGetGlTextureFormats(format:longint; var glInternalFormat:dword; var glFormat:dword; var glType:dword);cdecl;external cDllName;
procedure rlUnloadTexture(id:dword);cdecl;external cDllName;
procedure rlGenerateMipmaps(var texture:TTexture2D);cdecl;external cDllName;
function rlReadTexturePixels(texture:TTexture2D):pointer;cdecl;external cDllName;
function rlReadScreenPixels(width:longint; height:longint):Pbyte;cdecl;external cDllName;

// Framebuffer management (fbo)
function rlLoadFramebuffer(width:longint; height:longint):dword;cdecl;external cDllName;
procedure rlFramebufferAttach(fboId:dword; texId:dword; attachType:longint; texType:longint; mipLevel:longint);cdecl;external cDllName;
function rlFramebufferComplete(id:dword):boolean;cdecl;external cDllName;
procedure rlUnloadFramebuffer(id:dword);cdecl;external cDllName;

// Shaders management
function rlLoadShaderCode(vsCode:Pchar; fsCode:Pchar):dword;cdecl;external cDllName;
function rlCompileShader(shaderCode:Pchar; _type:longint):dword;cdecl;external cDllName;
function rlLoadShaderProgram(vShaderId:dword; fShaderId:dword):dword;cdecl;external cDllName;
procedure rlUnloadShaderProgram(id:dword);cdecl;external cDllName;

function rlGetLocationUniform(shaderId:dword; uniformName:Pchar):longint;cdecl;external cDllName;
function rlGetLocationAttrib(shaderId:dword; attribName:Pchar):longint;cdecl;external cDllName;
procedure rlSetUniform(locIndex:longint; value:pointer; uniformType:longint; count:longint);cdecl;external cDllName;
procedure rlSetUniformMatrix(locIndex:longint; mat:TMatrix);cdecl;external cDllName;
procedure rlSetUniformSampler(locIndex:longint; textureId:dword);cdecl;external cDllName;
procedure rlSetShader(shader:TShader);cdecl;external cDllName;

// Matrix state management
function rlGetMatrixModelview:TMatrix;cdecl;external cDllName;
function rlGetMatrixProjection:TMatrix;cdecl;external cDllName;
function rlGetMatrixTransform:TMatrix;cdecl;external cDllName;
function rlGetMatrixProjectionStereo(eye:longint):TMatrix;cdecl;external cDllName;
function rlGetMatrixViewOffsetStereo(eye:longint):TMatrix;cdecl;external cDllName;
procedure rlSetMatrixProjection(proj:TMatrix);cdecl;external cDllName;
procedure rlSetMatrixModelview(view:TMatrix);cdecl;external cDllName;
procedure rlSetMatrixProjectionStereo(right:TMatrix; left:TMatrix);cdecl;external cDllName;
procedure rlSetMatrixViewOffsetStereo(right:TMatrix; left:TMatrix);cdecl;external cDllName;

// Quick and dirty cube/quad buffers load->draw->unload
procedure rlLoadDrawCube;cdecl;external cDllName;
procedure rlLoadDrawQuad;cdecl;external cDllName;

implementation

end.
