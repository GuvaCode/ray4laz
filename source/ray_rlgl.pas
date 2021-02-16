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

//------------------------------------------------------------------------------------
// Functions Declaration - Matrix operations
//------------------------------------------------------------------------------------
procedure rlMatrixMode(aMode : Integer); cdecl; external cDllName;                    // Choose the current matrix to be transformed
procedure rlPushMatrix(); cdecl; external cDllName;                        // Push the current matrix to stack
procedure rlPopMatrix(); cdecl; external cDllName;                        // Pop lattest inserted matrix from stack
procedure rlLoadIdentity(); cdecl; external cDllName;                      // Reset current matrix to identity matrix
procedure rlTranslatef(aX, aY, aZ : Single); cdecl; external cDllName;   // Multiply the current matrix by a translation matrix
procedure rlRotatef(aAngleDeg, aX, aY, aZ : Single); cdecl; external cDllName;  // Multiply the current matrix by a rotation matrix
procedure rlScalef(aX, aY, aZ : Single); cdecl; external cDllName;       // Multiply the current matrix by a scaling matrix
procedure rlMultMatrixf(aMatf : PSingle); cdecl; external cDllName;                // Multiply the current matrix by another matrix
procedure rlFrustum(aLeft, aRight, aBottom, aTop, aZnear, aZfar : Double); cdecl; external cDllName;
procedure rlOrtho(aLeft, aRight, aBottom, aTop, aZnear, aZfar : Double); cdecl; external cDllName;
procedure rlViewport(aX, aY, aWidth, aHeight : Integer); cdecl; external cDllName; // Set the viewport area

//------------------------------------------------------------------------------------
// Functions Declaration - Vertex level operations
//------------------------------------------------------------------------------------
procedure rlBegin(mode : Integer); cdecl; external cDllName;                         // Initialize drawing mode (how to organize vertex)
procedure rlEnd(); cdecl; external cDllName;                              // Finish vertex providing
procedure rlVertex2i(aX, aY : Integer); cdecl; external cDllName;                  // Define one vertex (position) - 2 int
procedure rlVertex2f(aX, aY : Single); cdecl; external cDllName;              // Define one vertex (position) - 2 float
procedure rlVertex3f(aX, aY, aZ : Single); cdecl; external cDllName;     // Define one vertex (position) - 3 float
procedure rlTexCoord2f(aX, aY : Single); cdecl; external cDllName;            // Define one vertex (texture coordinate) - 2 float
procedure rlNormal3f(aX, aY, aZ : Single); cdecl; external cDllName;     // Define one vertex (normal) - 3 float
procedure rlColor4ub(aR, aG, aB, aA : Byte); cdecl; external cDllName;    // Define one vertex (color) - 4 byte
procedure rlColor3f(aX, aY, aZ : Single); cdecl; external cDllName;          // Define one vertex (color) - 3 float
procedure rlColor4f(aX, aY, aZ, aW : Single); cdecl; external cDllName; // Define one vertex (color) - 4 float

//------------------------------------------------------------------------------------
// Functions Declaration - OpenGL equivalent functions (common to 1.1, 3.3+, ES2)
// NOTE: This functions are used to completely abstract raylib code from OpenGL layer
//------------------------------------------------------------------------------------
procedure rlEnableTexture(aId : Cardinal); cdecl; external cDllName;                  // Enable texture usage
procedure rlDisableTexture(); cdecl; external cDllName;                            // Disable texture usage
procedure rlTextureParameters(aId : Cardinal; aParam, aValue : Integer); cdecl; external cDllName; // Set texture parameters (filter, wrap)
procedure rlEnableRenderTexture(aId : Cardinal); cdecl; external cDllName;            // Enable render texture (fbo)
procedure rlDisableRenderTexture(); cdecl; external cDllName;                      // Disable render texture (fbo), return to default framebuffer
procedure rlEnableDepthTest(); cdecl; external cDllName;                           // Enable depth test
procedure rlDisableDepthTest(); cdecl; external cDllName;                          // Disable depth test
procedure rlEnableBackfaceCulling(); cdecl; external cDllName;                     // Enable backface culling
procedure rlDisableBackfaceCulling(); cdecl; external cDllName;                    // Disable backface culling
procedure rlEnableWireMode(); cdecl; external cDllName;                            // Enable wire mode
procedure rlDisableWireMode(); cdecl; external cDllName;                           // Disable wire mode
procedure rlDeleteTextures(aId : Cardinal); cdecl; external cDllName;                 // Delete OpenGL texture from GPU
procedure rlDeleteRenderTextures(aTarget : TRenderTexture2D); cdecl; external cDllName;    // Delete render textures (fbo) from GPU
procedure rlDeleteShader(aId : Cardinal); cdecl; external cDllName;                   // Delete OpenGL shader program from GPU
procedure rlDeleteVertexArrays(aId : Cardinal); cdecl; external cDllName;             // Unload vertex data (VAO) from GPU memory
procedure rlDeleteBuffers(aId : Cardinal); cdecl; external cDllName;                  // Unload vertex data (VBO) from GPU memory
procedure rlClearColor(aR, aG, aB, aA : Byte); cdecl; external cDllName;      // Clear color buffer with color
procedure rlClearScreenBuffers(); cdecl; external cDllName;                        // Clear used screen buffers (color and depth)
procedure rlUpdateBuffer(aBufferId : Integer; aData : Pointer; aDataSize : Integer); cdecl; external cDllName; // Update GPU buffer with new data
function  rlLoadAttribBuffer(aVaoId : Cardinal; aShaderLoc : Integer; aBuffer : Pointer; aSize : Integer; aDynamic : Boolean): Cardinal; cdecl; external cDllName;   // Load a new attributes buffer

//------------------------------------------------------------------------------------
// Functions Declaration - rlgl functionality
//------------------------------------------------------------------------------------
procedure rlglInit(aWidth, aHeight : Integer); cdecl; external cDllName;           // Initialize rlgl (buffers, shaders, textures, states)
procedure rlglClose(); cdecl; external cDllName;                           // De-inititialize rlgl (buffers, shaders, textures)
procedure rlglDraw(); cdecl; external cDllName;                            // Update and draw default internal buffers

function  rlGetVersion(): Integer; cdecl; external cDllName;                         // Returns current OpenGL version
function  rlCheckBufferLimit(aVCount : Integer): Boolean; cdecl; external cDllName;            // Check internal buffer overflow for a given number of vertex
procedure rlSetDebugMarker(aText : PAnsiChar); cdecl; external cDllName;        // Set debug marker for analysis
procedure rlLoadExtensions(aLoader : Pointer); cdecl; external cDllName;            // Load OpenGL extensions
function  rlUnproject(aSource : TVector3; aProj, aView : TMatrix): TVector3; cdecl; external cDllName;  // Get world coordinates from screen coordinates

// Textures data management
function  rlLoadTexture(aData : Pointer; aWidth, aHeight, aFormat, aMipmapCount : Integer): Cardinal; cdecl; external cDllName; // Load texture in GPU
function  rlLoadTextureDepth(aWidth, aHeight, aBits : Integer; aUseRenderBuffer : Boolean): Cardinal; cdecl; external cDllName;     // Load depth texture/renderbuffer (to be attached to fbo)
function  rlLoadTextureCubemap(aData : Pointer; aSize, aFormat : Integer): Cardinal; cdecl; external cDllName;                        // Load texture cubemap
procedure rlUpdateTexture(aId, aWidth, aHeight, aFormat : Integer; aData : Pointer); cdecl; external cDllName; // Update GPU texture with new data
procedure rlGetGlTextureFormats(aFormat : Integer; aGlInternalFormat, aGlFormat, aGlType : PCardinal); cdecl; external cDllName;  // Get OpenGL internal formats
procedure rlUnloadTexture(aId : Cardinal); cdecl; external cDllName;                              // Unload texture from GPU memory

procedure rlGenerateMipmaps(aTexture : PTexture2D); cdecl; external cDllName;                         // Generate mipmap data for selected texture
function  rlReadTexturePixels(aTexture : TTexture2D): Pointer; cdecl; external cDllName;                       // Read texture pixel data
function  rlReadScreenPixels(aWidth, aHeight : Integer): PAnsiChar; cdecl; external cDllName;           // Read screen pixel data (color buffer)

// Render texture management (fbo)
function  rlLoadRenderTexture(aWidth, aHeight, aFormat, aDepthBits : Integer; aUseDepthTexture : Boolean): TRenderTexture2D; cdecl; external cDllName;    // Load a render texture (with color and depth attachments)
procedure rlRenderTextureAttach(aTarget : TRenderTexture; aId : Cardinal; aAttachType : Integer); cdecl; external cDllName;  // Attach texture/renderbuffer to an fbo
function  rlRenderTextureComplete(aTarget : TRenderTexture): Boolean; cdecl; external cDllName;                 // Verify render texture is complete

// Vertex data management
procedure rlLoadMesh(aMesh : PMesh; aDynamic : Boolean); cdecl; external cDllName;                          // Upload vertex data into GPU and provided VAO/VBO ids
procedure rlUpdateMesh(aMesh : TMesh; aBuffer, aNumVertex : Integer); cdecl; external cDllName;            // Update vertex data on GPU (upload new data to one buffer)
procedure rlDrawMesh(aMesh : TMesh; aMaterial : TMaterial; aTransform : TMatrix); cdecl; external cDllName;    // Draw a 3d mesh with material and transform
procedure rlUnloadMesh(aMesh : PMesh); cdecl; external cDllName;                                      // Unload mesh data from CPU and GPU


implementation

end.
