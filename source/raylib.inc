// Configuration file for raylib

// Defines for GNU/Linux
{$IFDEF LINUX}
{$DEFINE RAY_STATIC}
{$ENDIF}

//Use selected OpenGL graphics backend, should be supported by platform
//Those preprocessor defines are only used on rlgl module, if OpenGL version is
//required by any other module, use rlGetVersion() to check it

{.$define GRAPHICS_API_OPENGL_11}
{.$define GRAPHICS_API_OPENGL_21}
{$define GRAPHICS_API_OPENGL_33}
{.$define GRAPHICS_API_OPENGL_43}
{.$define GRAPHICS_API_OPENGL_ES2}

// Default internal render batch elements limits
{.$define RL_DEFAULT_BATCH_BUFFER_ELEMENTS}     
