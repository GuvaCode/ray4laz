// Configuration file for raylib

// Defines for GNU/Linux

{$DEFINE RAY_MEDIA}

{$IFDEF LINUX}
  {$IFNDEF RAY_DYNAMIC}
    {$DEFINE RAY_STATIC}
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
  {$IFNDEF RAY_DYNAMIC}
    {$DEFINE RAY_STATIC}
  {$ENDIF}
{$IFEND}

{$IFDEF HAIKU}
  {$IFNDEF RAY_DYNAMIC}
    {.$DEFINE RAY_STATIC}
  {$ENDIF}
{$IFEND}


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

//
{.$define RAYGUI_NO_RICONS}

// Other Settings

{$PACKRECORDS C}
{$ALIGN 8}
{$MINENUMSIZE 4}



