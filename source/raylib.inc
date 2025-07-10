// Configuration file for raylib



{$IFDEF LINUX}
  {$IFDEF USE_R3D}
    // При USE_R3D форсируем динамическую линковку, даже если RAY_STATIC был определён ранее
    {$UNDEF RAY_STATIC}
    {$DEFINE RAY_DYNAMIC}
  {$ELSE}
    // Стандартное поведение, если USE_R3D не активирован
    {$IFNDEF RAY_DYNAMIC}
      {$DEFINE RAY_STATIC}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
  {$IFNDEF RAY_DYNAMIC}
    {.$DEFINE RAY_STATIC}
  {$ENDIF}
{$IFEND}

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

{$define GRAPHICS_API_OPENGL_33}

{$PACKRECORDS C}
{$ALIGN 8}
{$MINENUMSIZE 4}



