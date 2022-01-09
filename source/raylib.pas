{
██████╗  █████╗ ██╗   ██╗██╗     ██╗██████╗     ██╗  ██╗   ██╗
██╔══██╗██╔══██╗╚██╗ ██╔╝██║     ██║██╔══██╗    ██║  ██║  ███║
██████╔╝███████║ ╚████╔╝ ██║     ██║██████╔╝    ███████║  ╚██║
██╔══██╗██╔══██║  ╚██╔╝  ██║     ██║██╔══██╗    ╚════██║   ██║
██║  ██║██║  ██║   ██║   ███████╗██║██████╔╝         ██║██╗██║
╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝╚═════╝          ╚═╝╚═╝╚═╝
raylib v4.1-dev - A simple and easy-to-use library to enjoy videogames programming
(www.raylib.com)
Pascal header by Gunko Vadim (@guvacode)
}

unit raylib;

{$mode objfpc}{$H+}

interface


const
  cDllName = {$IFDEF WINDOWS} 'raylib.dll' {$IFEND}
             {$IFDEF DARWIN} 'libraylib.dylib' {$IFEND}
             {$IFDEF LINUX} 'libraylib.so' {$IFEND};

const
  DEG2RAD = (PI / 180.0);
  RAD2DEG = (180.0 / PI);
 // MAX_TOUCH_POINTS = 10;
 // MAX_SHADER_LOCATIONS = 32;
//  MAX_MATERIAL_MAPS = 12;

//----------------------------------------------------------------------------------
// Some basic Defines }
//----------------------------------------------------------------------------------

 (* Color, 4 components, R8G8B8A8 (32bit) *)
 type
   PColor = ^TColor;
   TColor = record
       r : byte; // Color red value
       g : byte; // Color green value
       b : byte; // Color blue value
       a : byte; // Color alpha value
      end;

const
  // Some Basic Colors
  // NOTE: Custom raylib color palette for amazing visuals on WHITE background
  LIGHTGRAY:      TColor = (r: 200; g: 200; b: 200; a: 255);  // Light Gray
  GRAY:           TColor = (r: 130; g: 130; b: 130; a: 255);  // Gray
  DARKGRAY:       TColor = (r: 80; g: 80; b: 80; a: 255);     // Dark Gray
  YELLOW:         TColor = (r: 253; g: 249; b: 0; a: 255);    // Yellow
  GOLD:           TColor = (r: 255; g: 203; b: 0; a: 255);    // Gold
  ORANGE:         TColor = (r: 255; g: 161; b: 0; a: 255);    // Orange
  PINK:           TColor = (r: 255; g: 109; b: 194; a: 255);  // Pink
  RED:            TColor = (r: 230; g: 41; b: 55; a: 255);    // Red
  MAROON:         TColor = (r: 190; g: 33; b: 55; a: 255);    // Maroon
  GREEN:          TColor = (r: 0; g: 228; b: 48; a: 255);     // Green
  LIME:           TColor = (r: 0; g: 158; b: 47; a: 255);     // Lime
  DARKGREEN:      TColor = (r: 0; g: 117; b: 44; a: 255);     // Dark Green
  SKYBLUE:        TColor = (r: 102; g: 191; b: 255; a: 255);  // Sky Blue
  BLUE:           TColor = (r: 0; g: 121; b: 241; a: 255);    // Blue
  DARKBLUE:       TColor = (r: 0; g: 82; b: 172; a: 255);     // Dark Blue
  PURPLE:         TColor = (r: 200; g: 122; b: 255; a: 255);  // Purple
  VIOLET:         TColor = (r: 135; g: 60; b: 190; a: 255);   // Violet
  DARKPURPLE:     TColor = (r: 112; g: 31; b: 126; a: 255);   // Dark Purple
  BEIGE:          TColor = (r: 211; g: 176; b: 131; a: 255);  // Beige
  BROWN:          TColor = (r: 127; g: 106; b: 79; a: 255);   // Brown
  DARKBROWN:      TColor = (r: 76; g: 63; b: 47; a: 255);     // Dark beown

  WHITE:          TColor = (r: 255; g: 255; b: 255; a: 255);  // White
  BLACK:          TColor = (r: 0; g: 0; b: 0; a: 255);        // Black
  BLANK:          TColor = (r: 0; g: 0; b: 0; a: 0);          // Black(Transparent)
  MAGENTA:        TColor = (r: 255; g: 0; b: 255; a: 255);    // Magenta
  RAYWHITE:       TColor = (r: 245; g: 245; b: 245; a: 255);  // My own White (raylib logo)

   (* Vector2, 2 components *)
   type
     PVector2 = ^TVector2;
     TVector2 = record
         x : single; // Vector x component
         y : single; // Vector y component
       end;

     (* Vector3, 3 components *)
     PVector3 = ^TVector3;
     TVector3 = record
         x : single; // Vector x component
         y : single; // Vector y component
         z : single; // Vector z component
       end;

     (* Vector4, 4 components *)
     PVector4 = ^TVector4;
     TVector4 = record
         x : single; // Vector x component
         y : single; // Vector y component
         z : single; // Vector z component
         w : single; // Vector w component
       end;

     (* Quaternion, 4 components (Vector4 alias) *)
     PQuaternion = ^TQuaternion;
     TQuaternion = TVector4;

     (* Matrix, 4x4 components, column major, OpenGL style, right handed *)
     PMatrix = ^TMatrix;
     TMatrix = record
         m0  : single; // Matrix first row (4 components)
         m4  : single; // Matrix first row (4 components)
         m8  : single; // Matrix first row (4 components)
         m12 : single; // Matrix first row (4 components)
         m1  : single; // Matrix second row (4 components)
         m5  : single; // Matrix second row (4 components)
         m9  : single; // Matrix second row (4 components)
         m13 : single; // Matrix second row (4 components)
         m2  : single; // Matrix third row (4 components)
         m6  : single; // Matrix third row (4 components)
         m10 : single; // Matrix third row (4 components)
         m14 : single; // Matrix third row (4 components)
         m3  : single; // Matrix fourth row (4 components)
         m7  : single; // Matrix fourth row (4 components)
         m11 : single; // Matrix fourth row (4 components)
         m15 : single; // Matrix fourth row (4 components)
       end;

     (* Rectangle, 4 components *)
     PPRectangle = ^PRectangle;
     PRectangle = ^TRectangle;
     TRectangle = record
         x      : single; // Rectangle top-left corner position x
         y      : single; // Rectangle top-left corner position y
         width  : single; // Rectangle width
         height : single; // Rectangle height
       end;

     (* Image, pixel data stored in CPU memory (RAM) *)
     PImage = ^TImage;
     TImage = record
         data    : pointer; // Image raw data
         width   : longint; // Image base width
         height  : longint; // Image base height
         mipmaps : longint; // Mipmap levels, 1 by default
         format  : longint; // Data format (PixelFormat type)
       end;

     (* Texture, tex data stored in GPU memory (VRAM) *)
     PTexture = ^TTexture;
     TTexture = record
         id      : dword;   // OpenGL texture id
         width   : longint; // Texture base width
         height  : longint; // Texture base height
         mipmaps : longint; // Mipmap levels, 1 by default
         format  : longint; // Data format (PixelFormat type)
       end;

     (* Texture2D, same as Texture *)
     PTexture2D = ^TTexture2D;
     TTexture2D = TTexture;

     (* TextureCubemap, same as Texture *)
     PTextureCubemap = ^TTextureCubemap;
     TTextureCubemap = TTexture;

     (* RenderTexture, fbo for texture rendering *)
     PRenderTexture = ^TRenderTexture;
     TRenderTexture = record
         id      : dword;    // OpenGL framebuffer object id
         texture : TTexture; // Color buffer attachment texture
         depth   : TTexture; // Depth buffer attachment texture
       end;

     (* RenderTexture2D, same as RenderTexture *)
     PRenderTexture2D = ^TRenderTexture2D;
     TRenderTexture2D = TRenderTexture;

     (* NPatchInfo, n-patch layout info *)
     PNPatchInfo = ^TNPatchInfo;
     TNPatchInfo = record
         source : TRectangle; // Texture source rectangle
         left   : longint;    // Left border offset
         top    : longint;    // Top border offset
         right  : longint;    // Right border offset
         bottom : longint;    // Bottom border offset
         layout : longint;    // Layout of the n-patch: 3x3, 1x3 or 3x1
       end;

     (* GlyphInfo, font characters glyphs info *)
     PGlyphInfo = ^TGlyphInfo;
     TGlyphInfo = record
         value    : longint; // Character value (Unicode)
         offsetX  : longint; // Character offset X when drawing
         offsetY  : longint; // Character offset Y when drawing
         advanceX : longint; // Character advance position X
         image    : TImage;  // Character image data
       end;

      (* Font, font texture and GlyphInfo array data *)
      PFont = ^TFont;
      TFont = record
          baseSize     : longint;    // Base size (default chars height)
          glyphCount   : longint;    // Number of glyph characters
          glyphPadding : longint;    // Padding around the glyph characters
          texture      : TTexture2D; // Texture atlas containing the glyphs
          recs         : PRectangle; // Rectangles in texture for the glyphs
          glyphs       : PGlyphInfo; // Glyphs info data
        end;

   (* Camera, defines position/orientation in 3d space *)
   type
     PCamera3D = ^TCamera3D;
     TCamera3D = record
         position   : TVector3; // Camera position
         target     : TVector3; // Camera target it looks-at
         up         : TVector3; // Camera up vector (rotation over its axis)
         fovy       : single;   // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
         projection : longint;  // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
       end;

     (* Camera type fallback, defaults to Camera3D *)
     PCamera = ^TCamera;
     TCamera = TCamera3D;

     (* Camera2D, defines position/orientation in 2d space *)
     PCamera2D = ^TCamera2D;
     TCamera2D = record
         offset   : TVector2; // Camera offset (displacement from target)
         target   : TVector2; // Camera target (rotation and zoom origin)
         rotation : single;   // Camera rotation in degrees
         zoom     : single;   // Camera zoom (scaling), should be 1.0f by default
       end;

     (* Mesh, vertex data and vao/vbo *)
     PMesh = ^TMesh;
     TMesh = record
         vertexCount   : longint;  // Number of vertices stored in arrays
         triangleCount : longint;  // Number of triangles stored (indexed or not)
         // Vertex attributes data
         vertices      : Psingle;  // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
         texcoords     : Psingle;  // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
         texcoords2    : Psingle;  // Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
         normals       : Psingle;  // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
         tangents      : Psingle;  // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
         colors        : Pbyte;    // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
         indices       : Pword;    // Vertex indices (in case vertex data comes indexed)
         // Animation vertex data
         animVertices  : Psingle;  // Animated vertex positions (after bones transformations)
         animNormals   : Psingle;  // Animated normals (after bones transformations)
         boneIds       : Pbyte;    // Vertex bone ids, up to 4 bones influence by vertex (skinning)
         boneWeights   : Psingle;  // Vertex bone weight, up to 4 bones influence by vertex (skinning)
         // OpenGL identifiers
         vaoId         : dword;    // OpenGL Vertex Array Object id
         vboId         : Pdword;   // OpenGL Vertex Buffer Objects id (default vertex data)
       end;

     (* Shader *)
     PShader = ^TShader;
     TShader = record
         id    : dword;    // Shader program id
         locs  : Plongint; // Shader locations array (RL_MAX_SHADER_LOCATIONS)
       end;

     (* MaterialMap *)
     PMaterialMap = ^TMaterialMap;
     TMaterialMap = record
         texture : TTexture2D; // Material map texture
         color   : TColor;     // Material map color
         value   : single;     // Material map value
       end;

     (* Material, includes shader and maps *)
     PMaterial = ^TMaterial;
     TMaterial = record
         shader  : TShader;                // Material shader
         maps    : PMaterialMap;           // Material maps array (MAX_MATERIAL_MAPS)
         params  : array[0..3] of single;  // Material generic parameters (if required)
       end;

     (* Transform, vectex transformation data *)
     PTransform = ^TTransform;
     TTransform = record
         translation : TVector3;     // Translation
         rotation    : TQuaternion;  // Rotation
         scale       : TVector3;     // Scale
       end;

     (* Bone, skeletal animation bone *)
     PBoneInfo = ^TBoneInfo;
     TBoneInfo = record
         name    : array[0..31] of char; // Bone name
         parent  : longint;              // Bone parent
       end;

     (* Model, meshes, materials and animation data *)
     PModel = ^TModel;
     TModel = record
         transform        : TMatrix;     // Local transform matrix
         meshCount        : longint;     // Number of meshes
         materialCount    : longint;     // Number of materials
         meshes           : PMesh;       // Meshes array
         materials        : PMaterial;   // Materials array
         meshMaterial     : Plongint;    // Mesh material number
         // Animation data
         boneCount        : longint;     // Number of bones
         bones            : PBoneInfo;   // Bones information (skeleton)
         bindPose         : PTransform;  // Bones base transformation (pose)
       end;

     (* ModelAnimation *)
     PModelAnimation = ^TModelAnimation;
     TModelAnimation = record
         boneCount : longint;      // Number of bones
         frameCount : longint;     // Number of animation frames
         bones : PBoneInfo;        // Bones information (skeleton)
         framePoses : ^PTransform; // Poses array by frame
       end;

      (* Ray, ray for raycasting *)
      PRay = ^TRay;
      TRay = record
         position  : TVector3; // Ray position (origin)
         direction : TVector3; // Ray direction
       end;

      (* RayCollision, ray hit information *)
      PRayCollision = ^TRayCollision;
      TRayCollision = record
          hit       : boolean;  // Did the ray hit something?
          distance  : single;   // Distance to nearest hit
          point     : TVector3; // Point of nearest hit
          normal    : TVector3; // Surface normal of hit
        end;

     (* BoundingBox *)
     PBoundingBox = ^TBoundingBox;
     TBoundingBox = record
         min : TVector3; // Minimum vertex box-corner
         max : TVector3; // Maximum vertex box-corner
       end;

     (* Wave, audio wave data *)
     PWave = ^TWave;
     TWave = record
         frameCount : dword;   // Total number of frames (considering channels)
         sampleRate : dword;   // Frequency (samples per second)
         sampleSize : dword;   // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
         channels   : dword;   // Number of channels (1-mono, 2-stereo, ...)
         data       : pointer; // Buffer data pointer
       end;

     PrAudioBuffer = ^TrAudioBuffer;
     TrAudioBuffer = record end;

     (* AudioStream, custom audio stream *)
     PAudioStream = ^TAudioStream;
     TAudioStream = record
         buffer     : PrAudioBuffer; // Pointer to internal data used by the audio system
         sampleRate : dword;         // Frequency (samples per second)
         sampleSize : dword;         // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
         channels   : dword;         // Number of channels (1-mono, 2-stereo, ...)
       end;

     (* Sound *)
     PSound = ^TSound;
     TSound = record
         stream     : TAudioStream; // Audio stream
         frameCount : dword;        // Total number of frames (considering channels)
       end;

     (*Music, audio stream, anything longer than ~10 seconds should be streamed *)
     PMusic = ^TMusic;
     TMusic = record
         stream     : TAudioStream; // Audio stream
         frameCount : dword;        // Total number of frames (considering channels)
         looping    : boolean;      // Music looping enable
         ctxType    : longint;      // Type of music context (audio filetype)
         ctxData    : pointer;      // Audio context data, depends on type
       end;

      (* VrDeviceInfo, Head-Mounted-Display device parameters *)
      PVrDeviceInfo = ^TVrDeviceInfo;
      TVrDeviceInfo = record
          hResolution            : longint;               // Horizontal resolution in pixels
          vResolution            : longint;               // Vertical resolution in pixels
          hScreenSize            : single;                // Horizontal size in meters
          vScreenSize            : single;                // Vertical size in meters
          vScreenCenter          : single;                // Screen center in meters
          eyeToScreenDistance    : single;                // Distance between eye and display in meters
          lensSeparationDistance : single;                // Lens separation distance in meters
          interpupillaryDistance : single;                // IPD (distance between pupils) in meters
          lensDistortionValues   : array[0..3] of single; // Lens distortion constant parameters
          chromaAbCorrection     : array[0..3] of single; // Chromatic aberration correction parameters
        end;

     (* VrStereoConfig, VR stereo rendering configuration for simulator *)
     PVrStereoConfig = ^TVrStereoConfig;
     TVrStereoConfig = record
         projection          : array[0..1] of TMatrix; // VR projection matrices (per eye)
         viewOffset          : array[0..1] of TMatrix; // VR view offset matrices (per eye)
         leftLensCenter      : array[0..1] of single;  // VR left lens center
         rightLensCenter     : array[0..1] of single;  // VR right lens center
         leftScreenCenter    : array[0..1] of single;  // VR left screen center
         rightScreenCenter   : array[0..1] of single;  // VR right screen center
         scale               : array[0..1] of single;  // VR distortion scale
         scaleIn             : array[0..1] of single;  // VR distortion scale in
       end;

//----------------------------------------------------------------------------------
// Enumerators Definition
//----------------------------------------------------------------------------------

     (* System/Window config flags *)
     // NOTE: Every bit registers one state (use it with bit masks)
     // By default all flags are set to 0
     PConfigFlags = ^TConfigFlags;
     TConfigFlags =  Longint;
       const
         FLAG_VSYNC_HINT            = $00000040; // Set to try enabling V-Sync on GPU
         FLAG_FULLSCREEN_MODE       = $00000002; // Set to run program in fullscreen
         FLAG_WINDOW_RESIZABLE      = $00000004; // Set to allow resizable window
         FLAG_WINDOW_UNDECORATED    = $00000008; // Set to disable window decoration (frame and buttons)
         FLAG_WINDOW_HIDDEN         = $00000080; // Set to hide window
         FLAG_WINDOW_MINIMIZED      = $00000200; // Set to minimize window (iconify)
         FLAG_WINDOW_MAXIMIZED      = $00000400; // Set to maximize window (expanded to monitor)
         FLAG_WINDOW_UNFOCUSED      = $00000800; // Set to window non focused
         FLAG_WINDOW_TOPMOST        = $00001000; // Set to window always on top
         FLAG_WINDOW_ALWAYS_RUN     = $00000100; // Set to allow windows running while minimized
         FLAG_WINDOW_TRANSPARENT    = $00000010; // Set to allow transparent framebuffer
         FLAG_WINDOW_HIGHDPI        = $00002000; // Set to support HighDPI
         FLAG_MSAA_4X_HINT          = $00000020; // Set to try enabling MSAA 4X
         FLAG_INTERLACED_HINT       = $00010000; // Set to try enabling interlaced video format (for V3D)

     (* Trace log level *)
     // NOTE: Organized by priority level
     type
       PTraceLogLevel = ^TTraceLogLevel;
       TTraceLogLevel =  Longint;
       const
         LOG_ALL      = 0; // Display all logs
         LOG_TRACE    = 1; // Trace logging, intended for internal use only
         LOG_DEBUG    = 2; // Debug logging, used for internal debugging, it should be disabled on release builds
         LOG_INFO     = 3; // Info logging, used for program execution info
         LOG_WARNING  = 4; // Warning logging, used on recoverable failures
         LOG_ERROR    = 5; // Error logging, used on unrecoverable failures
         LOG_FATAL    = 6; // Fatal logging, used to abort program: exit(EXIT_FAILURE)
         LOG_NONE     = 7; // Disable logging

     (* Keyboard keys (US keyboard layout) *)
     // NOTE: Use GetKeyPressed() to allow redefining
     // required keys for alternative layouts
     type
       PKeyboardKey = ^TKeyboardKey;
       TKeyboardKey =  Longint;
       const
         (* Alphanumeric keys *)
         KEY_NULL              = 0;    // Key: NULL, used for no key pressed
         KEY_APOSTROPHE        = 39;   // Key: '
         KEY_COMMA             = 44;   // Key: ,
         KEY_MINUS             = 45;   // Key: -
         KEY_PERIOD            = 46;   // Key: .
         KEY_SLASH             = 47;   // Key: /
         KEY_ZERO              = 48;   // Key: 0
         KEY_ONE               = 49;   // Key: 1
         KEY_TWO               = 50;   // Key: 2
         KEY_THREE             = 51;   // Key: 3
         KEY_FOUR              = 52;   // Key: 4
         KEY_FIVE              = 53;   // Key: 5
         KEY_SIX               = 54;   // Key: 6
         KEY_SEVEN             = 55;   // Key: 7
         KEY_EIGHT             = 56;   // Key: 8
         KEY_NINE              = 57;   // Key: 9
         KEY_SEMICOLON         = 59;   // Key: ;
         KEY_EQUAL             = 61;   // Key: =
         KEY_A                 = 65;   // Key: A | a
         KEY_B                 = 66;   // Key: B | b
         KEY_C                 = 67;   // Key: C | c
         KEY_D                 = 68;   // Key: D | d
         KEY_E                 = 69;   // Key: E | e
         KEY_F                 = 70;   // Key: F | f
         KEY_G                 = 71;   // Key: G | g
         KEY_H                 = 72;   // Key: H | h
         KEY_I                 = 73;   // Key: I | i
         KEY_J                 = 74;   // Key: J | j
         KEY_K                 = 75;   // Key: K | k
         KEY_L                 = 76;   // Key: L | l
         KEY_M                 = 77;   // Key: M | m
         KEY_N                 = 78;   // Key: N | n
         KEY_O                 = 79;   // Key: O | o
         KEY_P                 = 80;   // Key: P | p
         KEY_Q                 = 81;   // Key: Q | q
         KEY_R                 = 82;   // Key: R | r
         KEY_S                 = 83;   // Key: S | s
         KEY_T                 = 84;   // Key: T | t
         KEY_U                 = 85;   // Key: U | u
         KEY_V                 = 86;   // Key: V | v
         KEY_W                 = 87;   // Key: W | w
         KEY_X                 = 88;   // Key: X | x
         KEY_Y                 = 89;   // Key: Y | y
         KEY_Z                 = 90;   // Key: Z | z
         KEY_LEFT_BRACKET      = 91;   // Key: [
         KEY_BACKSLASH         = 92;   // Key: '\'
         KEY_RIGHT_BRACKET     = 93;   // Key: ]
         KEY_GRAVE             = 96;   // Key: `
         (* Function keys *)
         KEY_SPACE             = 32;   // Key: Space
         KEY_ESCAPE            = 256;  // Key: Esc
         KEY_ENTER             = 257;  // Key: Enter
         KEY_TAB               = 258;  // Key: Tab
         KEY_BACKSPACE         = 259;  // Key: Backspace
         KEY_INSERT            = 260;  // Key: Ins
         KEY_DELETE            = 261;  // Key: Del
         KEY_RIGHT             = 262;  // Key: Cursor right
         KEY_LEFT              = 263;  // Key: Cursor left
         KEY_DOWN              = 264;  // Key: Cursor down
         KEY_UP                = 265;  // Key: Cursor up
         KEY_PAGE_UP           = 266;  // Key: Page up
         KEY_PAGE_DOWN         = 267;  // Key: Page down
         KEY_HOME              = 268;  // Key: Home
         KEY_END               = 269;  // Key: End
         KEY_CAPS_LOCK         = 280;  // Key: Caps lock
         KEY_SCROLL_LOCK       = 281;  // Key: Scroll down
         KEY_NUM_LOCK          = 282;  // Key: Num lock
         KEY_PRINT_SCREEN      = 283;  // Key: Print screen
         KEY_PAUSE             = 284;  // Key: Pause
         KEY_F1                = 290;  // Key: F1
         KEY_F2                = 291;  // Key: F2
         KEY_F3                = 292;  // Key: F3
         KEY_F4                = 293;  // Key: F4
         KEY_F5                = 294;  // Key: F5
         KEY_F6                = 295;  // Key: F6
         KEY_F7                = 296;  // Key: F7
         KEY_F8                = 297;  // Key: F8
         KEY_F9                = 298;  // Key: F9
         KEY_F10               = 299;  // Key: F10
         KEY_F11               = 300;  // Key: F11
         KEY_F12               = 301;  // Key: F12
         KEY_LEFT_SHIFT        = 340;  // Key: Shift left
         KEY_LEFT_CONTROL      = 341;  // Key: Control left
         KEY_LEFT_ALT          = 342;  // Key: Alt left
         KEY_LEFT_SUPER        = 343;  // Key: Super left
         KEY_RIGHT_SHIFT       = 344;  // Key: Shift right
         KEY_RIGHT_CONTROL     = 345;  // Key: Control right
         KEY_RIGHT_ALT         = 346;  // Key: Alt right
         KEY_RIGHT_SUPER       = 347;  // Key: Super right
         KEY_KB_MENU           = 348;  // Key: KB menu
         (* Keypad keys *)
         KEY_KP_0              = 320;  // Key: Keypad 0
         KEY_KP_1              = 321;  // Key: Keypad 1
         KEY_KP_2              = 322;  // Key: Keypad 2
         KEY_KP_3              = 323;  // Key: Keypad 3
         KEY_KP_4              = 324;  // Key: Keypad 4
         KEY_KP_5              = 325;  // Key: Keypad 5
         KEY_KP_6              = 326;  // Key: Keypad 6
         KEY_KP_7              = 327;  // Key: Keypad 7
         KEY_KP_8              = 328;  // Key: Keypad 8
         KEY_KP_9              = 329;  // Key: Keypad 9
         KEY_KP_DECIMAL        = 330;  // Key: Keypad .
         KEY_KP_DIVIDE         = 331;  // Key: Keypad /
         KEY_KP_MULTIPLY       = 332;  // Key: Keypad *
         KEY_KP_SUBTRACT       = 333;  // Key: Keypad -
         KEY_KP_ADD            = 334;  // Key: Keypad +
         KEY_KP_ENTER          = 335;  // Key: Keypad Enter
         KEY_KP_EQUAL          = 336;  // Key: Keypad =
         // Android key buttons
         KEY_BACK              = 4;    // Key: Android back button
         KEY_MENU              = 82;   // Key: Android menu button
         KEY_VOLUME_UP         = 24;   // Key: Android volume up button
         KEY_VOLUME_DOWN       = 25;   // Key: Android volume down button

     (* Mouse buttons *)
     type
       PMouseButton = ^TMouseButton;
       TMouseButton =  Longint;
       const
         MOUSE_BUTTON_LEFT     = 0; // Mouse button left
         MOUSE_BUTTON_RIGHT    = 1; // Mouse button right
         MOUSE_BUTTON_MIDDLE   = 2; // Mouse button middle (pressed wheel)
         MOUSE_BUTTON_SIDE     = 3; // Mouse button side (advanced mouse device)
         MOUSE_BUTTON_EXTRA    = 4; // Mouse button extra (advanced mouse device)
         MOUSE_BUTTON_FORWARD  = 5; // Mouse button fordward (advanced mouse device)
         MOUSE_BUTTON_BACK     = 6; // Mouse button back (advanced mouse device)

         (* Add backwards compatibility support for deprecated names *)
         MOUSE_LEFT_BUTTON = MOUSE_BUTTON_LEFT;
         MOUSE_RIGHT_BUTTON = MOUSE_BUTTON_RIGHT;
         MOUSE_MIDDLE_BUTTON = MOUSE_BUTTON_MIDDLE;

     (* Mouse cursor *)
     type
       PMouseCursor = ^TMouseCursor;
       TMouseCursor =  Longint;
       const
         MOUSE_CURSOR_DEFAULT         = 0;  // Default pointer shape
         MOUSE_CURSOR_ARROW           = 1;  // Arrow shape
         MOUSE_CURSOR_IBEAM           = 2;  // Text writing cursor shape
         MOUSE_CURSOR_CROSSHAIR       = 3;  // Cross shape
         MOUSE_CURSOR_POINTING_HAND   = 4;  // Pointing hand cursor
         MOUSE_CURSOR_RESIZE_EW       = 5;  // Horizontal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NS       = 6;  // Vertical resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NWSE     = 7;  // Top-left to bottom-right diagonal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NESW     = 8;  // The top-right to bottom-left diagonal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_ALL      = 9;  // The omni-directional resize/move cursor shape
         MOUSE_CURSOR_NOT_ALLOWED     = 10; // The operation-not-allowed shape

   (* Gamepad buttons *)
   type
      PGamepadButton = ^TGamepadButton;
      TGamepadButton =  Longint;
      const
        GAMEPAD_BUTTON_UNKNOWN           = 0;  // Unknown button, just for error checking
        GAMEPAD_BUTTON_LEFT_FACE_UP      = 1;  // Gamepad left DPAD up button
        GAMEPAD_BUTTON_LEFT_FACE_RIGHT   = 2;  // Gamepad left DPAD right button
        GAMEPAD_BUTTON_LEFT_FACE_DOWN    = 3;  // Gamepad left DPAD down button
        GAMEPAD_BUTTON_LEFT_FACE_LEFT    = 4;  // Gamepad left DPAD left button
        GAMEPAD_BUTTON_RIGHT_FACE_UP     = 5;  // Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
        GAMEPAD_BUTTON_RIGHT_FACE_RIGHT  = 6;  // Gamepad right button right (i.e. PS3: Square, Xbox: X)
        GAMEPAD_BUTTON_RIGHT_FACE_DOWN   = 7;  // Gamepad right button down (i.e. PS3: Cross, Xbox: A)
        GAMEPAD_BUTTON_RIGHT_FACE_LEFT   = 8;  // Gamepad right button left (i.e. PS3: Circle, Xbox: B)
        GAMEPAD_BUTTON_LEFT_TRIGGER_1    = 9;  // Gamepad top/back trigger left (first), it could be a trailing button
        GAMEPAD_BUTTON_LEFT_TRIGGER_2    = 10; // Gamepad top/back trigger left (second), it could be a trailing button
        GAMEPAD_BUTTON_RIGHT_TRIGGER_1   = 11; // Gamepad top/back trigger right (one), it could be a trailing button
        GAMEPAD_BUTTON_RIGHT_TRIGGER_2   = 12; // Gamepad top/back trigger right (second), it could be a trailing button
        GAMEPAD_BUTTON_MIDDLE_LEFT       = 13; // Gamepad center buttons, left one (i.e. PS3: Select)
        GAMEPAD_BUTTON_MIDDLE            = 14; // Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
        GAMEPAD_BUTTON_MIDDLE_RIGHT      = 15; // Gamepad center buttons, right one (i.e. PS3: Start)
        GAMEPAD_BUTTON_LEFT_THUMB        = 16; // Gamepad joystick pressed button left
        GAMEPAD_BUTTON_RIGHT_THUMB       = 17; // Gamepad joystick pressed button right

   (* Gamepad axis *)
   type
     PGamepadAxis = ^TGamepadAxis;
     TGamepadAxis =  Longint;
     const
       GAMEPAD_AXIS_LEFT_X           = 0;  // Gamepad left stick X axis
       GAMEPAD_AXIS_LEFT_Y           = 1;  // Gamepad left stick Y axis
       GAMEPAD_AXIS_RIGHT_X          = 2;  // Gamepad right stick X axis
       GAMEPAD_AXIS_RIGHT_Y          = 3;  // Gamepad right stick Y axis
       GAMEPAD_AXIS_LEFT_TRIGGER     = 4;  // Gamepad back trigger left, pressure level: [1..-1]
       GAMEPAD_AXIS_RIGHT_TRIGGER    = 5;  // Gamepad back trigger right, pressure level: [1..-1]

     (* Material map index *)
     type
       PMaterialMapIndex = ^TMaterialMapIndex;
       TMaterialMapIndex =  Longint;
       const
         MATERIAL_MAP_ALBEDO        = 0;  // Albedo material (same as: MATERIAL_MAP_DIFFUSE)
         MATERIAL_MAP_METALNESS     = 1;  // Metalness material (same as: MATERIAL_MAP_SPECULAR)
         MATERIAL_MAP_NORMAL        = 2;  // Normal material
         MATERIAL_MAP_ROUGHNESS     = 3;  // Roughness material
         MATERIAL_MAP_OCCLUSION     = 4;  // Ambient occlusion material
         MATERIAL_MAP_EMISSION      = 5;  // Emission material
         MATERIAL_MAP_HEIGHT        = 6;  // Heightmap material
         MATERIAL_MAP_CUBEMAP       = 7;  // Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_IRRADIANCE    = 8;  // Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_PREFILTER     = 9;  // Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_BRDF          = 10; // Brdf material

         MATERIAL_MAP_DIFFUSE = MATERIAL_MAP_ALBEDO;
         MATERIAL_MAP_SPECULAR = MATERIAL_MAP_METALNESS;

     (* Shader location index *)
     type
       PShaderLocationIndex = ^TShaderLocationIndex;
       TShaderLocationIndex =  Longint;
       const
         SHADER_LOC_VERTEX_POSITION     = 0;  // Shader location: vertex attribute: position
         SHADER_LOC_VERTEX_TEXCOORD01   = 1;  // Shader location: vertex attribute: texcoord01
         SHADER_LOC_VERTEX_TEXCOORD02   = 2;  // Shader location: vertex attribute: texcoord02
         SHADER_LOC_VERTEX_NORMAL       = 3;  // Shader location: vertex attribute: normal
         SHADER_LOC_VERTEX_TANGENT      = 4;  // Shader location: vertex attribute: tangent
         SHADER_LOC_VERTEX_COLOR        = 5;  // Shader location: vertex attribute: color
         SHADER_LOC_MATRIX_MVP          = 6;  // Shader location: matrix uniform: model-view-projection
         SHADER_LOC_MATRIX_VIEW         = 7;  // Shader location: matrix uniform: view (camera transform)
         SHADER_LOC_MATRIX_PROJECTION   = 8;  // Shader location: matrix uniform: projection
         SHADER_LOC_MATRIX_MODEL        = 9;  // Shader location: matrix uniform: model (transform)
         SHADER_LOC_MATRIX_NORMAL       = 10; // Shader location: matrix uniform: normal
         SHADER_LOC_VECTOR_VIEW         = 11; // Shader location: vector uniform: view
         SHADER_LOC_COLOR_DIFFUSE       = 12; // Shader location: vector uniform: diffuse color
         SHADER_LOC_COLOR_SPECULAR      = 13; // Shader location: vector uniform: specular color
         SHADER_LOC_COLOR_AMBIENT       = 14; // Shader location: vector uniform: ambient color
         SHADER_LOC_MAP_ALBEDO          = 15; // Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
         SHADER_LOC_MAP_METALNESS       = 16; // Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
         SHADER_LOC_MAP_NORMAL          = 17; // Shader location: sampler2d texture: normal
         SHADER_LOC_MAP_ROUGHNESS       = 18; // Shader location: sampler2d texture: roughness
         SHADER_LOC_MAP_OCCLUSION       = 19; // Shader location: sampler2d texture: occlusion
         SHADER_LOC_MAP_EMISSION        = 20; // Shader location: sampler2d texture: emission
         SHADER_LOC_MAP_HEIGHT          = 21; // Shader location: sampler2d texture: height
         SHADER_LOC_MAP_CUBEMAP         = 22; // Shader location: samplerCube texture: cubemap
         SHADER_LOC_MAP_IRRADIANCE      = 23; // Shader location: samplerCube texture: irradiance
         SHADER_LOC_MAP_PREFILTER       = 24; // Shader location: samplerCube texture: prefilter
         SHADER_LOC_MAP_BRDF            = 25; // Shader location: sampler2d texture: brdf

         SHADER_LOC_MAP_DIFFUSE = SHADER_LOC_MAP_ALBEDO;
         SHADER_LOC_MAP_SPECULAR = SHADER_LOC_MAP_METALNESS;

     (* Shader uniform data type *)
     type
       PShaderUniformDataType = ^TShaderUniformDataType;
       TShaderUniformDataType =  Longint;
       Const
         SHADER_UNIFORM_FLOAT      = 0; // Shader uniform type: float
         SHADER_UNIFORM_VEC2       = 1; // Shader uniform type: vec2 (2 float)
         SHADER_UNIFORM_VEC3       = 2; // Shader uniform type: vec3 (3 float)
         SHADER_UNIFORM_VEC4       = 3; // Shader uniform type: vec4 (4 float)
         SHADER_UNIFORM_INT        = 4; // Shader uniform type: int
         SHADER_UNIFORM_IVEC2      = 5; // Shader uniform type: ivec2 (2 int)
         SHADER_UNIFORM_IVEC3      = 6; // Shader uniform type: ivec3 (3 int)
         SHADER_UNIFORM_IVEC4      = 7; // Shader uniform type: ivec4 (4 int)
         SHADER_UNIFORM_SAMPLER2D  = 8; // Shader uniform type: sampler2d

    (* Shader attribute data types *)
    type
      PShaderAttributeDataType = ^TShaderAttributeDataType;
      TShaderAttributeDataType =  Longint;
      const
        SHADER_ATTRIB_FLOAT     = 0; // Shader attribute type: float
        SHADER_ATTRIB_VEC2      = 1; // Shader attribute type: vec2 (2 float)
        SHADER_ATTRIB_VEC3      = 2; // Shader attribute type: vec3 (3 float)
        SHADER_ATTRIB_VEC4      = 3; // Shader attribute type: vec4 (4 float)

     (* Pixel formats *)
     //NOTE: Support depends on OpenGL version and platform
     type
       PPixelFormat = ^TPixelFormat;
       TPixelFormat =  Longint;
       const
         PIXELFORMAT_UNCOMPRESSED_GRAYSCALE     = 1;  // 8 bit per pixel (no alpha)
         PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA    = 2;  // 8*2 bpp (2 channels)
         PIXELFORMAT_UNCOMPRESSED_R5G6B5        = 3;  // 16 bpp
         PIXELFORMAT_UNCOMPRESSED_R8G8B8        = 4;  // 24 bpp
         PIXELFORMAT_UNCOMPRESSED_R5G5B5A1      = 5;  // 16 bpp (1 bit alpha)
         PIXELFORMAT_UNCOMPRESSED_R4G4B4A4      = 6;  // 16 bpp (4 bit alpha)
         PIXELFORMAT_UNCOMPRESSED_R8G8B8A8      = 7;  // 32 bpp
         PIXELFORMAT_UNCOMPRESSED_R32           = 8;  // 32 bpp (1 channel - float)
         PIXELFORMAT_UNCOMPRESSED_R32G32B32     = 9;  // 32*3 bpp (3 channels - float)
         PIXELFORMAT_UNCOMPRESSED_R32G32B32A32  = 10; // 32*4 bpp (4 channels - float)
         PIXELFORMAT_COMPRESSED_DXT1_RGB        = 11; // 4 bpp (no alpha)
         PIXELFORMAT_COMPRESSED_DXT1_RGBA       = 12; // 4 bpp (1 bit alpha)
         PIXELFORMAT_COMPRESSED_DXT3_RGBA       = 13; // 8 bpp
         PIXELFORMAT_COMPRESSED_DXT5_RGBA       = 14; // 8 bpp
         PIXELFORMAT_COMPRESSED_ETC1_RGB        = 15; // 4 bpp
         PIXELFORMAT_COMPRESSED_ETC2_RGB        = 16; // 4 bpp
         PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA   = 17; // 8 bpp
         PIXELFORMAT_COMPRESSED_PVRT_RGB        = 18; // 4 bpp
         PIXELFORMAT_COMPRESSED_PVRT_RGBA       = 19; // 4 bpp
         PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA   = 20; // 8 bpp
         PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA   = 21; // 2 bpp

   (* Texture parameters: filter mode *)
   //NOTE 1: Filtering considers mipmaps if available in the texture
   //NOTE 2: Filter is accordingly set for minification and magnification
   type
     PTextureFilter = ^TTextureFilter;
     TTextureFilter =  Longint;
     const
       TEXTURE_FILTER_POINT            = 0; // No filter, just pixel approximation
       TEXTURE_FILTER_BILINEAR         = 1; // Linear filtering
       TEXTURE_FILTER_TRILINEAR        = 2; // Trilinear filtering (linear with mipmaps)
       TEXTURE_FILTER_ANISOTROPIC_4X   = 3; // Anisotropic filtering 4x
       TEXTURE_FILTER_ANISOTROPIC_8X   = 4; // Anisotropic filtering 8x
       TEXTURE_FILTER_ANISOTROPIC_16X  = 5; // Anisotropic filtering 16x

   (* Texture parameters: wrap mode *)
   type
     PTextureWrap = ^TTextureWrap;
     TTextureWrap =  Longint;
     Const
       TEXTURE_WRAP_REPEAT        = 0; // Repeats texture in tiled mode
       TEXTURE_WRAP_CLAMP         = 1; // Clamps texture to edge pixel in tiled mode
       TEXTURE_WRAP_MIRROR_REPEAT = 2; // Mirrors and repeats the texture in tiled mode
       TEXTURE_WRAP_MIRROR_CLAMP  = 3; // Mirrors and clamps to border the texture in tiled mode

   (* Cubemap layouts *)
   type
     PCubemapLayout = ^TCubemapLayout;
     TCubemapLayout =  Longint;
     Const
       CUBEMAP_LAYOUT_AUTO_DETECT         = 0; // Automatically detect layout type
       CUBEMAP_LAYOUT_LINE_VERTICAL       = 1; // Layout is defined by a vertical line with faces
       CUBEMAP_LAYOUT_LINE_HORIZONTAL     = 2; // Layout is defined by an horizontal line with faces
       CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR = 3; // Layout is defined by a 3x4 cross with cubemap faces
       CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE = 4; // Layout is defined by a 4x3 cross with cubemap faces
       CUBEMAP_LAYOUT_PANORAMA            = 5; // Layout is defined by a panorama image (equirectangular map)

   (* Font type, defines generation method *)
   type
     PFontType = ^TFontType;
     TFontType =  Longint;
     Const
       FONT_DEFAULT          = 0;    // Default font generation, anti-aliased
       FONT_BITMAP           = 1;    // Bitmap font generation, no anti-aliasing
       FONT_SDF              = 2;    // SDF font generation, requires external shader

   (* Color blending modes (pre-defined) *)
   type
     PBlendMode = ^TBlendMode;
     TBlendMode =  Longint;
     Const
       BLEND_ALPHA           = 0;    // Blend textures considering alpha (default)
       BLEND_ADDITIVE        = 1;    // Blend textures adding colors
       BLEND_MULTIPLIED      = 2;    // Blend textures multiplying colors
       BLEND_ADD_COLORS      = 3;    // Blend textures adding colors (alternative)
       BLEND_SUBTRACT_COLORS = 4;    // Blend textures subtracting colors (alternative)
       BLEND_CUSTOM          = 5;    // Belnd textures using custom src/dst factors (use rlSetBlendMode())

   (* Gestures *)
   //  NOTE: It could be used as flags to enable only some gestures
   type
     PGesture = ^TGesture;
     TGesture =  Longint;
     Const
       GESTURE_NONE          = 0;    // No gesture
       GESTURE_TAP           = 1;    // Tap gesture
       GESTURE_DOUBLETAP     = 2;    // Double tap gesture
       GESTURE_HOLD          = 4;    // Hold gesture
       GESTURE_DRAG          = 8;    // Drag gesture
       GESTURE_SWIPE_RIGHT   = 16;   // Swipe right gesture
       GESTURE_SWIPE_LEFT    = 32;   // Swipe left gesture
       GESTURE_SWIPE_UP      = 64;   // Swipe up gesture
       GESTURE_SWIPE_DOWN    = 128;  // Swipe down gesture
       GESTURE_PINCH_IN      = 256;  // Pinch in gesture
       GESTURE_PINCH_OUT     = 512;  // Pinch out gesture

   (* Camera system modes *)
   type
     PCameraMode = ^TCameraMode;
     TCameraMode =  Longint;
     Const
       CAMERA_CUSTOM = 0;      // Custom camera
       CAMERA_FREE = 1;        // Free camera
       CAMERA_ORBITAL = 2;     // Orbital camera
       CAMERA_FIRST_PERSON = 3;// First person camera
       CAMERA_THIRD_PERSON = 4;// Third person camera

   (* Camera projection *)
   type
     PCameraProjection = ^TCameraProjection;
     TCameraProjection =  Longint;
     const
       CAMERA_PERSPECTIVE = 0; // Perspective projection
       CAMERA_ORTHOGRAPHIC = 1;// Orthographic projection

     (* N-patch layout *)
     type
      PNPatchLayout = ^TNPatchLayout;
      TNPatchLayout =  Longint;

      const
        NPATCH_NINE_PATCH = 0;             // Npatch layout: 3x3 tiles
        NPATCH_THREE_PATCH_VERTICAL = 1;   // Npatch layout: 1x3 tiles
        NPATCH_THREE_PATCH_HORIZONTAL = 2; // Npatch layout: 3x1 tiles

(* Callbacks to hook some internal functions *)
// WARNING: This callbacks are intended for advance users
type
  TTraceLogCallback = procedure (logLevel:longint; text:Pchar; args: array of const);cdecl;// Logging: Redirect trace log messages
  PLoadFileDataCallback = ^TLoadFileDataCallback;
  TLoadFileDataCallback = function (fileName:Pchar; bytesRead:Pdword):Pbyte;cdecl;// FileIO: Load binary data
  TSaveFileDataCallback = function (fileName:Pchar; data:pointer; bytesToWrite:dword):boolean;cdecl;// FileIO: Save binary data
  PLoadFileTextCallback = ^TLoadFileTextCallback;
  TLoadFileTextCallback = function (fileName:Pchar):Pchar;cdecl;// FileIO: Load text data
  TSaveFileTextCallback = function (fileName:Pchar; text:Pchar):boolean;cdecl;// FileIO: Save text data

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

(* Window-related function *)
procedure InitWindow(width:longint; height:longint; title:Pchar);cdecl;external cDllName;// Initialize window and OpenGL context
function WindowShouldClose:boolean;cdecl;external cDllName;// Check if KEY_ESCAPE pressed or Close icon pressed
procedure CloseWindow;cdecl;external cDllName;// Close window and unload OpenGL context
function IsWindowReady:boolean;cdecl;external cDllName;// Check if window has been initialized successfully
function IsWindowFullscreen:boolean;cdecl;external cDllName;// Check if window is currently fullscreen
function IsWindowHidden:boolean;cdecl;external cDllName;// Check if window is currently hidden (only PLATFORM_DESKTOP)
function IsWindowMinimized:boolean;cdecl;external cDllName;// Check if window is currently minimized (only PLATFORM_DESKTOP)
function IsWindowMaximized:boolean;cdecl;external cDllName;// Check if window is currently maximized (only PLATFORM_DESKTOP)
function IsWindowFocused:boolean;cdecl;external cDllName;// Check if window is currently focused (only PLATFORM_DESKTOP)
function IsWindowResized:boolean;cdecl;external cDllName;// Check if window has been resized last frame
function IsWindowState(flag:dword):boolean;cdecl;external cDllName;// Check if one specific window flag is enabled
procedure SetWindowState(flags:dword);cdecl;external cDllName;// Set window configuration state using flags (only PLATFORM_DESKTOP)
procedure ClearWindowState(flags:dword);cdecl;external cDllName;// Clear window configuration state flags
procedure ToggleFullscreen;cdecl;external cDllName;// Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
procedure MaximizeWindow;cdecl;external cDllName;// Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
procedure MinimizeWindow;cdecl;external cDllName;// Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
procedure RestoreWindow;cdecl;external cDllName;// Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
procedure SetWindowIcon(image:TImage);cdecl;external cDllName;// Set icon for window (only PLATFORM_DESKTOP)
procedure SetWindowTitle(title:Pchar);cdecl;external cDllName;// Set title for window (only PLATFORM_DESKTOP)
procedure SetWindowPosition(x:longint; y:longint);cdecl;external cDllName;// Set window position on screen (only PLATFORM_DESKTOP)
procedure SetWindowMonitor(monitor:longint);cdecl;external cDllName;// Set monitor for the current window (fullscreen mode)
procedure SetWindowMinSize(width:longint; height:longint);cdecl;external cDllName;// Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
procedure SetWindowSize(width:longint; height:longint);cdecl;external cDllName;// Set window dimensions
procedure SetWindowOpacity(opacity: single);cdecl;external cDllName;// Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
function GetWindowHandle:pointer;cdecl;external cDllName;// Get native window handle
function GetScreenWidth:longint;cdecl;external cDllName;// Get current screen width
function GetScreenHeight:longint;cdecl;external cDllName;// Get current screen height
function GetRenderWidth:longint;cdecl;external cDllName;// Get current render width (it considers HiDPI)
function GetRenderHeight:longint;cdecl;external cDllName;// Get current render height (it considers HiDPI)
function GetMonitorCount:longint;cdecl;external cDllName;// Get number of connected monitors
function GetCurrentMonitor:longint;cdecl;external cDllName;// Get current connected monitor
function GetMonitorPosition(monitor:longint):TVector2;cdecl;external cDllName;// Get specified monitor position
function GetMonitorWidth(monitor:longint):longint;cdecl;external cDllName;// Get specified monitor width (max available by monitor)
function GetMonitorHeight(monitor:longint):longint;cdecl;external cDllName;// Get specified monitor height (max available by monitor)
function GetMonitorPhysicalWidth(monitor:longint):longint;cdecl;external cDllName;// Get specified monitor physical width in millimetres
function GetMonitorPhysicalHeight(monitor:longint):longint;cdecl;external cDllName;// Get specified monitor physical height in millimetres
function GetMonitorRefreshRate(monitor:longint):longint;cdecl;external cDllName;// Get specified monitor refresh rate
function GetWindowPosition:TVector2;cdecl;external cDllName;// Get window position XY on monitor
function GetWindowScaleDPI:TVector2;cdecl;external cDllName;// Get window scale DPI factor
function GetMonitorName(monitor:longint):Pchar;cdecl;external cDllName;// Get the human-readable, UTF-8 encoded name of the primary monitor

procedure SetClipboardText(text:Pchar);cdecl;external cDllName;// Set clipboard text content
function GetClipboardText:Pchar;cdecl;external cDllName;// Get clipboard text content

(* Custom frame control functions *)
// NOTE: Those functions are intended for advance users that want full control over the frame processing
// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
procedure SwapScreenBuffer;cdecl;external cDllName;// Swap back buffer with front buffer (screen drawing)
procedure PollInputEvents;cdecl;external cDllName;// Register all input events
procedure WaitTime(ms:single);cdecl;external cDllName;// Wait for some milliseconds (halt program execution)

(* Cursor-related functions *)
procedure ShowCursor;cdecl;external cDllName;// Shows cursor
procedure HideCursor;cdecl;external cDllName;// Hides cursor
function IsCursorHidden:boolean;cdecl;external cDllName;// Check if cursor is not visible
procedure EnableCursor;cdecl;external cDllName;// Enables cursor (unlock cursor)
procedure DisableCursor;cdecl;external cDllName;// Disables cursor (lock cursor)
function IsCursorOnScreen:boolean;cdecl;external cDllName;// Check if cursor is on the current screen.

(* Drawing-related functions *)
procedure ClearBackground(color:TColor);cdecl;external cDllName;// Set background color (framebuffer clear color)
procedure BeginDrawing;cdecl;external cDllName;// Setup canvas (framebuffer) to start drawing
procedure EndDrawing;cdecl;external cDllName;// End canvas drawing and swap buffers (double buffering)
procedure BeginMode2D(camera:TCamera2D);cdecl;external cDllName;// Initialize 2D mode with custom camera (2D)
procedure EndMode2D;cdecl;external cDllName;// Ends 2D mode with custom camera
procedure BeginMode3D(camera:TCamera3D);cdecl;external cDllName;// Initializes 3D mode with custom camera (3D)
procedure EndMode3D;cdecl;external cDllName;// Ends 3D mode and returns to default 2D orthographic mode
procedure BeginTextureMode(target:TRenderTexture2D);cdecl;external cDllName;// Initializes render texture for drawing
procedure EndTextureMode;cdecl;external cDllName;// Ends drawing to render texture
procedure BeginShaderMode(shader:TShader);cdecl;external cDllName;// Begin custom shader drawing
procedure EndShaderMode;cdecl;external cDllName;// End custom shader drawing (use default shader)
procedure BeginBlendMode(mode: longint);cdecl;external cDllName;// Begin blending mode (alpha, additive, multiplied)
procedure EndBlendMode;cdecl;external cDllName;// End blending mode (reset to default: alpha blending)
procedure BeginScissorMode(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName;// Begin scissor mode (define screen area for following drawing)
procedure EndScissorMode;cdecl;external cDllName;// End scissor mode
procedure BeginVrStereoMode(config:TVrStereoConfig);cdecl;external cDllName;// Begin stereo rendering (requires VR simulator)
procedure EndVrStereoMode;cdecl;external cDllName;// End stereo rendering (requires VR simulator)

(* VR stereo config functions for VR simulator *)
function LoadVrStereoConfig(device:TVrDeviceInfo):TVrStereoConfig;cdecl;external cDllName;// Load VR stereo config for VR simulator device parameters
procedure UnloadVrStereoConfig(config:TVrStereoConfig);cdecl;external cDllName;// Unload VR stereo config

(* Shader management functions *)
// NOTE: Shader functionality is not available on OpenGL 1.1
function LoadShader(vsFileName:Pchar; fsFileName:Pchar):TShader;cdecl;external cDllName;// Load shader from files and bind default locations
function LoadShaderFromMemory(vsCode:Pchar; fsCode:Pchar):TShader;cdecl;external cDllName;// Load shader from code strings and bind default locations
function GetShaderLocation(shader:TShader; uniformName:Pchar):longint;cdecl;external cDllName;// Get shader uniform location
function GetShaderLocationAttrib(shader:TShader; attribName:Pchar):longint;cdecl;external cDllName;// Get shader attribute location
procedure SetShaderValue(shader:TShader; locIndex:longint; value:pointer; uniformType:longint);cdecl;external cDllName;// Set shader uniform value
procedure SetShaderValueV(shader:TShader; locIndex:longint; value:pointer; uniformType:longint; count:longint);cdecl;external cDllName;// Set shader uniform value vector
procedure SetShaderValueMatrix(shader:TShader; locIndex:longint; mat:TMatrix);cdecl;external cDllName;// Set shader uniform value (matrix 4x4)
procedure SetShaderValueTexture(shader:TShader; locIndex:longint; texture:TTexture2D);cdecl;external cDllName;// Set shader uniform value for texture (sampler2d)
procedure UnloadShader(shader:TShader);cdecl;external cDllName;// Unload shader from GPU memory (VRAM)

(* Screen-space-related functions *)
function GetMouseRay(mousePosition:TVector2; camera:TCamera):TRay;cdecl;external cDllName;// Get a ray trace from mouse position
function GetCameraMatrix(camera:TCamera):TMatrix;cdecl;external cDllName;// Get camera transform matrix (view matrix)
function GetCameraMatrix2D(camera:TCamera2D):TMatrix;cdecl;external cDllName;// Get camera 2d transform matrix
function GetWorldToScreen(position:TVector3; camera:TCamera):TVector2;cdecl;external cDllName;// Get the screen space position for a 3d world space position
function GetWorldToScreenEx(position:TVector3; camera:TCamera; width:longint; height:longint):TVector2;cdecl;external cDllName;// Get size position for a 3d world space position
function GetWorldToScreen2D(position:TVector2; camera:TCamera2D):TVector2;cdecl;external cDllName;// Get the screen space position for a 2d camera world space position
function GetScreenToWorld2D(position:TVector2; camera:TCamera2D):TVector2;cdecl;external cDllName;// Get the world space position for a 2d camera screen space position

(* Timing-related functions *)
procedure SetTargetFPS(fps:longint);cdecl;external cDllName;// Set target FPS (maximum)
function GetFPS:longint;cdecl;external cDllName;// Returns current FPS
function GetFrameTime:single;cdecl;external cDllName;// Returns time in seconds for last frame drawn (delta time)
function GetTime:double;cdecl;external cDllName;// Returns elapsed time in seconds since InitWindow()

(* Misc. functions *)
function GetRandomValue(min:longint; max:longint):longint;cdecl;external cDllName;// Get a random value between min and max (both included)
procedure SetRandomSeed(seed:Pdword);cdecl;external cDllName;// Set the seed for the random number generator
procedure TakeScreenshot(fileName:Pchar);cdecl;external cDllName;// Takes a screenshot of current screen (filename extension defines format)
procedure SetConfigFlags(flags:dword);cdecl;external cDllName;// Setup init configuration flags (view FLAGS)
procedure TraceLog(logLevel:longint; text:Pchar; args:array of const);cdecl;external cDllName;
procedure TraceLog(logLevel:longint; text:Pchar);cdecl;external cDllName;// Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
procedure SetTraceLogLevel(logLevel:longint);cdecl;external cDllName;// Set the current threshold (minimum) log level
function MemAlloc(size:longint):pointer;cdecl;external cDllName;// Internal memory allocator
function MemRealloc(ptr:pointer; size:longint):pointer;cdecl;external cDllName;// Internal memory reallocator
procedure MemFree(ptr:pointer);cdecl;external cDllName;// Internal memory free

(* Set custom callbacks *)
// WARNING: Callbacks setup is intended for advance users
procedure SetTraceLogCallback(callback:TTraceLogCallback);cdecl;external cDllName;// Set custom trace log
procedure SetLoadFileDataCallback(callback:TLoadFileDataCallback);cdecl;external cDllName;// Set custom file binary data loader
procedure SetSaveFileDataCallback(callback:TSaveFileDataCallback);cdecl;external cDllName;// Set custom file binary data saver
procedure SetLoadFileTextCallback(callback:TLoadFileTextCallback);cdecl;external cDllName;// Set custom file text data loader
procedure SetSaveFileTextCallback(callback:TSaveFileTextCallback);cdecl;external cDllName;// Set custom file text data saver

(* Files management functions *)
function LoadFileData(fileName:Pchar; bytesRead:Pdword):Pbyte;cdecl;external cDllName;// Load file data as byte array (read)
procedure UnloadFileData(data:Pbyte);cdecl;external cDllName;// Unload file data allocated by LoadFileData()
function SaveFileData(fileName:Pchar; data:pointer; bytesToWrite:dword):boolean;cdecl;external cDllName;// Save data to file from byte array (write), returns true on success
function LoadFileText(fileName:Pchar):Pchar;cdecl;external cDllName;// Load text data from file (read), returns a '\0' terminated string
procedure UnloadFileText(text:Pchar);cdecl;external cDllName;// Unload file text data allocated by LoadFileText()
function SaveFileText(fileName:Pchar; boolean:Pchar):boolean;cdecl;external cDllName;// Save text data to file (write), string must be '\0' terminated, returns true on success
function FileExists(fileName:Pchar):boolean;cdecl;external cDllName;// Check if file exists
function DirectoryExists(dirPath:Pchar):boolean;cdecl;external cDllName;// Check if a directory path exists
function IsFileExtension(fileName:Pchar; ext:Pchar):boolean;cdecl;external cDllName;// Check file extension (including point: .png, .wav)
function GetFileExtension(fileName:Pchar):Pchar;cdecl;external cDllName;// Get pointer to extension for a filename string (includes dot: '.png')
function GetFileName(filePath:Pchar):Pchar;cdecl;external cDllName;// Get pointer to filename for a path string
function GetFileNameWithoutExt(filePath:Pchar):Pchar;cdecl;external cDllName;// Get filename string without extension (uses static string)
function GetDirectoryPath(filePath:Pchar):Pchar;cdecl;external cDllName;// Get full path for a given fileName with path (uses static string)
function GetPrevDirectoryPath(dirPath:Pchar):Pchar;cdecl;external cDllName;// Get previous directory path for a given path (uses static string)
function GetWorkingDirectory:Pchar;cdecl;external cDllName;// Get current working directory (uses static string)
function GetApplicationDirectory:Pchar;cdecl;external cDllName;// Get the directory if the running application (uses static string)
function GetDirectoryFiles(dirPath:Pchar; count:Plongint):PPchar;cdecl;external cDllName;// Get filenames in a directory path (memory should be freed)
procedure ClearDirectoryFiles;cdecl;external cDllName;// Clear directory files paths buffers (free memory)
function ChangeDirectory(dir:Pchar):boolean;cdecl;external cDllName;// Change working directory, return true on success
function IsFileDropped:boolean;cdecl;external cDllName;// Check if a file has been dropped into window
function GetDroppedFiles(count:Plongint):PPchar;cdecl;external cDllName;// Get dropped files names (memory should be freed)
procedure ClearDroppedFiles;cdecl;external cDllName;// Clear dropped files paths buffer (free memory)
function GetFileModTime(fileName:Pchar):longint;cdecl;external cDllName;// Get file modification time (last write time)

(* Compression/Encoding functionality *)
function CompressData(data:Pbyte; dataLength:longint; compDataLength:Plongint):Pbyte;cdecl;external cDllName;// Compress data (DEFLATE algorithm)
function DecompressData(compData:Pbyte; compDataLength:longint; dataLength:Plongint):Pbyte;cdecl;external cDllName;// Decompress data (DEFLATE algorithm)
function EncodeDataBase64(data:Pchar; dataLength:longint; outputLength:Plongint):Pchar;cdecl;external cDllName;// Encode data to Base64 string
function DecodeDataBase64(data:Pchar; outputLength:Plongint):Pchar;cdecl;external cDllName;// Decode Base64 string data

(* Persistent storage management *)
function SaveStorageValue(position:dword; value:longint):boolean;cdecl;external cDllName;// Save integer value to storage file (to defined position), returns true on success
function LoadStorageValue(position:dword):longint;cdecl;external cDllName;// Load integer value from storage file (from defined position)
procedure OpenURL(url:Pchar);cdecl;external cDllName;// Open URL with default system browser (if available)


//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

(* Input-related functions: keyboard *)
function IsKeyPressed(key:longint):boolean;cdecl;external cDllName;// Check if a key has been pressed once
function IsKeyDown(key:longint):boolean;cdecl;external cDllName;// Check if a key is being pressed
function IsKeyReleased(key:longint):boolean;cdecl;external cDllName;// Check if a key has been released once
function IsKeyUp(key:longint):boolean;cdecl;external cDllName;// Check if a key is NOT being pressed
procedure SetExitKey(key:longint);cdecl;external cDllName;// Set a custom key to exit program (default is ESC)
function GetKeyPressed:longint;cdecl;external cDllName;// Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
function GetCharPressed:longint;cdecl;external cDllName;// Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty

(* Input-related functions: gamepads *)
function IsGamepadAvailable(gamepad:longint):boolean;cdecl;external cDllName;// Check if a gamepad is available
function GetGamepadName(gamepad:longint):Pchar;cdecl;external cDllName;// Get gamepad internal name id
function IsGamepadButtonPressed(gamepad:longint; button:longint):boolean;cdecl;external cDllName;// Check if a gamepad button has been pressed once
function IsGamepadButtonDown(gamepad:longint; button:longint):boolean;cdecl;external cDllName;// Check if a gamepad button is being pressed
function IsGamepadButtonReleased(gamepad:longint; button:longint):boolean;cdecl;external cDllName;// Check if a gamepad button has been released once
function IsGamepadButtonUp(gamepad:longint; button:longint):boolean;cdecl;external cDllName;// Check if a gamepad button is NOT being pressed
function GetGamepadButtonPressed:longint;cdecl;external cDllName;// Get the last gamepad button pressed
function GetGamepadAxisCount(gamepad:longint):longint;cdecl;external cDllName;// Get gamepad axis count for a gamepad
function GetGamepadAxisMovement(gamepad:longint; axis:longint):single;cdecl;external cDllName;// Get axis movement value for a gamepad axis
function SetGamepadMappings(mappings:Pchar):longint;cdecl;external cDllName;// Set internal gamepad mappings (SDL_GameControllerDB)

(* Input-related functions: mouse *)
function IsMouseButtonPressed(button:longint):boolean;cdecl;external cDllName;// Check if a mouse button has been pressed once
function IsMouseButtonDown(button:longint):boolean;cdecl;external cDllName;// Check if a mouse button is being pressed
function IsMouseButtonReleased(button:longint):boolean;cdecl;external cDllName;// Check if a mouse button has been released once
function IsMouseButtonUp(button:longint):boolean;cdecl;external cDllName;// Check if a mouse button is NOT being pressed
function GetMouseX:longint;cdecl;external cDllName;// Get mouse position X
function GetMouseY:longint;cdecl;external cDllName;// Get mouse position Y
function GetMousePosition:TVector2;cdecl;external cDllName;// Get mouse position XY
function GetMouseDelta:TVector2;cdecl;external cDllName;// Get mouse delta between frames
procedure SetMousePosition(x:longint; y:longint);cdecl;external cDllName;// Set mouse position XY
procedure SetMouseOffset(offsetX:longint; offsetY:longint);cdecl;external cDllName;// Set mouse offset
procedure SetMouseScale(scaleX:single; scaleY:single);cdecl;external cDllName;// Set mouse scaling
function GetMouseWheelMove:single;cdecl;external cDllName;// Get mouse wheel movement Y
procedure SetMouseCursor(cursor:longint);cdecl;external cDllName;//  Set mouse cursor

(* Input-related functions: touch *)
function GetTouchX:longint;cdecl;external cDllName;// Get touch position X for touch point 0 (relative to screen size)
function GetTouchY:longint;cdecl;external cDllName;// Get touch position Y for touch point 0 (relative to screen size)
function GetTouchPointId(index:longint):longint;cdecl;external cDllName;// Get touch point identifier for given index
function GetTouchPosition(index:longint):TVector2;cdecl;external cDllName;// Get touch position XY for a touch point index (relative to screen size)
function GetTouchPointCount:longint;cdecl;external cDllName;// Get touch points count
function GetTouchEvent:longint;cdecl;external cDllName;// Get last touch event registered

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------

procedure SetGesturesEnabled(flags:dword);cdecl;external cDllName;// Enable a set of gestures using flags
function IsGestureDetected(gesture:longint):boolean;cdecl;external cDllName;// Check if a gesture have been detected
function GetGestureDetected:longint;cdecl;external cDllName;// Get latest detected gesture
function GetGestureHoldDuration:single;cdecl;external cDllName;// Get gesture hold time in milliseconds
function GetGestureDragVector:TVector2;cdecl;external cDllName;// Get gesture drag vector
function GetGestureDragAngle:single;cdecl;external cDllName;// Get gesture drag angle
function GetGesturePinchVector:TVector2;cdecl;external cDllName;// Get gesture pinch delta
function GetGesturePinchAngle:single;cdecl;external cDllName;// Get gesture pinch angle

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------

procedure SetCameraMode(camera:TCamera; mode:longint);cdecl;external cDllName;// Set camera mode (multiple camera modes available)
procedure UpdateCamera(camera:PCamera);cdecl;external cDllName;// Update camera position for selected mode
procedure SetCameraPanControl(keyPan:longint);cdecl;external cDllName;// Set camera pan key to combine with mouse movement (free camera)
procedure SetCameraAltControl(keyAlt:longint);cdecl;external cDllName;// Set camera alt key to combine with mouse movement (free camera)
procedure SetCameraSmoothZoomControl(keySmoothZoom:longint);cdecl;external cDllName;// Set camera smooth zoom key to combine with mouse (free camera)
procedure SetCameraMoveControls(keyFront:longint; keyBack:longint; keyRight:longint; keyLeft:longint; keyUp:longint; keyDown:longint);cdecl;external cDllName;// Set camera move controls (1st person and 3rd person cameras)

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------
// Set texture and rectangle to be used on shapes drawing
// NOTE: It can be useful when using basic shapes and one single font,
// defining a font char white rectangle would allow drawing everything in a single draw call
procedure SetShapesTexture(texture:TTexture2D; source:TRectangle);cdecl;external cDllName;// Set texture and rectangle to be used on shapes drawing

(* Basic shapes drawing functions *)
procedure DrawPixel(posX:longint; posY:longint; color:TColor);cdecl;external cDllName;// Draw a pixel
procedure DrawPixelV(position:TVector2; color:TColor);cdecl;external cDllName;// Draw a pixel (Vector version)
procedure DrawLine(startPosX:longint; startPosY:longint; endPosX:longint; endPosY:longint; color:TColor);cdecl;external cDllName;// Draw a line
procedure DrawLineV(startPos:TVector2; endPos:TVector2; color:TColor);cdecl;external cDllName;// Draw a line (Vector version)
procedure DrawLineEx(startPos:TVector2; endPos:TVector2; thick:single; color:TColor);cdecl;external cDllName;// Draw a line defining thickness
procedure DrawLineBezier(startPos:TVector2; endPos:TVector2; thick:single; color:TColor);cdecl;external cDllName;// Draw a line using cubic-bezier curves in-out
procedure DrawLineBezierQuad(startPos:TVector2; endPos:TVector2; controlPos:TVector2; thick:single; color:TColor);cdecl;external cDllName;// Draw line using quadratic bezier curves with a control point
procedure DrawLineBezierCubic(startPos:TVector2; endPos:TVector2; startControlPos:TVector2; endControlPos:TVector2; thick:single; color:TColor);cdecl;external cDllName;// Draw line using cubic bezier curves with 2 control points
procedure DrawLineStrip(points:PVector2; pointCount:longint; color:TColor);cdecl;external cDllName;// Draw lines sequence
procedure DrawCircle(centerX:longint; centerY:longint; radius:single; color:TColor);cdecl;external cDllName;// Draw a color-filled circle
procedure DrawCircleSector(center:TVector2; radius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName;// Draw a piece of a circle
procedure DrawCircleSectorLines(center:TVector2; radius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName;// Draw circle sector outline
procedure DrawCircleGradient(centerX:longint; centerY:longint; radius:single; color1:TColor; color2:TColor);cdecl;external cDllName;// Draw a gradient-filled circle
procedure DrawCircleV(center:TVector2; radius:single; color:TColor);cdecl;external cDllName;// Draw a color-filled circle (Vector version)
procedure DrawCircleLines(centerX:longint; centerY:longint; radius:single; color:TColor);cdecl;external cDllName;// Draw circle outline
procedure DrawEllipse(centerX:longint; centerY:longint; radiusH:single; radiusV:single; color:TColor);cdecl;external cDllName;// Draw ellipse
procedure DrawEllipseLines(centerX:longint; centerY:longint; radiusH:single; radiusV:single; color:TColor);cdecl;external cDllName;// Draw ellipse outline
procedure DrawRing(center:TVector2; innerRadius:single; outerRadius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName;// Draw ring
procedure DrawRingLines(center:TVector2; innerRadius:single; outerRadius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName;// Draw ring outline
procedure DrawRectangle(posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName;// Draw a color-filled rectangle
procedure DrawRectangleV(position:TVector2; size:TVector2; color:TColor);cdecl;external cDllName;// Draw a color-filled rectangle (Vector version)
procedure DrawRectangleRec(rec:TRectangle; color:TColor);cdecl;external cDllName;// Draw a color-filled rectangle
procedure DrawRectanglePro(rec:TRectangle; origin:TVector2; rotation:single; color:TColor);cdecl;external cDllName;// Draw a color-filled rectangle with pro parameters
procedure DrawRectangleGradientV(posX:longint; posY:longint; width:longint; height:longint; color1:TColor; color2:TColor);cdecl;external cDllName;// Draw a vertical-gradient-filled rectangle
procedure DrawRectangleGradientH(posX:longint; posY:longint; width:longint; height:longint; color1:TColor; color2:TColor);cdecl;external cDllName;// Draw a horizontal-gradient-filled rectangle
procedure DrawRectangleGradientEx(rec:TRectangle; col1:TColor; col2:TColor; col3:TColor; col4:TColor);cdecl;external cDllName;// Draw a gradient-filled rectangle with custom vertex colors
procedure DrawRectangleLines(posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName;// Draw rectangle outline
procedure DrawRectangleLinesEx(rec:TRectangle; lineThick:single; color:TColor);cdecl;external cDllName;// Draw rectangle outline with extended parameters
procedure DrawRectangleRounded(rec:TRectangle; roundness:single; segments:longint; color:TColor);cdecl;external cDllName;// Draw rectangle with rounded edges
procedure DrawRectangleRoundedLines(rec:TRectangle; roundness:single; segments:longint; lineThick:single; color:TColor);cdecl;external cDllName;// Draw rectangle with rounded edges outline
procedure DrawTriangle(v1:TVector2; v2:TVector2; v3:TVector2; color:TColor);cdecl;external cDllName;// Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleLines(v1:TVector2; v2:TVector2; v3:TVector2; color:TColor);cdecl;external cDllName;// Draw triangle outline (vertex in counter-clockwise order!)
procedure DrawTriangleFan(points:PVector2; pointCount:longint; color:TColor);cdecl;external cDllName;// Draw a triangle fan defined by points (first vertex is the center)
procedure DrawTriangleStrip(points:PVector2; pointCount:longint; color:TColor);cdecl;external cDllName;// Draw a triangle strip defined by points
procedure DrawPoly(center:TVector2; sides:longint; radius:single; rotation:single; color:TColor);cdecl;external cDllName;// Draw a regular polygon (Vector version)
procedure DrawPolyLines(center:TVector2; sides:longint; radius:single; rotation:single; color:TColor);cdecl;external cDllName;// Draw a polygon outline of n sides
procedure DrawPolyLinesEx(center:TVector2; sides:longint; radius:single; rotation:single; lineThick:single; color:TColor);cdecl;external cDllName;// Draw a polygon outline of n sides with extended parameters

(* Basic shapes collision detection functions *)
function CheckCollisionRecs(rec1:TRectangle; rec2:TRectangle):boolean;cdecl;external cDllName;// Check collision between two rectangles
function CheckCollisionCircles(center1:TVector2; radius1:single; center2:TVector2; radius2:single):boolean;cdecl;external cDllName;// Check collision between two circles
function CheckCollisionCircleRec(center:TVector2; radius:single; rec:TRectangle):boolean;cdecl;external cDllName;// Check collision between circle and rectangle
function CheckCollisionPointRec(point:TVector2; rec:TRectangle):boolean;cdecl;external cDllName;// Check if point is inside rectangle
function CheckCollisionPointCircle(point:TVector2; center:TVector2; radius:single):boolean;cdecl;external cDllName;// Check if point is inside circle
function CheckCollisionPointTriangle(point:TVector2; p1:TVector2; p2:TVector2; p3:TVector2):boolean;cdecl;external cDllName;// Check if point is inside a triangle
function CheckCollisionLines(startPos1:TVector2; endPos1:TVector2; startPos2:TVector2; endPos2:TVector2; collisionPoint:PVector2):boolean;cdecl;external cDllName;// Check the collision between two lines defined by two points each, returns collision point by reference
function CheckCollisionPointLine(point:TVector2; p1:TVector2; p2:TVector2; threshold:longint):boolean;cdecl;external cDllName;// Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
function GetCollisionRec(rec1:TRectangle; rec2:TRectangle):TRectangle;cdecl;external cDllName;// Get collision rectangle for two rectangles collision

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

(* Image loading functions *)
// NOTE: This functions do not require GPU access
function LoadImage(fileName:Pchar):TImage;cdecl;external cDllName;// Load image from file into CPU memory (RAM)
function LoadImageRaw(fileName:Pchar; width:longint; height:longint; format:longint; headerSize:longint):TImage;cdecl;external cDllName;// Load image from RAW file data
function LoadImageAnim(fileName:Pchar; frames:Plongint):TImage;cdecl;external cDllName;// Load image sequence from file (frames appended to image.data)
function LoadImageFromMemory(fileType:Pchar; fileData:Pbyte; dataSize:longint):TImage;cdecl;external cDllName;// Load image from memory buffer, fileType refers to extension: i.e. '.png'
function LoadImageFromTexture(texture:TTexture2D):TImage;cdecl;external cDllName;// Load image from GPU texture data
function LoadImageFromScreen:TImage;cdecl;external cDllName;// Load image from screen buffer and (screenshot)
procedure UnloadImage(image:TImage);cdecl;external cDllName;// Unload image from CPU memory (RAM)
function ExportImage(image:TImage; fileName:Pchar):boolean;cdecl;external cDllName;// Export image data to file, returns true on success
function ExportImageAsCode(image:TImage; fileName:Pchar):boolean;cdecl;external cDllName;// Export image as code file defining an array of bytes, returns true on success

(* Image generation functions *)
function GenImageColor(width:longint; height:longint; color:TColor):TImage;cdecl;external cDllName;// Generate image: plain color
function GenImageGradientV(width:longint; height:longint; top:TColor; bottom:TColor):TImage;cdecl;external cDllName;// Generate image: vertical gradient
function GenImageGradientH(width:longint; height:longint; left:TColor; right:TColor):TImage;cdecl;external cDllName;// Generate image: horizontal gradient
function GenImageGradientRadial(width:longint; height:longint; density:single; inner:TColor; outer:TColor):TImage;cdecl;external cDllName;// Generate image: radial gradient
function GenImageChecked(width:longint; height:longint; checksX:longint; checksY:longint; col1:TColor; col2:TColor):TImage;cdecl;external cDllName;// Generate image: checked
function GenImageWhiteNoise(width:longint; height:longint; factor:single):TImage;cdecl;external cDllName;// Generate image: white noise
function GenImageCellular(width:longint; height:longint; tileSize:longint):TImage;cdecl;external cDllName;// Generate image: cellular algorithm, bigger tileSize means bigger cells

(* Image manipulation functions *)
function ImageCopy(image:TImage):TImage;cdecl;external cDllName;// Create an image duplicate (useful for transformations)
function ImageFromImage(image:TImage; rec:TRectangle):TImage;cdecl;external cDllName;// Create an image from another image piece
function ImageText(text:Pchar; fontSize:longint; color:TColor):TImage;cdecl;external cDllName;// Create an image from text (default font)
function ImageTextEx(font:TFont; text:Pchar; fontSize:single; spacing:single; tint:TColor):TImage;cdecl;external cDllName;// Create an image from text (custom sprite font)
procedure ImageFormat(image:PImage; newFormat:longint);cdecl;external cDllName;// Convert image data to desired format
procedure ImageToPOT(image:PImage; fill:TColor);cdecl;external cDllName;// Convert image to POT (power-of-two)
procedure ImageCrop(image:PImage; crop:TRectangle);cdecl;external cDllName;// Crop an image to a defined rectangle
procedure ImageAlphaCrop(image:PImage; threshold:single);cdecl;external cDllName;// Crop image depending on alpha value
procedure ImageAlphaClear(image:PImage; color:TColor; threshold:single);cdecl;external cDllName;// Clear alpha channel to desired color
procedure ImageAlphaMask(image:PImage; alphaMask:TImage);cdecl;external cDllName;// Apply alpha mask to image
procedure ImageAlphaPremultiply(image:PImage);cdecl;external cDllName;// Premultiply alpha channel
procedure ImageResize(image:PImage; newWidth:longint; newHeight:longint);cdecl;external cDllName;// Resize image (Bicubic scaling algorithm)
procedure ImageResizeNN(image:PImage; newWidth:longint; newHeight:longint);cdecl;external cDllName;// Resize image (Nearest-Neighbor scaling algorithm)
procedure ImageResizeCanvas(image:PImage; newWidth:longint; newHeight:longint; offsetX:longint; offsetY:longint; fill:TColor);cdecl;external cDllName;// Resize canvas and fill with color
procedure ImageMipmaps(image:PImage);cdecl;external cDllName;// Compute all mipmap levels for a provided image
procedure ImageDither(image:PImage; rBpp:longint; gBpp:longint; bBpp:longint; aBpp:longint);cdecl;external cDllName;// Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
procedure ImageFlipVertical(image:PImage);cdecl;external cDllName;// Flip image vertically
procedure ImageFlipHorizontal(image:PImage);cdecl;external cDllName;// Flip image horizontally
procedure ImageRotateCW(image:PImage);cdecl;external cDllName;// Rotate image clockwise 90deg
procedure ImageRotateCCW(image:PImage);cdecl;external cDllName;// Rotate image counter-clockwise 90deg
procedure ImageColorTint(image:PImage; color:TColor);cdecl;external cDllName;// Modify image color: tint
procedure ImageColorInvert(image:PImage);cdecl;external cDllName;// Modify image color: invert
procedure ImageColorGrayscale(image:PImage);cdecl;external cDllName;// Modify image color: grayscale
procedure ImageColorContrast(image:PImage; contrast:single);cdecl;external cDllName;// Modify image color: contrast (-100 to 100)
procedure ImageColorBrightness(image:PImage; brightness:longint);cdecl;external cDllName;// Modify image color: brightness (-255 to 255)
procedure ImageColorReplace(image:PImage; color:TColor; replace:TColor);cdecl;external cDllName;// Modify image color: replace color
function LoadImageColors(image:TImage):PColor;cdecl;external cDllName;// Load color data from image as a Color array (RGBA - 32bit)
function LoadImagePalette(image:TImage; maxPaletteSize:longint; colorCount:Plongint):PColor;cdecl;external cDllName;// Load colors palette from image as a Color array (RGBA - 32bit)
procedure UnloadImageColors(colors:PColor);cdecl;external cDllName;// Unload color data loaded with LoadImageColors()
procedure UnloadImagePalette(colors:PColor);cdecl;external cDllName;// Unload colors palette loaded with LoadImagePalette()
function GetImageAlphaBorder(image:TImage; threshold:single):TRectangle;cdecl;external cDllName;// Get image alpha border rectangle
function GetImageColor(image:TImage; x:longint; y:longint):TColor;cdecl;external cDllName;// Get image pixel color at (x, y) position

(* Image drawing functions *)
// NOTE: Image software-rendering functions (CPU)
procedure ImageClearBackground(dst:PImage; color:TColor);cdecl;external cDllName;// Clear image background with given color
procedure ImageDrawPixel(dst:PImage; posX:longint; posY:longint; color:TColor);cdecl;external cDllName;// Draw pixel within an image
procedure ImageDrawPixelV(dst:PImage; position:TVector2; color:TColor);cdecl;external cDllName;// Draw pixel within an image (Vector version)
procedure ImageDrawLine(dst:PImage; startPosX:longint; startPosY:longint; endPosX:longint; endPosY:longint; color:TColor);cdecl;external cDllName;// Draw line within an image
procedure ImageDrawLineV(dst:PImage; start:TVector2; end_:TVector2; color:TColor);cdecl;external cDllName;// Draw line within an image (Vector version)
procedure ImageDrawCircle(dst:PImage; centerX:longint; centerY:longint; radius:longint; color:TColor);cdecl;external cDllName;// Draw circle within an image
procedure ImageDrawCircleV(dst:PImage; center:TVector2; radius:longint; color:TColor);cdecl;external cDllName;// Draw circle within an image (Vector version)
procedure ImageDrawRectangle(dst:PImage; posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName;// Draw rectangle within an image
procedure ImageDrawRectangleV(dst:PImage; position:TVector2; size:TVector2; color:TColor);cdecl;external cDllName;// Draw rectangle within an image (Vector version)
procedure ImageDrawRectangleRec(dst:PImage; rec:TRectangle; color:TColor);cdecl;external cDllName;// Draw rectangle within an image
procedure ImageDrawRectangleLines(dst:PImage; rec:TRectangle; thick:longint; color:TColor);cdecl;external cDllName;// Draw rectangle lines within an image
procedure ImageDraw(dst:PImage; src:TImage; srcRec:TRectangle; dstRec:TRectangle; tint:TColor);cdecl;external cDllName;// Draw a source image within a destination image (tint applied to source)
procedure ImageDrawText(dst:PImage; text:Pchar; posX:longint; posY:longint; fontSize:longint; color:TColor);cdecl;external cDllName;// Draw text (using default font) within an image (destination)
procedure ImageDrawTextEx(dst:PImage; font:TFont; text:Pchar; position:TVector2; fontSize:single; spacing:single; tint:TColor);cdecl;external cDllName;// Draw text (custom sprite font) within an image (destination)

(* Texture loading functions *)
// NOTE: These functions require GPU access
function LoadTexture(fileName:Pchar):TTexture2D;cdecl;external cDllName;// Load texture from file into GPU memory (VRAM)
function LoadTextureFromImage(image:TImage):TTexture2D;cdecl;external cDllName;// Load texture from image data
function LoadTextureCubemap(image:TImage; layout:longint):TTextureCubemap;cdecl;external cDllName;// Load cubemap from image, multiple image cubemap layouts supported
function LoadRenderTexture(width:longint; height:longint):TRenderTexture2D;cdecl;external cDllName;// Load texture for rendering (framebuffer)
procedure UnloadTexture(texture:TTexture2D);cdecl;external cDllName;// Unload texture from GPU memory (VRAM)
procedure UnloadRenderTexture(target:TRenderTexture2D);cdecl;external cDllName;// Unload render texture from GPU memory (VRAM)
procedure UpdateTexture(texture:TTexture2D; pixels:pointer);cdecl;external cDllName;// Update GPU texture with new data
procedure UpdateTextureRec(texture:TTexture2D; rec:TRectangle; pixels:pointer);cdecl;external cDllName;// Update GPU texture rectangle with new data

(* Texture configuration functions *)
procedure GenTextureMipmaps(texture:PTexture2D);cdecl;external cDllName;// Generate GPU mipmaps for a texture
procedure SetTextureFilter(texture:TTexture2D; filter:longint);cdecl;external cDllName;// Set texture scaling filter mode
procedure SetTextureWrap(texture:TTexture2D; wrap:longint);cdecl;external cDllName;// Set texture wrapping mode

(* Texture drawing functions *)
procedure DrawTexture(texture:TTexture2D; posX:longint; posY:longint; tint:TColor);cdecl;external cDllName;// Draw a Texture2D
procedure DrawTextureV(texture:TTexture2D; position:TVector2; tint:TColor);cdecl;external cDllName;// Draw a Texture2D with position defined as Vector2
procedure DrawTextureEx(texture:TTexture2D; position:TVector2; rotation:single; scale:single; tint:TColor);cdecl;external cDllName;// Draw a Texture2D with extended parameters
procedure DrawTextureRec(texture:TTexture2D; source:TRectangle; position:TVector2; tint:TColor);cdecl;external cDllName;// Draw a part of a texture defined by a rectangle
procedure DrawTextureQuad(texture:TTexture2D; tiling:TVector2; offset:TVector2; quad:TRectangle; tint:TColor);cdecl;external cDllName;// Draw texture quad with tiling and offset parameters
procedure DrawTextureTiled(texture:TTexture2D; source:TRectangle; dest:TRectangle; origin:TVector2; rotation:single; scale:single; tint:TColor);cdecl;external cDllName;// Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
procedure DrawTexturePro(texture:TTexture2D; source:TRectangle; dest:TRectangle; origin:TVector2; rotation:single; tint:TColor);cdecl;external cDllName;// Draw a part of a texture defined by a rectangle with 'pro' parameters
procedure DrawTextureNPatch(texture:TTexture2D; nPatchInfo:TNPatchInfo; dest:TRectangle; origin:TVector2; rotation:single; tint:TColor);cdecl;external cDllName;// Draws a texture (or part of it) that stretches or shrinks nicely
procedure DrawTexturePoly(texture:TTexture2D; center:TVector2; points:PVector2; texcoords:PVector2; pointCount:longint; tint:TColor);cdecl;external cDllName;// Draw a textured polygon

(* Color/pixel related functions *)
function Fade(color:TColor; alpha:single):TColor;cdecl;external cDllName;// Get color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorToInt(color:TColor):longint;cdecl;external cDllName;// Get hexadecimal value for a Color
function ColorNormalize(color:TColor):TVector4;cdecl;external cDllName;// Get Color normalized as float [0..1]
function ColorFromNormalized(normalized:TVector4):TColor;cdecl;external cDllName;// Get Color from normalized values [0..1]
function ColorToHSV(color:TColor):TVector3;cdecl;external cDllName;// Get HSV values for a Color, hue [0..360], saturation/value [0..1]
function ColorFromHSV(hue:single; saturation:single; value:single):TColor;cdecl;external cDllName;// Get a Color from HSV values, hue [0..360], saturation/value [0..1]
function ColorAlpha(color:TColor; alpha:single):TColor;cdecl;external cDllName;// Get color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorAlphaBlend(dst:TColor; src:TColor; tint:TColor):TColor;cdecl;external cDllName;// Get src alpha-blended into dst color with tint
function GetColor(hexValue:dword):TColor;cdecl;external cDllName;// Get Color structure from hexadecimal value
function GetPixelColor(srcPtr:pointer; format:longint):TColor;cdecl;external cDllName;// Get Color from a source pixel pointer of certain format
procedure SetPixelColor(dstPtr:pointer; color:TColor; format:longint);cdecl;external cDllName;// Set color formatted into destination pixel pointer
function GetPixelDataSize(width:longint; height:longint; format:longint):longint;cdecl;external cDllName;// Get pixel data size in bytes for certain format

//------------------------------------------------------------------------------------
// TFont Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

(* Font loading/unloading functions *)
function GetFontDefault:TFont;cdecl;external cDllName;// Get the default Font
function LoadFont(fileName:Pchar):TFont;cdecl;external cDllName;// Load font from file into GPU memory (VRAM)
function LoadFontEx(fileName:Pchar; fontSize:longint; fontChars:Plongint; glyphCount:longint):TFont;cdecl;external cDllName;// Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
function LoadFontFromImage(image:TImage; key:TColor; firstChar:longint):TFont;cdecl;external cDllName;// Load font from Image (XNA style)
function LoadFontFromMemory(fileType:Pchar; fileData:Pbyte; dataSize:longint; fontSize:longint; fontChars:Plongint; glyphCount:longint):TFont;cdecl;external cDllName;// Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
function LoadFontData(fileData:Pbyte; dataSize:longint; fontSize:longint; fontChars:Plongint; glyphCount:longint; _type:longint):PGlyphInfo;cdecl;external cDllName;// Load font data for further use
function GenImageFontAtlas(chars:PGlyphInfo; recs:PPRectangle; glyphCount:longint; fontSize:longint; padding:longint; packMethod:longint):TImage;cdecl;external cDllName;// Generate image font atlas using chars info
procedure UnloadFontData(chars:PGlyphInfo; glyphCount:longint);cdecl;external cDllName;// Unload font chars info data (RAM)
procedure UnloadFont(font:TFont);cdecl;external cDllName;// Unload Font from GPU memory (VRAM)
procedure ExportFontAsCode(font: TFont; fileName:Pchar);cdecl;external cDllName;// Export font as code file, returns true on success

(* Text drawing functions *)
procedure DrawFPS(posX:longint; posY:longint);cdecl;external cDllName;// Draw current FPS
procedure DrawText(text:Pchar; posX:longint; posY:longint; fontSize:longint; color:TColor);cdecl;external cDllName;// Draw text (using default font)
procedure DrawTextEx(font:TFont; text:Pchar; position:TVector2; fontSize:single; spacing:single; tint:TColor);cdecl;external cDllName;// Draw text using font and additional parameters
procedure DrawTextPro(font:TFont; text:Pchar; position:TVector2; origin:TVector2; rotation:single; fontSize:single; spacing:single; tint:TColor);cdecl;external cDllName;// Draw text using Font and pro parameters (rotation)
procedure DrawTextCodepoint(font:TFont; codepoint:longint; position:TVector2; fontSize:single; tint:TColor);cdecl;external cDllName;// Draw one character (codepoint)

(* Text font info functions *)
function MeasureText(text:Pchar; fontSize:longint):longint;cdecl;external cDllName;// Measure string width for default font
function MeasureTextEx(font:TFont; text:Pchar; fontSize:single; spacing:single):TVector2;cdecl;external cDllName;// Measure string size for Font
function GetGlyphIndex(font:TFont; codepoint:longint):longint;cdecl;external cDllName;// Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
function GetGlyphInfo(font:TFont; codepoint:longint):TGlyphInfo;cdecl;external cDllName;// Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
function GetGlyphAtlasRec(font:TFont; codepoint:longint):TRectangle;cdecl;external cDllName;// Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found

(* Text codepoints management functions (unicode characters) *)
function LoadCodepoints(text:Pchar; count:Plongint):Plongint;cdecl;external cDllName;// Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
procedure UnloadCodepoints(codepoints:Plongint);cdecl;external cDllName;// Unload codepoints data from memory
function GetCodepointCount(text:Pchar):longint;cdecl;external cDllName;// Get total number of codepoints in a UTF-8 encoded string
function GetCodepoint(text:Pchar; bytesProcessed:Plongint):longint;cdecl;external cDllName;// Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
function CodepointToUTF8(codepoint:longint; byteSize:Plongint):Pchar;cdecl;external cDllName;// Encode one codepoint into UTF-8 byte array (array length returned as parameter)
function TextCodepointsToUTF8(codepoints:Plongint; length:longint):Pchar;cdecl;external cDllName;// Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)

(* Text strings management functions (no UTF-8 strings, only byte chars) *)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
function TextCopy(dst:Pchar; src:Pchar):longint;cdecl;external cDllName;// Copy one string to another, returns bytes copied
function TextIsEqual(text1:Pchar; text2:Pchar):boolean;cdecl;external cDllName;// Check if two text string are equal
function TextLength(text:Pchar):dword;cdecl;external cDllName;// Get text length, checks for '\0' ending
function TextFormat(text:PChar; args:array of const):PChar;cdecl;external cDllName; // Text formatting with variables (sprintf() style)
function TextSubtext(text:Pchar; position:longint; length:longint):Pchar;cdecl;external cDllName;// Get a piece of a text string
function TextReplace(text:Pchar; replace:Pchar; by:Pchar):Pchar;cdecl;external cDllName;// Replace text string (WARNING: memory must be freed!)
function TextInsert(text:Pchar; insert:Pchar; position:longint):Pchar;cdecl;external cDllName;// Insert text in a position (WARNING: memory must be freed!)
function TextJoin(textList:PPchar; count:longint; delimiter:Pchar):Pchar;cdecl;external cDllName;// Join text strings with delimiter
function TextSplit(text:Pchar; delimiter:char; count:Plongint):PPchar;cdecl;external cDllName;// Split text into multiple strings
procedure TextAppend(text:Pchar; append:Pchar; position:Plongint);cdecl;external cDllName;// Append text at specific position and move cursor!
function TextFindIndex(text:Pchar; find:Pchar):longint;cdecl;external cDllName;// Find first text occurrence within a string
function TextToUpper(text:Pchar):Pchar;cdecl;external cDllName;// Get upper case version of provided string
function TextToLower(text:Pchar):Pchar;cdecl;external cDllName;// Get lower case version of provided string
function TextToPascal(text:Pchar):Pchar;cdecl;external cDllName;// Get Pascal case notation version of provided string
function TextToInteger(text:Pchar):longint;cdecl;external cDllName;// Get integer value from text (negative values not supported)

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Basic geometric 3D shapes drawing functions *)
procedure DrawLine3D(startPos:TVector3; endPos:TVector3; color:TColor);cdecl;external cDllName;// Draw a line in 3D world space
procedure DrawPoint3D(position:TVector3; color:TColor);cdecl;external cDllName;// Draw a point in 3D space, actually a small line
procedure DrawCircle3D(center:TVector3; radius:single; rotationAxis:TVector3; rotationAngle:single; color:TColor);cdecl;external cDllName;// Draw a circle in 3D world space
procedure DrawTriangle3D(v1:TVector3; v2:TVector3; v3:TVector3; color:TColor);cdecl;external cDllName;// Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleStrip3D(points:PVector3; pointCount:longint; color:TColor);cdecl;external cDllName;// Draw a triangle strip defined by points
procedure DrawCube(position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName;// Draw cube
procedure DrawCubeV(position:TVector3; size:TVector3; color:TColor);cdecl;external cDllName;// Draw cube (Vector version)
procedure DrawCubeWires(position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName;// Draw cube wires
procedure DrawCubeWiresV(position:TVector3; size:TVector3; color:TColor);cdecl;external cDllName;// Draw cube wires (Vector version)
procedure DrawCubeTexture(texture:TTexture2D; position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName;// Draw cube textured
procedure DrawCubeTextureRec(texture:TTexture2D; source:TRectangle; position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName;// Draw cube with a region of a texture
procedure DrawSphere(centerPos:TVector3; radius:single; color:TColor);cdecl;external cDllName;// Draw sphere
procedure DrawSphereEx(centerPos:TVector3; radius:single; rings:longint; slices:longint; color:TColor);cdecl;external cDllName;// Draw sphere with extended parameters }
procedure DrawSphereWires(centerPos:TVector3; radius:single; rings:longint; slices:longint; color:TColor);cdecl;external cDllName;// Draw sphere wires
procedure DrawCylinder(position:TVector3; radiusTop:single; radiusBottom:single; height:single; slices:longint; color:TColor);cdecl;external cDllName;// Draw a cylinder/cone
procedure DrawCylinderEx(startPos:TVector3; endPos:TVector3; startRadius:single; endRadius:single; sides:longint; color:TColor);cdecl;external cDllName;// Draw a cylinder with base at startPos and top at endPos
procedure DrawCylinderWires(position:TVector3; radiusTop:single; radiusBottom:single; height:single; slices:longint; color:TColor);cdecl;external cDllName;// Draw a cylinder/cone wires
procedure DrawCylinderWiresEx(startPos: TVector3; endPos: TVector3; startRadius:single; endRadius: single; sides:longint; color:TColor);cdecl;external cDllName;// Draw a cylinder wires with base at startPos and top at endPos
procedure DrawPlane(centerPos:TVector3; size:TVector2; color:TColor);cdecl;external cDllName;// Draw a plane XZ
procedure DrawRay(ray:TRay; color:TColor);cdecl;external cDllName;// Draw a ray line
procedure DrawGrid(slices:longint; spacing:single);cdecl;external cDllName;// Draw a grid (centered at (0, 0, 0))

//------------------------------------------------------------------------------------
// TModel 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Model management functions *)
function LoadModel(fileName:Pchar):TModel;cdecl;external cDllName;// Load model from files (meshes and materials)
function LoadModelFromMesh(mesh:TMesh):TModel;cdecl;external cDllName;// Load model from generated mesh (default material)
procedure UnloadModel(model:TModel);cdecl;external cDllName;// Unload model (including meshes) from memory (RAM and/or VRAM)
procedure UnloadModelKeepMeshes(model:TModel);cdecl;external cDllName;// Unload model (but not meshes) from memory (RAM and/or VRAM)
function GetModelBoundingBox(model:TModel):TBoundingBox;cdecl;external cDllName;// Compute model bounding box limits (considers all meshes)

(* Model drawing functions *)
procedure DrawModel(model:TModel; position:TVector3; scale:single; tint:TColor);cdecl;external cDllName;// Draw a model (with texture if set)
procedure DrawModelEx(model:TModel; position:TVector3; rotationAxis:TVector3; rotationAngle:single; scale:TVector3; tint:TColor);cdecl;external cDllName;// Draw a model with extended parameters
procedure DrawModelWires(model:TModel; position:TVector3; scale:single; tint:TColor);cdecl;external cDllName;// Draw a model wires (with texture if set)
procedure DrawModelWiresEx(model:TModel; position:TVector3; rotationAxis:TVector3; rotationAngle:single; scale:TVector3; tint:TColor);cdecl;external cDllName;// Draw a model wires (with texture if set) with extended parameters
procedure DrawBoundingBox(box:TBoundingBox; color:TColor);cdecl;external cDllName;// Draw bounding box (wires)
procedure DrawBillboard(camera:TCamera; texture:TTexture2D; position:TVector3; size:single; tint:TColor);cdecl;external cDllName;// Draw a billboard texture
procedure DrawBillboardRec(camera:TCamera; texture:TTexture2D; source:TRectangle; position:TVector3; size:TVector2; tint:TColor);cdecl;external cDllName;// Draw a billboard texture defined by source
procedure DrawBillboardPro(camera:TCamera; texture:TTexture2D; source:TRectangle; position:TVector3; up:TVector3; size:TVector2; origin:TVector2; rotation:single; tint:TColor);cdecl;external cDllName;// Draw a billboard texture defined by source and rotation
(* Mesh management functions *)
procedure UploadMesh(mesh:PMesh; dynamic:boolean);cdecl;external cDllName;// Upload mesh vertex data in GPU and provide VAO/VBO ids
procedure UpdateMeshBuffer(mesh:TMesh; index:longint; data:pointer; dataSize:longint; offset:longint);cdecl;external cDllName;// Update mesh vertex data in GPU for a specific buffer index
procedure UnloadMesh(mesh:TMesh);cdecl;external cDllName;// Unload mesh data from CPU and GPU
procedure DrawMesh(mesh:TMesh; material:TMaterial; transform:TMatrix);cdecl;external cDllName;// Draw a 3d mesh with material and transform
procedure DrawMeshInstanced(mesh:TMesh; material:TMaterial; transforms:PMatrix; instances:longint);cdecl;external cDllName;// Draw multiple mesh instances with material and different transforms
function ExportMesh(mesh:TMesh; fileName:Pchar):boolean;cdecl;external cDllName;// Export mesh data to file, returns true on success
function GetMeshBoundingBox(mesh:TMesh):TBoundingBox;cdecl;external cDllName;// Compute mesh bounding box limits
procedure GenMeshTangents(mesh:PMesh);cdecl;external cDllName;// Compute mesh tangents
procedure GenMeshBinormals(mesh:PMesh);cdecl;external cDllName;// Compute mesh binormals

(* Mesh generation functions *)
function GenMeshPoly(sides:longint; radius:single):TMesh;cdecl;external cDllName;// Generate polygonal mesh
function GenMeshPlane(width:single; length:single; resX:longint; resZ:longint):TMesh;cdecl;external cDllName;// Generate plane mesh (with subdivisions)
function GenMeshCube(width:single; height:single; length:single):TMesh;cdecl;external cDllName;// Generate cuboid mesh
function GenMeshSphere(radius:single; rings:longint; slices:longint):TMesh;cdecl;external cDllName;// Generate sphere mesh (standard sphere)
function GenMeshHemiSphere(radius:single; rings:longint; slices:longint):TMesh;cdecl;external cDllName;// Generate half-sphere mesh (no bottom cap)
function GenMeshCylinder(radius:single; height:single; slices:longint):TMesh;cdecl;external cDllName;// Generate cylinder mesh
function GenMeshCone(radius:single; height:single; slices:longint):TMesh;cdecl;external cDllName;// Generate cone/pyramid mesh
function GenMeshTorus(radius:single; size:single; radSeg:longint; sides:longint):TMesh;cdecl;external cDllName;// Generate torus mesh
function GenMeshKnot(radius:single; size:single; radSeg:longint; sides:longint):TMesh;cdecl;external cDllName;// Generate trefoil knot mesh
function GenMeshHeightmap(heightmap:TImage; size:TVector3):TMesh;cdecl;external cDllName;// Generate heightmap mesh from image data
function GenMeshCubicmap(cubicmap:TImage; cubeSize:TVector3):TMesh;cdecl;external cDllName;// Generate cubes-based map mesh from image data

(* Material loading/unloading functions *)
function LoadMaterials(fileName:Pchar; materialCount:Plongint):PMaterial;cdecl;external cDllName;// Load materials from model file
function LoadMaterialDefault:TMaterial;cdecl;external cDllName;// Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
procedure UnloadMaterial(material:TMaterial);cdecl;external cDllName;// Unload material from GPU memory (VRAM)
procedure SetMaterialTexture(material:PMaterial; mapType:longint; texture:TTexture2D);cdecl;external cDllName;// Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...) }
procedure SetModelMeshMaterial(model:PModel; meshId:longint; materialId:longint);cdecl;external cDllName;// Set material for a mesh

(* Model animations loading/unloading functions *)
function LoadModelAnimations(fileName:Pchar; animCount:Pdword):PModelAnimation;cdecl;external cDllName;// Load model animations from file
procedure UpdateModelAnimation(model:TModel; anim:TModelAnimation; frame:longint);cdecl;external cDllName;// Update model animation pose
procedure UnloadModelAnimation(anim:TModelAnimation);cdecl;external cDllName;// Unload animation data
procedure UnloadModelAnimations(animations:PModelAnimation; count:dword);cdecl;external cDllName;// Unload animation array data
function IsModelAnimationValid(model:TModel; anim:TModelAnimation):boolean;cdecl;external cDllName;// Check model animation skeleton match

(* Collision detection functions *)
function CheckCollisionSpheres(center1:TVector3; radius1:single; center2:TVector3; radius2:single):boolean;cdecl;external cDllName;// Check collision between two spheres
function CheckCollisionBoxes(box1:TBoundingBox; box2:TBoundingBox):boolean;cdecl;external cDllName;// Check collision between two bounding boxes
function CheckCollisionBoxSphere(box:TBoundingBox; center:TVector3; radius:single):boolean;cdecl;external cDllName;// Check collision between box and sphere
function GetRayCollisionSphere(ray:TRay; center:TVector3; radius:single):TRayCollision;cdecl;external cDllName;// Get collision info between ray and sphere
function GetRayCollisionBox(ray:TRay; box:TBoundingBox):TRayCollision;cdecl;external cdllName;// Get collision info between ray and box
function GetRayCollisionModel(ray:TRay; model:TModel):TRayCollision;cdecl;external cdllName;// Get collision info between ray and model
function GetRayCollisionMesh(ray:TRay; mesh:TMesh; transform:TMatrix):TRayCollision;cdecl;external cdllName;// Get collision info between ray and mesh
function GetRayCollisionTriangle(ray:TRay; p1:TVector3; p2:TVector3; p3:TVector3):TRayCollision;cdecl;external cDllName;// Get collision info between ray and triangle
function GetRayCollisionQuad(ray:TRay; p1:TVector3; p2:TVector3; p3:TVector3; p4:TVector3):TRayCollision;cdecl;external cDllName;// Get collision info between ray and quad

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

(* Audio device management functions *)
procedure InitAudioDevice;cdecl;external cDllName;// Initialize audio device and context
procedure CloseAudioDevice;cdecl;external cDllName;// Close the audio device and context
function IsAudioDeviceReady:boolean;cdecl;external cDllName;// Check if audio device has been initialized successfully
procedure SetMasterVolume(volume:single);cdecl;external cDllName;// Set master volume (listener)

(* Wave/Sound loading/unloading functions *)
function LoadWave(fileName:Pchar):TWave;cdecl;external cDllName;// Load wave data from file
function LoadWaveFromMemory(fileType:Pchar; fileData:Pbyte; dataSize:longint):TWave;cdecl;external cdllName;// Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
function LoadSound(fileName:Pchar):TSound;cdecl;external cDllName;// Load sound from file
function LoadSoundFromWave(wave:TWave):TSound;cdecl;external cDllName;// Load sound from wave data
procedure UpdateSound(sound:TSound; data:pointer; sampleCount:longint);cdecl;external cdllName;// Update sound buffer with new data
procedure UnloadWave(wave:TWave);cdecl;external cDllName;// Unload wave data
procedure UnloadSound(sound:TSound);cdecl;external cDllName;// Unload sound
function ExportWave(wave:TWave; fileName:Pchar):boolean;cdecl;external cDllName;// Export wave data to file, returns true on success
function ExportWaveAsCode(wave:TWave; fileName:Pchar):boolean;cdecl;external cDllName;// Export wave sample data to code (.h), returns true on success

(* Wave/Sound management functions *)
procedure PlaySound(sound:TSound);cdecl;external cDllName;// Play a sound
procedure StopSound(sound:TSound);cdecl;external cDllName;// Stop playing a sound
procedure PauseSound(sound:TSound);cdecl;external cDllName;// Pause a sound
procedure ResumeSound(sound:TSound);cdecl;external cDllName;// Resume a paused sound
procedure PlaySoundMulti(sound:TSound);cdecl;external cDllName;// Play a sound (using multichannel buffer pool)
procedure StopSoundMulti;cdecl;external cDllName;// Stop any sound playing (using multichannel buffer pool)
function GetSoundsPlaying:longint;cdecl;external cDllName;// Get number of sounds playing in the multichannel
function IsSoundPlaying(sound:TSound):boolean;cdecl;external cDllName;// Check if a sound is currently playing
procedure SetSoundVolume(sound:TSound; volume:single);cdecl;external cDllName;// Set volume for a sound (1.0 is max level)
procedure SetSoundPitch(sound:TSound; pitch:single);cdecl;external cDllName;// Set pitch for a sound (1.0 is base level)
procedure WaveFormat(wave:PWave; sampleRate:longint; sampleSize:longint; channels:longint);cdecl;external cDllName;// Convert wave data to desired format
function WaveCopy(wave:TWave):TWave;cdecl;external cDllName;// Copy a wave to a new wave
procedure WaveCrop(wave:PWave; initSample:longint; finalSample:longint);cdecl;external cDllName; // Crop a wave to defined samples range
function LoadWaveSamples(wave:TWave):Psingle;cdecl;external cDllName;// Load samples data from wave as a floats array
procedure UnloadWaveSamples(samples:Psingle);cdecl;external cDllName;// Unload samples data loaded with LoadWaveSamples()

(* Music management functions *)
function LoadMusicStream(fileName:Pchar):TMusic;cdecl;external cDllName;// Load music stream from file
function LoadMusicStreamFromMemory(fileType:Pchar; data:Pbyte; dataSize:longint):TMusic;cdecl;external cDllName;// Load music stream from data
procedure UnloadMusicStream(music:TMusic);cdecl;external cDllName;// Unload music stream
procedure PlayMusicStream(music:TMusic);cdecl;external cDllName;// Start music playing
function IsMusicStreamPlaying(music:TMusic):boolean;cdecl;external cDllName;// Check if music is playing
procedure UpdateMusicStream(music:TMusic);cdecl;external cDllName;// Updates buffers for music streaming
procedure StopMusicStream(music:TMusic);cdecl;external cDllName;// Stop music playing
procedure PauseMusicStream(music:TMusic);cdecl;external cDllName;// Pause music playing
procedure ResumeMusicStream(music:TMusic);cdecl;external cDllName;// Resume playing paused music
procedure SeekMusicStream(music:TMusic; position:single);cdecl;external cDllName;// Seek music to a position (in seconds)
procedure SetMusicVolume(music:TMusic; volume:single);cdecl;external cDllName;// Set volume for music (1.0 is max level)
procedure SetMusicPitch(music:TMusic; pitch:single);cdecl;external cDllName;// Set pitch for a music (1.0 is base level)
function GetMusicTimeLength(music:TMusic):single;cdecl;external cDllName;// Get music time length (in seconds)
function GetMusicTimePlayed(music:TMusic):single;cdecl;external cDllName;// Get current music time played (in seconds)

(* AudioStream management functions *)
function LoadAudioStream(sampleRate:dword; sampleSize:dword; channels:dword):TAudioStream;cdecl;external cDllName;// Load audio stream (to stream raw audio pcm data) }
procedure UnloadAudioStream(stream:TAudioStream);cdecl;external cDllName;// Unload audio stream and free memory
procedure UpdateAudioStream(stream:TAudioStream; data:pointer; frameCount:longint);cdecl;external cDllName;//  Update audio stream buffers with data
function IsAudioStreamProcessed(stream:TAudioStream):boolean;cdecl;external cDllName;//  Check if any audio stream buffers requires refill
procedure PlayAudioStream(stream:TAudioStream);cdecl;external cDllName;//  Play audio stream
procedure PauseAudioStream(stream:TAudioStream);cdecl;external cDllName;//  Pause audio stream
procedure ResumeAudioStream(stream:TAudioStream);cdecl;external cDllName;// Resume audio stream
function IsAudioStreamPlaying(stream:TAudioStream):boolean;cdecl;external cDllName;//  Check if audio stream is playing
procedure StopAudioStream(stream:TAudioStream);cdecl;external cDllName;//  Stop audio stream
procedure SetAudioStreamVolume(stream:TAudioStream; volume:single);cdecl;external cDllName;//  Set volume for audio stream (1.0 is max level)
procedure SetAudioStreamPitch(stream:TAudioStream; pitch:single);cdecl;external cDllName;//  Set pitch for audio stream (1.0 is base level)
procedure SetAudioStreamBufferSizeDefault(size:longint);cdecl;external cDllName;//  Default size for new audio streams


// Custom Misc Functions to help simplify a few things
function Vector2Create(aX: single; aY: single): TVector2;
procedure Vector2Set(aVec: PVector2; aX: single; aY: single);
function Vector3Create(aX: single; aY: single; aZ: single): TVector3;
procedure Vector3Set(aVec: PVector3; aX: single; aY: single; aZ: single);
function ColorCreate(aR: byte; aG: byte; aB: byte; aA: byte): TColor;
procedure ColorSet(aColor: PColor; aR: byte; aG: byte; aB: byte; aA: byte);

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single): TRectangle;
procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single; aWidth: Single; aHeight: Single);
function Camera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer): TCamera3D;
procedure Camera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);

implementation


function Vector2Create(aX: single; aY: single): TVector2;
begin
  Result.x := aX;
  Result.y := aY;
end;

procedure Vector2Set(aVec: PVector2; aX: single; aY: single);
begin
  aVec^.x := aX;
  aVec^.y := aY;
end;

function Vector3Create(aX: single; aY: single; aZ: single): TVector3;
begin
  Result.x := aX;
  Result.y := aY;
  Result.z := aZ;
end;

procedure Vector3Set(aVec: PVector3; aX: single; aY: single; aZ: single);
begin
  aVec^.x := aX;
  aVec^.y := aY;
  aVec^.z := aZ;
end;

function ColorCreate(aR: byte; aG: byte; aB: byte; aA: byte): TColor;
begin
  Result.r := aR;
  Result.g := aG;
  Result.b := aB;
  Result.a := aA;
end;

procedure ColorSet(aColor: PColor; aR: byte; aG: byte; aB: byte; aA: byte);
begin
  aColor^.r := aR;
  aColor^.g := aG;
  aColor^.b := aB;
  aColor^.a := aA;
end;

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single): TRectangle;
begin
  Result.x := aX;
  Result.y := aY;
  Result.Width := aWidth;
  Result.Height := aHeight;
end;

procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single; aWidth: Single; aHeight: Single);
begin
  aRect^.x := aX;
  aRect^.y := aY;
  aRect^.Width := aWidth;
  aRect^.Height := aHeight;
end;

function Camera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer): TCamera3D;
begin
  Result.position := aPosition;
  Result.target := aTarget;
  Result.up := aUp;
  Result.fovy := aFOVY;
  Result.projection := aType;
end;

procedure Camera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);
begin
  aCam^.position := aPosition;
  aCam^.target := aTarget;
  aCam^.up := aUp;
  aCam^.fovy := aFOVY;
  aCam^.projection := aType;
end;


initialization

end.
