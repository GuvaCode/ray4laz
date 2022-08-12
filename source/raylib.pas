{
██████╗░░█████╗░██╗░░░██╗██╗░░░░░██╗██████╗░  ░░██╗██╗░░░██████╗░
██╔══██╗██╔══██╗╚██╗░██╔╝██║░░░░░██║██╔══██╗  ░██╔╝██║░░░╚════██╗
██████╔╝███████║░╚████╔╝░██║░░░░░██║██████╦╝  ██╔╝░██║░░░░░███╔═╝
██╔══██╗██╔══██║░░╚██╔╝░░██║░░░░░██║██╔══██╗  ███████║░░░██╔══╝░░
██║░░██║██║░░██║░░░██║░░░███████╗██║██████╦╝  ╚════██║██╗███████╗
╚═╝░░╚═╝╚═╝░░╚═╝░░░╚═╝░░░╚══════╝╚═╝╚═════╝░  ░░░░░╚═╝╚═╝╚══════╝
A simple and easy-to-use library to enjoy videogames programming ( www.raylib.com )
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

  //----------------------------------------------------------------------------------
// Some basic Defines }
//----------------------------------------------------------------------------------

 (* Color, 4 components, R8G8B8A8 (32bit) *)
 type
   TColorB = record
       r,g,b,a : byte; // Color value
     end;
  PColorB = ^TColorB;
  TColor = TColorB;
  PColor = PColorB;

const
  // Some Basic Colors
  // NOTE: Custom raylib color palette for amazing visuals on WHITE background
  LIGHTGRAY:      TColorB = (r: 200; g: 200; b: 200; a: 255);  // Light Gray
  GRAY:           TColorB = (r: 130; g: 130; b: 130; a: 255);  // Gray
  DARKGRAY:       TColorB = (r: 80; g: 80; b: 80; a: 255);     // Dark Gray
  YELLOW:         TColorB = (r: 253; g: 249; b: 0; a: 255);    // Yellow
  GOLD:           TColorB = (r: 255; g: 203; b: 0; a: 255);    // Gold
  ORANGE:         TColorB = (r: 255; g: 161; b: 0; a: 255);    // Orange
  PINK:           TColorB = (r: 255; g: 109; b: 194; a: 255);  // Pink
  RED:            TColorB = (r: 230; g: 41; b: 55; a: 255);    // Red
  MAROON:         TColorB = (r: 190; g: 33; b: 55; a: 255);    // Maroon
  GREEN:          TColorB = (r: 0; g: 228; b: 48; a: 255);     // Green
  LIME:           TColorB = (r: 0; g: 158; b: 47; a: 255);     // Lime
  DARKGREEN:      TColorB = (r: 0; g: 117; b: 44; a: 255);     // Dark Green
  SKYBLUE:        TColorB = (r: 102; g: 191; b: 255; a: 255);  // Sky Blue
  BLUE:           TColorB = (r: 0; g: 121; b: 241; a: 255);    // Blue
  DARKBLUE:       TColorB = (r: 0; g: 82; b: 172; a: 255);     // Dark Blue
  PURPLE:         TColorB = (r: 200; g: 122; b: 255; a: 255);  // Purple
  VIOLET:         TColorB = (r: 135; g: 60; b: 190; a: 255);   // Violet
  DARKPURPLE:     TColorB = (r: 112; g: 31; b: 126; a: 255);   // Dark Purple
  BEIGE:          TColorB = (r: 211; g: 176; b: 131; a: 255);  // Beige
  BROWN:          TColorB = (r: 127; g: 106; b: 79; a: 255);   // Brown
  DARKBROWN:      TColorB = (r: 76; g: 63; b: 47; a: 255);     // Dark beown
  WHITE:          TColorB = (r: 255; g: 255; b: 255; a: 255);  // White
  BLACK:          TColorB = (r: 0; g: 0; b: 0; a: 255);        // Black
  BLANK:          TColorB = (r: 0; g: 0; b: 0; a: 0);          // Black(Transparent)
  MAGENTA:        TColorB = (r: 255; g: 0; b: 255; a: 255);    // Magenta
  RAYWHITE:       TColorB = (r: 245; g: 245; b: 245; a: 255);  // My own White (raylib logo)

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
         width   : Integer; // Image base width
         height  : Integer; // Image base height
         mipmaps : Integer; // Mipmap levels, 1 by default
         format  : Integer; // Data format (PixelFormat type)
       end;

     (* Texture, tex data stored in GPU memory (VRAM) *)
     PTexture = ^TTexture;
     TTexture = record
         id      : LongWord;   // OpenGL texture id
         width   : Integer; // Texture base width
         height  : Integer; // Texture base height
         mipmaps : Integer; // Mipmap levels, 1 by default
         format  : Integer; // Data format (PixelFormat type)
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
         id      : LongWord;    // OpenGL framebuffer object id
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
         left   : Integer;    // Left border offset
         top    : Integer;    // Top border offset
         right  : Integer;    // Right border offset
         bottom : Integer;    // Bottom border offset
         layout : Integer;    // Layout of the n-patch: 3x3, 1x3 or 3x1
       end;

     (* GlyphInfo, font characters glyphs info *)
     PGlyphInfo = ^TGlyphInfo;
     TGlyphInfo = record
         value    : Integer; // Character value (Unicode)
         offsetX  : Integer; // Character offset X when drawing
         offsetY  : Integer; // Character offset Y when drawing
         advanceX : Integer; // Character advance position X
         image    : TImage;  // Character image data
       end;

      (* Font, font texture and GlyphInfo array data *)
      PFont = ^TFont;
      TFont = record
          baseSize     : Integer;    // Base size (default chars height)
          glyphCount   : Integer;    // Number of glyph characters
          glyphPadding : Integer;    // Padding around the glyph characters
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
         projection : Integer;  // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
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
         vertexCount   : Integer;  // Number of vertices stored in arrays
         triangleCount : Integer;  // Number of triangles stored (indexed or not)
         // Vertex attributes data
         vertices      : PSingle;  // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
         texcoords     : PSingle;  // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
         texcoords2    : PSingle;  // Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
         normals       : PSingle;  // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
         tangents      : PSingle;  // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
         colors        : PByte;    // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
         indices       : PWord;    // Vertex indices (in case vertex data comes indexed)
         // Animation vertex data
         animVertices  : PSingle;  // Animated vertex positions (after bones transformations)
         animNormals   : PSingle;  // Animated normals (after bones transformations)
         boneIds       : PByte;    // Vertex bone ids, up to 4 bones influence by vertex (skinning)
         boneWeights   : PSingle;  // Vertex bone weight, up to 4 bones influence by vertex (skinning)
         // OpenGL identifiers
         vaoId         : LongWord;    // OpenGL Vertex Array Object id
         vboId         : PLongWord;   // OpenGL Vertex Buffer Objects id (default vertex data)
       end;

     (* Shader *)
     PShader = ^TShader;
     TShader = record
         id    : LongWord;    // Shader program id
         locs  : PInteger;    // Shader locations array (RL_MAX_SHADER_LOCATIONS)
       end;

     (* MaterialMap *)
     PMaterialMap = ^TMaterialMap;
     TMaterialMap = record
         texture : TTexture2D; // Material map texture
         color   : TColorB;  // Material map color
         value   : Single;     // Material map value
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
         name    : array[0..31] of Char; // Bone name
         parent  : Integer;              // Bone parent
       end;

     (* Model, meshes, materials and animation data *)
     PModel = ^TModel;
     TModel = record
         transform        : TMatrix;     // Local transform matrix
         meshCount        : Integer;     // Number of meshes
         materialCount    : Integer;     // Number of materials
         meshes           : PMesh;       // Meshes array
         materials        : PMaterial;   // Materials array
         meshMaterial     : PInteger;    // Mesh material number
         // Animation data
         boneCount        : Integer;     // Number of bones
         bones            : PBoneInfo;   // Bones information (skeleton)
         bindPose         : PTransform;  // Bones base transformation (pose)
       end;

     (* ModelAnimation *)
     PModelAnimation = ^TModelAnimation;
     TModelAnimation = record
         boneCount : Integer;      // Number of bones
         frameCount : Integer;     // Number of animation frames
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
          hit       : Boolean;  // Did the ray hit something?
          distance  : Single;   // Distance to nearest hit
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
         frameCount : LongWord;   // Total number of frames (considering channels)
         sampleRate : LongWord;   // Frequency (samples per second)
         sampleSize : LongWord;   // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
         channels   : LongWord;   // Number of channels (1-mono, 2-stereo, ...)
         data       : Pointer; // Buffer data pointer
       end;

     (* Opaque structs declaration *)
     (* NOTE: Actual structs are defined internally in raudio module *)
     PrAudioBuffer = ^TrAudioBuffer;
     TrAudioBuffer = record end;

     PrAudioProcessor = ^TrAudioProcessor;
     TrAudioProcessor = record end;

     (* AudioStream, custom audio stream *)
     PAudioStream = ^TAudioStream;
     TAudioStream = record
         buffer     : PrAudioBuffer;    // Pointer to internal data used by the audio system
         processor  : PrAudioProcessor; // Pointer to internal data processor, useful for audio effects
         sampleRate : LongWord;         // Frequency (samples per second)
         sampleSize : LongWord;         // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
         channels   : LongWord;         // Number of channels (1-mono, 2-stereo, ...)
       end;

     (* Sound *)
     PSound = ^TSound;
     TSound = record
         stream     : TAudioStream; // Audio stream
         frameCount : LongWord;        // Total number of frames (considering channels)
       end;

     (*Music, audio stream, anything longer than ~10 seconds should be streamed *)
     PMusic = ^TMusic;
     TMusic = record
         stream     : TAudioStream; // Audio stream
         frameCount : LongWord;     // Total number of frames (considering channels)
         looping    : Boolean;      // Music looping enable
         ctxType    : Integer;      // Type of music context (audio filetype)
         ctxData    : Pointer;      // Audio context data, depends on type
       end;

      (* VrDeviceInfo, Head-Mounted-Display device parameters *)
      PVrDeviceInfo = ^TVrDeviceInfo;
      TVrDeviceInfo = record
          hResolution            : Integer;               // Horizontal resolution in pixels
          vResolution            : Integer;               // Vertical resolution in pixels
          hScreenSize            : Single;                // Horizontal size in meters
          vScreenSize            : Single;                // Vertical size in meters
          vScreenCenter          : Single;                // Screen center in meters
          eyeToScreenDistance    : Single;                // Distance between eye and display in meters
          lensSeparationDistance : Single;                // Lens separation distance in meters
          interpupillaryDistance : Single;                // IPD (distance between pupils) in meters
          lensDistortionValues   : array[0..3] of Single; // Lens distortion constant parameters
          chromaAbCorrection     : array[0..3] of Single; // Chromatic aberration correction parameters
        end;

     (* VrStereoConfig, VR stereo rendering configuration for simulator *)
     PVrStereoConfig = ^TVrStereoConfig;
     TVrStereoConfig = record
         projection          : array[0..1] of TMatrix; // VR projection matrices (per eye)
         viewOffset          : array[0..1] of TMatrix; // VR view offset matrices (per eye)
         leftLensCenter      : array[0..1] of Single;  // VR left lens center
         rightLensCenter     : array[0..1] of Single;  // VR right lens center
         leftScreenCenter    : array[0..1] of Single;  // VR left screen center
         rightScreenCenter   : array[0..1] of Single;  // VR right screen center
         scale               : array[0..1] of Single;  // VR distortion scale
         scaleIn             : array[0..1] of Single;  // VR distortion scale in
       end;

     (* File path list *)
     PFilePathList = ^TFilePathList;
     TFilePathList = record
       capacity  : LongWord;  // Filepaths max entries
       count     : LongWord;  // Filepaths entries count
       paths     : PPChar; // Filepaths entries
     end;

//----------------------------------------------------------------------------------
// Enumerators Definition
//----------------------------------------------------------------------------------

     (* System/Window config flags *)
     // NOTE: Every bit registers one state (use it with bit masks)
     // By default all flags are set to 0
     PConfigFlags = ^TConfigFlags;
     TConfigFlags =  Integer;
       const
         FLAG_VSYNC_HINT                = $00000040; // Set to try enabling V-Sync on GPU
         FLAG_FULLSCREEN_MODE           = $00000002; // Set to run program in fullscreen
         FLAG_WINDOW_RESIZABLE          = $00000004; // Set to allow resizable window
         FLAG_WINDOW_UNDECORATED        = $00000008; // Set to disable window decoration (frame and buttons)
         FLAG_WINDOW_HIDDEN             = $00000080; // Set to hide window
         FLAG_WINDOW_MINIMIZED          = $00000200; // Set to minimize window (iconify)
         FLAG_WINDOW_MAXIMIZED          = $00000400; // Set to maximize window (expanded to monitor)
         FLAG_WINDOW_UNFOCUSED          = $00000800; // Set to window non focused
         FLAG_WINDOW_TOPMOST            = $00001000; // Set to window always on top
         FLAG_WINDOW_ALWAYS_RUN         = $00000100; // Set to allow windows running while minimized
         FLAG_WINDOW_TRANSPARENT        = $00000010; // Set to allow transparent framebuffer
         FLAG_WINDOW_HIGHDPI            = $00002000; // Set to support HighDPI
         FLAG_WINDOW_MOUSE_PASSTHROUGH  = $00004000; // Set to support mouse passthrough, only supported when FLAG_WINDOW_UNDECORATED
         FLAG_MSAA_4X_HINT              = $00000020; // Set to try enabling MSAA 4X
         FLAG_INTERLACED_HINT           = $00010000; // Set to try enabling interlaced video format (for V3D)

     (* Trace log level *)
     // NOTE: Organized by priority level
     type
       PTraceLogLevel = ^TTraceLogLevel;
       TTraceLogLevel =  Integer;
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
       TKeyboardKey =  Integer;
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
       TMouseCursor =  Integer;
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
      TGamepadButton =  Integer;
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
     TGamepadAxis =  Integer;
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
       TMaterialMapIndex =  Integer;
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
       TShaderLocationIndex =  Integer;
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
       TShaderUniformDataType =  Integer;
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
       TPixelFormat =  Integer;
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
     TTextureFilter =  Integer;
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
     TTextureWrap =  Integer;
     Const
       TEXTURE_WRAP_REPEAT        = 0; // Repeats texture in tiled mode
       TEXTURE_WRAP_CLAMP         = 1; // Clamps texture to edge pixel in tiled mode
       TEXTURE_WRAP_MIRROR_REPEAT = 2; // Mirrors and repeats the texture in tiled mode
       TEXTURE_WRAP_MIRROR_CLAMP  = 3; // Mirrors and clamps to border the texture in tiled mode

   (* Cubemap layouts *)
   type
     PCubemapLayout = ^TCubemapLayout;
     TCubemapLayout =  Integer;
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
     TFontType =  Integer;
     Const
       FONT_DEFAULT          = 0;    // Default font generation, anti-aliased
       FONT_BITMAP           = 1;    // Bitmap font generation, no anti-aliasing
       FONT_SDF              = 2;    // SDF font generation, requires external shader

   (* Color blending modes (pre-defined) *)
   type
     PBlendMode = ^TBlendMode;
     TBlendMode =  Integer;
     Const
       BLEND_ALPHA             = 0;    // Blend textures considering alpha (default)
       BLEND_ADDITIVE          = 1;    // Blend textures adding colors
       BLEND_MULTIPLIED        = 2;    // Blend textures multiplying colors
       BLEND_ADD_COLORS        = 3;    // Blend textures adding colors (alternative)
       BLEND_SUBTRACT_COLORS   = 4;    // Blend textures subtracting colors (alternative)
       BLEND_ALPHA_PREMULTIPLY = 5;    // Blend premultiplied textures considering alpha
       BLEND_CUSTOM            = 6;    // Blend textures using custom src/dst factors (use rlSetBlendMode())

   (* Gestures *)
   //  NOTE: It could be used as flags to enable only some gestures
   type
     PGesture = ^TGesture;
     TGesture =  Integer;
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
     TCameraMode =  Integer;
     Const
       CAMERA_CUSTOM = 0;      // Custom camera
       CAMERA_FREE = 1;        // Free camera
       CAMERA_ORBITAL = 2;     // Orbital camera
       CAMERA_FIRST_PERSON = 3;// First person camera
       CAMERA_THIRD_PERSON = 4;// Third person camera

   (* Camera projection *)
   type
     PCameraProjection = ^TCameraProjection;
     TCameraProjection =  Integer;
     const
       CAMERA_PERSPECTIVE = 0; // Perspective projection
       CAMERA_ORTHOGRAPHIC = 1;// Orthographic projection

     (* N-patch layout *)
     type
      PNPatchLayout = ^TNPatchLayout;
      TNPatchLayout =  Integer;

      const
        NPATCH_NINE_PATCH = 0;             // Npatch layout: 3x3 tiles
        NPATCH_THREE_PATCH_VERTICAL = 1;   // Npatch layout: 1x3 tiles
        NPATCH_THREE_PATCH_HORIZONTAL = 2; // Npatch layout: 3x1 tiles


//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

(* Window-related function *)

{Initialize window and OpenGL context}
procedure InitWindow(width, height: Integer; title: PChar); cdecl; external cDllName;
{Check if KEY_ESCAPE pressed or Close icon pressed}
function WindowShouldClose: Boolean; cdecl; external cDllName;
{Close window and unload OpenGL context}
procedure CloseWindow; cdecl; external cDllName;
{Check if window has been initialized successfully}
function IsWindowReady: Boolean; cdecl; external cDllName;
{Check if window is currently fullscreen }
function IsWindowFullscreen: Boolean; cdecl; external cDllName;
{Check if window is currently hidden (only PLATFORM_DESKTOP)}
function IsWindowHidden: Boolean; cdecl; external cDllName;
{Check if window is currently minimized (only PLATFORM_DESKTOP)}
function IsWindowMinimized: Boolean; cdecl; external cDllName;
{Check if window is currently maximized (only PLATFORM_DESKTOP)}
function IsWindowMaximized: Boolean; cdecl; external cDllName;
{Check if window is currently focused (only PLATFORM_DESKTOP)}
function IsWindowFocused: Boolean; cdecl; external cDllName;
{Check if window has been resized last frame}
function IsWindowResized: Boolean;cdecl; external cDllName;
{Check if one specific window flag is enabled}
function IsWindowState(flag: LongWord): Boolean; cdecl; external cDllName;
{Set window configuration state using flags (only PLATFORM_DESKTOP)}
procedure SetWindowState(flags: LongWord); cdecl; external cDllName;
{Clear window configuration state flags}
procedure ClearWindowState(flags: LongWord); cdecl; external cDllName;
{Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)}
procedure ToggleFullscreen; cdecl; external cDllName;
{Set window state: maximized, if resizable (only PLATFORM_DESKTOP)}
procedure MaximizeWindow; cdecl; external cDllName;
{Set window state: minimized, if resizable (only PLATFORM_DESKTOP)}
procedure MinimizeWindow; cdecl; external cDllName;
{Set window state: not minimized/maximized (only PLATFORM_DESKTOP)}
procedure RestoreWindow; cdecl; external cDllName;
{Set icon for window (only PLATFORM_DESKTOP)}
procedure SetWindowIcon(image: TImage); cdecl; external cDllName;
{Set title for window (only PLATFORM_DESKTOP)}
procedure SetWindowTitle(title: PChar); cdecl; external cDllName;
{Set window position on screen (only PLATFORM_DESKTOP)}
procedure SetWindowPosition(x, y: Integer); cdecl; external cDllName;
{Set monitor for the current window (fullscreen mode)}
procedure SetWindowMonitor(monitor: Integer); cdecl; external cDllName;
{Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)}
procedure SetWindowMinSize(width, height: Integer); cdecl; external cDllName;
{Set window dimensions}
procedure SetWindowSize(width, height: Integer);cdecl; external cDllName;
{Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)}
procedure SetWindowOpacity(opacity: Single); cdecl; external cDllName;
{Get native window handle}
function GetWindowHandle: Pointer; cdecl; external cDllName;
{Get current screen width}
function GetScreenWidth: Integer; cdecl; external cDllName;
{Get current screen height}
function GetScreenHeight: Integer; cdecl; external cDllName;
{Get current render width (it considers HiDPI)}
function GetRenderWidth: Integer; cdecl; external cDllName;
{Get current render height (it considers HiDPI)}
function GetRenderHeight: Integer; cdecl; external cDllName;
{Get number of connected monitors}
function GetMonitorCount: Integer; cdecl; external cDllName;
{Get current connected monitor}
function GetCurrentMonitor: Integer; cdecl; external cDllName;
{Get specified monitor position}
function GetMonitorPosition(monitor: Integer): TVector2; cdecl; external cDllName;
{Get specified monitor width (current video mode used by monitor)}
function GetMonitorWidth(monitor: Integer): Integer; cdecl; external cDllName;
{Get specified monitor height (current video mode used by monitor)}
function GetMonitorHeight(monitor: Integer): Integer; cdecl; external cDllName;
{Get specified monitor physical width in millimetres}
function GetMonitorPhysicalWidth(monitor: Integer): Integer; cdecl; external cDllName;
{Get specified monitor physical height in millimetres}
function GetMonitorPhysicalHeight(monitor: Integer): Integer; cdecl; external cDllName;
{Get specified monitor refresh rate}
function GetMonitorRefreshRate(monitor: Integer): Integer; cdecl; external cDllName;
{Get window position XY on monitor}
function GetWindowPosition: TVector2; cdecl; external cDllName;
{Get window scale DPI factor}
function GetWindowScaleDPI: TVector2; cdecl; external cDllName;
{Get the human-readable, UTF-8 encoded name of the primary monitor}
function GetMonitorName(monitor: Integer): PChar; cdecl; external cDllName;
{Set clipboard text content}
procedure SetClipboardText(text: PChar); cdecl; external cDllName;
{Get clipboard text content}
function GetClipboardText: PChar; cdecl; external cDllName;
{Enable waiting for events on EndDrawing(), no automatic event polling}
procedure EnableEventWaiting; cdecl; external cDllName;
{Disable waiting for events on EndDrawing(), automatic events polling}
procedure DisableEventWaiting; cdecl; external cDllName;

(* Custom frame control functions *)
// NOTE: Those functions are intended for advance users that want full control over the frame processing
// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL

{Swap back buffer with front buffer (screen drawing)}
procedure SwapScreenBuffer; cdecl; external cDllName;
{Register all input events}
procedure PollInputEvents; cdecl; external cDllName;
{Wait for some time (halt program execution) }
procedure WaitTime(ms: Double); cdecl; external cDllName;

(* Cursor-related functions *)

{Shows cursor}
procedure ShowCursor; cdecl; external cDllName;
{Hides cursor}
procedure HideCursor; cdecl; external cDllName;
{Check if cursor is not visible}
function IsCursorHidden: Boolean; cdecl; external cDllName;
{Enables cursor (unlock cursor)}
procedure EnableCursor; cdecl; external cDllName;
{Disables cursor (lock cursor)}
procedure DisableCursor; cdecl; external cDllName;
{Check if cursor is on the current screen.}
function IsCursorOnScreen: Boolean; cdecl; external cDllName;

(* Drawing-related functions *)

{Set background color (framebuffer clear color)}
procedure ClearBackground(color: TColorB); cdecl; external cDllName;
{Setup canvas (framebuffer) to start drawing}
procedure BeginDrawing; cdecl; external cDllName;
{End canvas drawing and swap buffers (double buffering)}
procedure EndDrawing; cdecl; external cDllName;
{Initialize 2D mode with custom camera (2D)}
procedure BeginMode2D(camera: TCamera2D); cdecl; external cDllName;
{Ends 2D mode with custom camera}
procedure EndMode2D; cdecl; external cDllName;
{Initializes 3D mode with custom camera (3D)}
procedure BeginMode3D(camera: TCamera3D); cdecl; external cDllName;
{Ends 3D mode and returns to default 2D orthographic mode}
procedure EndMode3D; cdecl; external cDllName;
{Initializes render texture for drawing}
procedure BeginTextureMode(target: TRenderTexture2D); cdecl; external cDllName;
{Ends drawing to render texture}
procedure EndTextureMode; cdecl; external cDllName;
{Begin custom shader drawing}
procedure BeginShaderMode(shader: TShader); cdecl; external cDllName;
{End custom shader drawing (use default shader)}
procedure EndShaderMode;cdecl; external cDllName;
{Begin blending mode (alpha, additive, multiplied)}
procedure BeginBlendMode(mode: Integer); cdecl; external cDllName;
{End blending mode (reset to default: alpha blending)}
procedure EndBlendMode; cdecl; external cDllName;
{Begin scissor mode (define screen area for following drawing)}
procedure BeginScissorMode(x, y, width, height: Integer); cdecl; external cDllName;
{End scissor mode}
procedure EndScissorMode; cdecl; external cDllName;
{Begin stereo rendering (requires VR simulator)}
procedure BeginVrStereoMode(config: TVrStereoConfig); cdecl; external cDllName;
{End stereo rendering (requires VR simulator)}
procedure EndVrStereoMode; cdecl; external cDllName;

(* VR stereo config functions for VR simulator *)

{Load VR stereo config for VR simulator device parameters}
function LoadVrStereoConfig(device: TVrDeviceInfo): TVrStereoConfig; cdecl; external cDllName;
{Unload VR stereo config }
procedure UnloadVrStereoConfig(config: TVrStereoConfig); cdecl; external cDllName;


(* Shader management functions *)
// NOTE: Shader functionality is not available on OpenGL 1.1

{Load shader from files and bind default locations}
function LoadShader(vsFileName, fsFileName: PChar): TShader; cdecl; external cDllName;
{Load shader from code strings and bind default locations}
function LoadShaderFromMemory(vsCode, fsCode: PChar): TShader;cdecl;external cDllName;
{Get shader uniform location}
function GetShaderLocation(shader: TShader; uniformName: PChar): Integer; cdecl; external cDllName;
{Get shader attribute location}
function GetShaderLocationAttrib(shader:TShader; attribName:PChar): Integer; cdecl; external cDllName;
{Set shader uniform value}
procedure SetShaderValue(shader: TShader; locIndex: Integer; value: Pointer; uniformType: Integer); cdecl; external cDllName;
{Set shader uniform value vector}
procedure SetShaderValueV(shader: TShader; locIndex: Integer; value: Pointer; uniformType, count: Integer); cdecl; external cDllName;
{Set shader uniform value (matrix 4x4)}
procedure SetShaderValueMatrix(shader: TShader; locIndex: Integer; mat:TMatrix); cdecl; external cDllName;
{Set shader uniform value for texture (sampler2d) }
procedure SetShaderValueTexture(shader: TShader; locIndex: Integer; texture: TTexture2D); cdecl; external cDllName;
{Unload shader from GPU memory (VRAM)}
procedure UnloadShader(shader: TShader); cdecl; external cDllName;


(* Screen-space-related functions *)

{Get a ray trace from mouse position}
function GetMouseRay(mousePosition: TVector2; camera: TCamera): TRay; cdecl; external cDllName;
{Get camera transform matrix (view matrix)}
function GetCameraMatrix(camera: TCamera): TMatrix; cdecl; external cDllName;
{Get camera 2d transform matrix}
function GetCameraMatrix2D(camera: TCamera2D): TMatrix; cdecl; external cDllName;
{Get the screen space position for a 3d world space position}
function GetWorldToScreen(position: TVector3; camera: TCamera): TVector2; cdecl; external cDllName;
{Get size position for a 3d world space position}
function GetWorldToScreenEx(position: TVector3; camera: TCamera; width, height: Integer): TVector2; cdecl; external cDllName;
{Get the screen space position for a 2d camera world space position}
function GetWorldToScreen2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external cDllName;
{Get the world space position for a 2d camera screen space position}
function GetScreenToWorld2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external cDllName;


(* Timing-related functions *)

{Set target FPS (maximum)}
procedure SetTargetFPS(fps: Integer); cdecl; external cDllName;
{Returns current FPS}
function GetFPS: Integer; cdecl; external cDllName;
{Returns time in seconds for last frame drawn (delta time)}
function GetFrameTime: Single; cdecl; external cDllName;
{Returns elapsed time in seconds since InitWindow()}
function GetTime: Double; cdecl; external cDllName;

(* Misc. functions *)

{Get a random value between min and max (both included)}
function GetRandomValue(min, max: Integer): Integer; cdecl; external cDllName;
{Set the seed for the random number generator}
procedure SetRandomSeed(seed: LongWord); cdecl; external cDllName;
{Takes a screenshot of current screen (filename extension defines format)}
procedure TakeScreenshot(fileName: PChar); cdecl; external cDllName;
{Setup init configuration flags (view FLAGS)}
procedure SetConfigFlags(flags:LongWord); cdecl; external cDllName;
{Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)}
procedure TraceLog(logLevel: Integer; text: PChar); cdecl; varargs; external cDllName;
{Set the current threshold (minimum) log level}
procedure SetTraceLogLevel(logLevel: Integer); cdecl; external cDllName;
{Internal memory allocator}
function MemAlloc(size: Integer): Pointer; cdecl; external cDllName;
{Internal memory reallocator}
function MemRealloc(ptr: Pointer; size: Integer): Pointer; cdecl; external cDllName;
{Internal memory free}
procedure MemFree(ptr: Pointer); cdecl; external cDllName;
{Open URL with default system browser (if available)}
procedure OpenURL(const url: PChar); cdecl; external cDllName;

(* Set custom callbacks *)
(* Callbacks to hook some internal functions *)
// WARNING: This callbacks are intended for advance users
type
  TTraceLogCallback = procedure(logLevel: Integer; text: PChar); cdecl; varargs; // , va_list args varargs ???
  TLoadFileDataCallback = function(fileName: PChar; out bytesRead: LongWord): PChar; cdecl;
  TSaveFileDataCallback = function(fileName: PChar; data: Pointer; bytesToWrite: LongWord): Boolean; cdecl;
  TLoadFileTextCallback = function(fileName: PChar): PChar; cdecl;
  TSaveFileTextCallback = function(fileName, text: PChar): Boolean; cdecl;

  { Set custom trace log }
  procedure SetTraceLogCallback(callback: TTraceLogCallback); cdecl; external cDllName;
  { Set custom file binary data loader }
  procedure SetLoadFileDataCallback(callback: TLoadFileDataCallback); cdecl; external cDllName;
  { Set custom file binary data saver }
  procedure SetSaveFileDataCallback(callback: TSaveFileDataCallback); cdecl; external cDllName;
  { Set custom file text data loader }
  procedure SetLoadFileTextCallback(callback: TLoadFileTextCallback); cdecl; external cDllName;
  { Set custom file text data saver }
  procedure SetSaveFileTextCallback(callback: TSaveFileTextCallback); cdecl; external cDllName;


(* Files management functions *)

{Load file data as byte array (read)}
function LoadFileData(fileName: PChar; bytesRead: PLongWord): Pointer; cdecl; external cDllName;
{Unload file data allocated by LoadFileData()}
procedure UnloadFileData(data: Pointer); cdecl; external cDllName;
{Save data to file from byte array (write), returns true on success}
function SaveFileData(fileName: PChar; data: Pointer; bytesToWrite: LongWord): Boolean; cdecl; external cDllName;
{Export data to code (.h), returns true on success}
function ExportDataAsCode(data: PChar; size: LongWord; fileName: PChar): Boolean; cdecl; external cDllName;
{Load text data from file (read), returns a '\0' terminated string}
function LoadFileText(fileName: Pchar): Pchar; cdecl; external cDllName;
{Unload file text data allocated by LoadFileText()}
procedure UnloadFileText(text: PChar); cdecl; external cDllName;
{Save text data to file (write), string must be '\0' terminated, returns true on success}
function SaveFileText(fileName, text: PChar): Boolean; cdecl; external cDllName;
{Check if file exists}
function FileExists(fileName: PChar): Boolean; cdecl; external cDllName;
{Check if a directory path exists}
function DirectoryExists(dirPath: PChar): Boolean; cdecl; external cDllName;
{Check file extension (including point: .png, .wav)}
function IsFileExtension(fileName, ext: PChar): Boolean; cdecl; external cDllName;
{Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)}
function GetFileLength(fileName: PChar): Integer; cdecl; external cDllName;
{Get pointer to extension for a filename string (includes dot: '.png')}
function GetFileExtension(fileName: PChar): PChar; cdecl; external cDllName;
{Get pointer to filename for a path string }
function GetFileName(filePath: PChar): PChar; cdecl; external cDllName;
{Get filename string without extension (uses static string)}
function GetFileNameWithoutExt(filePath: PChar): PChar; cdecl; external cDllName;
{Get full path for a given fileName with path (uses static string)}
function GetDirectoryPath(filePath: PChar): PChar; cdecl; external cDllName;
{Get previous directory path for a given path (uses static string)}
function GetPrevDirectoryPath(dirPath: PChar): PChar; cdecl; external cDllName;
{Get current working directory (uses static string)}
function GetWorkingDirectory: PChar; cdecl; external cDllName;
{Get the directory if the running application (uses static string)}
function GetApplicationDirectory: PChar; cdecl; external cDllName;
{Change working directory, return true on success}
function ChangeDirectory(dir: PChar): Boolean; cdecl; external cDllName;
{Check if a given path is a file or a directory}
function IsPathFile(path: PChar): Boolean; cdecl; external cDllName;
{Load directory filepaths}
function LoadDirectoryFiles(dirPath: PChar): TFilePathList; cdecl; external cDllName;
{Load directory filepaths with extension filtering and recursive directory scan}
function LoadDirectoryFilesEx(basePath, filter: PChar; scanSubdirs: Boolean): TFilePathList; cdecl; external cDllName;
{Unload filepaths}
procedure UnloadDirectoryFiles(files: TFilePathList); cdecl; external cDllName;
{Check if a file has been dropped into window}
function IsFileDropped: Boolean; cdecl; external cDllName;
{Load dropped filepaths}
function LoadDroppedFiles: TFilePathList; cdecl; external cDllName;
{Unload dropped filepaths}
procedure UnloadDroppedFiles(files: TFilePathList); cdecl; external cDllName;
{Get file modification time (last write time)}
function GetFileModTime(fileName: PChar): QWord; cdecl; external cDllName;

(* Compression/Encoding functionality *)

{Compress data (DEFLATE algorithm), memory must be MemFree()}
function CompressData(const data: Pointer; dataSize: Integer; compDataSize: PInteger): Pointer; cdecl; external cDllName;
{Decompress data (DEFLATE algorithm), memory must be MemFree()}
function DecompressData(const compData: Pointer; compDataSize: Integer; dataSize: PInteger): Pointer; cdecl; external cDllName;
{Encode data to Base64 string, memory must be MemFree()}
function EncodeDataBase64(const data: PChar; dataSize: Integer; outputSize: PInteger): PChar; cdecl; external cDllName;
{Decode Base64 string data, memory must be MemFree()}
function DecodeDataBase64(const data: PChar; outputSize: PInteger): PChar; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

(* Input-related functions: keyboard *)

{Check if a key has been pressed once}
function IsKeyPressed(key: Integer): Boolean; cdecl; external cDllName;
{Check if a key is being pressed}
function IsKeyDown(key: Integer): Boolean;cdecl;external cDllName;
{Check if a key has been released once}
function IsKeyReleased(key: Integer): Boolean;cdecl;external cDllName;
{Check if a key is NOT being pressed}
function IsKeyUp(key: Integer): Boolean;cdecl;external cDllName;
{Set a custom key to exit program (default is ESC)}
procedure SetExitKey(key: Integer); cdecl; external cDllName;
{Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty}
function GetKeyPressed: Integer; cdecl; external cDllName;
{Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty}
function GetCharPressed: Integer; cdecl; external cDllName;

(* Input-related functions: gamepads *)

{Check if a gamepad is available}
function IsGamepadAvailable(gamepad: Integer): Boolean; cdecl; external cDllName;
{Get gamepad internal name id}
function GetGamepadName(gamepad: Integer): PChar; cdecl; external cDllName;
{Check if a gamepad button has been pressed once}
function IsGamepadButtonPressed(gamepad, button: Integer): Boolean; cdecl; external cDllName;
{Check if a gamepad button is being pressed}
function IsGamepadButtonDown(gamepad, button: Integer): Boolean; cdecl; external cDllName;
{Check if a gamepad button has been released once}
function IsGamepadButtonReleased(gamepad, button: Integer): Boolean; cdecl; external cDllName;
{Check if a gamepad button is NOT being pressed}
function IsGamepadButtonUp(gamepad, button: Integer): Boolean; cdecl; external cDllName;
{Get the last gamepad button pressed}
function GetGamepadButtonPressed: Integer; cdecl; external cDllName;
{Get gamepad axis count for a gamepad}
function GetGamepadAxisCount(gamepad: Integer): Integer; cdecl; external cDllName;
{Get axis movement value for a gamepad axis}
function GetGamepadAxisMovement(gamepad, axis: Integer): Single; cdecl; external cDllName;
{Set internal gamepad mappings (SDL_GameControllerDB)}
function SetGamepadMappings(mappings: PChar): Integer; cdecl; external cDllName;

(* Input-related functions: mouse *)

{Check if a mouse button has been pressed once}
function IsMouseButtonPressed(button: Integer): Boolean; cdecl; external cDllName;
{Check if a mouse button is being pressed}
function IsMouseButtonDown(button: Integer): Boolean; cdecl; external cDllName;
{Check if a mouse button has been released once}
function IsMouseButtonReleased(button: Integer): Boolean; cdecl; external cDllName;
{Check if a mouse button is NOT being pressed}
function IsMouseButtonUp(button: Integer): Boolean; cdecl; external cDllName;
{Get mouse position X}
function GetMouseX: Integer; cdecl; external cDllName;
{Get mouse position Y}
function GetMouseY: Integer; cdecl; external cDllName;
{Get mouse position XY}
function GetMousePosition: TVector2; cdecl;external cDllName;
{Get mouse delta between frames}
function GetMouseDelta: TVector2; cdecl; external cDllName;
{Set mouse position XY}
procedure SetMousePosition(x,y: Integer); cdecl; external cDllName;
{Set mouse offset}
procedure SetMouseOffset(offsetX, offsetY: Integer); cdecl; external cDllName;
{Set mouse scaling}
procedure SetMouseScale(scaleX, scaleY: Single); cdecl; external cDllName;
{Get mouse wheel movement for X or Y, whichever is larger}
function GetMouseWheelMove: Single; cdecl; external cDllName;
{Get mouse wheel movement for both X and Y}
function GetMouseWheelMoveV: TVector2; cdecl; external cDllName;
{ Set mouse cursor}
procedure SetMouseCursor(cursor: Integer); cdecl; external cDllName;

(* Input-related functions: touch *)

{Get touch position X for touch point 0 (relative to screen size)}
function GetTouchX: Integer; cdecl; external cDllName;
{Get touch position Y for touch point 0 (relative to screen size)}
function GetTouchY: Integer; cdecl; external cDllName;
{Get touch point identifier for given index}
function GetTouchPointId(index: Integer): Integer; cdecl; external cDllName;
{Get touch position XY for a touch point index (relative to screen size)}
function GetTouchPosition(index: Integer): TVector2; cdecl; external cDllName;
{Get touch points count}
function GetTouchPointCount: Integer; cdecl; external cDllName;
{Get last touch event registered}
function GetTouchEvent: Integer; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------

{Enable a set of gestures using flags}
procedure SetGesturesEnabled(flags: LongWord); cdecl; external cDllName;
{Check if a gesture have been detected}
function IsGestureDetected(gesture: Integer): Boolean;cdecl;external cDllName;
{Get latest detected gesture}
function GetGestureDetected: Integer; cdecl; external cDllName;
{Get gesture hold time in milliseconds}
function GetGestureHoldDuration: Single; cdecl; external cDllName;
{Get gesture drag vector}
function GetGestureDragVector: TVector2; cdecl; external cDllName;
{Get gesture drag angle}
function GetGestureDragAngle: Single; cdecl; external cDllName;
{Get gesture pinch delta}
function GetGesturePinchVector: TVector2; cdecl; external cDllName;
{Get gesture pinch angle}
function GetGesturePinchAngle: Single; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------

{Set camera mode (multiple camera modes available)}
procedure SetCameraMode(camera: TCamera; mode: Integer); cdecl; external cDllName;
{Update camera position for selected mode}
procedure UpdateCamera(camera: PCamera); cdecl; external cDllName;
{Set camera pan key to combine with mouse movement (free camera)}
procedure SetCameraPanControl(keyPan: Integer); cdecl; external cDllName;
{Set camera alt key to combine with mouse movement (free camera)}
procedure SetCameraAltControl(keyAlt: Integer); cdecl; external cDllName;
{Set camera smooth zoom key to combine with mouse (free camera)}
procedure SetCameraSmoothZoomControl(keySmoothZoom: Integer); cdecl; external cDllName;
{Set camera move controls (1st person and 3rd person cameras)}
procedure SetCameraMoveControls(keyFront, keyBack, keyRight, keyLeft, keyUp, keyDown: Integer); cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------
// Set texture and rectangle to be used on shapes drawing
// NOTE: It can be useful when using basic shapes and one single font,
// defining a font char white rectangle would allow drawing everything in a single draw call

{Set texture and rectangle to be used on shapes drawing}
procedure SetShapesTexture(texture: TTexture2D; source: TRectangle); cdecl; external cDllName;

(* Basic shapes drawing functions *)

{Draw a pixel}
procedure DrawPixel(posX, posY: Integer; color: TColorB); cdecl; external cDllName;
{Draw a pixel (Vector version)}
procedure DrawPixelV(position: TVector2; color: TColorB); cdecl; external cDllName;
{Draw a line}
procedure DrawLine(startPosX, startPosY, endPosX, endPosY: Integer; color: TColorB); cdecl; external cDllName;
{Draw a line (Vector version)}
procedure DrawLineV(startPos, endPos: TVector2; color: TColorB); cdecl; external cDllName;
{Draw a line defining thickness}
procedure DrawLineEx(startPos, endPos: TVector2; thick: Single; color: TColorB); cdecl; external cDllName;
{Draw a line using cubic-bezier curves in-out}
procedure DrawLineBezier(startPos, endPos: TVector2; thick: Single; color: TColorB); cdecl; external cDllName;
{Draw line using quadratic bezier curves with a control point}
procedure DrawLineBezierQuad(startPos, endPos, controlPos: TVector2; thick: Single; color:TColorB); cdecl; external cDllName;
{Draw line using cubic bezier curves with 2 control points}
procedure DrawLineBezierCubic(startPos, endPos, startControlPos, endControlPos: TVector2; thick: Single; color: TColorB); cdecl; external cDllName;
{Draw lines sequence}
procedure DrawLineStrip(points: PVector2; pointCount: Integer; color: TColorB); cdecl; external cDllName;
{Draw a color-filled circle}
procedure DrawCircle(centerX, centerY: Integer; radius: Single; color: TColorB); cdecl; external cDllName;
{Draw a piece of a circle}
procedure DrawCircleSector(center: TVector2; radius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external cDllName;
{Draw circle sector outline}
procedure DrawCircleSectorLines(center: TVector2; radius, startAngle, endAngle: Single; segments: Integer; color: TColorB);cdecl;external cDllName;
{Draw a gradient-filled circle}
procedure DrawCircleGradient(centerX, centerY: Integer; radius: Single; color1, color2: TColorB); cdecl; external cDllName;
{Draw a color-filled circle (Vector version)}
procedure DrawCircleV(center: TVector2; radius: Single; color: TColorB); cdecl; external cDllName;
{Draw circle outline}
procedure DrawCircleLines(centerX, centerY: Integer; radius: Single; color: TColorB); cdecl; external cDllName;
{Draw ellipse}
procedure DrawEllipse(centerX, centerY: Integer; radiusH, radiusV: Single; color: TColorB); cdecl; external cDllName;
{Draw ellipse outline}
procedure DrawEllipseLines(centerX, centerY: Integer; radiusH, radiusV: Single; color: TColorB); cdecl; external cDllName;
{Draw ring}
procedure DrawRing(center: TVector2; innerRadius, outerRadius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external cDllName;
{Draw ring outline}
procedure DrawRingLines(center: TVector2; innerRadius, outerRadius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external cDllName;
{Draw a color-filled rectangle}
procedure DrawRectangle(posX, posY, width, height: Integer; color: TColorB); cdecl; external cDllName;
{Draw a color-filled rectangle (Vector version)}
procedure DrawRectangleV(position, size: TVector2; color: TColorB); cdecl; external cDllName;
{Draw a color-filled rectangle}
procedure DrawRectangleRec(rec: TRectangle; color: TColorB); cdecl; external cDllName;
{Draw a color-filled rectangle with pro parameters}
procedure DrawRectanglePro(rec: TRectangle; origin: TVector2; rotation: Single; color: TColorB); cdecl; external cDllName;
{Draw a vertical-gradient-filled rectangle}
procedure DrawRectangleGradientV(posX, posY, width, height: Integer; color1, color2: TColorB); cdecl; external cDllName;
{Draw a horizontal-gradient-filled rectangle}
procedure DrawRectangleGradientH(posX, posY, width, height: Integer; color1, color2: TColorB); cdecl; external cDllName;
{Draw a gradient-filled rectangle with custom vertex colors}
procedure DrawRectangleGradientEx(rec: TRectangle; col1, col2, col3, col4: TColorB); cdecl; external cDllName;
{Draw rectangle outline}
procedure DrawRectangleLines(posX, posY, width, height: Integer; color: TColorB); cdecl; external cDllName;
{Draw rectangle outline with extended parameters}
procedure DrawRectangleLinesEx(rec: TRectangle; lineThick: Single; color: TColorB); cdecl; external cDllName;
{Draw rectangle with rounded edges}
procedure DrawRectangleRounded(rec: TRectangle; roundness: Single; segments: Integer; color: TColorB); cdecl; external cDllName;
{Draw rectangle with rounded edges outline}
procedure DrawRectangleRoundedLines(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Single; color: TColorB); cdecl; external cDllName;
{Draw a color-filled triangle (vertex in counter-clockwise order!)}
procedure DrawTriangle(v1, v2, v3: TVector2; color: TColorB); cdecl; external cDllName;
{Draw triangle outline (vertex in counter-clockwise order!)}
procedure DrawTriangleLines(v1, v2, v3: TVector2; color: TColorB); cdecl; external cDllName;
{Draw a triangle fan defined by points (first vertex is the center)}
procedure DrawTriangleFan(points: PVector2; pointCount: Integer; color: TColorB); cdecl; external cDllName;
{Draw a triangle strip defined by points}
procedure DrawTriangleStrip(points: PVector2; pointCount: Integer; color: TColorB); cdecl; external cDllName;
{Draw a regular polygon (Vector version)}
procedure DrawPoly(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColorB); cdecl; external cDllName;
{Draw a polygon outline of n sides}
procedure DrawPolyLines(center: TVector2; sides: Integer; radius, rotation: Single; color: TColorB); cdecl; external cDllName;
{Draw a polygon outline of n sides with extended parameters }
procedure DrawPolyLinesEx(center: TVector2; sides: Integer; radius, rotation, lineThick: single; color: TColorB); cdecl; external cDllName;

(* Basic shapes collision detection functions *)

{Check collision between two rectangles}
function CheckCollisionRecs(rec1, rec2: TRectangle): Boolean; cdecl; external cDllName;
{Check collision between two circles}
function CheckCollisionCircles(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): Boolean; cdecl; external cDllName;
{Check collision between circle and rectangle}
function CheckCollisionCircleRec(center: TVector2; radius: Single; rec: TRectangle): Boolean; cdecl; external cDllName;
{Check if point is inside rectangle}
function CheckCollisionPointRec(point: TVector2; rec: TRectangle): Boolean; cdecl; external cDllName;
{Check if point is inside circle}
function CheckCollisionPointCircle(point, center: TVector2; radius: Single): Boolean; cdecl; external cDllName;
{Check if point is inside a triangle}
function CheckCollisionPointTriangle(point, p1, p2, p3: TVector2): Boolean; cdecl; external cDllName;
{Check the collision between two lines defined by two points each, returns collision point by reference}
function CheckCollisionLines(startPos1, endPos1, startPos2, endPos2: TVector2; collisionPoint: PVector2): Boolean; cdecl; external cDllName;
{Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]}
function CheckCollisionPointLine(point, p1, p2: TVector2; threshold: Integer): Boolean; cdecl; external cDllName;
{Get collision rectangle for two rectangles collision}
function GetCollisionRec(rec1, rec2: TRectangle): TRectangle; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

(* Image loading functions *)
// NOTE: This functions do not require GPU access

{Load image from file into CPU memory (RAM)}
function LoadImage(fileName: PChar): TImage; cdecl; external cDllName;
{Load image from RAW file data}
function LoadImageRaw(fileName: PChar; width, height, format, headerSize: Integer): TImage; cdecl; external cDllName;
{Load image sequence from file (frames appended to image.data)}
function LoadImageAnim(fileName: PChar; frames: PInteger):TImage; cdecl; external cDllName;
{Load image from memory buffer, fileType refers to extension: i.e. '.png'}
function LoadImageFromMemory(fileType: PChar; fileData: Pointer; dataSize: Integer): TImage; cdecl; external cDllName;
{Load image from GPU texture data}
function LoadImageFromTexture(texture: TTexture2D): TImage; cdecl; external cDllName;
{Load image from screen buffer and (screenshot)}
function LoadImageFromScreen: TImage; cdecl; external cDllName;
{Unload image from CPU memory (RAM)}
procedure UnloadImage(image: TImage); cdecl; external cDllName;
{Export image data to file, returns true on success}
function ExportImage(image: TImage; fileName: PChar): Boolean; cdecl; external cDllName;
{Export image as code file defining an array of bytes, returns true on success  }
function ExportImageAsCode(image: TImage; fileName: PChar): Boolean; cdecl; external cDllName;


(* Image generation functions *)

{Generate image: plain color}
function GenImageColor(width, height: Integer; color: TColorB): TImage; cdecl; external cDllName;
{Generate image: vertical gradient}
function GenImageGradientV(width, height: Integer; top, bottom: TColorB): TImage; cdecl; external cDllName;
{Generate image: horizontal gradient}
function GenImageGradientH(width, height: Integer; left, right: TColorB): TImage; cdecl; external cDllName;
{Generate image: radial gradient}
function GenImageGradientRadial(width, height: Integer; density: Single; inner, outer: TColorB): TImage; cdecl; external cDllName;
{Generate image: checked}
function GenImageChecked(width, height, checksX, checksY: Integer; col1, col2: TColorB): TImage; cdecl; external cDllName;
{Generate image: white noise}
function GenImageWhiteNoise(width, height: Integer; factor: Single): TImage; cdecl; external cDllName;
{Generate image: cellular algorithm, bigger tileSize means bigger cells}
function GenImageCellular(width, height, tileSize: Integer): TImage; cdecl; external cDllName;


(* Image manipulation functions *)

{Create an image duplicate (useful for transformations)}
function ImageCopy(image: TImage): TImage; cdecl; external cDllName;
{Create an image from another image piece}
function ImageFromImage(image: TImage; rec: TRectangle): TImage; cdecl; external cDllName;
{Create an image from text (default font)}
function ImageText(text: PChar; fontSize: Integer; color: TColorB): TImage; cdecl; external cDllName;
{Create an image from text (custom sprite font)}
function ImageTextEx(font: TFont; text: PChar; fontSize, spacing: Single; tint: TColorB): TImage; cdecl; external cDllName;
{Convert image data to desired format}
procedure ImageFormat(image: PImage; newFormat: Integer); cdecl; external cDllName;
{Convert image to POT (power-of-two)}
procedure ImageToPOT(image: PImage; fill: TColorB); cdecl; external cDllName;
{Crop an image to a defined rectangle}
procedure ImageCrop(image: PImage; crop: TRectangle); cdecl; external cDllName;
{Crop image depending on alpha value}
procedure ImageAlphaCrop(image: PImage; threshold: Single); cdecl; external cDllName;
{Clear alpha channel to desired color}
procedure ImageAlphaClear(image: PImage; color: TColorB; threshold: Single); cdecl; external cDllName;
{Apply alpha mask to image}
procedure ImageAlphaMask(image: PImage; alphaMask: TImage); cdecl; external cDllName;
{Premultiply alpha channel}
procedure ImageAlphaPremultiply(image: PImage); cdecl; external cDllName;
{Resize image (Bicubic scaling algorithm)}
procedure ImageResize(image: PImage; newWidth, newHeight: Integer); cdecl; external cDllName;
{Resize image (Nearest-Neighbor scaling algorithm)}
procedure ImageResizeNN(image: PImage; newWidth, newHeight: Integer); cdecl; external cDllName;
{Resize canvas and fill with color}
procedure ImageResizeCanvas(image: PImage; newWidth, newHeight, offsetX, offsetY: Integer; fill: TColorB); cdecl; external cDllName;
{Compute all mipmap levels for a provided image}
procedure ImageMipmaps(image: PImage); cdecl; external cDllName;
{Dither image data to 16bpp or lower (Floyd-Steinberg dithering)}
procedure ImageDither(image: PImage; rBpp, gBpp, bBpp, aBpp: Integer); cdecl; external cDllName;
{Flip image vertically}
procedure ImageFlipVertical(image: PImage); cdecl; external cDllName;
{Flip image horizontally}
procedure ImageFlipHorizontal(image: PImage); cdecl; external cDllName;
{Rotate image clockwise 90deg}
procedure ImageRotateCW(image: PImage); cdecl; external cDllName;
{Rotate image counter-clockwise 90deg}
procedure ImageRotateCCW(image: PImage); cdecl; external cDllName;
{Modify image color: tint}
procedure ImageColorTint(image: PImage; color: TColorB); cdecl; external cDllName;
{Modify image color: invert}
procedure ImageColorInvert(image: PImage); cdecl; external cDllName;
{Modify image color: grayscale}
procedure ImageColorGrayscale(image: PImage); cdecl; external cDllName;
{Modify image color: contrast (-100 to 100)}
procedure ImageColorContrast(image: PImage; contrast: Single); cdecl; external cDllName;
{Modify image color: brightness (-255 to 255)}
procedure ImageColorBrightness(image: PImage; brightness: Single); cdecl; external cDllName;
{Modify image color: replace color}
procedure ImageColorReplace(image: PImage; color, replace: TColorB); cdecl; external cDllName;
{Load color data from image as a Color array (RGBA - 32bit)}
function LoadImageColors(image: TImage): PColorB; cdecl; external cDllName;
{Load colors palette from image as a Color array (RGBA - 32bit)}
function LoadImagePalette(image: TImage; maxPaletteSize: Integer; colorCount: PInteger): PColorB; cdecl; external cDllName;
{Unload color data loaded with LoadImageColors()}
procedure UnloadImageColors(colors: PColorB); cdecl; external cDllName;
{Unload colors palette loaded with LoadImagePalette()}
procedure UnloadImagePalette(colors: PColorB); cdecl; external cDllName;
{Get image alpha border rectangle}
function GetImageAlphaBorder(image: TImage; threshold: Single): TRectangle; cdecl; external cDllName;
{Get image pixel color at (x, y) position}
function GetImageColor(image: TImage; x, y: Integer): TColorB; cdecl; external cDllName;

(* Image drawing functions *)
// NOTE: Image software-rendering functions (CPU)

{Clear image background with given color}
procedure ImageClearBackground(dst: PImage; color: TColorB); cdecl; external cDllName;
{Draw pixel within an image}
procedure ImageDrawPixel(dst: PImage; posX, posY: Integer; color: TColorB); cdecl; external cDllName;
{Draw pixel within an image (Vector version)}
procedure ImageDrawPixelV(dst: PImage; position: TVector2; color: TColorB); cdecl; external cDllName;
{Draw line within an image}
procedure ImageDrawLine(dst: PImage; startPosX, startPosY, endPosX, endPosY: Integer; color: TColorB); cdecl; external cDllName;
{Draw line within an image (Vector version)}
procedure ImageDrawLineV(dst: PImage; start, _end: TVector2; color: TColorB); cdecl; external cDllName;
{Draw circle within an image}
procedure ImageDrawCircle(dst: PImage; centerX, centerY, radius: Integer; color: TColorB); cdecl; external cDllName;
{Draw circle within an image (Vector version)}
procedure ImageDrawCircleV(dst: PImage; center: TVector2; radius: Integer; color: TColorB); cdecl; external cDllName;
{Draw rectangle within an image}
procedure ImageDrawRectangle(dst: PImage; posX, posY, width, height: Integer; color: TColorB);cdecl;external cDllName;
{Draw rectangle within an image (Vector version)}
procedure ImageDrawRectangleV(dst: PImage; position, size: TVector2; color: TColorB); cdecl; external cDllName;
{Draw rectangle within an image}
procedure ImageDrawRectangleRec(dst: PImage; rec: TRectangle; color: TColorB); cdecl; external cDllName;
{Draw rectangle lines within an image}
procedure ImageDrawRectangleLines(dst: PImage; rec: TRectangle; thick: Integer; color: TColorB); cdecl; external cDllName;
{Draw a source image within a destination image (tint applied to source)}
procedure ImageDraw(dst: PImage; src: TImage; srcRec, dstRec: TRectangle; tint: TColorB); cdecl; external cDllName;
{Draw text (using default font) within an image (destination)}
procedure ImageDrawText(dst: PImage; text: PChar; posX, posY, fontSize: Integer; color:TColorB); cdecl; external cDllName;
{Draw text (custom sprite font) within an image (destination)}
procedure ImageDrawTextEx(dst: PImage; font: TFont; text: PChar; position: TVector2; fontSize, spacing: Single; tint: TColorB); cdecl; external cDllName;

(* Texture loading functions *)
// NOTE: These functions require GPU access

{Load texture from file into GPU memory (VRAM)}
function LoadTexture(fileName: PChar): TTexture2D; cdecl; external cDllName;
{Load texture from image data}
function LoadTextureFromImage(image: TImage): TTexture2D; cdecl; external cDllName;
{Load cubemap from image, multiple image cubemap layouts supported}
function LoadTextureCubemap(image: TImage; layout: Integer): TTextureCubemap; cdecl; external cDllName;
{Load texture for rendering (framebuffer)}
function LoadRenderTexture(width, height: Integer): TRenderTexture2D; cdecl; external cDllName;
{Unload texture from GPU memory (VRAM)}
procedure UnloadTexture(texture: TTexture2D); cdecl; external cDllName;
{Unload render texture from GPU memory (VRAM)}
procedure UnloadRenderTexture(target: TRenderTexture2D); cdecl; external cDllName;
{Update GPU texture with new data}
procedure UpdateTexture(texture: TTexture2D; pixels: Pointer); cdecl; external cDllName;
{Update GPU texture rectangle with new data}
procedure UpdateTextureRec(texture: TTexture2D; rec: TRectangle; pixels: Pointer); cdecl; external cDllName;


(* Texture configuration functions *)

{Generate GPU mipmaps for a texture}
procedure GenTextureMipmaps(texture: PTexture2D); cdecl; external cDllName;
{Set texture scaling filter mode}
procedure SetTextureFilter(texture: TTexture2D; filter: Integer); cdecl; external cDllName;
{Set texture wrapping mode}
procedure SetTextureWrap(texture: TTexture2D; wrap: Integer); cdecl; external cDllName;


(* Texture drawing functions *)

{Draw a Texture2D}
procedure DrawTexture(texture: TTexture2D; posX, posY: Integer; tint: TColorB); cdecl; external cDllName;
{Draw a Texture2D with position defined as Vector2}
procedure DrawTextureV(texture: TTexture2D; position: TVector2; tint: TColorB); cdecl; external cDllName;
{Draw a Texture2D with extended parameters}
procedure DrawTextureEx(texture: TTexture2D; position: TVector2; rotation, scale: Single; tint: TColorB); cdecl; external cDllName;
{Draw a part of a texture defined by a rectangle}
procedure DrawTextureRec(texture: TTexture2D; source: TRectangle; position: TVector2; tint: TColorB); cdecl; external cDllName;
{Draw texture quad with tiling and offset parameters}
procedure DrawTextureQuad(texture: TTexture2D; tiling, offset: TVector2; quad: TRectangle; tint: TColorB); cdecl; external cDllName;
{Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.}
procedure DrawTextureTiled(texture: TTexture2D; source, dest: TRectangle; origin: TVector2; rotation, scale: Single; tint: TColorB); cdecl; external cDllName;
{Draw a part of a texture defined by a rectangle with 'pro' parameters}
procedure DrawTexturePro(texture: TTexture2D; source, dest: TRectangle; origin: TVector2; rotation: Single; tint: TColorB); cdecl; external cDllName;
{Draws a texture (or part of it) that stretches or shrinks nicely}
procedure DrawTextureNPatch(texture: TTexture2D; nPatchInfo: TNPatchInfo; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColorB);cdecl;external cDllName;
{Draw a textured polygon}
procedure DrawTexturePoly(texture: TTexture2D; center: TVector2; points: PVector2; texcoords: PVector2; pointCount: Integer; tint: TColorB); cdecl; external cDllName;


(* Color/pixel related functions *)

{Get color with alpha applied, alpha goes from 0.0f to 1.0f}
function Fade(color: TColorB; alpha: Single): TColorB; cdecl; external cDllName;
{Get hexadecimal value for a Color}
function ColorToInt(color: TColorB): Integer; cdecl; external cDllName;
{Get Color normalized as float [0..1]}
function ColorNormalize(color: TColorB): TVector4; cdecl; external cDllName;
{Get Color from normalized values [0..1]}
function ColorFromNormalized(normalized: TVector4): TColorB; cdecl; external cDllName;
{Get HSV values for a Color, hue [0..360], saturation/value [0..1]}
function ColorToHSV(color: TColorB): TVector3; cdecl; external cDllName;
{Get a Color from HSV values, hue [0..360], saturation/value [0..1]}
function ColorFromHSV(hue, saturation, value: Single): TColorB; cdecl; external cDllName;
{Get color with alpha applied, alpha goes from 0.0f to 1.0f}
function ColorAlpha(color: TColorB; alpha: Single): TColorB; cdecl; external cDllName;
{Get src alpha-blended into dst color with tint}
function ColorAlphaBlend(dst, src, tint: TColorB): TColorB; cdecl; external cDllName;
{Get Color structure from hexadecimal value}
function GetColor(hexValue: LongWord): TColorB; cdecl; external cDllName;
{Get Color from a source pixel pointer of certain format}
function GetPixelColor(srcPtr: Pointer; format: Integer): TColorB; cdecl; external cDllName;
{Set color formatted into destination pixel pointer}
procedure SetPixelColor(dstPtr: Pointer; color: TColorB; format: Integer); cdecl; external cDllName;
{Get pixel data size in bytes for certain format}
function GetPixelDataSize(width, height, format: Integer): Integer; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// TFont Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

(* Font loading/unloading functions *)

{Get the default Font}
function GetFontDefault: TFont; cdecl; external cDllName;
{Load font from file into GPU memory (VRAM)}
function LoadFont(fileName: PChar): TFont; cdecl; external cDllName;
{Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set}
function LoadFontEx(fileName: Pchar; fontSize: Integer; fontChars: PInteger; glyphCount: Integer): TFont; cdecl; external cDllName;
{Load font from Image (XNA style)}
function LoadFontFromImage(image: TImage; key: TColorB; firstChar: Integer): TFont; cdecl; external cDllName;
{Load font from memory buffer, fileType refers to extension: i.e. '.ttf'}
function LoadFontFromMemory(fileType: PChar; fileData: Pointer; dataSize, fontSize: Integer; fontChars: PInteger; glyphCount: Integer): TFont; cdecl; external cDllName;
{Load font data for further uses}
function LoadFontData(fileData: Pointer; dataSize, fontSize: Integer; fontChars: PInteger; glyphCount, _type: Integer): PGlyphInfo; cdecl; external cDllName;
{Generate image font atlas using chars info}
function GenImageFontAtlas(chars: PGlyphInfo; recs: PPRectangle; glyphCount, fontSize, padding, packMethod: Integer): TImage; cdecl; external cDllName;
{Unload font chars info data (RAM)}
procedure UnloadFontData(chars: PGlyphInfo; glyphCount: Integer); cdecl; external cDllName;
{Unload Font from GPU memory (VRAM)}
procedure UnloadFont(font: TFont); cdecl; external cDllName;
{Export font as code file, returns true on success}
procedure ExportFontAsCode(font: TFont; fileName: PChar); cdecl; external cDllName;


(* Text drawing functions *)

{Draw current FPS}
procedure DrawFPS(posX, posY: Integer); cdecl; external cDllName;
{Draw text (using default font)}
procedure DrawText(text: PChar; posX, posY, fontSize: Integer; color: TColorB); cdecl; external cDllName;
{Draw text using font and additional parameters}
procedure DrawTextEx(font: TFont; text: PChar; position: TVector2; fontSize, spacing: Single; tint:TColorB); cdecl; external cDllName;
{Draw text using Font and pro parameters (rotation)}
procedure DrawTextPro(font: TFont; text: PChar; position, origin: TVector2; rotation, fontSize, spacing: Single; tint: TColorB); cdecl; external cDllName;
{Draw one character (codepoint)}
procedure DrawTextCodepoint(font: TFont; codepoint: Integer; position: TVector2; fontSize: Single; tint: TColorB); cdecl; external cDllName;
{Draw multiple character (codepoint)}
procedure DrawTextCodepoints(font: TFont; const codepoints: PInteger; count: Integer; position: TVector2; fontSize, spacing: Single; tint: TColorB); cdecl; external cDllName;


(* Text font info functions *)

{Measure string width for default font}
function MeasureText(text: PChar; fontSize: Integer): Integer; cdecl; external cDllName;
{Measure string size for Font}
function MeasureTextEx(font: TFont; text: PChar; fontSize, spacing: Single): TVector2; cdecl; external cDllName;
{Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphIndex(font: TFont; codepoint: Integer): Integer; cdecl; external cDllName;
{Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphInfo(font: TFont; codepoint: Integer): TGlyphInfo; cdecl; external cDllName;
{Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphAtlasRec(font: TFont; codepoint: Integer): TRectangle; cdecl; external cDllName;


(* Text codepoints management functions (unicode characters) *)

{Load all codepoints from a UTF-8 text string, codepoints count returned by parameter}
function LoadCodepoints(text: PChar; count: PInteger): PInteger; cdecl; external cDllName;
{Unload codepoints data from memory}
procedure UnloadCodepoints(codepoints: PInteger); cdecl; external cDllName;
{Get total number of codepoints in a UTF-8 encoded string}
function GetCodepointCount(text: PChar): Integer; cdecl; external cDllName;
{Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure}
function GetCodepoint(text: PChar; bytesProcessed: PInteger): Integer; cdecl; external cDllName;
{Encode one codepoint into UTF-8 byte array (array length returned as parameter)}
function CodepointToUTF8(codepoint: Integer; byteSize: PInteger): PChar; cdecl; external cDllName;
{Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)}
function TextCodepointsToUTF8(const codepoints: PInteger; length: Integer): PChar; cdecl; external cDllName;


(* Text strings management functions (no UTF-8 strings, only byte chars) *)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!

{Copy one string to another, returns bytes copied}
function TextCopy(dst, src: PChar): Integer; cdecl; external cDllName;
{Check if two text string are equal}
function TextIsEqual(text1, text2: PChar): Boolean; cdecl; external cDllName;
{Get text length, checks for '\0' ending}
function TextLength(text: PChar): LongWord; cdecl; external cDllName;

{Text formatting with variables (sprintf() style)}
function TextFormat(text: PChar): PChar; cdecl; varargs; external cDllName;
{Get a piece of a text string}
function TextSubtext(text: PChar; position, length: Integer): PChar; cdecl; external cDllName;
{Replace text string (WARNING: memory must be freed!)}
function TextReplace(text, replace, by: PChar): PChar; cdecl; external cDllName;
{Insert text in a position (WARNING: memory must be freed!)}
function TextInsert(text, insert: PChar; position: Integer): PChar; cdecl; external cDllName;
{Join text strings with delimiter}
function TextJoin(textList: PPChar; count: Integer; delimiter: PChar): PChar; cdecl; external cDllName;
{Split text into multiple strings}
function TextSplit(text: PChar; delimiter: Char; count: PInteger): PPChar; cdecl; external cDllName;
{Append text at specific position and move cursor!}
procedure TextAppend(text, append: PChar; position: PInteger); cdecl; external cDllName;
{Find first text occurrence within a string}
function TextFindIndex(text, find: PChar): Integer; cdecl; external cDllName;
{Get upper case version of provided string}
function TextToUpper(text: PChar): PChar; cdecl; external cDllName;
{Get lower case version of provided string}
function TextToLower(text: PChar):PChar; cdecl; external cDllName;
{Get Pascal case notation version of provided string}
function TextToPascal(text: PChar): PChar; cdecl; external cDllName;
{Get integer value from text (negative values not supported)}
function TextToInteger(text: PChar): Integer; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Basic geometric 3D shapes drawing functions *)

{Draw a line in 3D world space}
procedure DrawLine3D(startPos, endPos: TVector3; color: TColorB); cdecl; external cDllName;
{Draw a point in 3D space, actually a small line}
procedure DrawPoint3D(position: TVector3; color: TColorB); cdecl; external cDllName;
{Draw a circle in 3D world space}
procedure DrawCircle3D(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColorB); cdecl; external cDllName;
{Draw a color-filled triangle (vertex in counter-clockwise order!)}
procedure DrawTriangle3D(v1, v2, v3: TVector3; color: TColorB); cdecl; external cDllName;
{Draw a triangle strip defined by points}
procedure DrawTriangleStrip3D(points: PVector3; pointCount: Integer; color: TColorB); cdecl; external cDllName;
{Draw cube}
procedure DrawCube(position: TVector3; width, height, length: Single; color: TColorB); cdecl; external cDllName;
{Draw cube (Vector version)}
procedure DrawCubeV(position, size: TVector3; color: TColorB); cdecl; external cDllName;
{Draw cube wires}
procedure DrawCubeWires(position: TVector3; width, height, length: Single; color: TColorB); cdecl; external cDllName;
{Draw cube wires (Vector version)}
procedure DrawCubeWiresV(position, size: TVector3; color: TColorB); cdecl; external cDllName;
{Draw cube textured}
procedure DrawCubeTexture(texture: TTexture2D; position: TVector3; width, height, length: Single; color: TColorB); cdecl; external cDllName;
{Draw cube with a region of a texture}
procedure DrawCubeTextureRec(texture: TTexture2D; source: TRectangle; position: TVector3; width, height, length: Single; color: TColorB); cdecl; external cDllName;
{Draw sphere}
procedure DrawSphere(centerPos: TVector3; radius: Single; color: TColorB); cdecl; external cDllName;
{Draw sphere with extended parameters}
procedure DrawSphereEx(centerPos: TVector3; radius: Single; rings, slices: Integer; color: TColorB); cdecl; external cDllName;
{Draw sphere wires}
procedure DrawSphereWires(centerPos: TVector3; radius: Single; rings, slices: Integer; color: TColorB); cdecl; external cDllName;
{Draw a cylinder/cone}
procedure DrawCylinder(position: TVector3; radiusTop, radiusBottom, height: Single; slices: Integer; color: TColorB); cdecl; external cDllName;
{Draw a cylinder with base at startPos and top at endPos}
procedure DrawCylinderEx(startPos, endPos: TVector3; startRadius, endRadius: Single; sides: Integer; color: TColorB); cdecl; external cDllName;
{Draw a cylinder/cone wires}
procedure DrawCylinderWires(position: TVector3; radiusTop, radiusBottom, height: Single; slices: Integer; color: TColorB); cdecl; external cDllName;
{Draw a cylinder wires with base at startPos and top at endPos}
procedure DrawCylinderWiresEx(startPos, endPos: TVector3; startRadius, endRadius: Single; sides: Integer; color: TColorB); cdecl; external cDllName;
{Draw a plane XZ}
procedure DrawPlane(centerPos: TVector3; size: TVector2; color: TColorB); cdecl; external cDllName;
{Draw a ray line}
procedure DrawRay(ray: TRay; color: TColorB); cdecl; external cDllName;
{Draw a grid (centered at (0, 0, 0))}
procedure DrawGrid(slices: Integer; spacing: Single); cdecl; external cDllName;

//------------------------------------------------------------------------------------
// TModel 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Model management functions *)

{Load model from files (meshes and materials)}
function LoadModel(fileName: PChar): TModel; cdecl; external cDllName;
{Load model from generated mesh (default material)}
function LoadModelFromMesh(mesh: TMesh): TModel; cdecl; external cDllName;
{Unload model (including meshes) from memory (RAM and/or VRAM)}
procedure UnloadModel(model: TModel); cdecl; external cDllName;
{Unload model (but not meshes) from memory (RAM and/or VRAM)}
procedure UnloadModelKeepMeshes(model: TModel); cdecl; external cDllName;
{Compute model bounding box limits (considers all meshes)}
function GetModelBoundingBox(model: TModel): TBoundingBox; cdecl; external cDllName;


(* Model drawing functions *)

{Draw a model (with texture if set)}
procedure DrawModel(model: TModel; position: TVector3; scale: Single; tint: TColorB); cdecl; external cDllName;
{Draw a model with extended parameters}
procedure DrawModelEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB); cdecl; external cDllName;
{Draw a model wires (with texture if set)}
procedure DrawModelWires(model: TModel; position: TVector3; scale: Single; tint: TColorB); cdecl; external cDllName;
{Draw a model wires (with texture if set) with extended parameters}
procedure DrawModelWiresEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB); cdecl; external cDllName;
{Draw bounding box (wires)}
procedure DrawBoundingBox(box: TBoundingBox; color: TColorB); cdecl; external cDllName;
{Draw a billboard texture}
procedure DrawBillboard(camera: TCamera; texture: TTexture2D; position: TVector3; size: Single; tint: TColorB); cdecl; external cDllName;
{Draw a billboard texture defined by source}
procedure DrawBillboardRec(camera: TCamera; texture: TTexture2D; source: TRectangle; position: TVector3; size: TVector2; tint: TColorB); cdecl; external cDllName;
{Draw a billboard texture defined by source and rotation}
procedure DrawBillboardPro(camera: TCamera; texture: TTexture2D; source: TRectangle; position, up: TVector3; size, origin: TVector2; rotation: Single; tint: TColorB); cdecl; external cDllName;


(* Mesh management functions *)

{Upload mesh vertex data in GPU and provide VAO/VBO ids}
procedure UploadMesh(mesh: PMesh; _dynamic: Boolean); cdecl; external cDllName;
{Update mesh vertex data in GPU for a specific buffer index}
procedure UpdateMeshBuffer(mesh: TMesh; index: Integer; const data: Pointer; dataSize, offset: Integer); cdecl; external cDllName;
{Unload mesh data from CPU and GPU}
procedure UnloadMesh(mesh: TMesh); cdecl; external cDllName;
{Draw a 3d mesh with material and transform}
procedure DrawMesh(mesh: TMesh; material: TMaterial; transform: TMatrix); cdecl; external cDllName;
{Draw multiple mesh instances with material and different transforms}
procedure DrawMeshInstanced(mesh: TMesh; material: TMaterial;const transforms: PMatrix; instances: Integer); cdecl; external cDllName;
{Export mesh data to file, returns true on success}
function ExportMesh(mesh: TMesh; fileName: PChar): Boolean; cdecl; external cDllName;
{Compute mesh bounding box limits}
function GetMeshBoundingBox(mesh: TMesh): TBoundingBox; cdecl; external cDllName;
{Compute mesh tangents}
procedure GenMeshTangents(mesh: PMesh); cdecl; external cDllName;

(* Mesh generation functions *)

{Generate polygonal mesh}
function GenMeshPoly(sides: Integer; radius: Single): TMesh; cdecl; external cDllName;
{Generate plane mesh (with subdivisions)}
function GenMeshPlane(width, length: Single; resX, resZ: Integer): TMesh; cdecl; external cDllName;
{Generate cuboid mesh}
function GenMeshCube(width, height, length: Single): TMesh; cdecl; external cDllName;
{Generate sphere mesh (standard sphere)}
function GenMeshSphere(radius: Single; rings, slices: Integer): TMesh; cdecl; external cDllName;
{Generate half-sphere mesh (no bottom cap)}
function GenMeshHemiSphere(radius: Single; rings, slices: Integer): TMesh; cdecl; external cDllName;
{Generate cylinder mesh}
function GenMeshCylinder(radius, height: Single; slices: Integer): TMesh; cdecl; external cDllName;
{Generate cone/pyramid mesh}
function GenMeshCone(radius, height: Single; slices: Integer): TMesh; cdecl; external cDllName;
{Generate torus mesh}
function GenMeshTorus(radius, size: Single; radSeg, sides: Integer): TMesh; cdecl; external cDllName;
{Generate trefoil knot mesh}
function GenMeshKnot(radius, size: Single; radSeg, sides: Integer): TMesh; cdecl; external cDllName;
{Generate heightmap mesh from image data}
function GenMeshHeightmap(heightmap: TImage; size: TVector3): TMesh; cdecl; external cDllName;
{Generate cubes-based map mesh from image data}
function GenMeshCubicmap(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl; external cDllName;


(* Material loading/unloading functions *)

{Load materials from model file}
function LoadMaterials(fileName: PChar; materialCount: PInteger): PMaterial; cdecl; external cDllName;
{Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)}
function LoadMaterialDefault: TMaterial; cdecl; external cDllName;
{Unload material from GPU memory (VRAM)}
procedure UnloadMaterial(material: TMaterial); cdecl; external cDllName;
{Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)}
procedure SetMaterialTexture(material: PMaterial; mapType: Integer; texture: TTexture2D); cdecl; external cDllName;
{Set material for a mesh}
procedure SetModelMeshMaterial(model: PModel; meshId, materialId: Integer); cdecl; external cDllName;


(* Model animations loading/unloading functions *)

{Load model animations from file}
function LoadModelAnimations(fileName: PChar; animCount: PLongWord): PModelAnimation; cdecl; external cDllName;
{Update model animation pose}
procedure UpdateModelAnimation(model: TModel; anim: TModelAnimation; frame: Integer); cdecl; external cDllName;
{Unload animation data}
procedure UnloadModelAnimation(anim: TModelAnimation); cdecl; external cDllName;
{Unload animation array data}
procedure UnloadModelAnimations(animations: PModelAnimation; count: LongWord); cdecl; external cDllName;
{Check model animation skeleton match}
function IsModelAnimationValid(model: TModel; anim: TModelAnimation): Boolean; cdecl; external cDllName;


(* Collision detection functions *)

{Check collision between two spheres}
function CheckCollisionSpheres(center1: TVector3; radius1: Single; center2: TVector3; radius2: Single): Boolean; cdecl; external cDllName;
{Check collision between two bounding boxes}
function CheckCollisionBoxes(box1, box2: TBoundingBox): Boolean; cdecl; external cDllName;
{Check collision between box and sphere}
function CheckCollisionBoxSphere(box: TBoundingBox; center: TVector3; radius: Single): Boolean; cdecl; external cDllName;
{Get collision info between ray and sphere}
function GetRayCollisionSphere(ray:TRay; center: TVector3; radius: Single): TRayCollision; cdecl; external cDllName;
{Get collision info between ray and box}
function GetRayCollisionBox(ray: TRay; box: TBoundingBox): TRayCollision; cdecl; external cdllName;
{Get collision info between ray and mesh}
function GetRayCollisionMesh(ray: TRay; mesh: TMesh; transform: TMatrix): TRayCollision; cdecl; external cdllName;
{Get collision info between ray and triangle}
function GetRayCollisionTriangle(ray: TRay; p1, p2, p3: TVector3): TRayCollision; cdecl; external cDllName;
{Get collision info between ray and quad}
function GetRayCollisionQuad(ray: TRay; p1, p2, p3, p4: TVector3): TRayCollision; cdecl; external cDllName;

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------
type
PAudioCallback = ^TAudioCallback;
TAudioCallback = procedure (bufferData: Pointer; frames: LongWord); cdecl;

(* Audio device management functions *)

{Initialize audio device and context}
procedure InitAudioDevice; cdecl; external cDllName;
{Close the audio device and context}
procedure CloseAudioDevice; cdecl; external cDllName;
{Check if audio device has been initialized successfully}
function IsAudioDeviceReady: Boolean; cdecl; external cDllName;
{Set master volume (listener)}
procedure SetMasterVolume(volume: Single); cdecl; external cDllName;


(* Wave/Sound loading/unloading functions *)

{Load wave data from file}
function LoadWave(fileName: PChar): TWave; cdecl; external cDllName;
{Load wave from memory buffer, fileType refers to extension: i.e. '.wav' }
function LoadWaveFromMemory(fileType: PChar; fileData: Pointer; dataSize: Integer): TWave; cdecl; external cdllName;
{Load sound from file}
function LoadSound(fileName: PChar): TSound; cdecl; external cDllName;
{Load sound from wave data}
function LoadSoundFromWave(wave: TWave): TSound; cdecl; external cDllName;
{Update sound buffer with new data}
procedure UpdateSound(sound: TSound; data: Pointer; sampleCount: Integer); cdecl; external cdllName;
{Unload wave data}
procedure UnloadWave(wave: TWave); cdecl; external cDllName;
{Unload sound}
procedure UnloadSound(sound: TSound); cdecl; external cDllName;
{Export wave data to file, returns true on success}
function ExportWave(wave: TWave; fileName: PChar): Boolean; cdecl; external cDllName;
{Export wave sample data to code (.h), returns true on success}
function ExportWaveAsCode(wave: TWave; fileName: PChar): Boolean; cdecl; external cDllName;


(* Wave/Sound management functions *)

{Play a sound}
procedure PlaySound(sound: TSound); cdecl; external cDllName;
{Stop playing a sound}
procedure StopSound(sound: TSound); cdecl; external cDllName;
{Pause a sound}
procedure PauseSound(sound: TSound); cdecl; external cDllName;
{Resume a paused sound}
procedure ResumeSound(sound: TSound); cdecl; external cDllName;
{Play a sound (using multichannel buffer pool)}
procedure PlaySoundMulti(sound: TSound); cdecl; external cDllName;
{Stop any sound playing (using multichannel buffer pool)}
procedure StopSoundMulti; cdecl; external cDllName;
{Get number of sounds playing in the multichannel}
function GetSoundsPlaying: Integer; cdecl; external cDllName;
{Check if a sound is currently playing}
function IsSoundPlaying(sound: TSound): Boolean; cdecl; external cDllName;
{Set volume for a sound (1.0 is max level)}
procedure SetSoundVolume(sound: TSound; volume: Single); cdecl; external cDllName;
{Set pitch for a sound (1.0 is base level)}
procedure SetSoundPitch(sound: TSound; pitch: Single); cdecl; external cDllName;
{Set pan for a sound (0.5 is center)}
procedure SetSoundPan(sound: TSound; pan: Single); cdecl; external cDllName;
{Copy a wave to a new wave}
function WaveCopy(wave: TWave): TWave; cdecl; external cDllName;
{Crop a wave to defined samples range}
procedure WaveCrop(wave: PWave; initSample, finalSample: Integer); cdecl; external cDllName;
{Convert wave data to desired format}
procedure WaveFormat(wave: PWave; sampleRate, sampleSize, channels: Integer); cdecl; external cDllName;
{Load samples data from wave as a floats array}
function LoadWaveSamples(wave: TWave): PSingle; cdecl; external cDllName;
{Unload samples data loaded with LoadWaveSamples()}
procedure UnloadWaveSamples(samples: PSingle); cdecl; external cDllName;


(* Music management functions *)

{Load music stream from file}
function LoadMusicStream(fileName: PChar): TMusic; cdecl; external cDllName;
{Load music stream from data}
function LoadMusicStreamFromMemory(fileType: PChar; const data: Pointer; dataSize: Integer): TMusic; cdecl; external cDllName;
{Unload music stream}
procedure UnloadMusicStream(music: TMusic); cdecl; external cDllName;
{Start music playing}
procedure PlayMusicStream(music: TMusic); cdecl; external cDllName;
{Check if music is playing}
function IsMusicStreamPlaying(music: TMusic): Boolean; cdecl; external cDllName;
{Updates buffers for music streaming}
procedure UpdateMusicStream(music: TMusic); cdecl; external cDllName;
{Stop music playing}
procedure StopMusicStream(music: TMusic); cdecl; external cDllName;
{Pause music playing}
procedure PauseMusicStream(music: TMusic); cdecl; external cDllName;
{Resume playing paused music}
procedure ResumeMusicStream(music: TMusic); cdecl; external cDllName;
{Seek music to a position (in seconds)}
procedure SeekMusicStream(music: TMusic; position: Single); cdecl; external cDllName;
{Set volume for music (1.0 is max level)}
procedure SetMusicVolume(music: TMusic; volume: Single); cdecl; external cDllName;
{Set pitch for a music (1.0 is base level)}
procedure SetMusicPitch(music: TMusic; pitch: Single); cdecl; external cDllName;
{Set pan for a music (0.5 = center)}
procedure SetMusicPan(music: TMusic; pan: Single); cdecl; external cDllName;
{Get music time length (in seconds)}
function GetMusicTimeLength(music: TMusic): Single; cdecl; external cDllName;
{Get current music time played (in seconds)}
function GetMusicTimePlayed(music: TMusic): Single; cdecl; external cDllName;


(* AudioStream management functions *)

{Load audio stream (to stream raw audio pcm data)}
function LoadAudioStream(sampleRate, sampleSize, channels: LongWord): TAudioStream; cdecl; external cDllName;
{Unload audio stream and free memory}
procedure UnloadAudioStream(stream: TAudioStream); cdecl; external cDllName;
{Update audio stream buffers with data}
procedure UpdateAudioStream(stream: TAudioStream; data: Pointer; frameCount: Integer); cdecl; external cDllName;
{Check if any audio stream buffers requires refill}
function IsAudioStreamProcessed(stream: TAudioStream): Boolean; cdecl; external cDllName;
{Play audio stream}
procedure PlayAudioStream(stream: TAudioStream); cdecl; external cDllName;
{Pause audio stream}
procedure PauseAudioStream(stream: TAudioStream); cdecl; external cDllName;
{Resume audio stream}
procedure ResumeAudioStream(stream: TAudioStream); cdecl; external cDllName;
{Check if audio stream is playing}
function IsAudioStreamPlaying(stream: TAudioStream): Boolean; cdecl; external cDllName;
{Stop audio stream}
procedure StopAudioStream(stream: TAudioStream); cdecl; external cDllName;
{Set volume for audio stream (1.0 is max level)}
procedure SetAudioStreamVolume(stream: TAudioStream; volume: Single); cdecl; external cDllName;
{Set pitch for audio stream (1.0 is base level)}
procedure SetAudioStreamPitch(stream: TAudioStream; pitch: Single); cdecl; external cDllName;
{Set pan for audio stream (0.5 is centered)}
procedure SetAudioStreamPan(stream: TAudioStream; pan: Single); cdecl; external cDllName;
{Default size for new audio streams}
procedure SetAudioStreamBufferSizeDefault(size: Integer); cdecl; external cDllName;
{Audio thread callback to request new data}
procedure SetAudioStreamCallback(stream: TAudioStream; callback: TAudioCallback); cdecl; external cDllName;

{Attach audio stream processor to stream}
procedure AttachAudioStreamProcessor(stream: TAudioStream; processor: TAudioCallback); cdecl; external cDllName;
{Detach audio stream processor from stream}
procedure DetachAudioStreamProcessor(stream: TAudioStream; processor: TAudioCallback); cdecl; external cDllName;


(* Custom Misc Functions to help simplify a few things *)

function Vector2Create(aX: Single; aY: Single): TVector2;
procedure Vector2Set(aVec: PVector2; aX: Single; aY: Single);
function Vector3Create(aX: Single; aY: Single; aZ: Single): TVector3;
procedure Vector3Set(aVec: PVector3; aX: Single; aY: Single; aZ: Single);
function ColorCreate(aR: Byte; aG: Byte; aB: Byte; aA: Byte): TColorB;
procedure ColorSet(aColor: PColorB; aR: Byte; aG: Byte; aB: Byte; aA: Byte);

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single): TRectangle;
procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single; aWidth: Single; aHeight: Single);
function Camera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: Single; aType: Integer): TCamera3D;
procedure Camera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: Single; aType: Integer);

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

function ColorCreate(aR: byte; aG: byte; aB: byte; aA: byte): TColorB;
begin
  Result.r := aR;
  Result.g := aG;
  Result.b := aB;
  Result.a := aA;
end;

procedure ColorSet(aColor: PColorB; aR: byte; aG: byte; aB: byte; aA: byte);
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

