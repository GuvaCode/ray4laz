{
raylib ver 5.6-dev
A simple and easy-to-use library to enjoy videogames programming ( www.raylib.com )
Pascal header by Gunko Vadim (@guvacode)
}
{$mode objfpc}{$H+}{$modeswitch advancedrecords}
unit raylib;
// Include configuration file
{$I raylib.inc}

interface

{$IFNDEF RAY_STATIC}
const
  cDllName =
             {$IFDEF WINDOWS} 'libraylib.dll'; {$IFEND}
             {$IFDEF UNIX}
             {$IFDEF DARWIN} 'libraylib.dylib';
             {$ELSE} 'libraylib.so'; {$IFEND}  // for Linux, FreeBSD, NetBSD, OpenBSD, DragonFly, Haiku
             {$IFEND}
{$ENDIF}


const
  DEG2RAD = (PI / 180.0);
  RAD2DEG = (180.0 / PI);

  //----------------------------------------------------------------------------------
// Some basic Defines }
//----------------------------------------------------------------------------------

 (* Color, 4 components, R8G8B8A8 (32bit) *)
 type
   { TColorB }
   PColorB = ^TColorB;
   TColorB = record
     r,g,b,a : byte; // Color value
     class operator = (aColor, bColor: TColorB): Boolean;
     procedure Create(aR: Byte; aG: Byte; aB: Byte; aA: Byte);
   end;
   TColorBData = array[0..3] of Byte;

  TColor = TColorB;
  PColor = PColorB;

const
  // Some Basic Colors
  // NOTE: Custom raylib color palette for amazing visuals on WHITE background
  LIGHTGRAY:      TColorB = (r: 200; g: 200; b: 200; a: 255); // Light Gray
  GRAY:           TColorB = (r: 130; g: 130; b: 130; a: 255); // Gray
  DARKGRAY:       TColorB = (r: 80; g: 80; b: 80; a: 255);    // Dark Gray
  YELLOW:         TColorB = (r: 253; g: 249; b: 0; a: 255);   // Yellow
  GOLD:           TColorB = (r: 255; g: 203; b: 0; a: 255);   // Gold
  ORANGE:         TColorB = (r: 255; g: 161; b: 0; a: 255);   // Orange
  PINK:           TColorB = (r: 255; g: 109; b: 194; a: 255); // Pink
  RED:            TColorB = (r: 230; g: 41; b: 55; a: 255);   // Red
  MAROON:         TColorB = (r: 190; g: 33; b: 55; a: 255);   // Maroon
  GREEN:          TColorB = (r: 0; g: 228; b: 48; a: 255);    // Green
  LIME:           TColorB = (r: 0; g: 158; b: 47; a: 255);    // Lime
  DARKGREEN:      TColorB = (r: 0; g: 117; b: 44; a: 255);    // Dark Green
  SKYBLUE:        TColorB = (r: 102; g: 191; b: 255; a: 255); // Sky Blue
  BLUE:           TColorB = (r: 0; g: 121; b: 241; a: 255);   // Blue
  DARKBLUE:       TColorB = (r: 0; g: 82; b: 172; a: 255);    // Dark Blue
  PURPLE:         TColorB = (r: 200; g: 122; b: 255; a: 255); // Purple
  VIOLET:         TColorB = (r: 135; g: 60; b: 190; a: 255);  // Violet
  DARKPURPLE:     TColorB = (r: 112; g: 31; b: 126; a: 255);  // Dark Purple
  BEIGE:          TColorB = (r: 211; g: 176; b: 131; a: 255); // Beige
  BROWN:          TColorB = (r: 127; g: 106; b: 79; a: 255);  // Brown
  DARKBROWN:      TColorB = (r: 76; g: 63; b: 47; a: 255);    // Dark beown
  WHITE:          TColorB = (r: 255; g: 255; b: 255; a: 255); // White
  BLACK:          TColorB = (r: 0; g: 0; b: 0; a: 255);       // Black
  BLANK:          TColorB = (r: 0; g: 0; b: 0; a: 0);         // Black(Transparent)
  MAGENTA:        TColorB = (r: 255; g: 0; b: 255; a: 255);   // Magenta
  RAYWHITE:       TColorB = (r: 245; g: 245; b: 245; a: 255); // My own White (raylib logo)


   type
    (* Vector2, 2 components *)
     PVector2 = ^TVector2;
     TVector2 = record
         x : single; // Vector x component
         y : single; // Vector y component
        // {$IFDEF ADVANCEDRECORDS}
         procedure Create(aX, aY: single);
        // {$ENDIF}
       end;
     TVector2Data = array[0..1] of Single;

     (* Vector3, 3 components *)
     PVector3 = ^TVector3;
     TVector3 = record
         x : single; // Vector x component
         y : single; // Vector y component
         z : single; // Vector z component
         procedure Create(aX, aY, aZ: single);
       end;
     TVector3Data = array[0..2] of Single;

     (* Vector4, 4 components *)
     PVector4 = ^TVector4;
     TVector4 = record
         x : single; // Vector x component
         y : single; // Vector y component
         z : single; // Vector z component
         w : single; // Vector w component
         procedure Create(aX, aY, aZ, aW: single);
       end;
     TVector4Data = array[0..3] of Single;

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

     { TRectangle }
     TRectangle = record
         x      : single; // Rectangle top-left corner position x
         y      : single; // Rectangle top-left corner position y
         width  : single; // Rectangle width
         height : single; // Rectangle height
         procedure Create(aX, aY, aWidth, aHeight: single);
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
         id      : LongWord; // OpenGL texture id
         width   : Integer;  // Texture base width
         height  : Integer;  // Texture base height
         mipmaps : Integer;  // Mipmap levels, 1 by default
         format  : Integer;  // Data format (PixelFormat type)
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
         id      : LongWord; // OpenGL framebuffer object id
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

     { TCamera3D }

     TCamera3D = record
       position   : TVector3; // Camera position
       target     : TVector3; // Camera target it looks-at
       up         : TVector3; // Camera up vector (rotation over its axis)
       fovy       : single;   // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
       projection : Integer;  // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
       procedure Create(aPosition, aTarget, aUp: TVector3; aFOVY: single; aProjection: integer = 0);
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
         vertexCount   : Integer;   // Number of vertices stored in arrays
         triangleCount : Integer;   // Number of triangles stored (indexed or not)
         // Vertex attributes data
         vertices      : PSingle;   // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
         texcoords     : PSingle;   // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
         texcoords2    : PSingle;   // Vertex texture second coordinates (UV - 2 components per vertex) (shader-location = 5)
         normals       : PSingle;   // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
         tangents      : PSingle;   // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
         colors        : PByte;     // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
         indices       : PWord;     // Vertex indices (in case vertex data comes indexed)
         // Animation vertex data
         animVertices  : PSingle;   // Animated vertex positions (after bones transformations)
         animNormals   : PSingle;   // Animated normals (after bones transformations)
         boneIds       : PByte;     // Vertex bone ids, up to 4 bones influence by vertex (skinning)
         boneWeights   : PSingle;   // Vertex bone weight, up to 4 bones influence by vertex (skinning)
         boneMatrices  : PMatrix;   // Bones animated transformation matrices
         boneCount     : Integer;          // Number of bones
         // OpenGL identifiers
         vaoId         : LongWord;  // OpenGL Vertex Array Object id
         vboId         : PLongWord; // OpenGL Vertex Buffer Objects id (default vertex data)
       end;

     (* Shader *)
     PShader = ^TShader;
     TShader = record
         id    : LongWord; // Shader program id
         locs  : PInteger; // Shader locations array (RL_MAX_SHADER_LOCATIONS)
       end;

     (* MaterialMap *)
     PMaterialMap = ^TMaterialMap;
     TMaterialMap = record
         texture : TTexture2D; // Material map texture
         color   : TColorB;    // Material map color
         value   : Single;     // Material map value
       end;

     (* Material, includes shader and maps *)
     PMaterial = ^TMaterial;
     TMaterial = record
         shader  : TShader;               // Material shader
         maps    : PMaterialMap;          // Material maps array (MAX_MATERIAL_MAPS)
         params  : array[0..3] of single; // Material generic parameters (if required)
       end;

     (* Transform, verctex transformation data *)
     PPTransform = ^PTransform;
     PTransform = ^TTransform;
     TTransform = record
         translation : TVector3;    // Translation
         rotation    : TQuaternion; // Rotation
         scale       : TVector3;    // Scale
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
         transform        : TMatrix;    // Local transform matrix
         meshCount        : Integer;    // Number of meshes
         materialCount    : Integer;    // Number of materials
         meshes           : PMesh;      // Meshes array
         materials        : PMaterial;  // Materials array
         meshMaterial     : PInteger;   // Mesh material number
         // Animation data
         boneCount        : Integer;    // Number of bones
         bones            : PBoneInfo;  // Bones information (skeleton)
         bindPose         : PTransform; // Bones base transformation (pose)
       end;

     (* ModelAnimation *)
     PModelAnimation = ^TModelAnimation;
     TModelAnimation = record
         boneCount : Integer;      // Number of bones
         frameCount : Integer;     // Number of animation frames
         bones : PBoneInfo;        // Bones information (skeleton)
         framePoses : PPTransform; // Poses array by frame
         name : array[0..31] of Char; // Animation name
       end;

      (* Ray, ray for raycasting *)
      PRay = ^TRay;
      TRay = record
         position  : TVector3; // Ray position (origin)
         direction : TVector3; // Ray direction (normalized)
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

     { TBoundingBox }

     TBoundingBox = record
         min : TVector3; // Minimum vertex box-corner
         max : TVector3; // Maximum vertex box-corner
         procedure Create(aMin, aMax: TVector3);
       end;

     (* Wave, audio wave data *)
     PWave = ^TWave;
     TWave = record
         frameCount : LongWord;   // Total number of frames (considering channels)
         sampleRate : LongWord;   // Frequency (samples per second)
         sampleSize : LongWord;   // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
         channels   : LongWord;   // Number of channels (1-mono, 2-stereo, ...)
         data       : Pointer;    // Buffer data pointer
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
         stream     : TAudioStream;    // Audio stream
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
       capacity  : LongWord; // Filepaths max entries
       count     : LongWord; // Filepaths entries count
       paths     : PPChar;   // Filepaths entries
     end;

     (* Automation event *)
     PAutomationEvent = ^TAutomationEvent;
     TAutomationEvent = record
       frame     : LongWord;               // Event frame
       type_     : LongWord;               // Event type (AutomationEventType)
       params    : array[0..3] of Integer; // Event parameters (if required)
     end;

     (* Automation event list *)
     PAutomationEventList = ^TAutomationEventList;
     TAutomationEventList = record
       capacity  : LongWord;          // Events max entries (MAX_AUTOMATION_EVENTS)
       count     : LongWord;          // Events entries count
       events    : PAutomationEvent;  // Events entries
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
         FLAG_VSYNC_HINT                 =  TConfigFlags($00000040); // Set to try enabling V-Sync on GPU
         FLAG_FULLSCREEN_MODE            =  TConfigFlags($00000002); // Set to run program in fullscreen
         FLAG_WINDOW_RESIZABLE           =  TConfigFlags($00000004); // Set to allow resizable window
         FLAG_WINDOW_UNDECORATED         =  TConfigFlags($00000008); // Set to disable window decoration (frame and buttons)
         FLAG_WINDOW_HIDDEN              =  TConfigFlags($00000080); // Set to hide window
         FLAG_WINDOW_MINIMIZED           =  TConfigFlags($00000200); // Set to minimize window (iconify)
         FLAG_WINDOW_MAXIMIZED           =  TConfigFlags($00000400); // Set to maximize window (expanded to monitor)
         FLAG_WINDOW_UNFOCUSED           =  TConfigFlags($00000800); // Set to window non focused
         FLAG_WINDOW_TOPMOST             =  TConfigFlags($00001000); // Set to window always on top
         FLAG_WINDOW_ALWAYS_RUN          =  TConfigFlags($00000100); // Set to allow windows running while minimized
         FLAG_WINDOW_TRANSPARENT         =  TConfigFlags($00000010); // Set to allow transparent framebuffer
         FLAG_WINDOW_HIGHDPI             =  TConfigFlags($00002000); // Set to support HighDPI
         FLAG_WINDOW_MOUSE_PASSTHROUGH   =  TConfigFlags($00004000); // Set to support mouse passthrough, only supported when FLAG_WINDOW_UNDECORATED
         FLAG_BORDERLESS_WINDOWED_MODE   =  TConfigFlags($00008000); // Set to run program in borderless windowed mode
         FLAG_MSAA_4X_HINT               =  TConfigFlags($00000020); // Set to try enabling MSAA 4X
         FLAG_INTERLACED_HINT            =  TConfigFlags($00010000); // Set to try enabling interlaced video format (for V3D)

     (* Trace log level *)
     // NOTE: Organized by priority level
     type
       PTraceLogLevel = ^TTraceLogLevel;
       TTraceLogLevel =  Integer;
       const
         LOG_ALL      =  TTraceLogLevel(0); // Display all logs
         LOG_TRACE    =  TTraceLogLevel(1); // Trace logging, intended for internal use only
         LOG_DEBUG    =  TTraceLogLevel(2); // Debug logging, used for internal debugging, it should be disabled on release builds
         LOG_INFO     =  TTraceLogLevel(3); // Info logging, used for program execution info
         LOG_WARNING  =  TTraceLogLevel(4); // Warning logging, used on recoverable failures
         LOG_ERROR    =  TTraceLogLevel(5); // Error logging, used on unrecoverable failures
         LOG_FATAL    =  TTraceLogLevel(6); // Fatal logging, used to abort program: exit(EXIT_FAILURE)
         LOG_NONE     =  TTraceLogLevel(7); // Disable logging

     (* Keyboard keys (US keyboard layout) *)
     // NOTE: Use GetKeyPressed() to allow redefining
     // required keys for alternative layouts
     type
       PKeyboardKey = ^TKeyboardKey;
       TKeyboardKey =  Integer;
       const
         (* Alphanumeric keys *)
         KEY_NULL              = TKeyboardKey(0);   // Key: NULL, used for no key pressed
         KEY_APOSTROPHE        = TKeyboardKey(39);  // Key: '
         KEY_COMMA             = TKeyboardKey(44);  // Key: ,
         KEY_MINUS             = TKeyboardKey(45);  // Key: -
         KEY_PERIOD            = TKeyboardKey(46);  // Key: .
         KEY_SLASH             = TKeyboardKey(47);  // Key: /
         KEY_ZERO              = TKeyboardKey(48);  // Key: 0
         KEY_ONE               = TKeyboardKey(49);  // Key: 1
         KEY_TWO               = TKeyboardKey(50);  // Key: 2
         KEY_THREE             = TKeyboardKey(51);  // Key: 3
         KEY_FOUR              = TKeyboardKey(52);  // Key: 4
         KEY_FIVE              = TKeyboardKey(53);  // Key: 5
         KEY_SIX               = TKeyboardKey(54);  // Key: 6
         KEY_SEVEN             = TKeyboardKey(55);  // Key: 7
         KEY_EIGHT             = TKeyboardKey(56);  // Key: 8
         KEY_NINE              = TKeyboardKey(57);  // Key: 9
         KEY_SEMICOLON         = TKeyboardKey(59);  // Key: ;
         KEY_EQUAL             = TKeyboardKey(61);  // Key: =
         KEY_A                 = TKeyboardKey(65);  // Key: A | a
         KEY_B                 = TKeyboardKey(66);  // Key: B | b
         KEY_C                 = TKeyboardKey(67);  // Key: C | c
         KEY_D                 = TKeyboardKey(68);  // Key: D | d
         KEY_E                 = TKeyboardKey(69);  // Key: E | e
         KEY_F                 = TKeyboardKey(70);  // Key: F | f
         KEY_G                 = TKeyboardKey(71);  // Key: G | g
         KEY_H                 = TKeyboardKey(72);  // Key: H | h
         KEY_I                 = TKeyboardKey(73);  // Key: I | i
         KEY_J                 = TKeyboardKey(74);  // Key: J | j
         KEY_K                 = TKeyboardKey(75);  // Key: K | k
         KEY_L                 = TKeyboardKey(76);  // Key: L | l
         KEY_M                 = TKeyboardKey(77);  // Key: M | m
         KEY_N                 = TKeyboardKey(78);  // Key: N | n
         KEY_O                 = TKeyboardKey(79);  // Key: O | o
         KEY_P                 = TKeyboardKey(80);  // Key: P | p
         KEY_Q                 = TKeyboardKey(81);  // Key: Q | q
         KEY_R                 = TKeyboardKey(82);  // Key: R | r
         KEY_S                 = TKeyboardKey(83);  // Key: S | s
         KEY_T                 = TKeyboardKey(84);  // Key: T | t
         KEY_U                 = TKeyboardKey(85);  // Key: U | u
         KEY_V                 = TKeyboardKey(86);  // Key: V | v
         KEY_W                 = TKeyboardKey(87);  // Key: W | w
         KEY_X                 = TKeyboardKey(88);  // Key: X | x
         KEY_Y                 = TKeyboardKey(89);  // Key: Y | y
         KEY_Z                 = TKeyboardKey(90);  // Key: Z | z
         KEY_LEFT_BRACKET      = TKeyboardKey(91);  // Key: [
         KEY_BACKSLASH         = TKeyboardKey(92);  // Key: '\'
         KEY_RIGHT_BRACKET     = TKeyboardKey(93);  // Key: ]
         KEY_GRAVE             = TKeyboardKey(96);  // Key: `
         (* Function keys *)
         KEY_SPACE             = TKeyboardKey(32);  // Key: Space
         KEY_ESCAPE            = TKeyboardKey(256); // Key: Esc
         KEY_ENTER             = TKeyboardKey(257); // Key: Enter
         KEY_TAB               = TKeyboardKey(258); // Key: Tab
         KEY_BACKSPACE         = TKeyboardKey(259); // Key: Backspace
         KEY_INSERT            = TKeyboardKey(260); // Key: Ins
         KEY_DELETE            = TKeyboardKey(261); // Key: Del
         KEY_RIGHT             = TKeyboardKey(262); // Key: Cursor right
         KEY_LEFT              = TKeyboardKey(263); // Key: Cursor left
         KEY_DOWN              = TKeyboardKey(264); // Key: Cursor down
         KEY_UP                = TKeyboardKey(265); // Key: Cursor up
         KEY_PAGE_UP           = TKeyboardKey(266); // Key: Page up
         KEY_PAGE_DOWN         = TKeyboardKey(267); // Key: Page down
         KEY_HOME              = TKeyboardKey(268); // Key: Home
         KEY_END               = TKeyboardKey(269); // Key: End
         KEY_CAPS_LOCK         = TKeyboardKey(280); // Key: Caps lock
         KEY_SCROLL_LOCK       = TKeyboardKey(281); // Key: Scroll down
         KEY_NUM_LOCK          = TKeyboardKey(282); // Key: Num lock
         KEY_PRINT_SCREEN      = TKeyboardKey(283); // Key: Print screen
         KEY_PAUSE             = TKeyboardKey(284); // Key: Pause
         KEY_F1                = TKeyboardKey(290); // Key: F1
         KEY_F2                = TKeyboardKey(291); // Key: F2
         KEY_F3                = TKeyboardKey(292); // Key: F3
         KEY_F4                = TKeyboardKey(293); // Key: F4
         KEY_F5                = TKeyboardKey(294); // Key: F5
         KEY_F6                = TKeyboardKey(295); // Key: F6
         KEY_F7                = TKeyboardKey(296); // Key: F7
         KEY_F8                = TKeyboardKey(297); // Key: F8
         KEY_F9                = TKeyboardKey(298); // Key: F9
         KEY_F10               = TKeyboardKey(299); // Key: F10
         KEY_F11               = TKeyboardKey(300); // Key: F11
         KEY_F12               = TKeyboardKey(301); // Key: F12
         KEY_LEFT_SHIFT        = TKeyboardKey(340); // Key: Shift left
         KEY_LEFT_CONTROL      = TKeyboardKey(341); // Key: Control left
         KEY_LEFT_ALT          = TKeyboardKey(342); // Key: Alt left
         KEY_LEFT_SUPER        = TKeyboardKey(343); // Key: Super left
         KEY_RIGHT_SHIFT       = TKeyboardKey(344); // Key: Shift right
         KEY_RIGHT_CONTROL     = TKeyboardKey(345); // Key: Control right
         KEY_RIGHT_ALT         = TKeyboardKey(346); // Key: Alt right
         KEY_RIGHT_SUPER       = TKeyboardKey(347); // Key: Super right
         KEY_KB_MENU           = TKeyboardKey(348); // Key: KB menu
         (* Keypad keys *)
         KEY_KP_0              = TKeyboardKey(320); // Key: Keypad 0
         KEY_KP_1              = TKeyboardKey(321); // Key: Keypad 1
         KEY_KP_2              = TKeyboardKey(322); // Key: Keypad 2
         KEY_KP_3              = TKeyboardKey(323); // Key: Keypad 3
         KEY_KP_4              = TKeyboardKey(324); // Key: Keypad 4
         KEY_KP_5              = TKeyboardKey(325); // Key: Keypad 5
         KEY_KP_6              = TKeyboardKey(326); // Key: Keypad 6
         KEY_KP_7              = TKeyboardKey(327); // Key: Keypad 7
         KEY_KP_8              = TKeyboardKey(328); // Key: Keypad 8
         KEY_KP_9              = TKeyboardKey(329); // Key: Keypad 9
         KEY_KP_DECIMAL        = TKeyboardKey(330); // Key: Keypad .
         KEY_KP_DIVIDE         = TKeyboardKey(331); // Key: Keypad /
         KEY_KP_MULTIPLY       = TKeyboardKey(332); // Key: Keypad *
         KEY_KP_SUBTRACT       = TKeyboardKey(333); // Key: Keypad -
         KEY_KP_ADD            = TKeyboardKey(334); // Key: Keypad +
         KEY_KP_ENTER          = TKeyboardKey(335); // Key: Keypad Enter
         KEY_KP_EQUAL          = TKeyboardKey(336); // Key: Keypad =
         // Android key buttons
         KEY_BACK              = TKeyboardKey(4);  // Key: Android back button
         KEY_MENU              = TKeyboardKey(5); // Key: Android menu button
         KEY_VOLUME_UP         = TKeyboardKey(24); // Key: Android volume up button
         KEY_VOLUME_DOWN       = TKeyboardKey(25); // Key: Android volume down button

     (* Mouse buttons *)
     type
       PMouseButton = ^TMouseButton;
       TMouseButton =  Longint;
       const
         MOUSE_BUTTON_LEFT     = TMouseButton(0); // Mouse button left
         MOUSE_BUTTON_RIGHT    = TMouseButton(1); // Mouse button right
         MOUSE_BUTTON_MIDDLE   = TMouseButton(2); // Mouse button middle (pressed wheel)
         MOUSE_BUTTON_SIDE     = TMouseButton(3); // Mouse button side (advanced mouse device)
         MOUSE_BUTTON_EXTRA    = TMouseButton(4); // Mouse button extra (advanced mouse device)
         MOUSE_BUTTON_FORWARD  = TMouseButton(5); // Mouse button forward (advanced mouse device)
         MOUSE_BUTTON_BACK     = TMouseButton(6); // Mouse button back (advanced mouse device)

         (* Add backwards compatibility support for deprecated names *)
         MOUSE_LEFT_BUTTON = MOUSE_BUTTON_LEFT;
         MOUSE_RIGHT_BUTTON = MOUSE_BUTTON_RIGHT;
         MOUSE_MIDDLE_BUTTON = MOUSE_BUTTON_MIDDLE;

     (* Mouse cursor *)
     type
       PMouseCursor = ^TMouseCursor;
       TMouseCursor =  Integer;
       const
         MOUSE_CURSOR_DEFAULT         = TMouseCursor(0);  // Default pointer shape
         MOUSE_CURSOR_ARROW           = TMouseCursor(1);  // Arrow shape
         MOUSE_CURSOR_IBEAM           = TMouseCursor(2);  // Text writing cursor shape
         MOUSE_CURSOR_CROSSHAIR       = TMouseCursor(3);  // Cross shape
         MOUSE_CURSOR_POINTING_HAND   = TMouseCursor(4);  // Pointing hand cursor
         MOUSE_CURSOR_RESIZE_EW       = TMouseCursor(5);  // Horizontal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NS       = TMouseCursor(6);  // Vertical resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NWSE     = TMouseCursor(7);  // Top-left to bottom-right diagonal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_NESW     = TMouseCursor(8);  // The top-right to bottom-left diagonal resize/move arrow shape
         MOUSE_CURSOR_RESIZE_ALL      = TMouseCursor(9);  // The omni-directional resize/move cursor shape
         MOUSE_CURSOR_NOT_ALLOWED     = TMouseCursor(10); // The operation-not-allowed shape

   (* Gamepad buttons *)
   type
      PGamepadButton = ^TGamepadButton;
      TGamepadButton =  Integer;
      const
        GAMEPAD_BUTTON_UNKNOWN           = TGamepadButton(0);  // Unknown button, just for error checking
        GAMEPAD_BUTTON_LEFT_FACE_UP      = TGamepadButton(1);  // Gamepad left DPAD up button
        GAMEPAD_BUTTON_LEFT_FACE_RIGHT   = TGamepadButton(2);  // Gamepad left DPAD right button
        GAMEPAD_BUTTON_LEFT_FACE_DOWN    = TGamepadButton(3);  // Gamepad left DPAD down button
        GAMEPAD_BUTTON_LEFT_FACE_LEFT    = TGamepadButton(4);  // Gamepad left DPAD left button
        GAMEPAD_BUTTON_RIGHT_FACE_UP     = TGamepadButton(5);  // Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
        GAMEPAD_BUTTON_RIGHT_FACE_RIGHT  = TGamepadButton(6);  // Gamepad right button right (i.e. PS3: Circle, Xbox: B)
        GAMEPAD_BUTTON_RIGHT_FACE_DOWN   = TGamepadButton(7);  // Gamepad right button down (i.e. PS3: Cross, Xbox: A)
        GAMEPAD_BUTTON_RIGHT_FACE_LEFT   = TGamepadButton(8);  // Gamepad right button left (i.e. PS3: Square, Xbox: X)
        GAMEPAD_BUTTON_LEFT_TRIGGER_1    = TGamepadButton(9);  // Gamepad top/back trigger left (first), it could be a trailing button
        GAMEPAD_BUTTON_LEFT_TRIGGER_2    = TGamepadButton(10); // Gamepad top/back trigger left (second), it could be a trailing button
        GAMEPAD_BUTTON_RIGHT_TRIGGER_1   = TGamepadButton(11); // Gamepad top/back trigger right (first), it could be a trailing button
        GAMEPAD_BUTTON_RIGHT_TRIGGER_2   = TGamepadButton(12); // Gamepad top/back trigger right (second), it could be a trailing button
        GAMEPAD_BUTTON_MIDDLE_LEFT       = TGamepadButton(13); // Gamepad center buttons, left one (i.e. PS3: Select)
        GAMEPAD_BUTTON_MIDDLE            = TGamepadButton(14); // Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
        GAMEPAD_BUTTON_MIDDLE_RIGHT      = TGamepadButton(15); // Gamepad center buttons, right one (i.e. PS3: Start)
        GAMEPAD_BUTTON_LEFT_THUMB        = TGamepadButton(16); // Gamepad joystick pressed button left
        GAMEPAD_BUTTON_RIGHT_THUMB       = TGamepadButton(17); // Gamepad joystick pressed button right

   (* Gamepad axes *)
   type
     PGamepadAxis = ^TGamepadAxis;
     TGamepadAxis =  Integer;
     const
       GAMEPAD_AXIS_LEFT_X           = TGamepadAxis(0); // Gamepad left stick X axis
       GAMEPAD_AXIS_LEFT_Y           = TGamepadAxis(1); // Gamepad left stick Y axis
       GAMEPAD_AXIS_RIGHT_X          = TGamepadAxis(2); // Gamepad right stick X axis
       GAMEPAD_AXIS_RIGHT_Y          = TGamepadAxis(3); // Gamepad right stick Y axis
       GAMEPAD_AXIS_LEFT_TRIGGER     = TGamepadAxis(4); // Gamepad back trigger left, pressure level: [1..-1]
       GAMEPAD_AXIS_RIGHT_TRIGGER    = TGamepadAxis(5); // Gamepad back trigger right, pressure level: [1..-1]

     (* Material map index *)
     type
       PMaterialMapIndex = ^TMaterialMapIndex;
       TMaterialMapIndex =  Integer;
       const
         MATERIAL_MAP_ALBEDO        = TMaterialMapIndex(0);  // Albedo material (same as: MATERIAL_MAP_DIFFUSE)
         MATERIAL_MAP_METALNESS     = TMaterialMapIndex(1);  // Metalness material (same as: MATERIAL_MAP_SPECULAR)
         MATERIAL_MAP_NORMAL        = TMaterialMapIndex(2);  // Normal material
         MATERIAL_MAP_ROUGHNESS     = TMaterialMapIndex(3);  // Roughness material
         MATERIAL_MAP_OCCLUSION     = TMaterialMapIndex(4);  // Ambient occlusion material
         MATERIAL_MAP_EMISSION      = TMaterialMapIndex(5);  // Emission material
         MATERIAL_MAP_HEIGHT        = TMaterialMapIndex(6);  // Heightmap material
         MATERIAL_MAP_CUBEMAP       = TMaterialMapIndex(7);  // Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_IRRADIANCE    = TMaterialMapIndex(8);  // Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_PREFILTER     = TMaterialMapIndex(9);  // Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
         MATERIAL_MAP_BRDF          = TMaterialMapIndex(10); // Brdf material

         MATERIAL_MAP_DIFFUSE = MATERIAL_MAP_ALBEDO;
         MATERIAL_MAP_SPECULAR = MATERIAL_MAP_METALNESS;

     (* Shader location index *)
     type
       PShaderLocationIndex = ^TShaderLocationIndex;
       TShaderLocationIndex =  Integer;
       const
         SHADER_LOC_VERTEX_POSITION     = TShaderLocationIndex(0);  // Shader location: vertex attribute: position
         SHADER_LOC_VERTEX_TEXCOORD01   = TShaderLocationIndex(1);  // Shader location: vertex attribute: texcoord01
         SHADER_LOC_VERTEX_TEXCOORD02   = TShaderLocationIndex(2);  // Shader location: vertex attribute: texcoord02
         SHADER_LOC_VERTEX_NORMAL       = TShaderLocationIndex(3);  // Shader location: vertex attribute: normal
         SHADER_LOC_VERTEX_TANGENT      = TShaderLocationIndex(4);  // Shader location: vertex attribute: tangent
         SHADER_LOC_VERTEX_COLOR        = TShaderLocationIndex(5);  // Shader location: vertex attribute: color
         SHADER_LOC_MATRIX_MVP          = TShaderLocationIndex(6);  // Shader location: matrix uniform: model-view-projection
         SHADER_LOC_MATRIX_VIEW         = TShaderLocationIndex(7);  // Shader location: matrix uniform: view (camera transform)
         SHADER_LOC_MATRIX_PROJECTION   = TShaderLocationIndex(8);  // Shader location: matrix uniform: projection
         SHADER_LOC_MATRIX_MODEL        = TShaderLocationIndex(9);  // Shader location: matrix uniform: model (transform)
         SHADER_LOC_MATRIX_NORMAL       = TShaderLocationIndex(10); // Shader location: matrix uniform: normal
         SHADER_LOC_VECTOR_VIEW         = TShaderLocationIndex(11); // Shader location: vector uniform: view
         SHADER_LOC_COLOR_DIFFUSE       = TShaderLocationIndex(12); // Shader location: vector uniform: diffuse color
         SHADER_LOC_COLOR_SPECULAR      = TShaderLocationIndex(13); // Shader location: vector uniform: specular color
         SHADER_LOC_COLOR_AMBIENT       = TShaderLocationIndex(14); // Shader location: vector uniform: ambient color
         SHADER_LOC_MAP_ALBEDO          = TShaderLocationIndex(15); // Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
         SHADER_LOC_MAP_METALNESS       = TShaderLocationIndex(16); // Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
         SHADER_LOC_MAP_NORMAL          = TShaderLocationIndex(17); // Shader location: sampler2d texture: normal
         SHADER_LOC_MAP_ROUGHNESS       = TShaderLocationIndex(18); // Shader location: sampler2d texture: roughness
         SHADER_LOC_MAP_OCCLUSION       = TShaderLocationIndex(19); // Shader location: sampler2d texture: occlusion
         SHADER_LOC_MAP_EMISSION        = TShaderLocationIndex(20); // Shader location: sampler2d texture: emission
         SHADER_LOC_MAP_HEIGHT          = TShaderLocationIndex(21); // Shader location: sampler2d texture: height
         SHADER_LOC_MAP_CUBEMAP         = TShaderLocationIndex(22); // Shader location: samplerCube texture: cubemap
         SHADER_LOC_MAP_IRRADIANCE      = TShaderLocationIndex(23); // Shader location: samplerCube texture: irradiance
         SHADER_LOC_MAP_PREFILTER       = TShaderLocationIndex(24); // Shader location: samplerCube texture: prefilter
         SHADER_LOC_MAP_BRDF            = TShaderLocationIndex(25); // Shader location: sampler2d texture: brdf
         SHADER_LOC_VERTEX_BONEIDS      = TShaderLocationIndex(26); // Shader location: vertex attribute: boneIds
         SHADER_LOC_VERTEX_BONEWEIGHTS  = TShaderLocationIndex(27); // Shader location: vertex attribute: boneWeights
         SHADER_LOC_BONE_MATRICES       = TShaderLocationIndex(28); // Shader location: array of matrices uniform: boneMatrices
         SHADER_LOC_VERTEX_INSTANCE_TX  = TShaderLocationIndex(29); // Shader location: vertex attribute: instanceTransform

         SHADER_LOC_MAP_DIFFUSE = SHADER_LOC_MAP_ALBEDO;
         SHADER_LOC_MAP_SPECULAR = SHADER_LOC_MAP_METALNESS;

     (* Shader uniform data type *)
     type
       PShaderUniformDataType = ^TShaderUniformDataType;
       TShaderUniformDataType =  Integer;
       Const
         SHADER_UNIFORM_FLOAT      = TShaderUniformDataType(0);  // Shader uniform type: float
         SHADER_UNIFORM_VEC2       = TShaderUniformDataType(1);  // Shader uniform type: vec2 (2 float)
         SHADER_UNIFORM_VEC3       = TShaderUniformDataType(2);  // Shader uniform type: vec3 (3 float)
         SHADER_UNIFORM_VEC4       = TShaderUniformDataType(3);  // Shader uniform type: vec4 (4 float)
         SHADER_UNIFORM_INT        = TShaderUniformDataType(4);  // Shader uniform type: int
         SHADER_UNIFORM_IVEC2      = TShaderUniformDataType(5);  // Shader uniform type: ivec2 (2 int)
         SHADER_UNIFORM_IVEC3      = TShaderUniformDataType(6);  // Shader uniform type: ivec3 (3 int)
         SHADER_UNIFORM_IVEC4      = TShaderUniformDataType(7);  // Shader uniform type: ivec4 (4 int)
         SHADER_UNIFORM_UINT       = TShaderUniformDataType(8);  // Shader uniform type: unsigned int
         SHADER_UNIFORM_UIVEC2     = TShaderUniformDataType(9);  // Shader uniform type: uivec2 (2 unsigned int)
         SHADER_UNIFORM_UIVEC3     = TShaderUniformDataType(10); // Shader uniform type: uivec3 (3 unsigned int)
         SHADER_UNIFORM_UIVEC4     = TShaderUniformDataType(11); // Shader uniform type: uivec4 (4 unsigned int)
         SHADER_UNIFORM_SAMPLER2D  = TShaderUniformDataType(12); // Shader uniform type: sampler2d

    (* Shader attribute data types *)
    type
      PShaderAttributeDataType = ^TShaderAttributeDataType;
      TShaderAttributeDataType =  Longint;
      const
        SHADER_ATTRIB_FLOAT     = TShaderAttributeDataType(0); // Shader attribute type: float
        SHADER_ATTRIB_VEC2      = TShaderAttributeDataType(1); // Shader attribute type: vec2 (2 float)
        SHADER_ATTRIB_VEC3      = TShaderAttributeDataType(2); // Shader attribute type: vec3 (3 float)
        SHADER_ATTRIB_VEC4      = TShaderAttributeDataType(3); // Shader attribute type: vec4 (4 float)

     (* Pixel formats *)
     //NOTE: Support depends on OpenGL version and platform
     type
       PPixelFormat = ^TPixelFormat;
       TPixelFormat =  Integer;
       const
         PIXELFORMAT_UNCOMPRESSED_GRAYSCALE     = TPixelFormat(1);  // 8 bit per pixel (no alpha)
         PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA    = TPixelFormat(2);  // 8*2 bpp (2 channels)
         PIXELFORMAT_UNCOMPRESSED_R5G6B5        = TPixelFormat(3);  // 16 bpp
         PIXELFORMAT_UNCOMPRESSED_R8G8B8        = TPixelFormat(4);  // 24 bpp
         PIXELFORMAT_UNCOMPRESSED_R5G5B5A1      = TPixelFormat(5);  // 16 bpp (1 bit alpha)
         PIXELFORMAT_UNCOMPRESSED_R4G4B4A4      = TPixelFormat(6);  // 16 bpp (4 bit alpha)
         PIXELFORMAT_UNCOMPRESSED_R8G8B8A8      = TPixelFormat(7);  // 32 bpp
         PIXELFORMAT_UNCOMPRESSED_R32           = TPixelFormat(8);  // 32 bpp (1 channel - float)
         PIXELFORMAT_UNCOMPRESSED_R32G32B32     = TPixelFormat(9);  // 32*3 bpp (3 channels - float)
         PIXELFORMAT_UNCOMPRESSED_R32G32B32A32  = TPixelFormat(10); // 32*4 bpp (4 channels - float)
         PIXELFORMAT_UNCOMPRESSED_R16           = TPixelFormat(11); // 16 bpp (1 channel - half float)
         PIXELFORMAT_UNCOMPRESSED_R16G16B16     = TPixelFormat(12); // 16*3 bpp (3 channels - half float)
         PIXELFORMAT_UNCOMPRESSED_R16G16B16A16  = TPixelFormat(13); // 16*4 bpp (4 channels - half float)
         PIXELFORMAT_COMPRESSED_DXT1_RGB        = TPixelFormat(14); // 4 bpp (no alpha)
         PIXELFORMAT_COMPRESSED_DXT1_RGBA       = TPixelFormat(15); // 4 bpp (1 bit alpha)
         PIXELFORMAT_COMPRESSED_DXT3_RGBA       = TPixelFormat(16); // 8 bpp
         PIXELFORMAT_COMPRESSED_DXT5_RGBA       = TPixelFormat(17); // 8 bpp
         PIXELFORMAT_COMPRESSED_ETC1_RGB        = TPixelFormat(18); // 4 bpp
         PIXELFORMAT_COMPRESSED_ETC2_RGB        = TPixelFormat(19); // 4 bpp
         PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA   = TPixelFormat(20); // 8 bpp
         PIXELFORMAT_COMPRESSED_PVRT_RGB        = TPixelFormat(21); // 4 bpp
         PIXELFORMAT_COMPRESSED_PVRT_RGBA       = TPixelFormat(22); // 4 bpp
         PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA   = TPixelFormat(23); // 8 bpp
         PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA   = TPixelFormat(24); // 2 bpp

   (* Texture parameters: filter mode *)
   //NOTE 1: Filtering considers mipmaps if available in the texture
   //NOTE 2: Filter is accordingly set for minification and magnification
   type
     PTextureFilter = ^TTextureFilter;
     TTextureFilter =  Integer;
     const
       TEXTURE_FILTER_POINT            = TTextureFilter(0); // No filter, just pixel approximation
       TEXTURE_FILTER_BILINEAR         = TTextureFilter(1); // Linear filtering
       TEXTURE_FILTER_TRILINEAR        = TTextureFilter(2); // Trilinear filtering (linear with mipmaps)
       TEXTURE_FILTER_ANISOTROPIC_4X   = TTextureFilter(3); // Anisotropic filtering 4x
       TEXTURE_FILTER_ANISOTROPIC_8X   = TTextureFilter(4); // Anisotropic filtering 8x
       TEXTURE_FILTER_ANISOTROPIC_16X  = TTextureFilter(5); // Anisotropic filtering 16x

   (* Texture parameters: wrap mode *)
   type
     PTextureWrap = ^TTextureWrap;
     TTextureWrap =  Integer;
     Const
       TEXTURE_WRAP_REPEAT        = TTextureWrap(0); // Repeats texture in tiled mode
       TEXTURE_WRAP_CLAMP         = TTextureWrap(1); // Clamps texture to edge pixel in tiled mode
       TEXTURE_WRAP_MIRROR_REPEAT = TTextureWrap(2); // Mirrors and repeats the texture in tiled mode
       TEXTURE_WRAP_MIRROR_CLAMP  = TTextureWrap(3); // Mirrors and clamps to border the texture in tiled mode

   (* Cubemap layouts *)
   type
     PCubemapLayout = ^TCubemapLayout;
     TCubemapLayout =  Integer;
     Const
       CUBEMAP_LAYOUT_AUTO_DETECT         = TCubemapLayout(0); // Automatically detect layout type
       CUBEMAP_LAYOUT_LINE_VERTICAL       = TCubemapLayout(1); // Layout is defined by a vertical line with faces
       CUBEMAP_LAYOUT_LINE_HORIZONTAL     = TCubemapLayout(2); // Layout is defined by an horizontal line with faces
       CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR = TCubemapLayout(3); // Layout is defined by a 3x4 cross with cubemap faces
       CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE = TCubemapLayout(4); // Layout is defined by a 4x3 cross with cubemap faces

   (* Font type, defines generation method *)
   type
     PFontType = ^TFontType;
     TFontType =  Integer;
     Const
       FONT_DEFAULT          = TFontType(0); // Default font generation, anti-aliased
       FONT_BITMAP           = TFontType(1); // Bitmap font generation, no anti-aliasing
       FONT_SDF              = TFontType(2); // SDF font generation, requires external shader

   (* Color blending modes (pre-defined) *)
   type
     PBlendMode = ^TBlendMode;
     TBlendMode =  Integer;
     Const
       BLEND_ALPHA             = TBlendMode(0); // Blend textures considering alpha (default)
       BLEND_ADDITIVE          = TBlendMode(1); // Blend textures adding colors
       BLEND_MULTIPLIED        = TBlendMode(2); // Blend textures multiplying colors
       BLEND_ADD_COLORS        = TBlendMode(3); // Blend textures adding colors (alternative)
       BLEND_SUBTRACT_COLORS   = TBlendMode(4); // Blend textures subtracting colors (alternative)
       BLEND_ALPHA_PREMULTIPLY = TBlendMode(5); // Blend premultiplied textures considering alpha
       BLEND_CUSTOM            = TBlendMode(6); // Blend textures using custom src/dst factors (use rlSetBlendFactors())
       BLEND_CUSTOM_SEPARATE   = TBlendMode(7); // Blend textures using custom rgb/alpha separate src/dst factors rlSetBlendFactorsSeparate())

   (* Gestures *)
   // NOTE: Provided as bit-wise flags to enable only desired gestures
   type
     PGesture = ^TGesture;
     TGesture =  Integer;
     Const
       GESTURE_NONE          = TGesture(0);   // No gesture
       GESTURE_TAP           = TGesture(1);   // Tap gesture
       GESTURE_DOUBLETAP     = TGesture(2);   // Double tap gesture
       GESTURE_HOLD          = TGesture(4);   // Hold gesture
       GESTURE_DRAG          = TGesture(8);   // Drag gesture
       GESTURE_SWIPE_RIGHT   = TGesture(16);  // Swipe right gesture
       GESTURE_SWIPE_LEFT    = TGesture(32);  // Swipe left gesture
       GESTURE_SWIPE_UP      = TGesture(64);  // Swipe up gesture
       GESTURE_SWIPE_DOWN    = TGesture(128); // Swipe down gesture
       GESTURE_PINCH_IN      = TGesture(256); // Pinch in gesture
       GESTURE_PINCH_OUT     = TGesture(512); // Pinch out gesture

   (* Camera system modes *)
   type
     PCameraMode = ^TCameraMode;
     TCameraMode =  Integer;
     Const
       CAMERA_CUSTOM        = TCameraMode(0); // Camera custom, controlled by user (UpdateCamera() does nothing)
       CAMERA_FREE          = TCameraMode(1); // Camera free mode
       CAMERA_ORBITAL       = TCameraMode(2); // Camera orbital, around target, zoom supported
       CAMERA_FIRST_PERSON  = TCameraMode(3); // Camera first person
       CAMERA_THIRD_PERSON  = TCameraMode(4); // Camera third person

   (* Camera projection *)
   type
     PCameraProjection = ^TCameraProjection;
     TCameraProjection =  Integer;
     const
       CAMERA_PERSPECTIVE  = TCameraProjection(0); // Perspective projection
       CAMERA_ORTHOGRAPHIC = TCameraProjection(1); // Orthographic projection

     (* N-patch layout *)
     type
      PNPatchLayout = ^TNPatchLayout;
      TNPatchLayout =  Integer;

      const
        NPATCH_NINE_PATCH             = TNPatchLayout(0); // Npatch layout: 3x3 tiles
        NPATCH_THREE_PATCH_VERTICAL   = TNPatchLayout(1); // Npatch layout: 1x3 tiles
        NPATCH_THREE_PATCH_HORIZONTAL = TNPatchLayout(2); // Npatch layout: 3x1 tiles


//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

(* Window-related function *)

{Initialize window and OpenGL context}
procedure InitWindow(width, height: Integer; const title: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'InitWindow';
{Close window and unload OpenGL context}
procedure CloseWindow; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CloseWindow';
{Check if KEY_ESCAPE pressed or Close icon pressed}
function WindowShouldClose: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'WindowShouldClose';
{Check if window has been initialized successfully}
function IsWindowReady: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowReady';
{Check if window is currently fullscreen }
function IsWindowFullscreen: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowFullscreen';
{Check if window is currently hidden}
function IsWindowHidden: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowHidden';
{Check if window is currently minimized}
function IsWindowMinimized: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowMinimized';
{Check if window is currently maximized}
function IsWindowMaximized: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowMaximized';
{Check if window is currently focused}
function IsWindowFocused: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowFocused';
{Check if window has been resized last frame}
function IsWindowResized: Boolean;cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowResized';
{Check if one specific window flag is enabled}
function IsWindowState(flag: TConfigFlags): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWindowState';
{Set window configuration state using flags}
procedure SetWindowState(flags: TConfigFlags); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowState';
{Clear window configuration state flags}
procedure ClearWindowState(flags: TConfigFlags); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ClearWindowState';
{Toggle window state: fullscreen/windowed, resizes monitor to match window resolution}
procedure ToggleFullscreen; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ToggleFullscreen';
{Toggle window state: borderless windowed, resizes window to match monitor resolution}
procedure ToggleBorderlessWindowed; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ToggleBorderlessWindowed';
{Set window state: maximized, if resizable}
procedure MaximizeWindow; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MaximizeWindow';
{Set window state: minimized, if resizable}
procedure MinimizeWindow; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MinimizeWindow';
{Restore window from being minimized/maximized}
procedure RestoreWindow; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'RestoreWindow';
{Set icon for window}
procedure SetWindowIcon(image: TImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowIcon';
{Set icon for window (multiple images, RGBA 32bit, only PLATFORM_DESKTOP)}
procedure SetWindowIcons(images: PImage; count: integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowIcons';
{Set title for window}
procedure SetWindowTitle(const title: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowTitle';
{Set window position on screen}
procedure SetWindowPosition(x, y: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowPosition';
{Set monitor for the current window}
procedure SetWindowMonitor(monitor: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowMonitor';
{Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)}
procedure SetWindowMinSize(width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowMinSize';
{Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)}
procedure SetWindowMaxSize(width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowMaxSize';
{Set window dimensions}
procedure SetWindowSize(width, height: Integer);cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowSize';
{Set window opacity [0.0f..1.0f]}
procedure SetWindowOpacity(opacity: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowOpacity';
{Set window focused}
procedure SetWindowFocused; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetWindowFocused';
{Get native window handle}
function GetWindowHandle: Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWindowHandle';
{Get current screen width}
function GetScreenWidth: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetScreenWidth';
{Get current screen height}
function GetScreenHeight: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetScreenHeight';
{Get current render width (it considers HiDPI)}
function GetRenderWidth: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRenderWidth';
{Get current render height (it considers HiDPI)}
function GetRenderHeight: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRenderHeight';
{Get number of connected monitors}
function GetMonitorCount: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorCount';
{Get current monitor where window is placed}
function GetCurrentMonitor: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCurrentMonitor';
{Get specified monitor position}
function GetMonitorPosition(monitor: Integer): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorPosition';
{Get specified monitor width (current video mode used by monitor)}
function GetMonitorWidth(monitor: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorWidth';
{Get specified monitor height (current video mode used by monitor)}
function GetMonitorHeight(monitor: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorHeight';
{Get specified monitor physical width in millimetres}
function GetMonitorPhysicalWidth(monitor: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorPhysicalWidth';
{Get specified monitor physical height in millimetres}
function GetMonitorPhysicalHeight(monitor: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorPhysicalHeight';
{Get specified monitor refresh rate}
function GetMonitorRefreshRate(monitor: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorRefreshRate';
{Get window position XY on monitor}
function GetWindowPosition: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWindowPosition';
{Get window scale DPI factor}
function GetWindowScaleDPI: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWindowScaleDPI';
{Get the human-readable, UTF-8 encoded name of the specified monitor}
function GetMonitorName(monitor: Integer): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMonitorName';
{Set clipboard text content}
procedure SetClipboardText(const text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetClipboardText';
{Get clipboard text content}
function GetClipboardText: PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetClipboardText';
{Get clipboard image content}
function GetClipboardImage: TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetClipboardImage';
{Enable waiting for events on EndDrawing(), no automatic event polling}
procedure EnableEventWaiting; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EnableEventWaiting';
{Disable waiting for events on EndDrawing(), automatic events polling}
procedure DisableEventWaiting; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DisableEventWaiting';

(* Cursor-related functions *)

{Shows cursor}
procedure ShowCursor; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ShowCursor';
{Hides cursor}
procedure HideCursor; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'HideCursor';
{Check if cursor is not visible}
function IsCursorHidden: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsCursorHidden';
{Enables cursor (unlock cursor)}
procedure EnableCursor; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EnableCursor';
{Disables cursor (lock cursor)}
procedure DisableCursor; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DisableCursor';
{Check if cursor is on the current screen.}
function IsCursorOnScreen: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsCursorOnScreen';

(* Drawing-related functions *)

{Set background color (framebuffer clear color)}
procedure ClearBackground(color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ClearBackground';
{Setup canvas (framebuffer) to start drawing}
procedure BeginDrawing; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginDrawing';
{End canvas drawing and swap buffers (double buffering)}
procedure EndDrawing; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndDrawing';
{Initialize 2D mode with custom camera (2D)}
procedure BeginMode2D(camera: TCamera2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginMode2D';
{Ends 2D mode with custom camera}
procedure EndMode2D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndMode2D';
{Initializes 3D mode with custom camera (3D)}
procedure BeginMode3D(camera: TCamera3D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginMode3D';
{Ends 3D mode and returns to default 2D orthographic mode}
procedure EndMode3D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndMode3D';
{Initializes render texture for drawing}
procedure BeginTextureMode(target: TRenderTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginTextureMode';
{Ends drawing to render texture}
procedure EndTextureMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndTextureMode';
{Begin custom shader drawing}
procedure BeginShaderMode(shader: TShader); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginShaderMode';
{End custom shader drawing (use default shader)}
procedure EndShaderMode;cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndShaderMode';
{Begin blending mode (alpha, additive, multiplied)}
procedure BeginBlendMode(mode: TBlendMode); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginBlendMode';
{End blending mode (reset to default: alpha blending)}
procedure EndBlendMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndBlendMode';
{Begin scissor mode (define screen area for following drawing)}
procedure BeginScissorMode(x, y, width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginScissorMode';
{End scissor mode}
procedure EndScissorMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndScissorMode';
{Begin stereo rendering (requires VR simulator)}
procedure BeginVrStereoMode(config: TVrStereoConfig); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'BeginVrStereoMode';
{End stereo rendering (requires VR simulator)}
procedure EndVrStereoMode; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EndVrStereoMode';

(* VR stereo config functions for VR simulator *)

{Load VR stereo config for VR simulator device parameters}
function LoadVrStereoConfig(device: TVrDeviceInfo): TVrStereoConfig; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadVrStereoConfig';
{Unload VR stereo config }
procedure UnloadVrStereoConfig(config: TVrStereoConfig); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadVrStereoConfig';

(* Shader management functions *)
// NOTE: Shader functionality is not available on OpenGL 1.1

{Load shader from files and bind default locations}
function LoadShader(const vsFileName, fsFileName: PChar): TShader; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadShader';
{Load shader from code strings and bind default locations}
function LoadShaderFromMemory(const vsCode, fsCode: PChar): TShader; cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadShaderFromMemory';
{Check if a shader is valid (loaded on GPU)}
function IsShaderValid(shader: TShader): Boolean; cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsShaderValid';
{Get shader uniform location}
function GetShaderLocation(shader: TShader; const uniformName: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetShaderLocation';
{Get shader attribute location}
function GetShaderLocationAttrib(shader:TShader; const  attribName:PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetShaderLocationAttrib';
{Set shader uniform value}
procedure SetShaderValue(shader: TShader; locIndex: Integer; const value: Pointer; uniformType: TShaderUniformDataType); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetShaderValue';
{Set shader uniform value vector}
procedure SetShaderValueV(shader: TShader; locIndex: Integer; const value: Pointer; uniformType: TShaderUniformDataType; count: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetShaderValueV';
{Set shader uniform value (matrix 4x4)}
procedure SetShaderValueMatrix(shader: TShader; locIndex: Integer; mat:TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetShaderValueMatrix';
{Set shader uniform value and bind the texture (sampler2d)}
procedure SetShaderValueTexture(shader: TShader; locIndex: Integer; texture: TTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetShaderValueTexture';
{Unload shader from GPU memory (VRAM)}
procedure UnloadShader(shader: TShader); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadShader';

(* Screen-space-related functions *)

{Get a ray trace from mouse position}
function GetMouseRay(mousePosition: TVector2; camera: TCamera): TRay; // Compatibility hack for previous raylib versions
{Get a ray trace from screen position (i.e mouse)}
function GetScreenToWorldRay(position: TVector2; camera: TCamera): TRay; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetScreenToWorldRay';
{Get a ray trace from screen position (i.e mouse) in a viewport}
function GetScreenToWorldRayEx(position: TVector2; camera: TCamera; width, height: Integer): TRay; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetScreenToWorldRayEx';
{Get a ray trace from mouse position in a viewport}
function GetViewRay(mousePosition: TVector2; camera: TCamera; width, height: Single): TRay; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetViewRay';
{Get the screen space position for a 3d world space position}
function GetWorldToScreen(position: TVector3; camera: TCamera): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWorldToScreen';
{Get size position for a 3d world space position}
function GetWorldToScreenEx(position: TVector3; camera: TCamera; width, height: Integer): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWorldToScreenEx';
{Get the screen space position for a 2d camera world space position}
function GetWorldToScreen2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWorldToScreen2D';
{Get the world space position for a 2d camera screen space position}
function GetScreenToWorld2D(position: TVector2; camera: TCamera2D): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetScreenToWorld2D';
{Get camera transform matrix (view matrix)}
function GetCameraMatrix(camera: TCamera): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraMatrix';
{Get camera 2d transform matrix}
function GetCameraMatrix2D(camera: TCamera2D): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCameraMatrix2D';

(* Timing-related functions *)

{Set target FPS (maximum)}
procedure SetTargetFPS(fps: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTargetFPS';
{Returns time in seconds for last frame drawn (delta time)}
function GetFrameTime: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFrameTime';
{Returns elapsed time in seconds since InitWindow()}
function GetTime: Double; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTime';
{Returns current FPS}
function GetFPS: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFPS';

(* Custom frame control functions *)
// NOTE: Those functions are intended for advanced users that want full control over the frame processing
// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL

{Swap back buffer with front buffer (screen drawing)}
procedure SwapScreenBuffer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SwapScreenBuffer';
{Register all input events}
procedure PollInputEvents; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PollInputEvents';
{Wait for some time (halt program execution) }
procedure WaitTime(ms: Double); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'WaitTime';

(* Random values generation functions *)

{Load random values sequence, no values repeated}
function LoadRandomSequence(count: LongWord; min, max: Integer): PInteger; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadRandomSequence';
{Unload random values sequence}
procedure UnloadRandomSequence(sequence: PInteger); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadRandomSequence';
{Set the seed for the random number generator}
procedure SetRandomSeed(seed: LongWord); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetRandomSeed';
{Get a random value between min and max (both included)}
function GetRandomValue(min, max: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRandomValue';

(* Misc. functions *)

{Takes a screenshot of current screen (filename extension defines format)}
procedure TakeScreenshot(const fileName: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TakeScreenshot';
{Setup init configuration flags (view FLAGS)}
procedure SetConfigFlags(flags: TConfigFlags); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetConfigFlags';
{Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)}
procedure TraceLog(logLevel: TTraceLogLevel;const text: PChar); cdecl; varargs; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TraceLog';
{Set the current threshold (minimum) log level}
procedure SetTraceLogLevel(logLevel: TTraceLogLevel); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTraceLogLevel';
{Internal memory allocator}
function MemAlloc(size: LongWord): Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MemAlloc';
{Internal memory reallocator}
function MemRealloc(ptr: Pointer; size: LongWord): Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MemRealloc';
{Internal memory free}
procedure MemFree(ptr: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MemFree';
{Open URL with default system browser (if available)}
procedure OpenURL(const url: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'OpenURL';

(* Set custom callbacks *)
(* Callbacks to hook some internal functions *)
// WARNING: Callbacks setup is intended for advanced users
type
  TTraceLogCallback = procedure(logLevel: TTraceLogLevel; const Text: PChar; Args: Pointer); cdecl varargs;
  TLoadFileDataCallback = function(const fileName: PChar; dataSize: PInteger): PChar; cdecl;
  TSaveFileDataCallback = function(const fileName: PChar; data: Pointer; dataSize: Integer): Boolean; cdecl;
  TLoadFileTextCallback = function(const fileName: PChar): PChar; cdecl;
  TSaveFileTextCallback = function(const fileName, text: PChar): Boolean; cdecl;

{ Set custom trace log }
procedure SetTraceLogCallback(callback: TTraceLogCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTraceLogCallback';
{ Set custom file binary data loader }
procedure SetLoadFileDataCallback(callback: TLoadFileDataCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetLoadFileDataCallback';
{ Set custom file binary data saver }
procedure SetSaveFileDataCallback(callback: TSaveFileDataCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetSaveFileDataCallback';
{ Set custom file text data loader }
procedure SetLoadFileTextCallback(callback: TLoadFileTextCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetLoadFileTextCallback';
{ Set custom file text data saver }
procedure SetSaveFileTextCallback(callback: TSaveFileTextCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetSaveFileTextCallback';

(* Files management functions *)

{Load file data as byte array (read)}
function LoadFileData(const fileName: PChar; dataSize: PInteger): PByte; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFileData';
{Unload file data allocated by LoadFileData()}
procedure UnloadFileData(data: PByte); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadFileData';
{Save data to file from byte array (write), returns true on success}
function SaveFileData(const fileName: PChar; data: Pointer; dataSize: integer): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SaveFileData';
{Export data to code (.h), returns true on success}
function ExportDataAsCode(const data: PChar; dataSize: integer; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportDataAsCode';
{Load text data from file (read), returns a '\0' terminated string}
function LoadFileText(const fileName: Pchar): Pchar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFileText';
{Unload file text data allocated by LoadFileText()}
procedure UnloadFileText(text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadFileText';
{Save text data to file (write), string must be '\0' terminated, returns true on success}
function SaveFileText(const fileName, text: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SaveFileText';
{Check if file exists}
function FileExists(const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'FileExists';
{Check if a directory path exists}
function DirectoryExists(const dirPath: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DirectoryExists';
{Check file extension (including point: .png, .wav)}
function IsFileExtension(const fileName, ext: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsFileExtension';
{Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)}
function GetFileLength(const fileName: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFileLength';
{Get pointer to extension for a filename string (includes dot: '.png')}
function GetFileExtension(const fileName: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFileExtension';
{Get pointer to filename for a path string }
function GetFileName(const filePath: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFileName';
{Get filename string without extension (uses static string)}
function GetFileNameWithoutExt(const filePath: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFileNameWithoutExt';
{Get full path for a given fileName with path (uses static string)}
function GetDirectoryPath(const filePath: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetDirectoryPath';
{Get previous directory path for a given path (uses static string)}
function GetPrevDirectoryPath(const dirPath: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetPrevDirectoryPath';
{Get current working directory (uses static string)}
function GetWorkingDirectory: PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetWorkingDirectory';
{Get the directory if the running application (uses static string)}
function GetApplicationDirectory: PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetApplicationDirectory';
{Create directories (including full path requested), returns 0 on success}
function MakeDirectory(const dirPath: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MakeDirectory';
{Change working directory, return true on success}
function ChangeDirectory(const dir: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ChangeDirectory';
{Check if a given path is a file or a directory}
function IsPathFile(const path: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsPathFile';
{Check if fileName is valid for the platform/OS}
function IsFileNameValid(const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsFileNameValid';
{Load directory filepaths}
function LoadDirectoryFiles(const dirPath: PChar): TFilePathList; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadDirectoryFiles';
{Load directory filepaths with extension filtering and recursive directory scan. Use 'DIR' in the filter string to include directories in the result}
function LoadDirectoryFilesEx(const basePath, filter: PChar; scanSubdirs: Boolean): TFilePathList; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadDirectoryFilesEx';
{Unload filepaths}
procedure UnloadDirectoryFiles(files: TFilePathList); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadDirectoryFiles';
{Check if a file has been dropped into window}
function IsFileDropped: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsFileDropped';
{Load dropped filepaths}
function LoadDroppedFiles: TFilePathList; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadDroppedFiles';
{Unload dropped filepaths}
procedure UnloadDroppedFiles(files: TFilePathList); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadDroppedFiles';
{Get file modification time (last write time)}
function GetFileModTime(const fileName: PChar): QWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFileModTime';

(* Compression/Encoding functionality *)

{Compress data (DEFLATE algorithm), memory must be MemFree()}
function CompressData(const data: PByte; dataSize: Integer; compDataSize: PInteger): Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CompressData';
{Decompress data (DEFLATE algorithm), memory must be MemFree()}
function DecompressData(const compData: PByte; compDataSize: Integer; dataSize: PInteger): Pointer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DecompressData';
{Encode data to Base64 string (includes NULL terminator), memory must be MemFree()}
function EncodeDataBase64(const data: PByte; dataSize: Integer; outputSize: PInteger): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'EncodeDataBase64';
{Decode Base64 string (expected NULL terminated), memory must be MemFree()}
function DecodeDataBase64(const text: PChar; outputSize: PInteger): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DecodeDataBase64';
{Compute CRC32 hash code}
function ComputeCRC32(data: PChar; dataSize: Integer): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ComputeCRC32';
{Compute MD5 hash code, returns static int[4] (16 bytes)}
function ComputeMD5(data: PChar; dataSize: Integer): PLongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ComputeMD5';
{Compute SHA1 hash code, returns static int[5] (20 bytes)}
function ComputeSHA1(data: PChar; dataSize: Integer): PLongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ComputeSHA1';

(* Automation events functionality *)

{Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS}
function LoadAutomationEventList(const fileName: PChar): TAutomationEventList; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadAutomationEventList';
{Unload automation events list from file}
procedure UnloadAutomationEventList(list: TAutomationEventList); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadAutomationEventList';
{Export automation events list as text file}
function ExportAutomationEventList(list: TAutomationEventList; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportAutomationEventList';
{Set automation event list to record to}
procedure SetAutomationEventList(list: PAutomationEventList); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAutomationEventList';
{Set automation event internal base frame to start recording}
procedure SetAutomationEventBaseFrame(frame: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAutomationEventBaseFrame';
{Start recording automation events (AutomationEventList must be set)}
procedure StartAutomationEventRecording(); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'StartAutomationEventRecording';
{Stop recording automation events}
procedure StopAutomationEventRecording(); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'StopAutomationEventRecording';
{Play automation event}
procedure PlayAutomationEvent(event: TAutomationEvent); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PlayAutomationEvent';

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

(* Input-related functions: keyboard *)

{Check if a key has been pressed once}
function IsKeyPressed(key: TKeyboardKey): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsKeyPressed';
{Check if a key has been pressed again}
function IsKeyPressedRepeat(key: TKeyboardKey): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsKeyPressedRepeat';
{Check if a key is being pressed}
function IsKeyDown(key: TKeyboardKey): Boolean;cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsKeyDown';
{Check if a key has been released once}
function IsKeyReleased(key: TKeyboardKey): Boolean;cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsKeyReleased';
{Check if a key is NOT being pressed}
function IsKeyUp(key: TKeyboardKey): Boolean;cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsKeyUp';
{Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty}
function GetKeyPressed: TKeyboardKey; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetKeyPressed';
{Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty}
function GetCharPressed: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCharPressed';
{Get name of a QWERTY key on the current keyboard layout (eg returns string 'q' for KEY_A on an AZERTY keyboard)}
function GetKeyName(key: Integer): PChar;cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetKeyName';
{Set a custom key to exit program (default is ESC)}
procedure SetExitKey(key: TKeyboardKey); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetExitKey';

(* Input-related functions: gamepads *)

{Check if a gamepad is available}
function IsGamepadAvailable(gamepad: Integer): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGamepadAvailable';
{Get gamepad internal name id}
function GetGamepadName(gamepad: Integer): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGamepadName';
{Check if a gamepad button has been pressed once}
function IsGamepadButtonPressed(gamepad: Integer; button: TGamepadButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGamepadButtonPressed';
{Check if a gamepad button is being pressed}
function IsGamepadButtonDown(gamepad: Integer; button: TGamepadButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGamepadButtonDown';
{Check if a gamepad button has been released once}
function IsGamepadButtonReleased(gamepad: Integer; button: TGamepadButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGamepadButtonReleased';
{Check if a gamepad button is NOT being pressed}
function IsGamepadButtonUp(gamepad: Integer; button: TGamepadButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGamepadButtonUp';
{Get the last gamepad button pressed}
function GetGamepadButtonPressed: TGamepadButton; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGamepadButtonPressed';
{Get axis count for a gamepad}
function GetGamepadAxisCount(gamepad: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGamepadAxisCount';
{Get movement value for a gamepad axis}
function GetGamepadAxisMovement(gamepad: Integer; axis: TGamepadAxis): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGamepadAxisMovement';
{Set internal gamepad mappings (SDL_GameControllerDB)}
function SetGamepadMappings(const mappings: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGamepadMappings';
{Set gamepad vibration for both motors (duration in seconds)}
procedure SetGamepadVibration(gamepad: Integer; leftMotor, rightMotor, duration: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGamepadVibration';

(* Input-related functions: mouse *)

{Check if a mouse button has been pressed once}
function IsMouseButtonPressed(button: TMouseButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMouseButtonPressed';
{Check if a mouse button is being pressed}
function IsMouseButtonDown(button: TMouseButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMouseButtonDown';
{Check if a mouse button has been released once}
function IsMouseButtonReleased(button: TMouseButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMouseButtonReleased';
{Check if a mouse button is NOT being pressed}
function IsMouseButtonUp(button: TMouseButton): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMouseButtonUp';
{Get mouse position X}
function GetMouseX: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMouseX';
{Get mouse position Y}
function GetMouseY: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMouseY';
{Get mouse position XY}
function GetMousePosition: TVector2; cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMousePosition';
{Get mouse delta between frames}
function GetMouseDelta: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMouseDelta';
{Set mouse position XY}
procedure SetMousePosition(x,y: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMousePosition';
{Set mouse offset}
procedure SetMouseOffset(offsetX, offsetY: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMouseOffset';
{Set mouse scaling}
procedure SetMouseScale(scaleX, scaleY: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMouseScale';
{Get mouse wheel movement for X or Y, whichever is larger}
function GetMouseWheelMove: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMouseWheelMove';
{Get mouse wheel movement for both X and Y}
function GetMouseWheelMoveV: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMouseWheelMoveV';
{ Set mouse cursor}
procedure SetMouseCursor(cursor: TMouseCursor); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMouseCursor';

(* Input-related functions: touch *)

{Get touch position X for touch point 0 (relative to screen size)}
function GetTouchX: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchX';
{Get touch position Y for touch point 0 (relative to screen size)}
function GetTouchY: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchY';
{Get touch point identifier for given index}
function GetTouchPointId(index: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchPointId';
{Get touch position XY for a touch point index (relative to screen size)}
function GetTouchPosition(index: Integer): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchPosition';
{Get touch points count}
function GetTouchPointCount: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchPointCount';
{Get last touch event registered}
function GetTouchEvent: Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetTouchEvent';

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------

{Enable a set of gestures using flags}
procedure SetGesturesEnabled(flags: TGesture); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGesturesEnabled';
{Check if a gesture have been detected}
function IsGestureDetected(gesture: TGesture): Boolean;cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsGestureDetected';
{Get latest detected gesture}
function GetGestureDetected: TGesture; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGestureDetected';
{Get gesture hold time in seconds}
function GetGestureHoldDuration: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGestureHoldDuration';
{Get gesture drag vector}
function GetGestureDragVector: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGestureDragVector';
{Get gesture drag angle}
function GetGestureDragAngle: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGestureDragAngle';
{Get gesture pinch delta}
function GetGesturePinchVector: TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGesturePinchVector';
{Get gesture pinch angle}
function GetGesturePinchAngle: Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGesturePinchAngle';

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------

{Update camera position for selected mode}
procedure UpdateCamera(camera: PCamera; mode: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateCamera';
{Update camera movement/rotation}
procedure UpdateCameraPro(camera: PCamera; movement, rotation: TVector3; zoom: single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateCameraPro';
//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------
// Set texture and rectangle to be used on shapes drawing
// NOTE: It can be useful when using basic shapes and one single font,
// defining a font char white rectangle would allow drawing everything in a single draw call

{Set texture and rectangle to be used on shapes drawing}
procedure SetShapesTexture(texture: TTexture2D; source: TRectangle); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetShapesTexture';
{Get texture that is used for shapes drawing}
function GetShapesTexture: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetShapesTexture';
{Get texture source rectangle that is used for shapes drawing}
function GetShapesTextureRectangle: TRectangle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetShapesTextureRectangle';


(* Basic shapes drawing functions *)

{Draw a pixel using geometry [Can be slow, use with care]}
procedure DrawPixel(posX, posY: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPixel';
{Draw a pixel using geometry (Vector version) [Can be slow, use with care]}
procedure DrawPixelV(position: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPixelV';
{Draw a line}
procedure DrawLine(startPosX, startPosY, endPosX, endPosY: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLine';
{Draw a line (using gl lines)}
procedure DrawLineV(startPos, endPos: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLineV';
{Draw a line (using triangles/quads)}
procedure DrawLineEx(startPos, endPos: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLineEx';
{Draw lines sequence (using gl lines)}
procedure DrawLineStrip(const points: PVector2; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLineStrip';
{Draw line segment cubic-bezier in-out interpolation}
procedure DrawLineBezier(startPos, endPos: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLineBezier';
{Draw a color-filled circle}
procedure DrawCircle(centerX, centerY: Integer; radius: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircle';
{Draw a piece of a circle}
procedure DrawCircleSector(center: TVector2; radius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleSector';
{Draw circle sector outline}
procedure DrawCircleSectorLines(center: TVector2; radius, startAngle, endAngle: Single; segments: Integer; color: TColorB);cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleSectorLines';
{Draw a gradient-filled circle}
procedure DrawCircleGradient(centerX, centerY: Integer; radius: Single; color1, color2: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleGradient';
{Draw a color-filled circle (Vector version)}
procedure DrawCircleV(center: TVector2; radius: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleV';
{Draw circle outline}
procedure DrawCircleLines(centerX, centerY: Integer; radius: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleLines';
{Draw circle outline (Vector version)}
procedure DrawCircleLinesV(center: TVector2; radius: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircleLinesV';
{Draw ellipse}
procedure DrawEllipse(centerX, centerY: Integer; radiusH, radiusV: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawEllipse';
{Draw ellipse (Vector version)}
procedure DrawEllipseV(center: TVector2; radiusH, radiusV: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawEllipseV';
{Draw ellipse outline}
procedure DrawEllipseLines(centerX, centerY: Integer; radiusH, radiusV: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawEllipseLines';
{Draw ellipse outline (Vector version)}
procedure DrawEllipseLinesV(center: TVector2; radiusH, radiusV: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawEllipseLinesV';
{Draw ring}
procedure DrawRing(center: TVector2; innerRadius, outerRadius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRing';
{Draw ring outline}
procedure DrawRingLines(center: TVector2; innerRadius, outerRadius, startAngle, endAngle: Single; segments: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRingLines';
{Draw a color-filled rectangle}
procedure DrawRectangle(posX, posY, width, height: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangle';
{Draw a color-filled rectangle (Vector version)}
procedure DrawRectangleV(position, size: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleV';
{Draw a color-filled rectangle}
procedure DrawRectangleRec(rec: TRectangle; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleRec';
{Draw a color-filled rectangle with pro parameters}
procedure DrawRectanglePro(rec: TRectangle; origin: TVector2; rotation: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectanglePro';
{Draw a vertical-gradient-filled rectangle}
procedure DrawRectangleGradientV(posX, posY, width, height: Integer; top, bottom: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleGradientV';
{Draw a horizontal-gradient-filled rectangle}
procedure DrawRectangleGradientH(posX, posY, width, height: Integer; left, right: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleGradientH';
{Draw a gradient-filled rectangle with custom vertex colors}
procedure DrawRectangleGradientEx(rec: TRectangle; topLeft, bottomLeft, bottomRight, topRight: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleGradientEx';
{Draw rectangle outline}
procedure DrawRectangleLines(posX, posY, width, height: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleLines';
{Draw rectangle outline with extended parameters}
procedure DrawRectangleLinesEx(rec: TRectangle; lineThick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleLinesEx';
{Draw rectangle with rounded edges}
procedure DrawRectangleRounded(rec: TRectangle; roundness: Single; segments: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleRounded';
{Draw rectangle lines with rounded edges}
procedure DrawRectangleRoundedLines(rec: TRectangle; roundness: Single; segments: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleRoundedLines';
{Draw rectangle with rounded edges outline}
procedure DrawRectangleRoundedLinesEx(rec: TRectangle; roundness: Single; segments: Integer; lineThick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRectangleRoundedLinesEx';
{Draw a color-filled triangle (vertex in counter-clockwise order!)}
procedure DrawTriangle(v1, v2, v3: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangle';
{Draw triangle outline (vertex in counter-clockwise order!)}
procedure DrawTriangleLines(v1, v2, v3: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangleLines';
{Draw a triangle fan defined by points (first vertex is the center)}
procedure DrawTriangleFan(const points: PVector2; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangleFan';
{Draw a triangle strip defined by points}
procedure DrawTriangleStrip(const points: PVector2; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangleStrip';
{Draw a regular polygon (Vector version)}
procedure DrawPoly(center: TVector2; sides: Integer; radius: Single; rotation: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPoly';
{Draw a polygon outline of n sides}
procedure DrawPolyLines(center: TVector2; sides: Integer; radius, rotation: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPolyLines';
{Draw a polygon outline of n sides with extended parameters }
procedure DrawPolyLinesEx(center: TVector2; sides: Integer; radius, rotation, lineThick: single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPolyLinesEx';

(* Splines drawing functions *)

{Draw spline: Linear, minimum 2 points}
procedure DrawSplineLinear(const points: PVector3; pointCount: Integer; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineLinear';
{Draw spline: B-Spline, minimum 4 points}
procedure DrawSplineBasis(const points: PVector2; pointCount: Integer; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineBasis';
{Draw spline: Catmull-Rom, minimum 4 points}
procedure DrawSplineCatmullRom(const points: PVector2; pointCount: Integer; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineCatmullRom';
{Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]}
procedure DrawSplineBezierQuadratic(const points: PVector2; pointCount: Integer; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineBezierQuadratic';
{Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]}
procedure DrawSplineBezierCubic(const points: PVector2; pointCount: Integer; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineBezierCubic';
{Draw spline segment: Linear, 2 points}
procedure DrawSplineSegmentLinear(p1, p2: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineSegmentLinear';
{Draw spline segment: B-Spline, 4 points}
procedure DrawSplineSegmentBasis(p1, p2, p3, p4: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineSegmentBasis';
{Draw spline segment: Catmull-Rom, 4 points}
procedure DrawSplineSegmentCatmullRom(p1, p2, p3, p4: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineSegmentCatmullRom';
{Draw spline segment: Quadratic Bezier, 2 points, 1 control point}
procedure DrawSplineSegmentBezierQuadratic(p1, c2, p3: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineSegmentBezierQuadratic';
{Draw spline segment: Cubic Bezier, 2 points, 2 control points}
procedure DrawSplineSegmentBezierCubic(p1, c2, c3, p4: TVector2; thick: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSplineSegmentBezierCubic';

(* Spline segment point evaluation functions, for a given t [0.0f .. 1.0f] *)

{Get (evaluate) spline point: Linear}
function GetSplinePointLinear(startPos, endPos: TVector2; t: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetSplinePointLinear';
{Get (evaluate) spline point: B-Spline}
function GetSplinePointBasis(p1, p2, p3, p4: TVector2; t: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetSplinePointBasis';
{Get (evaluate) spline point: Catmull-Rom}
function GetSplinePointCatmullRom(p1, p2, p3, p4: TVector2; t: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetSplinePointCatmullRom';
{Get (evaluate) spline point: Quadratic Bezier}
function GetSplinePointBezierQuad(p1, c2, p3: TVector2; t: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetSplinePointBezierQuad';
{Get (evaluate) spline point: Cubic Bezier}
function GetSplinePointBezierCubic(p1, c2, c3, p4: TVector2; t: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetSplinePointBezierCubic';

(* Basic shapes collision detection functions *)

{Check collision between two rectangles}
function CheckCollisionRecs(rec1, rec2: TRectangle): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionRecs';
{Check collision between two circles}
function CheckCollisionCircles(center1: TVector2; radius1: Single; center2: TVector2; radius2: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionCircles';
{Check collision between circle and rectangle}
function CheckCollisionCircleRec(center: TVector2; radius: Single; rec: TRectangle): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionCircleRec';
{Check if circle collides with a line created betweeen two points [p1] and [p2]}
function CheckCollisionCircleLine(center: TVector2; radius: Single; p1, p2: TVector2): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionCircleLine';
{Check if point is inside rectangle}
function CheckCollisionPointRec(point: TVector2; rec: TRectangle): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionPointRec';
{Check if point is inside circle}
function CheckCollisionPointCircle(point, center: TVector2; radius: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionPointCircle';
{Check if point is inside a triangle}
function CheckCollisionPointTriangle(point, p1, p2, p3: TVector2): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionPointTriangle';
{Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]}
function CheckCollisionPointLine(point, p1, p2: TVector2; threshold: Integer): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionPointLine';
{Check if point is within a polygon described by array of vertices}
function CheckCollisionPointPoly(point: TVector2; const points: PVector2; pointCount: Integer): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionPointPoly';
{Check the collision between two lines defined by two points each, returns collision point by reference}
function CheckCollisionLines(startPos1, endPos1, startPos2, endPos2: TVector2; collisionPoint: PVector2): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionLines';
{Get collision rectangle for two rectangles collision}
function GetCollisionRec(rec1, rec2: TRectangle): TRectangle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCollisionRec';

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

(* Image loading functions *)
// NOTE: This functions do not require GPU access

{Load image from file into CPU memory (RAM)}
function LoadImage(const fileName: PChar): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImage';
{Load image from RAW file data}
function LoadImageRaw(const fileName: PChar; width, height, format: TPixelFormat; headerSize: Integer): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageRaw';
{Load image sequence from file (frames appended to image.data)}
function LoadImageAnim(const fileName: PChar; frames: PInteger):TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageAnim';
{Load image sequence from memory buffer}
function LoadImageAnimFromMemory(const fileType: PChar; fileData: PChar; dataSize: Integer; frames: PInteger): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageAnimFromMemory';
{Load image from memory buffer, fileType refers to extension: i.e. '.png'}
function LoadImageFromMemory(const fileType: PChar; const fileData: PByte; dataSize: Integer): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageFromMemory';
{Load image from GPU texture data}
function LoadImageFromTexture(texture: TTexture2D): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageFromTexture';
{Load image from screen buffer and (screenshot)}
function LoadImageFromScreen: TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageFromScreen';
{Check if an image is valid (data and parameters)}
function IsImageValid(image: TImage): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsImageValid';
{Unload image from CPU memory (RAM)}
procedure UnloadImage(image: TImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadImage';
{Export image data to file, returns true on success}
function ExportImage(image: TImage; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportImage';
{Export image to memory buffer}
function ExportImageToMemory(image: TImage; const fileType: PChar; fileSize: PInteger): PByte; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportImageToMemory';
{Export image as code file defining an array of bytes, returns true on success  }
function ExportImageAsCode(image: TImage; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportImageAsCode';

(* Image generation functions *)

{Generate image: plain color}
function GenImageColor(width, height: Integer; color: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageColor';
{Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient}
function GenImageGradientLinear(width, height, direction: Integer; start, end_: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageGradientLinear';
{Generate image: radial gradient}
function GenImageGradientRadial(width, height: Integer; density: Single; inner, outer: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageGradientRadial';
{Generate image: square gradient}
function GenImageGradientSquare(width, height: Integer; density: Single; inner, outer: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageGradientSquare';
{Generate image: checked}
function GenImageChecked(width, height, checksX, checksY: Integer; col1, col2: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageChecked';
{Generate image: white noise}
function GenImageWhiteNoise(width, height: Integer; factor: Single): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageWhiteNoise';
{Generate image: perlin noise}
function GenImagePerlinNoise(width, height, offsetX, offsetY: Integer; scale: Single): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImagePerlinNoise';
{Generate image: cellular algorithm, bigger tileSize means bigger cells}
function GenImageCellular(width, height, tileSize: Integer): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageCellular';
{Generate image: grayscale image from text data}
function GenImageText(width, height: Integer; const text: PChar): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageText';

(* Image manipulation functions *)

{Create an image duplicate (useful for transformations)}
function ImageCopy(image: TImage): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageCopy';
{Create an image from another image piece}
function ImageFromImage(image: TImage; rec: TRectangle): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageFromImage';
{Create an image from a selected channel of another image (GRAYSCALE)}
function ImageFromChannel(image: TImage; selectedChannel: Integer): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageFromChannel';
{Create an image from text (default font)}
function ImageText(const text: PChar; fontSize: Integer; color: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageText';
{Create an image from text (custom sprite font)}
function ImageTextEx(font: TFont; const text: PChar; fontSize, spacing: Single; tint: TColorB): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageTextEx';
{Convert image data to desired format}
procedure ImageFormat(image: PImage; newFormat: TPixelFormat); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageFormat';
{Convert image to POT (power-of-two)}
procedure ImageToPOT(image: PImage; fill: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageToPOT';
{Crop an image to a defined rectangle}
procedure ImageCrop(image: PImage; crop: TRectangle); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageCrop';
{Crop image depending on alpha value}
procedure ImageAlphaCrop(image: PImage; threshold: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageAlphaCrop';
{Clear alpha channel to desired color}
procedure ImageAlphaClear(image: PImage; color: TColorB; threshold: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageAlphaClear';
{Apply alpha mask to image}
procedure ImageAlphaMask(image: PImage; alphaMask: TImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageAlphaMask';
{Premultiply alpha channel}
procedure ImageAlphaPremultiply(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageAlphaPremultiply';
{Apply Gaussian blur using a box blur approximation}
procedure ImageBlurGaussian(image: PImage; blurSize: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageBlurGaussian';
{Apply custom square convolution kernel to image}
procedure ImageKernelConvolution(image: PImage; const kernel: PSingle; kernelSize: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageKernelConvolution';
{Resize image (Bicubic scaling algorithm)}
procedure ImageResize(image: PImage; newWidth, newHeight: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageResize';
{Resize image (Nearest-Neighbor scaling algorithm)}
procedure ImageResizeNN(image: PImage; newWidth, newHeight: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageResizeNN';
{Resize canvas and fill with color}
procedure ImageResizeCanvas(image: PImage; newWidth, newHeight, offsetX, offsetY: Integer; fill: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageResizeCanvas';
{Compute all mipmap levels for a provided image}
procedure ImageMipmaps(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageMipmaps';
{Dither image data to 16bpp or lower (Floyd-Steinberg dithering)}
procedure ImageDither(image: PImage; rBpp, gBpp, bBpp, aBpp: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDither';
{Flip image vertically}
procedure ImageFlipVertical(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageFlipVertical';
{Flip image horizontally}
procedure ImageFlipHorizontal(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageFlipHorizontal';
{Rotate image by input angle in degrees (-359 to 359)}
procedure ImageRotate(image: PImage; degrees: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageRotate';
{Rotate image clockwise 90deg}
procedure ImageRotateCW(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageRotateCW';
{Rotate image counter-clockwise 90deg}
procedure ImageRotateCCW(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageRotateCCW';
{Modify image color: tint}
procedure ImageColorTint(image: PImage; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorTint';
{Modify image color: invert}
procedure ImageColorInvert(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorInvert';
{Modify image color: grayscale}
procedure ImageColorGrayscale(image: PImage); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorGrayscale';
{Modify image color: contrast (-100 to 100)}
procedure ImageColorContrast(image: PImage; contrast: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorContrast';
{Modify image color: brightness (-255 to 255)}
procedure ImageColorBrightness(image: PImage; brightness: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorBrightness';
{Modify image color: replace color}
procedure ImageColorReplace(image: PImage; color, replace: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageColorReplace';
{Load color data from image as a Color array (RGBA - 32bit)}
function LoadImageColors(image: TImage): PColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImageColors';
{Load colors palette from image as a Color array (RGBA - 32bit)}
function LoadImagePalette(image: TImage; maxPaletteSize: Integer; colorCount: PInteger): PColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadImagePalette';
{Unload color data loaded with LoadImageColors()}
procedure UnloadImageColors(colors: PColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadImageColors';
{Unload colors palette loaded with LoadImagePalette()}
procedure UnloadImagePalette(colors: PColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadImagePalette';
{Get image alpha border rectangle}
function GetImageAlphaBorder(image: TImage; threshold: Single): TRectangle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetImageAlphaBorder';
{Get image pixel color at (x, y) position}
function GetImageColor(image: TImage; x, y: Integer): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetImageColor';

(* Image drawing functions *)
// NOTE: Image software-rendering functions (CPU)

{Clear image background with given color}
procedure ImageClearBackground(dst: PImage; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageClearBackground';
{Draw pixel within an image}
procedure ImageDrawPixel(dst: PImage; posX, posY: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawPixel';
{Draw pixel within an image (Vector version)}
procedure ImageDrawPixelV(dst: PImage; position: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawPixelV';
{Draw line within an image}
procedure ImageDrawLine(dst: PImage; startPosX, startPosY, endPosX, endPosY: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawLine';
{Draw line within an image (Vector version)}
procedure ImageDrawLineV(dst: PImage; start, _end: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawLineV';
{Draw a line defining thickness within an image}
procedure ImageDrawLineEx(dst: PImage; start, _end: TVector2; thick: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawLineEx';
{Draw a filled circle within an image}
procedure ImageDrawCircle(dst: PImage; centerX, centerY, radius: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawCircle';
{Draw a filled circle within an image (Vector version)}
procedure ImageDrawCircleV(dst: PImage; center: TVector2; radius: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawCircleV';
{Draw circle outline within an image}
procedure ImageDrawCircleLines(dst: PImage; centerX, centerY, radius: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawCircleLines';
{Draw circle outline within an image (Vector version)}
procedure ImageDrawCircleLinesV(dst: PImage; center: TVector2; radius: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawCircleLinesV';
{Draw rectangle within an image}
procedure ImageDrawRectangle(dst: PImage; posX, posY, width, height: Integer; color: TColorB);cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawRectangle';
{Draw rectangle within an image (Vector version)}
procedure ImageDrawRectangleV(dst: PImage; position, size: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawRectangleV';
{Draw rectangle within an image}
procedure ImageDrawRectangleRec(dst: PImage; rec: TRectangle; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawRectangleRec';
{Draw rectangle lines within an image}
procedure ImageDrawRectangleLines(dst: PImage; rec: TRectangle; thick: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawRectangleLines';
{Draw triangle within an image}
procedure ImageDrawTriangle(dst: PImage; v1, v2, v3: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTriangle';
{Draw triangle with interpolated colors within an image}
procedure ImageDrawTriangleEx(dst: PImage; v1, v2, v3: TVector2; c1, c2, c3: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTriangleEx';
{Draw triangle outline within an image}
procedure ImageDrawTriangleLines(dst: PImage; v1, v2, v3: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTriangleLines';
{Draw a triangle fan defined by points within an image (first vertex is the center)}
procedure ImageDrawTriangleFan(dst: PImage; const points: PVector2; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTriangleFan';
{Draw a triangle strip defined by points within an image}
procedure ImageDrawTriangleStrip(dst: PImage; const points: PVector2; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTriangleStrip';
{Draw a source image within a destination image (tint applied to source)}
procedure ImageDraw(dst: PImage; src: TImage; srcRec, dstRec: TRectangle; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDraw';
{Draw text (using default font) within an image (destination)}
procedure ImageDrawText(dst: PImage; const text: PChar; posX, posY, fontSize: Integer; color:TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawText';
{Draw text (custom sprite font) within an image (destination)}
procedure ImageDrawTextEx(dst: PImage; font: TFont; const text: PChar; position: TVector2; fontSize, spacing: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ImageDrawTextEx';

(* Texture loading functions *)
// NOTE: These functions require GPU access

{Load texture from file into GPU memory (VRAM)}
function LoadTexture(const fileName: PChar): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadTexture';
{Load texture from image data}
function LoadTextureFromImage(image: TImage): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadTextureFromImage';
{Load cubemap from image, multiple image cubemap layouts supported}
function LoadTextureCubemap(image: TImage; layout: TCubemapLayout): TTextureCubemap; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadTextureCubemap';
{Load texture for rendering (framebuffer)}
function LoadRenderTexture(width, height: Integer): TRenderTexture2D; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadRenderTexture';
{Check if a texture is valid (loaded in GPU)}
function IsTextureValid(texture: TTexture2D): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsTextureValid';
{Unload texture from GPU memory (VRAM)}
procedure UnloadTexture(texture: TTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadTexture';
{Check if a render texture is valid (loaded in GPU)}
function IsRenderTextureValid(target: TRenderTexture2D): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsRenderTextureValid';
{Unload render texture from GPU memory (VRAM)}
procedure UnloadRenderTexture(target: TRenderTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadRenderTexture';
{Update GPU texture with new data (pixels should be able to fill texture)}
procedure UpdateTexture(texture: TTexture2D; const pixels: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateTexture';
{Update GPU texture rectangle with new data (pixels and rec should fit in texture)}
procedure UpdateTextureRec(texture: TTexture2D; rec: TRectangle; const pixels: Pointer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateTextureRec';

(* Texture configuration functions *)

{Generate GPU mipmaps for a texture}
procedure GenTextureMipmaps(texture: PTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenTextureMipmaps';
{Set texture scaling filter mode}
procedure SetTextureFilter(texture: TTexture2D; filter: TTextureFilter); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTextureFilter';
{Set texture wrapping mode}
procedure SetTextureWrap(texture: TTexture2D; wrap: TTextureWrap); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTextureWrap';

(* Texture drawing functions *)

{Draw a Texture2D}
procedure DrawTexture(texture: TTexture2D; posX, posY: Integer; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTexture';
{Draw a Texture2D with position defined as Vector2}
procedure DrawTextureV(texture: TTexture2D; position: TVector2; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextureV';
{Draw a Texture2D with extended parameters}
procedure DrawTextureEx(texture: TTexture2D; position: TVector2; rotation, scale: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextureEx';
{Draw a part of a texture defined by a rectangle}
procedure DrawTextureRec(texture: TTexture2D; source: TRectangle; position: TVector2; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextureRec';
{Draw a part of a texture defined by a rectangle with 'pro' parameters}
procedure DrawTexturePro(texture: TTexture2D; source, dest: TRectangle; origin: TVector2; rotation: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTexturePro';
{Draws a texture (or part of it) that stretches or shrinks nicely}
procedure DrawTextureNPatch(texture: TTexture2D; nPatchInfo: TNPatchInfo; dest: TRectangle; origin: TVector2; rotation: Single; tint: TColorB);cdecl;external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextureNPatch';

(* Color/pixel related functions *)

{Check if two colors are equal}
function ColorIsEqual(col1, col2: TColor): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorIsEqual';
{Get color with alpha applied, alpha goes from 0.0f to 1.0f}
function Fade(color: TColorB; alpha: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'Fade';
{Get hexadecimal value for a Color}
function ColorToInt(color: TColorB): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorToInt';
{Get Color normalized as float [0..1]}
function ColorNormalize(color: TColorB): TVector4; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorNormalize';
{Get Color from normalized values [0..1]}
function ColorFromNormalized(normalized: TVector4): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorFromNormalized';
{Get HSV values for a Color, hue [0..360], saturation/value [0..1]}
function ColorToHSV(color: TColorB): TVector3; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorToHSV';
{Get a Color from HSV values, hue [0..360], saturation/value [0..1]}
function ColorFromHSV(hue, saturation, value: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorFromHSV';
{Get color multiplied with another color}
function ColorTint(color, tint: TColorB): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorTint';
{Get color with brightness correction, brightness factor goes from -1.0 to 1.0}
function ColorBrightness(color: TColorB; factor: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorBrightness';
{Get color with contrast correction, contrast values between -1.0 and 1.0}
function ColorContrast(color: TColorB; contrast: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorContrast';
{Get color with alpha applied, alpha goes from 0.0 to 1.0}
function ColorAlpha(color: TColorB; alpha: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorAlpha';
{Get src alpha-blended into dst color with tint}
function ColorAlphaBlend(dst, src, tint: TColorB): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorAlphaBlend';
{Get color lerp interpolation between two colors, factor [0.0..1.0]}
function ColorLerp(color1, color2: TColorB; factor: Single): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ColorLerp';
{Get Color structure from hexadecimal value}
function GetColor(hexValue: LongWord): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetColor';
{Get Color from a source pixel pointer of certain format}
function GetPixelColor(srcPtr: Pointer; format: TPixelFormat): TColorB; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetPixelColor';
{Set color formatted into destination pixel pointer}
procedure SetPixelColor(dstPtr: Pointer; color: TColorB; format: TPixelFormat); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetPixelColor';
{Get pixel data size in bytes for certain format}
function GetPixelDataSize(width, height: Integer; format: TPixelFormat): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetPixelDataSize';

//------------------------------------------------------------------------------------
// TFont Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

(* Font loading/unloading functions *)

{Get the default Font}
function GetFontDefault: TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetFontDefault';
{Load font from file into GPU memory (VRAM)}
function LoadFont(const fileName: PChar): TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFont';
{Load font from file with extended parameters, use NULL for codepoints and 0 for codepointCount to load the default character set, font size is provided in pixels height}
function LoadFontEx(const fileName: Pchar; fontSize: Integer; codepoints: PInteger; codepointCount: Integer): TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFontEx';
{Load font from Image (XNA style)}
function LoadFontFromImage(image: TImage; key: TColorB; firstChar: Integer): TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFontFromImage';
{Load font from memory buffer, fileType refers to extension: i.e. '.ttf'}
function LoadFontFromMemory(const fileType: PChar; const fileData: PByte; dataSize, fontSize: Integer; codepoints: PInteger; codepointCount: Integer): TFont; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFontFromMemory';
{Check if a font is valid (font data loaded, WARNING: GPU texture not checked)}
function IsFontValid(font: TFont): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsFontValid';
{Load font data for further uses}
function LoadFontData(const fileData: PByte; dataSize, fontSize: Integer; codepoints: PInteger; codepointCount, _type: Integer): PGlyphInfo; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadFontData';
{Generate image font atlas using chars info}
function GenImageFontAtlas(const glyphs: PGlyphInfo; glyphRecs: PPRectangle; glyphCount, fontSize, padding, packMethod: Integer): TImage; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenImageFontAtlas';
{Unload font chars info data (RAM)}
procedure UnloadFontData(glyphs: PGlyphInfo; glyphCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadFontData';
{Unload Font from GPU memory (VRAM)}
procedure UnloadFont(font: TFont); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadFont';
{Export font as code file, returns true on success}
procedure ExportFontAsCode(font: TFont; const fileName: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportFontAsCode';

(* Text drawing functions *)

{Draw current FPS}
procedure DrawFPS(posX, posY: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawFPS';
{Draw text (using default font)}
procedure DrawText(const text: PChar; posX, posY, fontSize: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawText';
{Draw text using font and additional parameters}
procedure DrawTextEx(font: TFont; const text: PChar; position: TVector2; fontSize, spacing: Single; tint:TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextEx';
{Draw text using Font and pro parameters (rotation)}
procedure DrawTextPro(font: TFont; const text: PChar; position, origin: TVector2; rotation, fontSize, spacing: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextPro';
{Draw one character (codepoint)}
procedure DrawTextCodepoint(font: TFont; codepoint: Integer; position: TVector2; fontSize: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextCodepoint';
{Draw multiple character (codepoint)}
procedure DrawTextCodepoints(font: TFont; const codepoints: PInteger; codepointCount: Integer; position: TVector2; fontSize, spacing: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTextCodepoints';

(* Text font info functions *)

{Set vertical line spacing when drawing with line-breaks}
procedure SetTextLineSpacing(spacing: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetTextLineSpacing';
{Measure string width for default font}
function MeasureText(const text: PChar; fontSize: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MeasureText';
{Measure string size for Font}
function MeasureTextEx(font: TFont; const text: PChar; fontSize, spacing: Single): TVector2; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'MeasureTextEx';
{Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphIndex(font: TFont; codepoint: Integer): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGlyphIndex';
{Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphInfo(font: TFont; codepoint: Integer): TGlyphInfo; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGlyphInfo';
{Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found}
function GetGlyphAtlasRec(font: TFont; codepoint: Integer): TRectangle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetGlyphAtlasRec';

(* Text codepoints management functions (unicode characters) *)

{Load UTF-8 text encoded from codepoints array}
function LoadUTF8(const codepoints: PInteger; length: Integer): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadUTF8';
{Unload UTF-8 text encoded from codepoints array}
procedure UnloadUTF8(text: PChar); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadUTF8';
{Load all codepoints from a UTF-8 text string, codepoints count returned by parameter}
function LoadCodepoints(const text: PChar; count: PInteger): PInteger; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadCodepoints';
{Unload codepoints data from memory}
procedure UnloadCodepoints(codepoints: PInteger); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadCodepoints';
{Get total number of codepoints in a UTF-8 encoded string}
function GetCodepointCount(const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCodepointCount';
{Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure}
function GetCodepoint(const text: PChar; codepointSize: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCodepoint';
{Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure}
function GetCodepointNext(const text: PChar; codepointSize: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCodepointNext';
{Get previous codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure}
function GetCodepointPrevious(const text: PChar; codepointSize: PInteger): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetCodepointPrevious';
{Encode one codepoint into UTF-8 byte array (array length returned as parameter)}
function CodepointToUTF8(codepoint: Integer; utf8Size: PInteger): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CodepointToUTF8';

(* Text strings management functions (no UTF-8 strings, only byte chars) *)
// WARNING 1: Most of these functions use internal static buffers, it's recommended to store returned data on user-side for re-use
// WARNING 2: Some strings allocate memory internally for the returned strings, those strings must be free by user using MemFree()

{Copy one string to another, returns bytes copied}
function TextCopy(dst: PChar; const src: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextCopy';
{Check if two text string are equal}
function TextIsEqual(const text1, text2: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextIsEqual';
{Get text length, checks for '\0' ending}
function TextLength(const text: PChar): LongWord; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextLength';
{Text formatting with variables (sprintf() style)}
function TextFormat(const text: PChar): PChar; cdecl; varargs; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextFormat';
{Get a piece of a text string}
function TextSubtext(const text: PChar; position, length: Integer): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextSubtext';
{Replace text string (WARNING: memory must be freed!)}
function TextReplace(const text: PChar; const replace, by: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextReplace';
{Insert text in a position (WARNING: memory must be freed!)}
function TextInsert(const text, insert: PChar; position: Integer): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextInsert';
{Join text strings with delimiter}
function TextJoin(textList: PPChar; count: Integer; const delimiter: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextJoin';
{Split text into multiple strings}
function TextSplit(const text: PChar; delimiter: Char; count: PInteger): PPChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextSplit';
{Append text at specific position and move cursor!}
procedure TextAppend(text: PChar; const append: PChar; position: PInteger); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextAppend';
{Find first text occurrence within a string}
function TextFindIndex(const text, find: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextFindIndex';
{Get upper case version of provided string}
function TextToUpper(const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToUpper';
{Get lower case version of provided string}
function TextToLower(const text: PChar):PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToLower';
{Get Pascal case notation version of provided string}
function TextToPascal(const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToPascal';
{Get Snake case notation version of provided string}
function TextToSnake(const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToSnake';
{Get Camel case notation version of provided string}
function TextToCamel(const text: PChar): PChar; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToCamel';
{Get integer value from text (negative values not supported)}
function TextToInteger(const text: PChar): Integer; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToInteger';
{Get float value from text (negative values not supported)}
function TextToFloat(const text: PChar): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'TextToFloat';

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Basic geometric 3D shapes drawing functions *)

{Draw a line in 3D world space}
procedure DrawLine3D(startPos, endPos: TVector3; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawLine3D';
{Draw a point in 3D space, actually a small line}
procedure DrawPoint3D(position: TVector3; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPoint3D';
{Draw a circle in 3D world space}
procedure DrawCircle3D(center: TVector3; radius: Single; rotationAxis: TVector3; rotationAngle: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCircle3D';
{Draw a color-filled triangle (vertex in counter-clockwise order!)}
procedure DrawTriangle3D(v1, v2, v3: TVector3; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangle3D';
{Draw a triangle strip defined by points}
procedure DrawTriangleStrip3D(const points: PVector3; pointCount: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawTriangleStrip3D';
{Draw cube}
procedure DrawCube(position: TVector3; width, height, length: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCube';
{Draw cube (Vector version)}
procedure DrawCubeV(position, size: TVector3; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCubeV';
{Draw cube wires}
procedure DrawCubeWires(position: TVector3; width, height, length: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCubeWires';
{Draw cube wires (Vector version)}
procedure DrawCubeWiresV(position, size: TVector3; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCubeWiresV';
{Draw sphere}
procedure DrawSphere(centerPos: TVector3; radius: Single; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSphere';
{Draw sphere with extended parameters}
procedure DrawSphereEx(centerPos: TVector3; radius: Single; rings, slices: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSphereEx';
{Draw sphere wires}
procedure DrawSphereWires(centerPos: TVector3; radius: Single; rings, slices: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawSphereWires';
{Draw a cylinder/cone}
procedure DrawCylinder(position: TVector3; radiusTop, radiusBottom, height: Single; slices: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCylinder';
{Draw a cylinder with base at startPos and top at endPos}
procedure DrawCylinderEx(startPos, endPos: TVector3; startRadius, endRadius: Single; sides: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCylinderEx';
{Draw a cylinder/cone wires}
procedure DrawCylinderWires(position: TVector3; radiusTop, radiusBottom, height: Single; slices: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCylinderWires';
{Draw a cylinder wires with base at startPos and top at endPos}
procedure DrawCylinderWiresEx(startPos, endPos: TVector3; startRadius, endRadius: Single; sides: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCylinderWiresEx';
{Draw a capsule with the center of its sphere caps at startPos and endPos}
procedure DrawCapsule(startPos, endPos: TVector3;  radius: Single; slices, rings: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCapsule';
{Draw capsule wireframe with the center of its sphere caps at startPos and endPos}
procedure DrawCapsuleWires(startPos, endPos: TVector3;  radius: Single; slices, rings: Integer; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawCapsuleWires';
{Draw a plane XZ}
procedure DrawPlane(centerPos: TVector3; size: TVector2; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawPlane';
{Draw a ray line}
procedure DrawRay(ray: TRay; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawRay';
{Draw a grid (centered at (0, 0, 0))}
procedure DrawGrid(slices: Integer; spacing: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawGrid';

//------------------------------------------------------------------------------------
// TModel 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

(* Model management functions *)

{Load model from files (meshes and materials)}
function LoadModel(const fileName: PChar): TModel; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadModel';
{Load model from generated mesh (default material)}
function LoadModelFromMesh(mesh: TMesh): TModel; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadModelFromMesh';
{Check if a model is valid (loaded in GPU, VAO/VBOs)}
function IsModelValid(model: TModel): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsModelValid';
{Unload model (including meshes) from memory (RAM and/or VRAM)}
procedure UnloadModel(model: TModel); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadModel';
{Compute model bounding box limits (considers all meshes)}
function GetModelBoundingBox(model: TModel): TBoundingBox; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetModelBoundingBox';

(* Model drawing functions *)

{Draw a model (with texture if set)}
procedure DrawModel(model: TModel; position: TVector3; scale: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModel';
{Draw a model with extended parameters}
procedure DrawModelEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModelEx';
{Draw a model wires (with texture if set)}
procedure DrawModelWires(model: TModel; position: TVector3; scale: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModelWires';
{Draw a model wires (with texture if set) with extended parameters}
procedure DrawModelWiresEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModelWiresEx';
{Draw a model as points}
procedure DrawModelPoints(model: TModel; position: TVector3; scale: Single; tint: TColorB) cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModelPoints';
{Draw a model as points with extended parameters}
procedure DrawModelPointsEx(model: TModel; position: TVector3; rotationAxis: TVector3; rotationAngle: Single; scale: TVector3; tint: TColorB) cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawModelPointsEx';
{Draw bounding box (wires)}
procedure DrawBoundingBox(box: TBoundingBox; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawBoundingBox';
{Draw a billboard texture}
procedure DrawBillboard(camera: TCamera; texture: TTexture2D; position: TVector3; scale: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawBillboard';
{Draw a billboard texture defined by source}
procedure DrawBillboardRec(camera: TCamera; texture: TTexture2D; source: TRectangle; position: TVector3; size: TVector2; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawBillboardRec';
{Draw a billboard texture defined by source and rotation}
procedure DrawBillboardPro(camera: TCamera; texture: TTexture2D; source: TRectangle; position, up: TVector3; size, origin: TVector2; rotation: Single; tint: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawBillboardPro';

(* Mesh management functions *)

{Upload mesh vertex data in GPU and provide VAO/VBO ids}
procedure UploadMesh(mesh: PMesh; _dynamic: Boolean); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UploadMesh';
{Update mesh vertex data in GPU for a specific buffer index}
procedure UpdateMeshBuffer(mesh: TMesh; index: Integer; const data: Pointer; dataSize, offset: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateMeshBuffer';
{Unload mesh data from CPU and GPU}
procedure UnloadMesh(mesh: TMesh); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadMesh';
{Draw a 3d mesh with material and transform}
procedure DrawMesh(mesh: TMesh; material: TMaterial; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawMesh';
{Draw multiple mesh instances with material and different transforms}
procedure DrawMeshInstanced(mesh: TMesh; material: TMaterial; const transforms: PMatrix; instances: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawMeshInstanced';
{Compute mesh bounding box limits}
function GetMeshBoundingBox(mesh: TMesh): TBoundingBox; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMeshBoundingBox';
{Compute mesh tangents}
procedure GenMeshTangents(mesh: PMesh); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshTangents';
{Export mesh data to file, returns true on success}
function ExportMesh(mesh: TMesh; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportMesh';
{Export mesh as code file (.h) defining multiple arrays of vertex attributes}
function ExportMeshAsCode(mesh: TMesh; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportMeshAsCode';

(* Mesh generation functions *)

{Generate polygonal mesh}
function GenMeshPoly(sides: Integer; radius: Single): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshPoly';
{Generate plane mesh (with subdivisions)}
function GenMeshPlane(width, length: Single; resX, resZ: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshPlane';
{Generate cuboid mesh}
function GenMeshCube(width, height, length: Single): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshCube';
{Generate sphere mesh (standard sphere)}
function GenMeshSphere(radius: Single; rings, slices: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshSphere';
{Generate half-sphere mesh (no bottom cap)}
function GenMeshHemiSphere(radius: Single; rings, slices: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshHemiSphere';
{Generate cylinder mesh}
function GenMeshCylinder(radius, height: Single; slices: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshCylinder';
{Generate cone/pyramid mesh}
function GenMeshCone(radius, height: Single; slices: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshCone';
{Generate torus mesh}
function GenMeshTorus(radius, size: Single; radSeg, sides: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshTorus';
{Generate trefoil knot mesh}
function GenMeshKnot(radius, size: Single; radSeg, sides: Integer): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshKnot';
{Generate heightmap mesh from image data}
function GenMeshHeightmap(heightmap: TImage; size: TVector3): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshHeightmap';
{Generate cubes-based map mesh from image data}
function GenMeshCubicmap(cubicmap: TImage; cubeSize: TVector3): TMesh; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GenMeshCubicmap';

(* Material loading/unloading functions *)

{Load materials from model file}
function LoadMaterials(const fileName: PChar; materialCount: PInteger): PMaterial; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadMaterials';
{Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)}
function LoadMaterialDefault: TMaterial; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadMaterialDefault';
{Check if a material is valid (shader assigned, map textures loaded in GPU)}
function IsMaterialValid(material: TMaterial): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMaterialValid';
{Unload material from GPU memory (VRAM)}
procedure UnloadMaterial(material: TMaterial); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadMaterial';
{Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)}
procedure SetMaterialTexture(material: PMaterial; mapType: TMaterialMapIndex; texture: TTexture2D); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMaterialTexture';
{Set material for a mesh}
procedure SetModelMeshMaterial(model: PModel; meshId, materialId: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetModelMeshMaterial';

(* Model animations loading/unloading functions *)

{Load model animations from file}
function LoadModelAnimations(fileName: PChar; animCount: PInteger): PModelAnimation; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadModelAnimations';
{Update model animation pose (CPU)}
procedure UpdateModelAnimation(model: TModel; anim: TModelAnimation; frame: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateModelAnimation';
{Update model animation mesh bone matrices (GPU skinning)}
procedure UpdateModelAnimationBones(model: TModel; anim: TModelAnimation; frame: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateModelAnimationBones';
{Unload animation data}
procedure UnloadModelAnimation(anim: TModelAnimation); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadModelAnimation';
{Unload animation array data}
procedure UnloadModelAnimations(animations: PModelAnimation; animCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadModelAnimations';
{Check model animation skeleton match}
function IsModelAnimationValid(model: TModel; anim: TModelAnimation): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsModelAnimationValid';

(* Collision detection functions *)

{Check collision between two spheres}
function CheckCollisionSpheres(center1: TVector3; radius1: Single; center2: TVector3; radius2: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionSpheres';
{Check collision between two bounding boxes}
function CheckCollisionBoxes(box1, box2: TBoundingBox): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionBoxes';
{Check collision between box and sphere}
function CheckCollisionBoxSphere(box: TBoundingBox; center: TVector3; radius: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CheckCollisionBoxSphere';
{Get collision info between ray and sphere}
function GetRayCollisionSphere(ray: TRay; center: TVector3; radius: Single): TRayCollision; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRayCollisionSphere';
{Get collision info between ray and box}
function GetRayCollisionBox(ray: TRay; box: TBoundingBox): TRayCollision; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRayCollisionBox';
{Get collision info between ray and mesh}
function GetRayCollisionMesh(ray: TRay; mesh: TMesh; transform: TMatrix): TRayCollision; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRayCollisionMesh';
{Get collision info between ray and triangle}
function GetRayCollisionTriangle(ray: TRay; p1, p2, p3: TVector3): TRayCollision; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRayCollisionTriangle';
{Get collision info between ray and quad}
function GetRayCollisionQuad(ray: TRay; p1, p2, p3, p4: TVector3): TRayCollision; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetRayCollisionQuad';

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------
type
PAudioCallback = ^TAudioCallback;
TAudioCallback = procedure (bufferData: Pointer; frames: LongWord); cdecl;

(* Audio device management functions *)

{Initialize audio device and context}
procedure InitAudioDevice; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'InitAudioDevice';
{Close the audio device and context}
procedure CloseAudioDevice; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'CloseAudioDevice';
{Check if audio device has been initialized successfully}
function IsAudioDeviceReady: Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsAudioDeviceReady';
{Set master volume (listener)}
procedure SetMasterVolume(volume: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMasterVolume';
{Get master volume (listener)}
function GetMasterVolume(): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMasterVolume';

(* Wave/Sound loading/unloading functions *)

{Load wave data from file}
function LoadWave(const fileName: PChar): TWave; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadWave';
{Load wave from memory buffer, fileType refers to extension: i.e. '.wav' }
function LoadWaveFromMemory(const fileType: PChar; const fileData: PByte; dataSize: Integer): TWave; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadWaveFromMemory';
{Checks if wave data is valid (data loaded and parameters)}
function IsWaveValid(wave: TWave): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsWaveValid';
{Load sound from file}
function LoadSound(const fileName: PChar): TSound; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadSound';
{Load sound from wave data}
function LoadSoundFromWave(wave: TWave): TSound; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadSoundFromWave';
{Create a new sound that shares the same sample data as the source sound, does not own the sound data}
function LoadSoundAlias(source: TSound): TSound; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadSoundAlias';
{Checks if a sound is valid (data loaded and buffers initialized)}
function IsSoundValid(sound: TSound): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsSoundValid';
{Update sound buffer with new data (data and frame count should fit in sound)}
procedure UpdateSound(sound: TSound; const data: Pointer; sampleCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateSound';
{Unload wave data}
procedure UnloadWave(wave: TWave); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadWave';
{Unload sound}
procedure UnloadSound(sound: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadSound';
{Unload a sound alias (does not deallocate sample data)}
procedure UnloadSoundAlias(alias: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadSoundAlias';
{Export wave data to file, returns true on success}
function ExportWave(wave: TWave; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportWave';
{Export wave sample data to code (.h), returns true on success}
function ExportWaveAsCode(wave: TWave; const fileName: PChar): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ExportWaveAsCode';

(* Wave/Sound management functions *)

{Play a sound}
procedure PlaySound(sound: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PlaySound';
{Stop playing a sound}
procedure StopSound(sound: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'StopSound';
{Pause a sound}
procedure PauseSound(sound: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PauseSound';
{Resume a paused sound}
procedure ResumeSound(sound: TSound); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ResumeSound';
{Check if a sound is currently playing}
function IsSoundPlaying(sound: TSound): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsSoundPlaying';
{Set volume for a sound (1.0 is max level)}
procedure SetSoundVolume(sound: TSound; volume: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetSoundVolume';
{Set pitch for a sound (1.0 is base level)}
procedure SetSoundPitch(sound: TSound; pitch: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetSoundPitch';
{Set pan for a sound (0.5 is center)}
procedure SetSoundPan(sound: TSound; pan: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetSoundPan';
{Copy a wave to a new wave}
function WaveCopy(wave: TWave): TWave; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'WaveCopy';
{Crop a wave to defined frames range}
procedure WaveCrop(wave: PWave; initFrame, finalFrame: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'WaveCrop';
{Convert wave data to desired format}
procedure WaveFormat(wave: PWave; sampleRate, sampleSize, channels: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'WaveFormat';
{Load samples data from wave as a floats array}
function LoadWaveSamples(wave: TWave): PSingle; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadWaveSamples';
{Unload samples data loaded with LoadWaveSamples()}
procedure UnloadWaveSamples(samples: PSingle); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadWaveSamples';

(* Music management functions *)

{Load music stream from file}
function LoadMusicStream(const fileName: PChar): TMusic; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadMusicStream';
{Load music stream from data}
function LoadMusicStreamFromMemory(const fileType: PChar; const data: PByte; dataSize: Integer): TMusic; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadMusicStreamFromMemory';
{Checks if a music stream is valid (context and buffers initialized)}
function IsMusicValid(music: TMusic): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMusicValid';
{Unload music stream}
procedure UnloadMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadMusicStream';
{Start music playing}
procedure PlayMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PlayMusicStream';
{Check if music is playing}
function IsMusicStreamPlaying(music: TMusic): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsMusicStreamPlaying';
{Updates buffers for music streaming}
procedure UpdateMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateMusicStream';
{Stop music playing}
procedure StopMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'StopMusicStream';
{Pause music playing}
procedure PauseMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PauseMusicStream';
{Resume playing paused music}
procedure ResumeMusicStream(music: TMusic); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ResumeMusicStream';
{Seek music to a position (in seconds)}
procedure SeekMusicStream(music: TMusic; position: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SeekMusicStream';
{Set volume for music (1.0 is max level)}
procedure SetMusicVolume(music: TMusic; volume: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMusicVolume';
{Set pitch for a music (1.0 is base level)}
procedure SetMusicPitch(music: TMusic; pitch: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMusicPitch';
{Set pan for a music (0.5 = center)}
procedure SetMusicPan(music: TMusic; pan: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetMusicPan';
{Get music time length (in seconds)}
function GetMusicTimeLength(music: TMusic): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMusicTimeLength';
{Get current music time played (in seconds)}
function GetMusicTimePlayed(music: TMusic): Single; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GetMusicTimePlayed';

(* AudioStream management functions *)

{Load audio stream (to stream raw audio pcm data)}
function LoadAudioStream(sampleRate, sampleSize, channels: LongWord): TAudioStream; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'LoadAudioStream';
{Checks if an audio stream is valid (buffers initialized)}
function IsAudioStreamValid(stream: TAudioStream): boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsAudioStreamValid';
{Unload audio stream and free memory}
procedure UnloadAudioStream(stream: TAudioStream); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UnloadAudioStream';
{Update audio stream buffers with data}
procedure UpdateAudioStream(stream: TAudioStream; const data: Pointer; frameCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'UpdateAudioStream';
{Check if any audio stream buffers requires refill}
function IsAudioStreamProcessed(stream: TAudioStream): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsAudioStreamProcessed';
{Play audio stream}
procedure PlayAudioStream(stream: TAudioStream); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PlayAudioStream';
{Pause audio stream}
procedure PauseAudioStream(stream: TAudioStream); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'PauseAudioStream';
{Resume audio stream}
procedure ResumeAudioStream(stream: TAudioStream); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'ResumeAudioStream';
{Check if audio stream is playing}
function IsAudioStreamPlaying(stream: TAudioStream): Boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'IsAudioStreamPlaying';
{Stop audio stream}
procedure StopAudioStream(stream: TAudioStream); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'StopAudioStream';
{Set volume for audio stream (1.0 is max level)}
procedure SetAudioStreamVolume(stream: TAudioStream; volume: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAudioStreamVolume';
{Set pitch for audio stream (1.0 is base level)}
procedure SetAudioStreamPitch(stream: TAudioStream; pitch: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAudioStreamPitch';
{Set pan for audio stream (0.5 is centered)}
procedure SetAudioStreamPan(stream: TAudioStream; pan: Single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAudioStreamPan';
{Default size for new audio streams}
procedure SetAudioStreamBufferSizeDefault(size: Integer); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAudioStreamBufferSizeDefault';
{Audio thread callback to request new data}
procedure SetAudioStreamCallback(stream: TAudioStream; callback: TAudioCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetAudioStreamCallback';

{Attach audio stream processor to stream, receives frames x 2 samples as 'float' (stereo)}
procedure AttachAudioStreamProcessor(stream: TAudioStream; processor: TAudioCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'AttachAudioStreamProcessor';
{Attach audio stream processor to the entire audio pipeline, receives the samples as 'float'}
procedure DetachAudioStreamProcessor(stream: TAudioStream; processor: TAudioCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DetachAudioStreamProcessor';
{Attach audio stream processor to the entire audio pipeline, receives frames x 2 samples as 'float' (stereo)}
procedure AttachAudioMixedProcessor(processor: TAudioCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'AttachAudioMixedProcessor';
{Detach audio stream processor from the entire audio pipeline}
procedure DetachAudioMixedProcessor(processor: TAudioCallback); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DetachAudioMixedProcessor';

(* Custom Misc Functions to help simplify a few things *)

function Vector2Create(aX: Single; aY: Single): TVector2;
procedure Vector2Set(aVec: PVector2; aX: Single; aY: Single);

function Vector3Create(aX: Single; aY: Single; aZ: Single): TVector3;
procedure Vector3Set(aVec: PVector3; aX: Single; aY: Single; aZ: Single);

function Vector4Create(aX: Single; aY: Single; aZ: Single; aW: Single): TVector4;
procedure Vector4Set(aVec: PVector4; aX: Single; aY: Single; aZ: Single; aW: Single);

function QuaternionCreate(aX: Single; aY: Single; aZ: Single; aW: Single): TQuaternion;
procedure QuaternionSet(aQuat: PQuaternion; aX: Single; aY: Single; aZ: Single; aW: Single);

function ColorCreate(aR: Byte; aG: Byte; aB: Byte; aA: Byte): TColorB;
procedure ColorSet(aColor: PColorB; aR: Byte; aG: Byte; aB: Byte; aA: Byte);

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single): TRectangle;
procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single; aWidth: Single; aHeight: Single);

function BoundingBoxCreate(aMin, aMax: TVector3): TBoundingBox;
procedure BoundingBoxSet(aBoundingBox: PBoundingBox; aMin, aMax: TVector3);

function Camera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: Single; aProjection: Integer): TCamera3D;
procedure Camera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: Single; aType: Integer);

{ TVector2 operators }
operator := (a: TVector2Data): TVector2; inline;
operator + (a, b: TVector2): TVector2; overload; inline;
operator + (a: TVector2; b: Single): TVector2; overload; inline;
operator - (a: TVector2): TVector2; overload; inline;
operator - (a, b: TVector2): TVector2; overload; inline;
operator - (a: TVector2; b: Single): TVector2; overload; inline;
operator * (a, b: TVector2): TVector2; overload; inline;
operator * (a: TVector2; b: Single): TVector2; overload; inline;
operator / (a, b: TVector2): TVector2; inline;
operator = (a, b: TVector2): Boolean; inline;

{ TVector3 operators }
operator := (a: TVector3Data): TVector3; inline;
operator + (a, b: TVector3): TVector3; overload; inline;
operator + (a: TVector3; b: Single): TVector3; overload; inline;
operator - (a: TVector3): TVector3; overload; inline;
operator - (a, b: TVector3): TVector3; overload; inline;
operator - (a: TVector3; b: Single): TVector3; overload; inline;
operator * (a, b: TVector3): TVector3; overload; inline;
operator * (a: TVector3; b: Single): TVector3; overload; inline;
operator / (a, b: TVector3): TVector3; inline;
operator = (a, b: TVector3): Boolean; inline;

{ TVector4 operators }
operator := (a: TVector4Data): TVector4; inline;
operator + (a, b: TVector4): TVector4; overload; inline;
operator + (a: TVector4; b: Single): TVector4; overload; inline;
operator - (a: TVector4): TVector4; overload; inline;
operator - (a, b: TVector4): TVector4; overload; inline;
operator - (a: TVector4; b: Single): TVector4; overload; inline;
operator * (a, b: TVector4): TVector4; overload; inline;
operator * (a: TVector4; b: Single): TVector4; overload; inline;
operator / (a, b: TVector4): TVector4; inline;
operator = (a, b: TVector4): Boolean; inline;

{ TMatrix operators }
operator + (a, b: TMatrix): TMatrix; inline;
operator - (a, b: TMatrix): TMatrix; inline;
operator * (a, b: TMatrix): TMatrix; inline;
operator = (a, b: TMatrix): Boolean; inline;

{ TColorB operators }
operator := (a: TColorBData): TColorB; inline;
operator = (a, b: TColorB): Boolean; inline;

implementation
uses
  Math, raymath;

{$IFDEF UNIX}
  {$IFDEF RAY_STATIC}
    {$linklib c}
    {$linklib m}
    {$linklib dl}
    {$linklib pthread}
    {$linklib libraylib.a}
  {$ENDIF}
{$ENDIF}

function GetMouseRay(mousePosition: TVector2; camera: TCamera): TRay;
begin
  Result := GetScreenToWorldRay(mousePosition,camera);
end;

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

function Vector4Create(aX: Single; aY: Single; aZ: Single; aW: Single
  ): TVector4;
begin
  Result.x := aX;
  Result.y := aY;
  Result.z := aZ;
  Result.w := aW;
end;

procedure Vector4Set(aVec: PVector4; aX: Single; aY: Single; aZ: Single;
  aW: Single);
begin
  aVec^.x := aX;
  aVec^.y := aY;
  aVec^.z := aZ;
  aVec^.w := aW;
end;

function QuaternionCreate(aX: Single; aY: Single; aZ: Single; aW: Single
  ): TQuaternion;
begin
  Result.x := aX;
  Result.y := aY;
  Result.z := aZ;
  Result.w := aW;
end;

procedure QuaternionSet(aQuat: PQuaternion; aX: Single; aY: Single; aZ: Single;
  aW: Single);
begin
  aQuat^.x := aX;
  aQuat^.y := aY;
  aQuat^.z := aZ;
  aQuat^.w := aW;
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

function BoundingBoxCreate(aMin, aMax: TVector3): TBoundingBox;
begin
  Result.min := aMin;
  Result.max := aMax;
end;

procedure BoundingBoxSet(aBoundingBox: PBoundingBox; aMin, aMax: TVector3);
begin
  aBoundingBox^.min := aMin;
  aBoundingBox^.max := aMax;
end;

function Camera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aProjection: integer): TCamera3D;
begin
  Result.position := aPosition;
  Result.target := aTarget;
  Result.up := aUp;
  Result.fovy := aFOVY;
  Result.projection := aProjection;
end;

procedure Camera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);
begin
  aCam^.position := aPosition;
  aCam^.target := aTarget;
  aCam^.up := aUp;
  aCam^.fovy := aFOVY;
  aCam^.projection := aType;
end;

{ TColorB }

class operator TColorB.=(aColor, bColor: TColorB): Boolean;
begin
  Result := (aColor.R = bColor.R) and (aColor.G = bColor.G) and (aColor.B = bColor.B);
end;

procedure TColorB.Create(aR: Byte; aG: Byte; aB: Byte; aA: Byte);
begin
 self := ColorCreate(aR, aG, aB ,aA);
end;

{ TVector2 }

procedure TVector2.Create(aX, aY: single);
begin
 self := Vector2Create(aX,aY);
end;

{ TVector3 }

procedure TVector3.Create(aX, aY, aZ: single);
begin
  self := Vector3Create(aX, aY, aZ);
end;

{ TVector4 }

procedure TVector4.Create(aX, aY, aZ, aW: single);
begin
  self := Vector4Create(aX, aY, aZ, aW);
end;

{ TRectangle }

procedure TRectangle.Create(aX, aY, aWidth, aHeight: single);
begin
  self := RectangleCreate(aX, aY, aWidth, aHeight);
end;

{ TCamera3D }

procedure TCamera3D.Create(aPosition, aTarget, aUp: TVector3; aFOVY: single; aProjection: integer = 0);
begin
 self := Camera3DCreate(aPosition, aTarget, aUp, aFOVY, aProjection);
end;

{ TBoundingBox }

procedure TBoundingBox.Create(aMin, aMax: TVector3);
begin
  self := BoundingBoxCreate(aMin, aMax);
end;

{$include operators.inc}

initialization
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.

