{raylib - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)

   FEATURES:
       - NO external dependencies, all required libraries included with raylib
       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly, MacOS, UWP, Android, Raspberry Pi, HTML5.
       - Written in plain C code (C99) in PascalCase/camelCase notation
       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3 or ES2 - choose at compile)
       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
       - Multiple Fonts formats supported (TTF, XNA fonts, AngelCode fonts)
       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
       - Flexible Materials system, supporting classic maps and PBR maps
       - Skeletal Animation support (CPU bones-based animation)
       - Shaders support, including Model shaders and Postprocessing shaders
       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
       - VR stereo rendering with configurable HMD device parameters
       - Bindings to multiple programming languages available!

   NOTES:
       One custom font is loaded by default when InitWindow() [core]
       If using OpenGL 3.3 or ES2, one default shader is loaded automatically (internally defined) [rlgl]
       If using OpenGL 3.3 or ES2, several vertex buffers (VAO/VBO) are created to manage lines-triangles-quads

   DEPENDENCIES (included):
       [core] rglfw (github.com/glfw/glfw) for window/context management and input (only PLATFORM_DESKTOP)
       [rlgl] glad (github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (only PLATFORM_DESKTOP)
       [raudio] miniaudio (github.com/dr-soft/miniaudio) for audio device/context management

   OPTIONAL DEPENDENCIES (included):
       [core] rgif (Charlie Tangora, Ramon Santamaria) for GIF recording
       [textures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
       [textures] stb_image_write (Sean Barret) for image writting (BMP, TGA, PNG, JPG)
       [textures] stb_image_resize (Sean Barret) for image resizing algorithms
       [textures] stb_perlin (Sean Barret) for Perlin noise image generation
       [text] stb_truetype (Sean Barret) for ttf fonts loading
       [text] stb_rect_pack (Sean Barret) for rectangles packing
       [models] par_shapes (Philip Rideout) for parametric 3d shapes generation
       [models] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
       [models] cgltf (Johannes Kuhlmann) for models loading (glTF)
       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
       [raudio] dr_flac (David Reid) for FLAC audio file loading
       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading


   LICENSE: zlib/libpng

   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
   BSD-like license that allows static linking with closed source software:

   Copyright (c) 2013-2020 Ramon Santamaria (@raysan5)

   This software is provided "as-is", without any express or implied warranty. In no event
   will the authors be held liable for any damages arising from the use of this software.

   Permission is granted to anyone to use this software for any purpose, including commercial
   applications, and to alter it and redistribute it freely, subject to the following restrictions:

     1. The origin of this software must not be misrepresented; you must not claim that you
     wrote the original software. If you use this software in a product, an acknowledgment
     in the product documentation would be appreciated but is not required.

     2. Altered source versions must be plainly marked as such, and must not be misrepresented
     as being the original software.

     3. This notice may not be removed or altered from any source distribution.

        ---    raylib pascal - Header/DLLs Conversion   ---
       2019 Duvall Industries LLC.
       2020 GuvaCode.

       CHANGELOG
       Version 2019.10.24 - raylib-pas for raylib 2.6.0-dev
       Version 2020.09.17 - for raylib 3.0.0
       Version 2020.12.26 - for raylib 3.5.0
}
unit ray_header;

{$mode objfpc}{$H+}

interface


const
  cDllName = {$IFDEF WINDOWS} 'raylib.dll' {$IFEND}
             {$IFDEF DARWIN} 'libraylib.dylib' {$IFEND}
             {$IFDEF LINUX} 'libraylib.so' {$IFEND};
//{$ENDIF}

const
  // Some basic Defines
  //PI = 3.14159265358979323846;
  DEG2RAD = (PI / 180.0);
  RAD2DEG = (180.0 / PI);
  MAX_TOUCH_POINTS = 10;
  MAX_SHADER_LOCATIONS = 32;
  MAX_MATERIAL_MAPS = 12;


type
  // Color type, RGBA (32bit)
  PColor = ^TColor;
  TColor = record
    r: byte;
    g: byte;
    b: byte;
    a: byte;
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

   {  FormatText = TextFormat;
      LoadText = LoadFileText;
      GetExtension = GetFileExtension;
      GetImageData = LoadImageColors; }


type
  PVector2 = ^TVector2;
  TVector2 =  record
    x: single;
    y: single;
  end;

  // Vector3 type
  PVector3 = ^TVector3;
  TVector3 = record
    x: single;
    y: single;
    z: single;
  end;

  // Vector4 type
  PVector4 = ^TVector4;
  TVector4 = record
    x: single;
    y: single;
    z: single;
    w: single;
  end;

  // Quaternion type, same as Vector4
  PQuaternion = ^TQuaternion;
  TQuaternion = TVector4;

  // Matrix type (OpenGL style 4x4 - right handed, column major)
  PMatrix = ^TMatrix;
  TMatrix =  record
    m0: single;
    m4: single;
    m8: single;
    m12: single;
    m1: single;
    m5: single;
    m9: single;
    m13: single;
    m2: single;
    m6: single;
    m10: single;
    m14: single;
    m3: single;
    m7: single;
    m11: single;
    m15: single;
  end;

  // Rectangle type
  PPRectangle = ^PRectangle;
  PRectangle = ^TRectangle;
  TRectangle =  record
    x: single;
    y: single;
    Width: single;
    Height: single;
  end;

  // Image type, bpp always RGBA (32bit)
  // NOTE: Data stored in CPU memory (RAM)
  PImage = ^TImage;
  TImage =  record
    Data: Pointer;
    Width: integer;
    Height: integer;
    mipmaps: integer;
    format: integer;
  end;

  // Texture type
  // NOTE: Data stored in GPU memory
  PTexture = ^TTexture;
  TTexture =  record
    id: cardinal;
    Width: integer;
    Height: integer;
    mipmaps: integer;
    format: integer;
  end;

  // Texture type, same as Texture
  PTexture2D = ^TTexture;
  TTexture2D = TTexture;

  PTextureCubemap = ^TTexture;
  TTextureCubemap = TTexture;

  // RenderTexture type, for texture rendering
  PRenderTexture = ^TRenderTexture;
  TRenderTexture =  record
    id: cardinal;
    texture: TTexture;
    depth: TTexture;
  end;

 // RenderTexture2D type, same as RenderTexture
  PRenderTexture2D = ^TRenderTexture;
  TRenderTexture2D = TRenderTexture;


  // N-Patch layout info
  PNPatchInfo = ^TNPatchInfo;
  TNPatchInfo =  record
    source: TRectangle;
    left: integer;
    top: integer;
    right: integer;
    bottom: integer;
    _type: integer;
  end;


  // Font character info
  PCharInfo = ^TCharInfo;
  TCharInfo =  record
    Value: integer;
    offsetX: integer;
    offsetY: integer;
    advanceX: integer;
    image: TImage;
  end;

  // Font type, includes texture and charSet array data
  PFont = ^TFont;
  TFont =  record
    baseSize: integer;
    charsCount: integer;
    charsPadding: integer;
    texture: TTexture2D;
    recs: PRectangle;
    chars: PCharInfo;
  end;

  // SpriteFont type fallback, defaults to Font
  PSpriteFont = ^TFont;
  TSpriteFont = TFont;


  // Camera type, defines a camera position/orientation in 3d space
  PCamera3D = ^TCamera3D;
  TCamera3D =  record
    position: TVector3;
    target: TVector3;
    up: TVector3;
    fovy: single;
    _type: integer;
  end;

  PCamera = ^TCamera;
  TCamera = TCamera3D;


  // Camera2D type, defines a 2d camera
  PCamera2D = ^TCamera2D;
  TCamera2D =  record
    offset: TVector2;
    target: TVector2;
    rotation: single;
    zoom: single;
  end;

  // Vertex data definning a mesh
  // NOTE: Data stored in CPU memory (and GPU)
  PMesh = ^TMesh;
  TMesh =  record
    vertexCount: integer;    // Number of vertices stored in arrays
    triangleCount: integer;  // Number of triangles stored (indexed or not)

    // Default Vertex Data
    vertices: PSingle;      // Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
    texcoords: PSingle;     // Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
    texcoords2: PSingle;    // Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
    normals: PSingle;       // Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
    tangents: PSingle;      // Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
    colors: PByte;          // Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
    indices: PWord;         // Vertex indices (in case vertex data comes indexed)

    // Animation Vertex Data
    animVertices: PSingle;  // Animated vertex positions (after bones transformations)
    animNormals: PSingle;   // Animated normals (after bones transformations)
    boneIds: PInteger;      // Vertex bone ids, up to 4 bones influence by vertex (skinning)
    boneWeights: PSingle;   // Vertex bone weight, up to 4 bones influence by vertex (skinning)

    // OpenGL identifiers
    vaoId: cardinal;       // OpenGL Vertex Array Object id
    vboId: PCardinal;      // OpenGL Vertex Buffer Objects id (default vertex data)
  end;

  // Shader type (generic)
  PShader = ^TShader;
  TShader =  record
    id: cardinal;          // Shader program id
    locs: PInteger;        // Shader locations array (MAX_SHADER_LOCATIONS)
  end;

  // Material texture map
  PMaterialMap = ^TMaterialMap;
  TMaterialMap =  record
    texture: TTexture2D;   // Material map texture
    color: TColor;         // Material map color
    Value: single;         // Material map value
  end;

  // Material type (generic)
  PMaterial = ^TMaterial;
  TMaterial =  record
    shader: TShader;       // Material shader
    maps: ^TMaterialMap;   // Material maps array (MAX_MATERIAL_MAPS)
    params: PSingle;       // Material generic parameters (if required)
  end;

  // Transformation Properties
  PPTransform = ^PTransform;
  PTransform = ^TTransform;
  TTransform =  record
    translation: TVector3; // Translation
    rotation: TQuaternion; // Rotation
    scale: TVector3;       // Scale
  end;

  // Bone Information
  PBoneInfo = ^TBoneInfo;
  TBoneInfo =  record
    _name: array[0..31] of char; // Bone name
    parent: integer;             // Bone parent
  end;

  // Model type
  PModel = ^TModel;
  TModel =  record
    transform: TMatrix;         // Local transform matrix

    meshCount: integer;         // Number of meshes
    materialCount: integer;     // Number of materials
    meshes: PMesh;              // Meshes array
    materials: PMaterial;       // Materials array
    meshMaterial: PInteger;     // Mesh material number

    // Animation data
    boneCount: integer;         // Number of bones
    bones: PBoneInfo;           // Bones information (skeleton)
    bindPose: PTransform;       // Bones base transformation (pose)
  end;

  // Model Animation
  PModelAnimation = ^TModelAnimation;
  TModelAnimation = record
    boneCount: integer;         // Number of bones
    frameCount: integer;        // Number of animation frames
    bones: PBoneInfo;           // Bones information (skeleton)
    framePoses: PPTransform;    // Poses array by frame
  end;

  // Ray type (useful for raycast)
  PRay = ^TRay;
  TRay =  record
    position: TVector3;         // Ray position (origin)
    direction: TVector3;        // Ray direction
  end;

  // Raycast hit information
  PRayHitInfo = ^TRayHitInfo;
  TRayHitInfo =  record
    hit: boolean;               // Did the ray hit something?
    distance: single;           // Distance to nearest hit
    position: TVector3;         // Position of nearest hit
    normal: TVector3;           // Surface normal of hit
  end;

  // Bounding box type
  PBoundingBox = ^TBoundingBox;
  TBoundingBox =  record
    min: TVector3;              // Minimum vertex box-corner
    max: TVector3;              // Maximum vertex box-corner
  end;

  // Wave type, defines audio wave data
  PWave = ^TWave;
  TWave =  record
    sampleCount: dword;         // Total number of samples
    sampleRate: dword;          // Frequency (samples per second)
    sampleSize: dword;          // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels: dword;            // Number of channels (1-mono, 2-stereo)
    Data: Pointer;              // Buffer data pointer
  end;

  PrAudioBuffer = ^TrAudioBuffer;
  TrAudioBuffer = record
  end;

  // Audio stream type
  // NOTE: Useful to create custom audio streams not bound to a specific file
  PAudioStream = ^TAudioStream;
  TAudioStream =  record
    buffer: PrAudioBuffer;      // Pointer to internal data used by the audio system

    sampleRate: dword;          // Frequency (samples per second)
    sampleSize: dword;          // Bit depth (bits per sample): 8, 16, 32 (24 not supported)
    channels: dword;            // Number of channels (1-mono, 2-stereo)
  end;

  // Sound source type
  PSound = ^TSound;
  TSound = record
    stream: TAudioStream;        // Audio stream
    sampleCount: dword;         // Total number of samples

  end;

  // Music type (file streaming from memory)
  // NOTE: Anything longer than ~10 seconds should be streamed
  TMusic =  record
    stream: TAudioStream;       // Audio stream
    sampleCount: dword;         // Total number of samples
    looping: boolean;           // Music looping enable

    ctxType: longint;           // Type of music context (audio filetype)
    ctxData: Pointer;           // Audio context data, depends on type
  end;

  // Head-Mounted-Display device parameters
  PVrDeviceInfo = ^TVrDeviceInfo;
  TVrDeviceInfo =  record
    hResolution: integer;                        // HMD horizontal resolution in pixels
    vResolution: integer;                        // HMD vertical resolution in pixels
    hScreenSize: single;                         // HMD horizontal size in meters
    vScreenSize: single;                         // HMD vertical size in meters
    vScreenCenter: single;                       // HMD screen center in meters
    eyeToScreenDistance: single;                 // HMD distance between eye and display in meters
    lensSeparationDistance: single;              // HMD lens separation distance in meters
    interpupillaryDistance: single;              // HMD IPD (distance between pupils) in meters
    lensDistortionValues: array[0..3] of single; // HMD lens distortion constant parameters
    chromaAbCorrection: array[0..3] of single;   // HMD chromatic aberration correction parameters
  end;

//----------------------------------------------------------------------------------
// Enumerators Definition
//----------------------------------------------------------------------------------
// System/Window config flags
// NOTE: Every bit registers one state (use it with bit masks)
// By default all flags are set to 0
const
  FLAG_VSYNC_HINT         = $00000040;   // Set to try enabling V-Sync on GPU
  FLAG_FULLSCREEN_MODE    = $00000002;   // Set to run program in fullscreen
  FLAG_WINDOW_RESIZABLE   = $00000004;   // Set to allow resizable window
  FLAG_WINDOW_UNDECORATED = $00000008;   // Set to disable window decoration (frame and buttons)
  FLAG_WINDOW_HIDDEN      = $00000080;   // Set to hide window
  FLAG_WINDOW_MINIMIZED   = $00000200;   // Set to minimize window (iconify)
  FLAG_WINDOW_MAXIMIZED   = $00000400;   // Set to maximize window (expanded to monitor)
  FLAG_WINDOW_UNFOCUSED   = $00000800;   // Set to window non focused
  FLAG_WINDOW_TOPMOST     = $00001000;   // Set to window always on top
  FLAG_WINDOW_ALWAYS_RUN  = $00000100;   // Set to allow windows running while minimized
  FLAG_WINDOW_TRANSPARENT = $00000010;   // Set to allow transparent framebuffer
  FLAG_WINDOW_HIGHDPI     = $00002000;   // Set to support HighDPI
  FLAG_MSAA_4X_HINT       = $00000020;   // Set to try enabling MSAA 4X
  FLAG_INTERLACED_HINT    = $00010000;   // Set to try enabling interlaced video format (for V3D)

  // Trace log type
  LOG_ALL = 0;
  LOG_TRACE = 1;
  LOG_DEBUG = 3;
  LOG_INFO = 4;
  LOG_WARNING = 5;
  LOG_ERROR = 6;
  LOG_FATAL = 7;
  LOG_NONE = 8;

    // Alphanumeric keys
    KEY_APOSTROPHE      = 39;
    KEY_COMMA           = 44;
    KEY_MINUS           = 45;
    KEY_PERIOD          = 46;
    KEY_SLASH           = 47;
    KEY_ZERO            = 48;
    KEY_ONE             = 49;
    KEY_TWO             = 50;
    KEY_THREE           = 51;
    KEY_FOUR            = 52;
    KEY_FIVE            = 53;
    KEY_SIX             = 54;
    KEY_SEVEN           = 55;
    KEY_EIGHT           = 56;
    KEY_NINE            = 57;
    KEY_SEMICOLON       = 59;
    KEY_EQUAL           = 61;
    KEY_A               = 65;
    KEY_B               = 66;
    KEY_C               = 67;
    KEY_D               = 68;
    KEY_E               = 69;
    KEY_F               = 70;
    KEY_G               = 71;
    KEY_H               = 72;
    KEY_I               = 73;
    KEY_J               = 74;
    KEY_K               = 75;
    KEY_L               = 76;
    KEY_M               = 77;
    KEY_N               = 78;
    KEY_O               = 79;
    KEY_P               = 80;
    KEY_Q               = 81;
    KEY_R               = 82;
    KEY_S               = 83;
    KEY_T               = 84;
    KEY_U               = 85;
    KEY_V               = 86;
    KEY_W               = 87;
    KEY_X               = 88;
    KEY_Y               = 89;
    KEY_Z               = 90;

    // Function keys
    KEY_SPACE           = 32;
    KEY_ESCAPE          = 256;
    KEY_ENTER           = 257;
    KEY_TAB             = 258;
    KEY_BACKSPACE       = 259;
    KEY_INSERT          = 260;
    KEY_DELETE          = 261;
    KEY_RIGHT           = 262;
    KEY_LEFT            = 263;
    KEY_DOWN            = 264;
    KEY_UP              = 265;
    KEY_PAGE_UP         = 266;
    KEY_PAGE_DOWN       = 267;
    KEY_HOME            = 268;
    KEY_END             = 269;
    KEY_CAPS_LOCK       = 280;
    KEY_SCROLL_LOCK     = 281;
    KEY_NUM_LOCK        = 282;
    KEY_PRINT_SCREEN    = 283;
    KEY_PAUSE           = 284;
    KEY_F1              = 290;
    KEY_F2              = 291;
    KEY_F3              = 292;
    KEY_F4              = 293;
    KEY_F5              = 294;
    KEY_F6              = 295;
    KEY_F7              = 296;
    KEY_F8              = 297;
    KEY_F9              = 298;
    KEY_F10             = 299;
    KEY_F11             = 300;
    KEY_F12             = 301;
    KEY_LEFT_SHIFT      = 340;
    KEY_LEFT_CONTROL    = 341;
    KEY_LEFT_ALT        = 342;
    KEY_LEFT_SUPER      = 343;
    KEY_RIGHT_SHIFT     = 344;
    KEY_RIGHT_CONTROL   = 345;
    KEY_RIGHT_ALT       = 346;
    KEY_RIGHT_SUPER     = 347;
    KEY_KB_MENU         = 348;
    KEY_LEFT_BRACKET    = 91;
    KEY_BACKSLASH       = 92;
    KEY_RIGHT_BRACKET   = 93;
    KEY_GRAVE           = 96;

  // Keypad keys
  KEY_KP_0 = 320;
  KEY_KP_1 = 321;
  KEY_KP_2 = 322;
  KEY_KP_3 = 323;
  KEY_KP_4 = 324;
  KEY_KP_5 = 325;
  KEY_KP_6 = 326;
  KEY_KP_7 = 327;
  KEY_KP_8 = 328;
  KEY_KP_9 = 329;
  KEY_KP_DECIMAL = 330;
  KEY_KP_DIVIDE = 331;
  KEY_KP_MULTIPLY = 332;
  KEY_KP_SUBTRACT = 333;
  KEY_KP_ADD = 334;
  KEY_KP_ENTER = 335;
  KEY_KP_EQUAL = 336;

  // Android buttons
  KEY_BACK = 4;
  KEY_MENU = 82;
  KEY_VOLUME_UP = 24;
  KEY_VOLUME_DOWN = 25;

  // Mouse buttons
  MOUSE_LEFT_BUTTON = 0;
  MOUSE_RIGHT_BUTTON = 1;
  MOUSE_MIDDLE_BUTTON = 2;

  // Mouse cursor types
  MOUSE_CURSOR_DEFAULT       = 0;
  MOUSE_CURSOR_ARROW         = 1;
  MOUSE_CURSOR_IBEAM         = 2;
  MOUSE_CURSOR_CROSSHAIR     = 3;
  MOUSE_CURSOR_POINTING_HAND = 4;
  MOUSE_CURSOR_RESIZE_EW     = 5;     // The horizontal resize/move arrow shape
  MOUSE_CURSOR_RESIZE_NS     = 6;     // The vertical resize/move arrow shape
  MOUSE_CURSOR_RESIZE_NWSE   = 7;     // The top-left to bottom-right diagonal resize/move arrow shape
  MOUSE_CURSOR_RESIZE_NESW   = 8;     // The top-right to bottom-left diagonal resize/move arrow shape
  MOUSE_CURSOR_RESIZE_ALL    = 9;     // The omni-directional resize/move cursor shape
  MOUSE_CURSOR_NOT_ALLOWED   = 10;     // The operation-not-allowed shape

  // Gamepad Number
  GAMEPAD_PLAYER1 = 0;
  GAMEPAD_PLAYER2 = 1;
  GAMEPAD_PLAYER3 = 2;
  GAMEPAD_PLAYER4 = 3;

 // Gamepad buttons
 // This is here just for error checking
  GAMEPAD_BUTTON_UNKNOWN = 0;
 // This is normally a DPAD
  GAMEPAD_BUTTON_LEFT_FACE_UP = 1;
  GAMEPAD_BUTTON_LEFT_FACE_RIGHT = 2;
  GAMEPAD_BUTTON_LEFT_FACE_DOWN = 3;
  GAMEPAD_BUTTON_LEFT_FACE_LEFT = 4;
  // This normally corresponds with PlayStation and Xbox controllers
  // XBOX: [Y,X,A,B]
  // PS3: [Triangle,Square,Cross,Circle]
  // No support for 6 button controllers though..
  GAMEPAD_BUTTON_RIGHT_FACE_UP = 5;
  GAMEPAD_BUTTON_RIGHT_FACE_RIGHT = 6;
  GAMEPAD_BUTTON_RIGHT_FACE_DOWN = 7;
  GAMEPAD_BUTTON_RIGHT_FACE_LEFT = 8;
  // Triggers
  GAMEPAD_BUTTON_LEFT_TRIGGER_1 = 9;
  GAMEPAD_BUTTON_LEFT_TRIGGER_2 = 10;
  GAMEPAD_BUTTON_RIGHT_TRIGGER_1 = 11;
  GAMEPAD_BUTTON_RIGHT_TRIGGER_2 = 12;

  // These are buttons in the center of the gamepad
  GAMEPAD_BUTTON_MIDDLE_LEFT = 13;     //PS3 Select
  GAMEPAD_BUTTON_MIDDLE = 14;          //PS Button/XBOX Button
  GAMEPAD_BUTTON_MIDDLE_RIGHT = 15;    //PS3 Start

  // These are the joystick press in buttons
  GAMEPAD_BUTTON_LEFT_THUMB = 16;
  GAMEPAD_BUTTON_RIGHT_THUMB = 17;

  // Gamepad axis
  // Left stick
  GAMEPAD_AXIS_LEFT_X = 0;
  GAMEPAD_AXIS_LEFT_Y = 1;

  // Right stick
  GAMEPAD_AXIS_RIGHT_X = 2;
  GAMEPAD_AXIS_RIGHT_Y = 3;

  // Pressure levels for the back triggers
  GAMEPAD_AXIS_LEFT_TRIGGER = 4;      // [1..-1] (pressure-level)
  GAMEPAD_AXIS_RIGHT_TRIGGER = 5;     // [1..-1] (pressure-level)

  // Shader location points
  LOC_VERTEX_POSITION = 0;
  LOC_VERTEX_TEXCOORD01 = 1;
  LOC_VERTEX_TEXCOORD02 = 2;
  LOC_VERTEX_NORMAL = 3;
  LOC_VERTEX_TANGENT = 4;
  LOC_VERTEX_COLOR = 5;
  LOC_MATRIX_MVP = 6;
  LOC_MATRIX_MODEL = 7;
  LOC_MATRIX_VIEW = 8;
  LOC_MATRIX_PROJECTION = 9;
  LOC_VECTOR_VIEW = 10;
  LOC_COLOR_DIFFUSE = 11;
  LOC_COLOR_SPECULAR = 12;
  LOC_COLOR_AMBIENT = 13;
  LOC_MAP_ALBEDO = 14;       // LOC_MAP_DIFFUSE
  LOC_MAP_METALNESS = 15;    // LOC_MAP_SPECULAR
  LOC_MAP_NORMAL = 16;
  LOC_MAP_ROUGHNESS = 17;
  LOC_MAP_OCCLUSION = 18;
  LOC_MAP_EMISSION = 19;
  LOC_MAP_HEIGHT = 20;
  LOC_MAP_CUBEMAP = 21;
  LOC_MAP_IRRADIANCE = 22;
  LOC_MAP_PREFILTER = 23;
  LOC_MAP_BRDF = 24;

  LOC_MAP_DIFFUSE = LOC_MAP_ALBEDO;
  LOC_MAP_SPECULAR = LOC_MAP_METALNESS;

  // Shader uniform data types
  UNIFORM_FLOAT = 0;
  UNIFORM_VEC2 = 1;
  UNIFORM_VEC3 = 2;
  UNIFORM_VEC4 = 3;
  UNIFORM_INT = 4;
  UNIFORM_IVEC2 = 5;
  UNIFORM_IVEC3 = 6;
  UNIFORM_IVEC4 = 7;
  UNIFORM_SAMPLER2D = 8;

  // Material maps
  MAP_ALBEDO = 0;               // MAP_DIFFUSE
  MAP_METALNESS = 1;            // MAP_SPECULAR
  MAP_NORMAL = 2;
  MAP_ROUGHNESS = 3;
  MAP_OCCLUSION = 4;
  MAP_EMISSION = 5;
  MAP_HEIGHT = 6;
  MAP_CUBEMAP = 7;              // NOTE: Uses GL_TEXTURE_CUBE_MAP
  MAP_IRRADIANCE = 8;           // NOTE: Uses GL_TEXTURE_CUBE_MAP
  MAP_PREFILTER = 9;            // NOTE: Uses GL_TEXTURE_CUBE_MAP
  MAP_BRDF = 10;

  MAP_DIFFUSE = MAP_ALBEDO;
  MAP_SPECULAR = MAP_METALNESS;

  // Pixel formats
  // NOTE: Support depends on OpenGL version and platform
  UNCOMPRESSED_GRAYSCALE = 1;        // 8 bit per pixel (no alpha)
  UNCOMPRESSED_GRAY_ALPHA = 2;       // 8*2 bpp (2 channels)
  UNCOMPRESSED_R5G6B5 = 3;           // 16 bpp
  UNCOMPRESSED_R8G8B8 = 4;           // 24 bpp
  UNCOMPRESSED_R5G5B5A1 = 5;         // 16 bpp (1 bit alpha)
  UNCOMPRESSED_R4G4B4A4 = 6;         // 16 bpp (4 bit alpha)
  UNCOMPRESSED_R8G8B8A8 = 7;         // 32 bpp
  UNCOMPRESSED_R32 = 8;              // 32 bpp (1 channel - float)
  UNCOMPRESSED_R32G32B32 = 9;        // 32*3 bpp (3 channels - float)
  UNCOMPRESSED_R32G32B32A32 = 10;    // 32*4 bpp (4 channels - float)
  COMPRESSED_DXT1_RGB = 11;          // 4 bpp (no alpha)
  COMPRESSED_DXT1_RGBA = 12;         // 4 bpp (1 bit alpha)
  COMPRESSED_DXT3_RGBA = 13;         // 8 bpp
  COMPRESSED_DXT5_RGBA = 14;         // 8 bpp
  COMPRESSED_ETC1_RGB = 15;          // 4 bpp
  COMPRESSED_ETC2_RGB = 16;          // 4 bpp
  COMPRESSED_ETC2_EAC_RGBA = 17;     // 8 bpp
  COMPRESSED_PVRT_RGB = 18;          // 4 bpp
  COMPRESSED_PVRT_RGBA = 19;         // 8 bpp
  COMPRESSED_ASTC_4x4_RGBA = 20;     // 8 bpp
  COMPRESSED_ASTC_8x8_RGBA = 21;     // 2 bpp

  // Texture parameters: filter mode
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  FILTER_POINT = 0;                  // No filter, just pixel aproximation
  FILTER_BILINEAR = 1;               // Linear filtering
  FILTER_TRILINEAR = 2;              // Trilinear filtering (linear with mipmaps)
  FILTER_ANISOTROPIC_4X = 3;         // Anisotropic filtering 4x
  FILTER_ANISOTROPIC_8X = 4;         // Anisotropic filtering 8x
  FILTER_ANISOTROPIC_16X = 5;        // Anisotropic filtering 16x

  // Texture parameters: wrap mode
  WRAP_REPEAT = 0;                   // Repeats texture in tiled mode
  WRAP_CLAMP = 1;                    // Clamps texture to edge pixel in tiled mode
  WRAP_MIRROR_REPEAT = 2;            // Mirrors and repeats the texture in tiled mode
  WRAP_MIRROR_CLAMP = 3;             // Mirrors and clamps to border the texture in tiled mode

  // Cubemap layout type
  CUBEMAP_AUTO_DETECT = 0;            // Automatically detect layout type
  CUBEMAP_LINE_VERTICAL = 1;          // Layout is defined by a vertical line with faces
  CUBEMAP_LINE_HORIZONTAL = 2;        // Layout is defined by an horizontal line with faces
  CUBEMAP_CROSS_THREE_BY_FOUR = 3;    // Layout is defined by a 3x4 cross with cubemap faces
  CUBEMAP_CROSS_FOUR_BY_THREE = 4;    // Layout is defined by a 4x3 cross with cubemap faces
  CUBEMAP_PANORAMA = 5;               // Layout is defined by a panorama image (equirectangular map)

  // Font type, defines generation method
  FONT_DEFAULT = 0;             // Default font generation, anti-aliased
  FONT_BITMAP = 1;              // Bitmap font generation, no anti-aliasing
  FONT_SDF = 2;                 // SDF font generation, requires external shader

  // Color blending modes (pre-defined)
  BLEND_ALPHA = 0;              // Blend textures considering alpha (default)
  BLEND_ADDITIVE = 1;           // Blend textures adding colors
  BLEND_MULTIPLIED = 2;         // Blend textures multiplying colors
  BLEND_SUBTRACT_COLORS = 3;      // Blend textures subtracting colors (alternative)
  BLEND_CUSTOM = 4;               // Belnd textures using custom src/dst factors (use SetBlendModeCustom())

  // Gestures type
  // NOTE: It could be used as flags to enable only some gestures
  GESTURE_NONE = 0;
  GESTURE_TAP = 1;
  GESTURE_DOUBLETAP = 2;
  GESTURE_HOLD = 4;
  GESTURE_DRAG = 8;
  GESTURE_SWIPE_RIGHT = 16;
  GESTURE_SWIPE_LEFT = 32;
  GESTURE_SWIPE_UP = 64;
  GESTURE_SWIPE_DOWN = 128;
  GESTURE_PINCH_IN = 256;
  GESTURE_PINCH_OUT = 512;

  // Camera system modes
  CAMERA_CUSTOM = 0;
  CAMERA_FREE = 1;
  CAMERA_ORBITAL = 2;
  CAMERA_FIRST_PERSON = 3;
  CAMERA_THIRD_PERSON = 4;

  // Camera projection modes
  CAMERA_PERSPECTIVE = 0;
  CAMERA_ORTHOGRAPHIC = 1;

  // N-patch types
  NPT_9PATCH = 0;               // Npatch defined by 3x3 tiles
  NPT_3PATCH_VERTICAL = 1;      // Npatch defined by 1x3 tiles
  NPT_3PATCH_HORIZONTAL = 2;    // Npatch defined by 3x1 tiles

type
  TTraceLogCallback = procedure(aLogType: integer; aText, aArgs: PAnsiChar); cdecl;

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

// Window-related functions
procedure InitWindow(aWidth: integer; aHeight: integer; aTitle: PAnsiChar); cdecl; external cDllName; // Initialize window and OpenGL context
function WindowShouldClose(): boolean; cdecl; external cDllName; // Check if KEY_ESCAPE pressed or Close icon pressed
procedure CloseWindow(); cdecl; external cDllName; // Close window and unload OpenGL context
function IsWindowReady(): boolean; cdecl; external cDllName; // Check if window has been initialized successfully                                              // Check if window has been initialized successfully
function IsWindowFullscreen(): boolean; cdecl; external cDllName; // Check if window is currently fullscreen
function IsWindowHidden(): boolean; cdecl; external cDllName; // Check if window is currently hidden (only PLATFORM_DESKTOP)
function IsWindowMinimized(): boolean; cdecl; external cDllName; // Check if window is currently minimized (only PLATFORM_DESKTOP)
function IsWindowMaximized(): boolean; cdecl; external cDllName; // Check if window is currently maximized (only PLATFORM_DESKTOP)
function IsWindowFocused(): boolean; cdecl; external cDllName; // Check if window is currently focused (only PLATFORM_DESKTOP)
function IsWindowResized(): boolean; cdecl; external cDllName; // Check if window has been resized last frame
function IsWindowState(aFlag: cardinal): boolean; cdecl; external cDllName; // Check if one specific window flag is enabled
procedure SetWindowState(aFlags: cardinal); cdecl; external cDllName; // Set window configuration state using flags
procedure ClearWindowState(aFlags: cardinal); cdecl; external cDllName; // Clear window configuration state flags
procedure ToggleFullscreen(); cdecl; external cDllName; // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
procedure MaximizeWindow(); cdecl; external cDllName; // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
procedure MinimizeWindow(); cdecl; external cDllName; // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
procedure RestoreWindow(); cdecl; external cDllName; // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
procedure SetWindowIcon(aImage: TImage); cdecl; external cDllName; // Set icon for window (only PLATFORM_DESKTOP)
procedure SetWindowTitle(aTitle: PAnsiChar); cdecl; external cDllName; // Set title for window (only PLATFORM_DESKTOP)
procedure SetWindowPosition(aX: integer; aY: integer); cdecl; external cDllName; // Set window position on screen (only PLATFORM_DESKTOP)
procedure SetWindowMonitor(aMonitor: integer); cdecl; external cDllName; // Set monitor for the current window (fullscreen mode)
procedure SetWindowMinSize(aWidth: integer; aHeight: integer); cdecl; external cDllName; // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
procedure SetWindowSize(aWidth: integer; aHeight: integer); cdecl; external cDllName; // Set window dimensions
function GetWindowHandle(): Pointer; cdecl; external cDllName; // Get native window handle
function GetScreenWidth(): integer; cdecl; external cDllName;  // Get current screen width
function GetScreenHeight(): integer; cdecl; external cDllName; // Get current screen height
function GetMonitorCount(): integer; cdecl; external cDllName; // Get number of connected monitors
function GetMonitorPosition(aMonitor: integer): TVector2; cdecl; external cDllName; // Get specified monitor position
function GetMonitorWidth(aMonitor: integer): integer; cdecl; external cDllName; // Get specified monitor width
function GetMonitorHeight(aMonitor: integer): integer; cdecl; external cDllName; // Get specified monitor height
function GetMonitorPhysicalWidth(aMonitor: integer): integer; cdecl; external cDllName; // Get specified monitor physical width in millimetres
function GetMonitorPhysicalHeight(aMonitor: integer): integer; cdecl; external cDllName; // Get specified monitor physical height in millimetres
function GetMonitorRefreshRate(aMonitor: integer): integer; cdecl; external cDllName; // Get specified monitor refresh rate
function GetWindowPosition(): TVector2; cdecl; external cDllName; // Get window position XY on monitor
function GetWindowScaleDPI(): TVector2; cdecl; external cDllName; // Get window scale DPI factor
function GetMonitorName(aMonitor: integer): PAnsiChar; cdecl; external cDllName; // Get the human-readable, UTF-8 encoded name of the primary monitor
procedure SetClipboardText(aText: PAnsiChar); cdecl; external cDllName; // Set clipboard text content
function GetClipboardText(): PAnsiChar; cdecl; external cDllName; // Get clipboard text content

// Cursor-related functions
procedure ShowCursor(); cdecl; external cDllName; // Shows cursor
procedure HideCursor(); cdecl; external cDllName; // Hides cursor
function IsCursorHidden(): boolean; cdecl; external cDllName; // Check if cursor is not visible
procedure EnableCursor(); cdecl; external cDllName; // Enables cursor (unlock cursor)
procedure DisableCursor(); cdecl; external cDllName; // Disables cursor (lock cursor)
function IsCursorOnScreen(): boolean; cdecl; external cDllName; // Check if cursor is on the current screen.

// Drawing-related functions
procedure ClearBackground(aColor: TColor); cdecl; external cDllName; // Set background color (framebuffer clear color)
procedure BeginDrawing(); cdecl; external cDllName; // Setup canvas (framebuffer) to start drawing
procedure EndDrawing(); cdecl; external cDllName; // End canvas drawing and swap buffers (double buffering)
procedure BeginMode2D(aCamera: TCamera2D); cdecl; external cDllName; // Initialize 2D mode with custom camera (2D)
procedure EndMode2D(); cdecl; external cDllName; // Ends 2D mode with custom camera
procedure BeginMode3D(aCamera: TCamera3D); cdecl; external cDllName; // Initializes 3D mode with custom camera (3D)
procedure EndMode3D(); cdecl; external cDllName; // Ends 3D mode and returns to default 2D orthographic mode
procedure BeginTextureMode(aTarget: TRenderTexture2D); cdecl; external cDllName; // Initializes render texture for drawing
procedure EndTextureMode(); cdecl; external cDllName; // Ends drawing to render texture
procedure BeginScissorMode(aX, aY, aWidth, aHeight: integer); cdecl; external cDllName; // Begin scissor mode (define screen area for following drawing)
procedure EndScissorMode(); cdecl; external cDllName; // End scissor mode

// Screen-space-related functions
function GetMouseRay(aMousePosition: TVector2; aCamera: TCamera): TRay; cdecl; external cDllName; // Returns a ray trace from mouse position
function GetCameraMatrix(aCamera: TCamera): TMatrix; cdecl; external cDllName; // Returns camera transform matrix (view matrix)
function GetCameraMatrix2D(aCamera: TCamera2D): TMatrix; cdecl; external cDllName; // Returns camera 2d transform matrix
function GetWorldToScreen(aPosition: TVector3; aCamera: TCamera): TVector2; cdecl; external cDllName; // Returns the screen space position for a 3d world space position
function GetWorldToScreenEx(aPosition: TVector3; aCamera: TCamera; aWidth, aHeight: integer): TVector2; cdecl; external cDllName; // Returns size position for a 3d world space position
function GetWorldToScreen2D(aPosition: TVector2; aCamera: TCamera2D): TVector2; cdecl; external cDllName; // Returns the screen space position for a 2d camera world space position
function GetScreenToWorld2D(aPosition: TVector2; aCamera: TCamera2D): TVector2; cdecl; external cDllName; // Returns the world space position for a 2d camera screen space position

// Timming-related functions
procedure SetTargetFPS(aFPS: integer); cdecl; external cDllName; // Set target FPS (maximum)
function GetFPS(): integer; cdecl; external cDllName; // Returns current FPS
function GetFrameTime(): single; cdecl; external cDllName; // Returns time in seconds for last frame drawn
function GetTime(): double; cdecl; external cDllName; // Returns elapsed time in seconds since InitWindow()

// Misc. functions
procedure SetConfigFlags(aFlags: cardinal); cdecl; external cDllName; // Setup init configuration flags (view FLAGS)

procedure SetTraceLogLevel(aLogType: integer); cdecl; external cDllName; // Set the current threshold (minimum) log level
procedure SetTraceLogExit(aLogType: integer); cdecl; external cDllName; // Set the exit threshold (minimum) log level
procedure SetTraceLogCallback(aCallback: TTraceLogCallback); cdecl; external cDllName; // Set a trace log callback to enable custom logging
procedure TraceLog(aLogType: integer; aText: PAnsiChar); cdecl; external cDllName; // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
function MemAlloc(aSize:integer): Pointer; cdecl; external cDllName;   // Internal memory allocator
procedure MemFree(aPtr:Pointer); cdecl; external cDllName; // Internal memory free
procedure TakeScreenshot(aFilename: PAnsiChar); cdecl; external cDllName; // Takes a screenshot of current screen (saved a .png)
function GetRandomValue(aMin: integer; aMax: integer): integer; cdecl; external cDllName; // Returns a random value between min and max (both included)

// Files management functions
function LoadFileData(aFileName: PAnsiChar; bytesRead: PCardinal): PAnsiChar; cdecl; external; // Load file data as byte array (read)
procedure UnloadFileData(aData: PAnsiChar); cdecl; external; // Unload file data allocated by LoadFileData()
function SaveFileData(aFileName: PAnsiChar; aData: Pointer; bytesToWrite: cardinal): Boolean; cdecl; external; // Save data to file from byte array (write), returns true on success
function LoadFileText(aFileName: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Load text data from file (read), returns a '\0' terminated string
function SaveFileText(aFileName: PAnsiChar; aText: PAnsiChar): boolean; cdecl; external cDllName; // Save text data to file (write), string must be '\0' terminated
function FileExists(aFilename: PAnsiChar): boolean; cdecl; external cDllName; // Check if file exists
function IsFileExtension(aFilename: PAnsiChar; aExt: PAnsiChar): boolean; cdecl; external cDllName; // Check file extension
function DirectoryExists(aDirPath: PAnsiChar): boolean; cdecl; external cDllName; // Check if a directory path exists
function GetFileExtension(aFilename: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get pointer to extension for a filename string
function GetFileName(aFilepath: PAnsiChar): PAnsiChar; cdecl; external cDllName;  // Get pointer to filename for a path string
function GetFileNameWithoutExt(aFilepath: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get filename string without extension (uses static string)
function GetDirectoryPath(aFilename: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get full path for a given fileName with path (uses static string)
function GetPrevDirectoryPath(aDirPath: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get previous directory path for a given path (uses static string)
function GetWorkingDirectory(): PAnsiChar; cdecl; external cDllName; // Get current working directory (uses static string)
function GetDirectoryFiles(aDirpath: PAnsiChar; aCount: PInteger): PPAnsiChar; cdecl; external cDllName; // Get filenames in a directory path (memory should be freed)
procedure ClearDirectoryFiles(); cdecl; external cDllName; // Clear directory files paths buffers (free memory)
function ChangeDirectory(aDir: PAnsiChar): boolean; cdecl; external cDllName; // Change working directory, return true on success
function IsFileDropped(): boolean; cdecl; external cDllName;  // Check if a file has been dropped into window
function GetDroppedFiles(aCount: PInteger): PPAnsiChar; cdecl; external cDllName; // Get dropped files names (memory should be freed)
procedure ClearDroppedFiles; cdecl; external cDllName; // Clear dropped files paths buffer (free memory)
function GetFileModTime(aFilename: PAnsiChar): longint; cdecl; external cDllName; // Get file modification time (last write time)

function CompressData(aData: PByte; aDataLength: integer; aCompDataLength: PInteger): PByte; cdecl; external cDllName; // Compress data (DEFLATE algorythm)
function DecompressData(aCompData: PByte; aCompDataLength: integer; aDataLength: PInteger): PByte; cdecl; external cDllName; // Decompress data (DEFLATE algorythm)

// Persistent storage management
function SaveStorageValue(aPosition: cardinal ; aValue: integer): boolean; cdecl; external cDllName; // Save integer value to storage file (to defined position)
function LoadStorageValue(aPosition: cardinal): integer; cdecl; external cDllName;

procedure OpenURL(aUrl: PAnsiChar); cdecl; external cDllName; // Open URL with default system browser (if available)

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
function IsKeyPressed(aKey: integer): boolean; cdecl; external cDllName;  // Detect if a key has been pressed once
function IsKeyDown(aKey: integer): boolean; cdecl; external cDllName; // Detect if a key is being pressed
function IsKeyReleased(aKey: integer): boolean; cdecl; external cDllName; // Detect if a key has been released once
function IsKeyUp(aKey: integer): boolean; cdecl; external cDllName; // Detect if a key is NOT being pressed
procedure SetExitKey(aKey: integer); cdecl; external cDllName; // Set a custom key to exit program (default is ESC)
function GetKeyPressed(): integer; cdecl; external cDllName; // Get key pressed (keycode), call it multiple times for keys queued
function GetCharPressed(): integer; cdecl; external cDllName; // Get char pressed (unicode), call it multiple times for chars queued

// Input-related functions: gamepads
function IsGamepadAvailable(aGamepad: integer): boolean; cdecl; external cDllName; // Detect if a gamepad is available
function IsGamepadName(aGamepad: integer; aName: PAnsiChar): boolean; cdecl; external cDllName; // Check gamepad name (if available)
function GetGamepadName(aGamepad: integer): PAnsiChar; cdecl; external cDllName; // Return gamepad internal name id
function IsGamepadButtonPressed(aGamepad: integer; aButton: integer): boolean; cdecl; external cDllName; // Detect if a gamepad button has been pressed once
function IsGamepadButtonDown(aGamepad: integer; aButton: integer): boolean; cdecl; external cDllName; // Detect if a gamepad button is being pressed
function IsGamepadButtonReleased(aGamepad: integer; aButton: integer): boolean; cdecl; external cDllName; // Detect if a gamepad button has been released once
function IsGamepadButtonUp(aGamepad: integer; aButton: integer): boolean; cdecl; external cDllName; // Detect if a gamepad button is NOT being pressed
function GetGamepadButtonPressed(): integer; cdecl; external cDllName; // Get the last gamepad button pressed
function GetGamepadAxisCount(aGamepad: integer): integer; cdecl; external cDllName; // Return gamepad axis count for a gamepad
function GetGamepadAxisMovement(aGamepad: integer; aAxis: integer): single; cdecl; external cDllName; // Return axis movement value for a gamepad axis

// Input-related functions: mouse
function IsMouseButtonPressed(aButton: integer): boolean; cdecl; external cDllName; // Detect if a mouse button has been pressed once
function IsMouseButtonDown(aButton: integer): boolean; cdecl; external cDllName; // Detect if a mouse button is being pressed
function IsMouseButtonReleased(aButton: integer): boolean; cdecl; external cDllName; // Detect if a mouse button has been released once
function IsMouseButtonUp(aButton: integer): boolean; cdecl; external cDllName; // Detect if a mouse button is NOT being pressed
function GetMouseX(): integer; cdecl; external cDllName; // Returns mouse position X
function GetMouseY(): integer; cdecl; external cDllName; // Returns mouse position Y
function GetMousePosition(): TVector2; cdecl; external cDllName; // Returns mouse position XY
procedure SetMousePosition(aPosition: TVector2); cdecl; external cDllName; // Set mouse position XY
procedure SetMouseOffset(aOffsetX, aOffsetY: integer); cdecl; external cDllName; // Set mouse offset
procedure SetMouseScale(aScaleX, aScaleY: single); cdecl; external cDllName; // Set mouse scaling
function GetMouseWheelMove(): single; cdecl; external cDllName; // Returns mouse wheel movement Y
function GetMouseCursor(): integer; cdecl; external cDllName; // Returns mouse cursor if (MouseCursor enum)
procedure SetMouseCursor(aCurrsor: integer); cdecl; external cDllName; // Set mouse cursor

// Input-related functions: touch
function GetTouchX(): integer; cdecl; external cDllName; // Returns touch position X for touch point 0 (relative to screen size)
function GetTouchY(): integer; cdecl; external cDllName; // Returns touch position Y for touch point 0 (relative to screen size)
function GetTouchPosition(aIndex: integer): TVector2; cdecl; external cDllName; // Returns touch position XY for a touch point index (relative to screen size)

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------

procedure SetGesturesEnabled(aGestureFlags: cardinal); cdecl; external cDllName; // Enable a set of gestures using flags
function IsGestureDetected(aGesture: integer): boolean; cdecl; external cDllName; // Check if a gesture have been detected
function GetGestureDetected(): integer; cdecl; external cDllName; // Get latest detected gesture
function GetTouchPointsCount(): integer; cdecl; external cDllName; // Get touch points count
function GetGestureHoldDuration(): single; cdecl; external cDllName; // Get gesture hold time in milliseconds
function GetGestureDragVector(): TVector2; cdecl; external cDllName; // Get gesture drag vector
function GetGestureDragAngle(): single; cdecl; external cDllName; // Get gesture drag angle
function GetGesturePinchVector(): TVector2; cdecl; external cDllName; // Get gesture pinch delta
function GetGesturePinchAngle(): single; cdecl; external cDllName; // Get gesture pinch angle

//------------------------------------------------------------------------------------
// TCamera System Functions (Module: TCamera)
//------------------------------------------------------------------------------------

procedure SetCameraMode(aCamera: TCamera; aMode: integer); cdecl; external cDllName; // Set camera mode (multiple camera modes available)
procedure UpdateCamera(aCamera: PCamera); cdecl; external cDllName; // Update camera position for selected mode

procedure SetCameraPanControl(aKeyPan: integer); cdecl; external cDllName; // Set camera pan key to combine with mouse movement (free camera)
procedure SetCameraAltControl(aKeyPan: integer); cdecl; external cDllName; // Set camera alt key to combine with mouse movement (free camera)
procedure SetCameraSmoothZoomControl(aKeySmoothZoom: integer); cdecl; external cDllName; // Set camera smooth zoom key to combine with mouse (free camera)
procedure SetCameraMoveControls(aKeyFront: integer; aKeyBack: integer; aKeyRight: integer; aKeyLeft: integer; aKeyUp: integer; aKeyDown: integer); cdecl; external cDllName; // Set camera move controls (1st person and 3rd person cameras)

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

// Basic shapes drawing functions
procedure DrawPixel(aPosX: integer; aPosY: integer; aColor: TColor); cdecl; external cDllName; // Draw a pixel
procedure DrawPixelV(aPosition: TVector2; TColor: TColor); cdecl; external cDllName; // Draw a pixel (Vector version)
procedure DrawLine(aStartPosX: integer; aStartPosY: integer; aEndPosX: integer; aEndPosY: integer; aColor: TColor); cdecl; external cDllName; // Draw a line
procedure DrawLineV(aStartPos: TVector2; aEndPos: TVector2; aColor: TColor); cdecl; external cDllName; // Draw a line (Vector version)
procedure DrawLineEx(aStartPos: TVector2; aEndPos: TVector2; aThick: single; aColor: TColor); cdecl; external cDllName; // Draw a line defining thickness
procedure DrawLineBezier(aStartPos: TVector2; aEndPos: TVector2; aThick: single; aColor: TColor); cdecl; external cDllName; // Draw a line using cubic-bezier curves in-out
procedure DrawLineStrip(aPoints: PVector2; aPointsCount: integer; aColor: TColor); cdecl; external cDllName; // Draw lines sequence
procedure DrawCircle(aCenterX: integer; aCenterY: integer; aRadius: single; aColor: TColor); cdecl; external cDllName; // Draw a color-filled circle
procedure DrawCircleSector(aCenter: TVector2; aRadius: single; aStartAngle, aEndAngle, aSegments: integer; aColor: TColor); cdecl; external cDllName; // Draw a piece of a circle
procedure DrawCircleSectorLines(aCenter: TVector2; aRadius: single; aStartAngle, aEndAngle, aSegments: integer; aColor: TColor); cdecl; external cDllName; // Draw circle sector outline
procedure DrawCircleGradient(aCenterX: integer; aCenterY: integer; aRadius: single; aColor1: TColor; aColor2: TColor); cdecl; external cDllName; // Draw a gradient-filled circle
procedure DrawCircleV(aCenter: TVector2; aRadius: single; TColor: TColor); cdecl; external cDllName; // Draw a color-filled circle (Vector version)
procedure DrawCircleLines(aCenterX: integer; aCenterY: integer; aRadius: single; aColor: TColor); cdecl; external cDllName; // Draw circle outline

procedure DrawEllipse(aCenterX: integer; aCenterY: integer; aRadiusH: single; aRadiusV: single; aColor: TColor); cdecl; external cDllName; // Draw ellipse
procedure DrawEllipseLines(aCenterX: integer; aCenterY: integer; aRadiusH: single; aRadiusV: single; aColor: TColor); cdecl; external cDllName; // Draw ellipse outline

procedure DrawRing(aCenter: TVector2; aInnerRadius, aOuterRadius: single; aStartAngle, aEndAngle, aSegments: integer; aColor: TColor); cdecl; external cDllName; // Draw ring
procedure DrawRingLines(aCenter: TVector2; aInnerRadius, aOuterRadius: single; aStartAngle, aEndAngle, aSegments: integer; aColor: TColor); cdecl; external cDllName; // Draw ring outline

procedure DrawRectangle(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor: TColor); cdecl; external cDllName; // Draw a color-filled rectangle
procedure DrawRectangleV(aPosition: TVector2; size: TVector2; TColor: TColor); cdecl; external cDllName; // Draw a color-filled rectangle (Vector version)
procedure DrawRectangleRec(aRect: TRectangle; aColor: TColor); cdecl; external cDllName; // Draw a color-filled rectangle
procedure DrawRectanglePro(aRect: TRectangle; origin: TVector2; aRotation: single; aColor: TColor); cdecl; external cDllName; // Draw a color-filled rectangle with pro parameters
procedure DrawRectangleGradientV(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor1: TColor; aColor2: TColor); cdecl; external cDllName; // Draw a vertical-gradient-filled rectangle
procedure DrawRectangleGradientH(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor1: TColor; aColor2: TColor); cdecl; external cDllName; // Draw a horizontal-gradient-filled rectangle
procedure DrawRectangleGradientEx(aRect: TRectangle; aCol1: TColor; aCol2: TColor; aCol3: TColor; aCol4: TColor); cdecl; external cDllName; // Draw a gradient-filled rectangle with custom vertex colors
procedure DrawRectangleLines(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; TColor: TColor); cdecl; external cDllName; // Draw rectangle outline
procedure DrawRectangleLinesEx(aRect: TRectangle; aLineThick: integer; aColor: TColor); cdecl; external cDllName; // Draw rectangle outline with extended parameters

procedure DrawRectangleRounded(aRec: TRectangle; aRoundness: single; aSegments: integer; aColor: TColor); cdecl; external cDllName;  // Draw rectangle with rounded edges
procedure DrawRectangleRoundedLines(aRec: TRectangle; aRoundness: single; aSegments: integer; aLineThick: integer; aColor: TColor); cdecl; external cDllName; // Draw rectangle with rounded edges outline

procedure DrawTriangle(aVec1: TVector2; aVec2: TVector2; aVec3: TVector2; aColor: TColor); cdecl; external cDllName; // Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleLines(aVec1: TVector2; aVec2: TVector2; aVec3: TVector2; aColor: TColor); cdecl; external cDllName; // Draw triangle outline (vertex in counter-clockwise order!)
procedure DrawTriangleFan(aPoints: PVector2; aPointsCount: integer; aColor: TColor); cdecl; external cDllName; // Draw a triangle fan defined by points (first vertex is the center)
procedure DrawTriangleStrip(aPoints: PVector2; aPointsCount: integer; aColor: TColor); cdecl; external cDllName; // Draw a triangle strip defined by points
procedure DrawPoly(aCenter: TVector2; aSides: integer; aRadius: single; aRotation: single; aColor: TColor); cdecl; external cDllName; // Draw a regular polygon (Vector version)
procedure DrawPolyLines(aCenter: TVector2; aSides: integer; aRadius: single; aRotation: single; aColor: TColor); cdecl; external cDllName; // Draw a polygon outline of n sides

// Basic shapes collision detection functions
function CheckCollisionRecs(aRect1: TRectangle; aRect2: TRectangle): boolean; cdecl; external cDllName; // Check collision between two rectangles
function CheckCollisionCircles(aCenter1: TVector2; aRadius1: single; aCenter2: TVector2; aRadius2: single): boolean; cdecl; external cDllName; // Check collision between two circles
function CheckCollisionCircleRec(aCenter: TVector2; aRadius: single; aRect: TRectangle): boolean; cdecl; external cDllName; // Check collision between circle and rectangle
function CheckCollisionPointRec(aPoint: TVector2; aRect: TRectangle): boolean; cdecl; external cDllName; // Check if point is inside rectangle
function CheckCollisionPointCircle(aPoint: TVector2; aCenter: TVector2; aRadius: single): boolean; cdecl; external cDllName; // Check if point is inside circle
function CheckCollisionPointTriangle(aPoint: TVector2; aP1: TVector2; aP2: TVector2; aP3: TVector2): boolean; cdecl; external cDllName; // Check if point is inside a triangle
function CheckCollisionLines( aStartPos1: TVector2; aEndPos1: TVector2; aStartPos2: TVector2; aEndPos2: TVector2; aCollisionPoint: PVector2): boolean; cdecl; external cDllName; // Check the collision between two lines defined by two points each, returns collision point by reference
function GetCollisionRec(aRect1: TRectangle; aRect2: TRectangle): TRectangle; cdecl; external cDllName; // Get collision rectangle for two rectangles collision

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

// Image loading functions
// NOTE: This functions do not require GPU access
function LoadImage(aFilename: PAnsiChar): TImage; cdecl; external cDllName; // Load image from file into CPU memory (RAM)
function LoadImageRaw(aFilename: PAnsiChar; aWidth: integer; aHeight: integer; aFormat: integer; headerSize: integer): TImage; cdecl; external cDllName; // Load image from RAW file data
function LoadImageAnim(aFilename: PAnsiChar; aFrames: integer): TImage; cdecl; external cDllName; // Load image sequence from file (frames appended to image.data)
function LoadImageFromMemory(aFileType: PAnsiChar; aFileData: PAnsiChar; aDataSize: integer): TImage; cdecl; external cDllName; // Load image from memory buffer, fileType refers to extension: i.e. "png"
procedure UnloadImage(aImage: TImage); cdecl; external cDllName; // Unload image from CPU memory (RAM)
function ExportImage(aImage: Timage; aFilename: PAnsiChar): boolean; cdecl; external cDllName; // Export image data to file
function ExportImageAsCode(aImage: TImage; aFilename: PAnsiChar): boolean; cdecl; external cDllName; // Export image as code file defining an array of bytes

// TImage generation functions
function GenImageColor(aWidth: integer; aHeight: integer; aColor: TColor): TImage; cdecl; external cDllName; // Generate image: plain color
function GenImageGradientV(aWidth: integer; aHeight: integer; aTop: TColor; aBottom: TColor): TImage; cdecl; external cDllName; // Generate image: vertical gradient
function GenImageGradientH(aWidth: integer; aHeight: integer; aLeft: TColor; aRight: TColor): TImage; cdecl; external cDllName; // Generate image: horizontal gradient
function GenImageGradientRadial(aWidth: integer; aHeight: integer; aDensity: single; aInner: TColor; aOuter: TColor): TImage; cdecl; external cDllName; // Generate image: radial gradient
function GenImageChecked(aWidth: integer; aHeight: integer; aChecksX: integer; aChecksY: integer; aCol1: TColor; aCol2: TColor): TImage; cdecl; external cDllName; // Generate image: checked
function GenImageWhiteNoise(aWidth: integer; aHeight: integer; aFactor: single): TImage; cdecl; external cDllName; // Generate image: white noise
function GenImagePerlinNoise(aWidth: integer; aHeight: integer; aOffsetX: integer; aOffsetY: integer; aScale: single): TImage; cdecl; external cDllName; // Generate image: perlin noise
function GenImageCellular(aWidth: integer; aHeight: integer; aTileSize: integer): TImage; cdecl; external cDllName; // Generate image: cellular algorithm. Bigger tileSize means bigger cells

// TImage manipulation functions
function ImageCopy(aImage: TImage): TImage; cdecl; external cDllName; // Create an image duplicate (useful for transformations)
function ImageFromImage(aImage: TImage; aRec: TRectangle): TImage; cdecl; external cDllName; // Create an image from another image piece
function ImageText(aText: PAnsiChar; aFontSize: integer; aColor: TColor): TImage; cdecl; external cDllName; // Create an image from text (default font)
function ImageTextEx(aFont: TFont; aText: PAnsiChar; aFontSize: single; aSpacing: single; aTint: TColor): TImage; cdecl; external cDllName; // Create an image from text (custom sprite font)
procedure ImageFormat(aImage: PImage; aNewFormat: integer); cdecl; external cDllName; // Convert image data to desired format
procedure ImageToPOT(aImage: PImage; fillColor: TColor); cdecl; external cDllName; // Convert image to POT (power-of-two)
procedure ImageCrop(aImage: PImage; crop: TRectangle); cdecl; external cDllName; // Crop an image to a defined rectangle
procedure ImageAlphaCrop(aImage: PImage; threshold: single); cdecl; external cDllName; // Crop image depending on alpha value
procedure ImageAlphaClear(aImage: PImage; TColor: TColor; threshold: single); cdecl; external cDllName; // Clear alpha channel to desired color
procedure ImageAlphaMask(aImage: PImage; alphaMask: TImage); cdecl; external cDllName; // Apply alpha mask to image
procedure ImageAlphaPremultiply(aImage: PImage); cdecl; external cDllName; // Premultiply alpha channel
procedure ImageResize(aImage: PImage; aNewWidth: integer; aNewHeight: integer); cdecl; external cDllName; // Resize image (Bicubic scaling algorithm)
procedure ImageResizeNN(aImage: PImage; aNewWidth: integer; aNewHeight: integer); cdecl; external cDllName; // Resize image (Nearest-Neighbor scaling algorithm)
procedure ImageResizeCanvas(aImage: PImage; aNewWidth: integer; aNewHeight: integer; aOffsetX: integer; aOffsetY: integer; aFill: TColor); cdecl; external cDllName; // Resize canvas and fill with color
procedure ImageMipmaps(aImage: PImage); cdecl; external cDllName; // Generate all mipmap levels for a provided image
procedure ImageDither(aImage: PImage; aRedBpp: integer; aGreenBpp: integer; aBlueBpp: integer; aAlphaBpp: integer); cdecl; external cDllName; // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
procedure ImageFlipVertical(aImage: PImage); cdecl; external cDllName; // Flip image vertically
procedure ImageFlipHorizontal(aImage: PImage); cdecl; external cDllName; // Flip image horizontally
procedure ImageRotateCW(aImage: PImage); cdecl; external cDllName; // Rotate image clockwise 90deg
procedure ImageRotateCCW(aImage: PImage); cdecl; external cDllName; // Rotate image counter-clockwise 90deg
procedure ImageColorTint(aImage: PImage; aColor: TColor); cdecl; external cDllName; // Modify image color: tint
procedure ImageColorInvert(aImage: PImage); cdecl; external cDllName; // Modify image color: invert
procedure ImageColorGrayscale(aImage: PImage); cdecl; external cDllName; // Modify image color: grayscale
procedure ImageColorContrast(aImage: PImage; aContrast: single); cdecl; external cDllName; // Modify image color: contrast (-100 to 100)
procedure ImageColorBrightness(aImage: PImage; aBrightness: integer); cdecl; external cDllName; // Modify image color: brightness (-255 to 255)
procedure ImageColorReplace(aImage: PImage; aColor: TColor; aReplace: TColor); cdecl; external cDllName; // Modify image color: replace color
function LoadImageColors(aImage: TImage): PColor; cdecl; external cDllName; // Load color data from image as a Color array (RGBA - 32bit)
function LoadImagePalette(aImage: TImage; aMaxPaletteSize: integer; aColorsCount: PInteger): PColor; cdecl; external cDllName; // Load colors palette from image as a Color array (RGBA - 32bit)
procedure UnloadImageColors(aColor: PColor); cdecl; external cDllName; // Unload color data loaded with LoadImageColors()
procedure UnloadImagePalette(aColor: PColor); cdecl; external cDllName; // Unload colors palette loaded with LoadImagePalette()
function GetImageAlphaBorder(aImage: TImage; aThreshold: single): TRectangle; cdecl; external cDllName; // Get image alpha border rectangle

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
procedure ImageClearBackground(aDst: PImage; aColor: TColor); cdecl; external cDllName; // Clear image background with given color
procedure ImageDrawPixel(aDst: PImage; aPosX: integer; aPosY: integer; aColor: TColor); cdecl; external cDllName; // Draw pixel within an image
procedure ImageDrawPixelV(aDst: PImage; aPosition: TVector2; aColor: TColor); cdecl; external cDllName; // Draw pixel within an image (Vector version)
procedure ImageDrawLine(aDst: PImage; aStartPosX: integer; aStartPosY: integer; aEndPosX: integer; aEndPosY: integer; aColor: TColor); cdecl; external cDllName; // Draw line within an image
procedure ImageDrawLineV(aDst: PImage; aStart: TVector2; aEnd: TVector2; aColor: TColor); cdecl; external cDllName; // Draw line within an image (Vector version)
procedure ImageDrawCircle(aDst: PImage; aCenterX: integer; aCenterY: integer; aRadius: integer; aColor: TColor); cdecl; external cDllName; // Draw circle within an image
procedure ImageDrawCircleV(aDst: PImage; aCenter: TVector2; aRadius: integer; aColor: TColor); cdecl; external cDllName; // Draw circle within an image (Vector version)
procedure ImageDrawRectangle(aDst: PImage; aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer); cdecl; external cDllName; // Draw rectangle within an image
procedure ImageDrawRectangleV(aDst: PImage; aPosition: TVector2; aSize: TVector2; aColor: TColor); cdecl; external cDllName; // Draw rectangle within an image (Vector version)
procedure ImageDrawRectangleRec(aDst: PImage; aRec: TRectangle; aColor: TColor); cdecl; external cDllName; // Draw rectangle within an image
procedure ImageDrawRectangleLines(aDst: PImage; aRec: TRectangle; aThick: integer; aColor: TColor); cdecl; external cDllName; // Draw rectangle lines within an image
procedure ImageDraw(aDest: PImage; aSrc: TImage; aSrcRec: TRectangle; aDestRec: TRectangle; aTint: TColor); cdecl; external cDllName; // Draw a source image within a destination image (tint applied to source)
procedure ImageDrawText(aDst: PImage; aText: PAnsiChar; aPosX: integer; aPosY: integer; aFontSize: integer; aColor: TColor); cdecl; external cDllName; // Draw text (using default font) within an image (destination)
procedure ImageDrawTextEx(aDst: PImage; aFont: TFont; aText: PAnsiChar; aPosition:TVector2; aFontSize: single; aSpacing: single; aTint: TColor); cdecl; external cDllName; // Draw text (custom sprite font) within an image (destination)


// Texture loading functions
// NOTE: These functions require GPU access
function LoadTexture(aFilename: PAnsiChar): TTexture2D; cdecl; external cDllName; // Load texture from file into GPU memory (VRAM)
function LoadTextureFromImage(aImage: TImage): TTexture2D; cdecl; external cDllName; // Load texture from image data
function LoadTextureCubemap(aImage: TImage; aLayoutType: integer): TTextureCubemap; cdecl; external cDllName; // Load cubemap from image, multiple image cubemap layouts supported
function LoadRenderTexture(aWidth: integer; aHeight: integer): TRenderTexture2D; cdecl; external cDllName; // Load texture for rendering (framebuffer)
procedure UnloadTexture(aTexture: TTexture2D); cdecl; external cDllName; // Unload texture from GPU memory (VRAM)
procedure UnloadRenderTexture(aTarget: TRenderTexture2D); cdecl; external cDllName; // Unload render texture from GPU memory (VRAM)
procedure UpdateTexture(aTexture: TTexture2D; aPixels: Pointer); cdecl; external cDllName; // Update GPU texture with new data
procedure UpdateTextureRec(aTexture: TTexture2D;aRec: TRectangle; aPixels: Pointer); cdecl; external cDllName; // Update GPU texture rectangle with new data
function GetTextureData(aTexture: TTexture2D): TImage; cdecl; external cDllName; // Get pixel data from GPU texture and return an Image
function GetScreenData(): TImage; cdecl; external cDllName; // Get pixel data from screen buffer and return an Image (screenshot)

// TTexture2D configuration functions
procedure GenTextureMipmaps(aTexture: PTexture2D); cdecl; external cDllName; // Generate GPU mipmaps for a texture
procedure SetTextureFilter(aTexture: TTexture2D; aFilterMode: integer); cdecl; external cDllName; // Set texture scaling filter mode
procedure SetTextureWrap(aTexture: TTexture2D; aWrapMode: integer); cdecl; external cDllName; // Set texture wrapping mode

// TTexture2D drawing functions
procedure DrawTexture(aTexture: TTexture2D; posX: integer; aPosY: integer; aTint: TColor); cdecl; external cDllName; // Draw a Texture2D
procedure DrawTextureV(aTexture: TTexture2D; position: TVector2; aTint: TColor); cdecl; external cDllName; // Draw a Texture2D with position defined as Vector2
procedure DrawTextureEx(aTexture: TTexture2D; position: TVector2; aRotation: single; aScale: single; aTint: TColor); cdecl; external cDllName; // Draw a Texture2D with extended parameters
procedure DrawTextureRec(aTexture: TTexture2D; aSource: TRectangle; aPosition: TVector2; aTint: TColor); cdecl; external cDllName; // Draw a part of a texture defined by a rectangle
procedure DrawTextureQuad(aTexture: TTexture2D; aTiling, aOffset: TVector2; aQuad: TRectangle; aTint: TColor); cdecl; external cDllName; // Draw texture quad with tiling and offset parameters
procedure DrawTextureTiled(aTexture: TTexture2D; aSource: TRectangle; aDest: TRectangle; aOrigin: TVector2; aRotation: single; aScale: single; aTint: TColor); cdecl; external cDllName; // Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
procedure DrawTexturePro(aTexture: TTexture2D; aSource: TRectangle; aDest: TRectangle; aOrigin: TVector2; aRotation: single; aTint: TColor); cdecl; external cDllName; // Draw a part of a texture defined by a rectangle with 'pro' parameters
procedure DrawTextureNPatch(aTexture: TTexture2D; aNPatchInfo: TNPatchInfo; aDest: TRectangle; aOrigin: TVector2; aRotation: single; aTint: TColor); cdecl; external cDllName; // Draws a texture (or part of it) that stretches or shrinks nicely

// Color/pixel related functions
function Fade(aColor: TColor; aAlpha: single): TColor; cdecl; external cDllName; /// Returns color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorToInt(aColor: TColor): integer; cdecl; external cDllName; // Returns hexadecimal value for a Color
function ColorNormalize(aColor: TColor): TVector4; cdecl; external cDllName; // Returns color normalized as float [0..1]
function ColorFromNormalized(aNormalized: TVector4): TColor; cdecl; external cDllName; // Returns color from normalized values [0..1]
function ColorToHSV(aColor: TColor): TVector3; cdecl; external cDllName; // Returns HSV values for a Color
function ColorFromHSV(aHsv: TVector3): TColor; cdecl; external cDllName; // Returns a Color from HSV values
function ColorAlpha(aColor: TColor; aAlpha: single): TColor; cdecl; external cDllName; // Returns color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorAlphaBlend(aDst: TColor; aSrc: TColor; aTint: TColor): TColor; cdecl; external cDllName; // Returns src alpha-blended into dst color with tint
function GetColor(aHexValue: integer):TColor; cdecl; external cDllName; // Get Color structure from hexadecimal value
function GetPixelColor(aSrcPtr:Pointer; aFormat:integer):TColor; cdecl; external cDllName; // Get Color from a source pixel pointer of certain format
procedure SetPixelColor(aDstPtr:Pointer; aColor:TColor; aFormat:Integer); cdecl; external cDllName; // Set color formatted into destination pixel pointer
function GetPixelDataSize(aWidth: integer; aHeight: integer; aFormat: integer): integer; cdecl; external cDllName; // Get pixel data size in bytes (image or texture)

//------------------------------------------------------------------------------------
// TFont Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

// TFont loading/unloading functions
function GetFontDefault(): TFont; cdecl; external cDllName; // Get the default Font
function LoadFont(aFilename: PAnsiChar): TFont; cdecl; external cDllName; // Load font from file into GPU memory (VRAM)
function LoadFontEx(aFilename: PAnsiChar; aFontSize: integer; aFontChars: PInteger; aCharsCount: integer): TFont; cdecl; external cDllName; // Load font from file with extended parameters
function LoadFontFromImage(aImage: TImage; aKey: TColor; aFirstChar: integer): TFont; cdecl; external cDllName; // Load font from Image (XNA style)
function LoadFontFromMemory(aFileType: PAnsiChar; aFileData: PAnsiChar; aDataSize: integer; aFontSize:integer; aFontChars: PansiChar; aCharsCount: integer):TFont; cdecl; external cDllName; // Load font from memory buffer, fileType refers to extension: i.e. "ttf"
function LoadFontData(aFilename: PAnsiChar; aFontSize: integer; aFontChars: PInteger; aCharsCount, atype: integer): PCharInfo; cdecl; external cDllName; // Load font data for further use
function GenImageFontAtlas(aChars: PCharInfo; aRecs: PPRectangle; aCharsCount, aFontSize, aPadding, aPackMethod: integer): TImage; cdecl; external cDllName; // Generate image font atlas using chars info
procedure UnloadFontData(aChars:PansiChar; aCharsCount: integer); cdecl; external cDllName; // Unload font chars info data (RAM)
procedure UnloadFont(aFont: TFont); cdecl; external cDllName; // Unload Font from GPU memory (VRAM)

// aText drawing functions
procedure DrawFPS(aPosX: integer; aPosY: integer); cdecl; external cDllName; // Shows current FPS
procedure DrawText(aText: PAnsiChar; aPosX: integer; aPosY: integer; aFontSize: integer; TColor: TColor); cdecl; external cDllName; // Draw text (using default font)
procedure DrawTextEx(aFont: TFont; aText: PAnsiChar; aPosition: TVector2; aFontSize: single; aSpacing: single; aTint: TColor); cdecl; external cDllName; // Draw text using font and additional parameters
procedure DrawTextRec(afont: TFont; aText: PAnsiChar; rec: TRectangle; aFontSize, aSpacing: single; aWordWrap: boolean; aTint: TColor); cdecl; external cDllName; // Draw text using font inside rectangle limits
procedure DrawTextRecEx(aFont: TFont; aText: PAnsiChar; aRec: TRectangle; aFontSize, aSpacing: single; aWordWrap: boolean; aTint: TColor; aSelectStart, aSelectLength: integer; aSelectText, selectBack: TColor); cdecl; external cDllName; // Draw text using font inside rectangle limits with support for text selection
procedure DrawTextCodepoint(aFont: TFont; aCodepoint: integer; aPosition: TVector2; aScale: single; aColor: TColor); cdecl; external cDllName; // Draw one character (codepoint)

// Text misc. functions
function MeasureText(aText: PAnsiChar; aFontSize: integer): integer; cdecl; external cDllName; // Measure string width for default font
function MeasureTextEx(aFont: TFont; aText: PAnsiChar; aFontSize, aSpacing: single): TVector2; cdecl; external cDllName; // Measure string size for Font
function GetGlyphIndex(aFont: TFont; character: integer): integer; cdecl; external cDllName; // Get index position for a unicode character on font

// Text strings management functions (no utf8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
function TextCopy(aDst: PAnsiChar; aScr: PAnsichar): integer; cdecl; external cDllName; // Copy one string to another, returns bytes copied
function TextIsEqual(aText1, aText2: PAnsiChar): boolean; cdecl; external cDllName; // Check if two text string are equal
function TextLength(aText: PAnsiChar): cardinal; cdecl; external cDllName; // Get text length, checks for '\0' ending
function TextFormat(aText: PAnsiChar; aArg: integer): PAnsiChar; cdecl; external cDllName; // Text formatting with variables (sprintf style)
function TextSubtext(aText: PAnsiChar; aPosition: integer; aLength: integer): PAnsiChar; cdecl; external cDllName; // Get a piece of a text string
function TextReplace(aText, aReplace, aBy: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Replace text string (memory must be freed!)
function TextInsert(aText, aInsert: PAnsiChar; aPosition: integer): PAnsiChar; cdecl; external cDllName; // Insert text in a position (memory should be freed!)
function TextJoin(aTextList: PPAnsiChar; aCount: integer; aDelimiter: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Join text strings with delimiter
function TextSplit(aText: PAnsiChar; aDelimiter: char; aCount: PInteger): PPAnsiChar; cdecl; external cDllName; // Split text into multiple strings
procedure TextAppend(aText, aAppend: PAnsiChar; aPosition: PInteger); cdecl; external cDllName; // Append text at specific position and move cursor!
function TextFindIndex(aText, aFind: PAnsiChar): integer; cdecl; external cDllName; // Find first text occurrence within a string
function TextToUpper(aText: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get upper case version of provided string
function TextToLower(aText: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get lower case version of provided string
function TextToPascal(aText: PAnsiChar): PAnsiChar; cdecl; external cDllName; // Get Pascal case notation version of provided string
function TextToInteger(aText: PAnsiChar): integer; cdecl; external cDllName; // Get integer value from text (negative values not supported)
function TextToUtf8(aCodepoints: PInteger; aLength: integer): PAnsiChar; cdecl; external cDllName; // Encode text codepoint into utf8 text (memory must be freed!)

// UTF8 text strings management functions
function GetCodepoints(aText: PAnsiChar; aCount: PInteger): PInteger; cdecl; external cDllName;// Get all codepoints in a string, codepoints count returned by parameters
function GetCodepointsCount(aText: PAnsiChar): integer; cdecl; external cDllName;// Get total number of characters (codepoints) in a UTF8 encoded string
function GetNextCodepoint(aText: PAnsiChar; aBytesProcessed: PInteger): integer; cdecl; external cDllName;// Returns next codepoint in a UTF8 encoded string; 0x3f('?') is returned on failure
function CodepointToUtf8(aCodepoint: integer; aByteLength: PInteger): PAnsiChar; cdecl; external cDllName;// Encode codepoint into utf8 text (char array length returned as parameter)

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Basic geometric 3D shapes drawing functions
procedure DrawLine3D(aStartPos: TVector3; aEndPos: TVector3; aColor: TColor); cdecl; external cDllName;  // Draw a line in 3D world space
procedure DrawPoint3D(aPosition: TVector3; aColor: TColor); cdecl; external cDllName; // Draw a point in 3D space, actually a small line
procedure DrawCircle3D(aCenter: TVector3; aRadius: single; aRotationAxis: TVector3; aRotationAngle: single; aColor: TColor); cdecl; external cDllName; // Draw a circle in 3D world space
procedure DrawTriangle3D(aV1: TVector3; aV2: TVector3; aV3: TVector3; aColor:TColor); cdecl; external cDllName; // Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleStrip3D(aPoints:PVector3; aPointsCount:integer; aColor:TColor); cdecl; external cDllName; // Draw a triangle strip defined by points
procedure DrawCube(aPosition: TVector3; aWidth: single; aHeight: single; aLength: single; aColor: TColor); cdecl; external cDllName; // Draw cube
procedure DrawCubeV(aPosition: TVector3; aSize: TVector3; aColor: TColor); cdecl; external cDllName; // Draw cube (Vector version)
procedure DrawCubeWires(aPosition: TVector3; aWidth: single; aHeight: single; aLength: single; aColor: TColor); cdecl; external cDllName; // Draw cube wires
procedure DrawCubeWiresV(aPosition, aSize: TVector3; aColor: TColor); cdecl; external cDllName; // Draw cube wires (Vector version)
procedure DrawCubeTexture(aTexture: TTexture2D; aPosition: TVector3; aWidth: single; aHeight: single; aLength: single; aColor: TColor); cdecl; external cDllName; // Draw cube textured
procedure DrawSphere(aCenterPos: TVector3; aRadius: single; aColor: TColor); cdecl; external cDllName; // Draw sphere
procedure DrawSphereEx(aCenterPos: TVector3; aRadius: single; aRings: integer; aSlices: integer; aColor: TColor); cdecl; external cDllName; // Draw sphere with extended parameters
procedure DrawSphereWires(aCenterPos: TVector3; aRadius: single; aRings: integer; aSlices: integer; aColor: TColor); cdecl; external cDllName; // Draw sphere wires
procedure DrawCylinder(aPosition: TVector3; radiusTop: single; aRadiusBottom: single; aHeight: single; aSlices: integer; aColor: TColor); cdecl; external cDllName; // Draw a cylinder/cone
procedure DrawCylinderWires(aPosition: TVector3; radiusTop: single; aRadiusBottom: single; aHeight: single; aSlices: integer; aColor: TColor); cdecl; external cDllName; // Draw a cylinder/cone wires
procedure DrawPlane(aCenterPos: TVector3; aSize: TVector2; aColor: TColor); cdecl; external cDllName; // Draw a plane XZ
procedure DrawRay(TRay: TRay; aColor: TColor); cdecl; external cDllName; // Draw a ray line
procedure DrawGrid(aSlices: integer; aSpacing: single); cdecl; external cDllName; // Draw a grid (centered at (0, 0, 0))
procedure DrawGizmo(aPosition: TVector3); cdecl; external cDllName;  // Draw simple gizmo
//DrawTorus(), DrawTeapot() could be useful?

//------------------------------------------------------------------------------------
// TModel 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// TModel loading/unloading functions
function LoadModel(aFilename: PAnsiChar): TModel; cdecl; external cDllName; // Load model from files (meshes and materials)
function LoadModelFromMesh(aMesh: TMesh): TModel; cdecl; external cDllName; // Load model from generated mesh (default material)
procedure UnloadModel(aModel: TModel); cdecl; external cDllName; // Unload model from memory (RAM and/or VRAM)
procedure UnloadModelKeepMeshes(aModel: TModel); cdecl; external cDllName; // Unload model (but not meshes) from memory (RAM and/or VRAM)

// TMesh loading/unloading functions
function LoadMeshes(aFilename: PAnsiChar; aCount: PInteger): PMesh; cdecl; external cDllName; // Load meshes from model file
procedure UnloadMesh(aMesh: TMesh); cdecl; external cDllName; // Unload mesh from memory (RAM and/or VRAM)
function ExportMesh(aMesh: TMesh; aFilename: PAnsiChar): boolean; cdecl; external cDllName;  // Export mesh data to file, returns true on success

// Material loading/unloading functions
function LoadMaterials(aFilename: PAnsiChar; aMaterialCount: PInteger): PMaterial; cdecl; external cDllName; // Load materials from model file
function LoadMaterialDefault(): TMaterial; cdecl; external cDllName; // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
procedure UnloadMaterial(aMaterial: TMaterial); cdecl; external cDllName; // Unload material from GPU memory (VRAM)
procedure SetMaterialTexture(aMaterial: PMaterial; aMapType: integer; aTexture: TTexture2D); cdecl; external cDllName; // Set texture for a material map type (MAP_DIFFUSE, MAP_SPECULAR...)
procedure SetModelMeshMaterial(aModel: PModel; aMeshId, aMaterialId: integer); cdecl; external cDllName; // Check model animation skeleton match

// Model animations loading/unloading functions
function LoadModelAnimations(aFilename: PAnsiChar; aAnimsCount: PInteger): PModelAnimation; cdecl; external cDllName; // Load model animations from file
procedure UpdateModelAnimation(aModel: TModel; aAnim: TModelAnimation; aFrame: integer); cdecl; external cDllName ;// Update model animation pose
procedure UnloadModelAnimation(aAnim: TModelAnimation); cdecl; external cDllName; // Unload animation data
function IsModelAnimationValid(aModel: TModel; aAnim: TModelAnimation): boolean; cdecl; external cDllName; // Check model animation skeleton match

// TMesh generation functions
function GenMeshPoly(aSides: integer; aRadius: single): TMesh; cdecl; external cDllName; // Generate polygonal mesh
function GenMeshPlane(aWidth: single; aLength: single; aResX: integer; aResZ: integer): TMesh; cdecl; external cDllName; // Generate plane mesh (with subdivisions)
function GenMeshCube(aWidth: single; aHeight: single; aLength: single): TMesh; cdecl; external cDllName; // Generate cuboid mesh
function GenMeshSphere(aRadius: single; aRings: integer; aSlices: integer): TMesh; cdecl; external cDllName; // Generate sphere mesh (standard sphere)
function GenMeshHemiSphere(aRadius: single; aRings: integer; aSlices: integer): TMesh; cdecl; external cDllName; // Generate half-sphere mesh (no bottom cap)
function GenMeshCylinder(aRadius: single; aHeight: single; aSlices: integer): TMesh; cdecl; external cDllName; // Generate cylinder mesh
function GenMeshTorus(aRadius: single; aSize: single; radSeg: integer; sides: integer): TMesh; cdecl; external cDllName; // Generate torus mesh
function GenMeshKnot(aRadius: single; aSize: single; radSeg: integer; sides: integer): TMesh; cdecl; external cDllName; // Generate trefoil knot mesh
function GenMeshHeightmap(aHeightMap: TImage; aSize: TVector3): TMesh; cdecl; external cDllName; // Generate heightmap mesh from image data
function GenMeshCubicmap(aCubicMap: TImage; aCubeSize: TVector3): TMesh; cdecl; external cDllName; // Generate cubes-based map mesh from image data

// TMesh manipulation functions
function MeshBoundingBox(aMesh: TMesh): TBoundingBox; cdecl; external cDllName;// Compute mesh bounding box limits
procedure MeshTangents(aMesh: PMesh); cdecl; external cDllName; // Compute mesh tangents
procedure MeshBinormals(aMesh: PMesh); cdecl; external cDllName; // Compute mesh binormals
procedure MeshNormalsSmooth(aMesh: PMesh); cdecl; external cDllName; // Smooth (average) vertex normals

// TModel drawing functions
procedure DrawModel(aModel: TModel; aPosition: TVector3; aScale: single; aTint: TColor); cdecl; external cDllName; // Draw a model (with texture if set)
procedure DrawModelEx(aModel: TModel; aPosition: TVector3; aRotationAxis: TVector3; aRotationAngle: single; aScale: TVector3; aTint: TColor); cdecl; external cDllName; // Draw a model with extended parameters
procedure DrawModelWires(aModel: TModel; aPosition: TVector3; aScale: single; aTint: TColor); cdecl; external cDllName;  // Draw a model wires (with texture if set)
procedure DrawModelWiresEx(aModel: TModel; aPosition: TVector3; aRotationAxis: TVector3; aRotationAngle: single; aScale: TVector3; aTint: TColor); cdecl; external cDllName; // Draw a model wires (with texture if set) with extended parameters
procedure DrawBoundingBox(aBox: TBoundingBox; TColor: TColor); cdecl; external cDllName; // Draw bounding box (wires)
procedure DrawBillboard(aCamera: TCamera; aTexture: TTexture2D; aCenter: TVector3; aSize: single; aTint: TColor); cdecl; external cDllName; // Draw a billboard texture
procedure DrawBillboardRec(aCamera: TCamera; aTexture: TTexture2D; aSource: TRectangle; aCenter: TVector3; aSize: single; aTint: TColor); cdecl; external cDllName; // Draw a billboard texture defined by sourceRec

// Collision detection functions
function CheckCollisionSpheres(aCenter1: TVector3; aRadius1: single; aCenter2: TVector3; aRadius2: single): boolean; cdecl; external cDllName; // Detect collision between two spheres
function CheckCollisionBoxes(aBox1: TBoundingBox; aBox2: TBoundingBox): boolean; cdecl; external cDllName; // Detect collision between two bounding boxes
function CheckCollisionBoxSphere(aBox: TBoundingBox; aCenterSphere: TVector3; aRadiusSphere: single): boolean; cdecl; external cDllName; // Detect collision between box and sphere
function CheckCollisionRaySphere(aRay: TRay; aSpherePosition: TVector3; aSphereRadius: single): boolean; cdecl; external cDllName; // Detect collision between ray and sphere
function CheckCollisionRaySphereEx(aRay: TRay; aSpherePosition: TVector3; aSphereRadius: single; var collisionPoint: TVector3): boolean; cdecl; external cDllName; // Detect collision between ray and sphere, returns collision point
function CheckCollisionRayBox(aRay: TRay; aBox: TBoundingBox): boolean; cdecl; external cDllName; // Detect collision between ray and box
function GetCollisionRayMesh(aRay: TRay; aMesh: TMesh; aTransform: TMatrix):TRayHitInfo; cdecl; external cDllName; // Get collision info between ray and mesh
function GetCollisionRayModel(aRay: TRay; var aModel: TModel): TRayHitInfo; cdecl; external cDllName; // Get collision info between ray and model
function GetCollisionRayTriangle(aRay: TRay; aP1: TVector3; aP2: TVector3; aP3: TVector3): TRayHitInfo; cdecl; external cDllName; // Get collision info between ray and triangle
function GetCollisionRayGround(aRay: TRay; aGroundHeight: single): TRayHitInfo; cdecl; external cDllName; // Get collision info between ray and ground plane (Y-normal plane)

//------------------------------------------------------------------------------------
// Shaders System Functions (Module: rlgl)
// NOTE: This functions are useless when using OpenGL 1.1
//------------------------------------------------------------------------------------

// TShader loading/unloading functions
function LoadShader(avsFileName: PAnsiChar; afsFileName: PAnsiChar): TShader; cdecl; external cDllName; // Load shader from files and bind default locations
function LoadShaderCode(avsCode: PAnsiChar; afsCode: PAnsiChar): TShader; cdecl; external cDllName; // Load shader from code strings and bind default locations
procedure UnloadShader(aShader: TShader); cdecl; external cDllName; // Unload shader from GPU memory (VRAM)

function GetShaderDefault(): TShader; cdecl; external cDllName; // Get default shader
function GetTextureDefault(): TTexture2D; cdecl; external cDllName; // Get default texture
function GetShapesTexture(): TTexture2D; cdecl; external cDllName; // Get texture to draw shapes
function GetShapesTextureRec(): TRectangle; cdecl; external cDllName; // Get texture rectangle to draw shapes
procedure SetShapesTexture(aTexture: TTexture2D; aSource: TRectangle); cdecl; external cDllName; // Define default texture used to draw shapes

// Shader configuration functions
function GetShaderLocation(aShader: TShader; aUniformName: PAnsiChar): integer; cdecl; external cDllName; // Get shader uniform location
function GetShaderLocationAttrib(aShader: TShader; aAttribName: PAnsiChar):integer; cdecl; external cDllName; // Get shader attribute location
procedure SetShaderValue(aShader: TShader; aUniformLoc: integer; aValue: Pointer; aUniformType: integer); cdecl; external cDllName; // Set shader uniform value
procedure SetShaderValueV(aShader: TShader; aUniformLoc: integer; aValue: Pointer; aUniformType, aCount: integer); cdecl; external cDllName; // Set shader uniform value vector
procedure SetShaderValueMatrix(TShader: TShader; aUniformLoc: integer; mat: TMatrix); cdecl; external cDllName; // Set shader uniform value (matrix 4x4)
procedure SetShaderValueTexture(aShader: TShader; aUniformLoc: integer; aTexture: TTexture2D); cdecl; external cDllName; // Set shader uniform value for texture
procedure SetMatrixProjection(aProj: TMatrix); cdecl; external cDllName; // Set a custom projection matrix (replaces internal projection matrix)
procedure SetMatrixModelview(aView: TMatrix); cdecl; external cDllName; // Set a custom modelview matrix (replaces internal modelview matrix)
function GetMatrixModelview(): TMatrix; cdecl; external cDllName; // Get internal modelview matrix
function GetMatrixProjection(): TMatrix; cdecl; external cDllName; // Get internal projection matrix

// aTexture maps generation (PBR)
//NOTE: Required shaders should be provided
function  GenTextureCubemap(aShader: TShader; aPanorama:TTexture2D; aSize: integer; aFormat:integer): TTextureCubemap; cdecl; external cDllName; // Generate cubemap texture from 2D panorama texture
function GenTextureIrradiance(aShader: TShader; aCubemap: TTextureCubemap; aSize: integer): TTextureCubemap; cdecl; external cDllName; // Generate irradiance texture using cubemap data
function GenTexturePrefilter(aShader: TShader; aCubemap: TTextureCubemap; aSize: integer): TTextureCubemap; cdecl; external cDllName; // Generate prefilter texture using cubemap data
function GenTextureBRDF(aShader: TShader; aCubemap: TTexture2D; aSize: integer): TTexture2D; cdecl; external cDllName; // Generate BRDF texture

// Shading begin/end functions
procedure BeginShaderMode(aShader: TShader); cdecl; external cDllName;// Begin custom shader drawing
procedure EndShaderMode(); cdecl; external cDllName;// End custom shader drawing (use default shader)
procedure BeginBlendMode(aMode: integer); cdecl; external cDllName;// Begin blending mode (alpha, additive, multiplied)
procedure EndBlendMode(); cdecl; external cDllName;// End blending mode (reset to default: alpha blending)

// VR control functions
procedure InitVrSimulator(); cdecl; external cDllName; // Init VR simulator for selected device parameters
procedure CloseVrSimulator(); cdecl; external cDllName; // Close VR simulator for current device
procedure UpdateVrTracking(aCamera: PCamera); cdecl; external cDllName; // Update VR tracking (position and orientation) and camera
procedure SetVrConfiguration(aInfo: TVrDeviceInfo; aDistortion: TShader); cdecl; external cDllName; // Set stereo rendering configuration parameters
function IsVrSimulatorReady(): boolean; cdecl; external cDllName; // Detect if VR simulator is ready
procedure ToggleVrMode(); cdecl; external cDllName; // Enable/Disable VR experience
procedure BeginVrDrawing(); cdecl; external cDllName; // Begin VR simulator stereo rendering
procedure EndVrDrawing(); cdecl; external cDllName; // End VR simulator stereo rendering

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

// Audio device management functions
procedure InitAudioDevice; cdecl; external cDllName; // Initialize audio device and context
procedure CloseAudioDevice; cdecl; external cDllName; // Close the audio device and context
function IsAudioDeviceReady: boolean; cdecl; external cDllName; // Check if audio device has been initialized successfully
procedure SetMasterVolume(aVolume: single); cdecl; external cDllName; // Set master volume (listener)

// TWave/TSound loading/unloading functions
function LoadWave(aFilename: PAnsiChar): TWave; cdecl; external cDllName; // Load wave data from file
function LoadWaveFromMemory(aFileType: PAnsiChar; aFileData: PAnsiChar; aDataSize: integer): TWave; cdecl; external cDllName; // Load wave from memory buffer, fileType refers to extension: i.e. "wav"
function LoadSound(aFilename: PAnsiChar): TSound; cdecl; external cDllName; // Load sound from file
function LoadSoundFromWave(aWave: TWave): TSound; cdecl; external cDllName; // Load sound from wave data
procedure UpdateSound(aSound: TSound; aData: Pointer; samplesCount: integer); cdecl; external cDllName; // Update sound buffer with new data
procedure UnloadWave(aWave: TWave); cdecl; external cDllName; // Unload wave data
procedure UnloadSound(aSound: TSound); cdecl; external cDllName; // Unload sound
function ExportWave(aWave: TWave; aFileName: PAnsiChar): boolean; cdecl; external cDllName;// Export wave data to file
function ExportWaveAsCode(aWave: TWave; aFileName: PAnsiChar):boolean ; cdecl; external cDllName; // Export wave sample data to code (.h)

// TWave/TSound management functions
procedure PlaySound(aSound: TSound); cdecl; external cDllName; // Play a sound
procedure StopSound(aSound: TSound); cdecl; external cDllName; // Stop playing a sound
procedure PauseSound(aSound: TSound); cdecl; external cDllName; // Pause a sound
procedure ResumeSound(aSound: TSound); cdecl; external cDllName; // Resume a paused sound
procedure PlaySoundMulti(aSound: TSound); cdecl; external cDllName; // Play a sound (using multichannel buffer pool)
procedure StopSoundMulti(); cdecl; external cDllName; // Stop any sound playing (using multichannel buffer pool)
function GetSoundsPlaying(): integer; cdecl; external cDllName; // Get number of sounds playing in the multichannel
function IsSoundPlaying(aSound: TSound): boolean; cdecl; external cDllName; // Check if a sound is currently playing
procedure SetSoundVolume(aSound: TSound; aVolume: single); cdecl; external cDllName; // Set volume for a sound (1.0 is max level)
procedure SetSoundPitch(aSound: TSound; aPitch: single); cdecl; external cDllName; // Set pitch for a sound (1.0 is base level)
procedure WaveFormat(aWave: PWave; aSampleRate: integer; aSampleSize: integer; aChannels: integer); cdecl; external cDllName; // Convert wave data to desired format
function WaveCopy(aWave: TWave): TWave; cdecl; external cDllName; // Copy a wave to a new wave
procedure WaveCrop(aWave: PWave; initSample, finalSample: integer); cdecl; external cDllName; // Crop a wave to defined samples range
function LoadWaveSamples(aWave: TWave): PSingle; cdecl; external cDllName; /// Load samples data from wave as a floats array
procedure UnloadWaveSamples(aSample:PSingle); cdecl; external cDllName; // Unload samples data loaded with LoadWaveSamples()

// aMusic management functions
function LoadMusicStream(aFilename: PAnsiChar): TMusic; cdecl; external cDllName; // Load music stream from file
procedure UnloadMusicStream(aMusic: TMusic); cdecl; external cDllName; // Unload music stream
procedure PlayMusicStream(aMusic: TMusic); cdecl; external cDllName; // Start music playing
procedure UpdateMusicStream(aMusic: TMusic); cdecl; external cDllName; // Updates buffers for music streaming
procedure StopMusicStream(aMusic: TMusic); cdecl; external cDllName; // Stop music playing
procedure PauseMusicStream(aMusic: TMusic); cdecl; external cDllName; // Pause music playing
procedure ResumeMusicStream(aMusic: TMusic); cdecl; external cDllName; // Resume playing paused music
function IsMusicPlaying(aMusic: TMusic): boolean; cdecl; external cDllName; // Check if music is playing
procedure SetMusicVolume(aMusic: TMusic; aVolume: single); cdecl; external cDllName; // Set volume for music (1.0 is max level)
procedure SetMusicPitch(aMusic: TMusic; aPitch: single); cdecl; external cDllName; // Set pitch for a music (1.0 is base level)
function GetMusicTimeLength(aMusic: TMusic): single; cdecl; external cDllName; // Get music time length (in seconds)
function GetMusicTimePlayed(aMusic: TMusic): single; cdecl; external cDllName; // Get current music time played (in seconds)

// TAudioStream management functions
function InitAudioStream(aSampleRate, aSampleSize, aChannels: cardinal): TAudioStream; cdecl; external cDllName; // Init audio stream (to stream raw audio pcm data)
procedure UpdateAudioStream(aStream: TAudioStream; aData: Pointer; samplesCount: integer); cdecl; external cDllName; // Update audio stream buffers with data
procedure CloseAudioStream(aStream: TAudioStream); cdecl; external cDllName; // Close audio stream and free memory
function IsAudioBufferProcessed(aStream: TAudioStream): boolean; cdecl; external cDllName; // Check if any audio stream buffers requires refill
procedure PlayAudioStream(aStream: TAudioStream); cdecl; external cDllName; // Play audio stream
procedure PauseAudioStream(aStream: TAudioStream); cdecl; external cDllName; // Pause audio stream
procedure ResumeAudioStream(aStream: TAudioStream); cdecl; external cDllName; // Resume audio stream
function IsAudioStreamPlaying(aStream: TAudioStream): boolean; cdecl; external cDllName; // Check if audio stream is playing
procedure StopAudioStream(aStream: TAudioStream); cdecl; external cDllName; // Stop audio stream
procedure SetAudioStreamVolume(aStream: TAudioStream; aVolume: single); cdecl; external cDllName; // Set volume for audio stream (1.0 is max level)
procedure SetAudioStreamPitch(aStream: TAudioStream; aPitch: single); cdecl; external cDllName; // Set pitch for audio stream (1.0 is base level)
procedure SetAudioStreamBufferSizeDefault(aSize: integer); cdecl; external cDllName; // Default size for new audio streams

// Custom Misc Functions to help simplify a few things
function Vector2Create(aX: single; aY: single): TVector2;
procedure Vector2Set(aVec: PVector2; aX: single; aY: single);
function Vector3Create(aX: single; aY: single; aZ: single): TVector3;
procedure Vector3Set(aVec: PVector3; aX: single; aY: single; aZ: single);
function ColorCreate(aR: byte; aG: byte; aB: byte; aA: byte): TColor;
procedure TColorSet(aColor: PColor; aR: byte; aG: byte; aB: byte; aA: byte);

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single): TRectangle;
procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single; aWidth: Single; aHeight: Single);
function TCamera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer): TCamera3D;
procedure TCamera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);




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

procedure TColorSet(aColor: PColor; aR: byte; aG: byte; aB: byte; aA: byte);
begin
  aColor^.r := aR;
  aColor^.g := aG;
  aColor^.b := aB;
  aColor^.a := aA;
end;

function RectangleCreate(aX: Single; aY: Single; aWidth: Single; aHeight: Single
  ): TRectangle;
begin
  Result.x := aX;
  Result.y := aY;
  Result.Width := aWidth;
  Result.Height := aHeight;
end;

procedure RectangleSet(aRect: PRectangle; aX: Single; aY: Single;
  aWidth: Single; aHeight: Single);
begin
  aRect^.x := aX;
  aRect^.y := aY;
  aRect^.Width := aWidth;
  aRect^.Height := aHeight;
end;

function TCamera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer): TCamera3D;
begin
  Result.position := aPosition;
  Result.target := aTarget;
  Result.up := aUp;
  Result.fovy := aFOVY;
  Result._type := aType;
end;

procedure TCamera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);
begin
  aCam^.position := aPosition;
  aCam^.target := aTarget;
  aCam^.up := aUp;
  aCam^.fovy := aFOVY;
  aCam^._type := aType;
end;


initialization

end.
