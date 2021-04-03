unit ray_header;

{$mode objfpc}{$H+}

interface


const
  cDllName = {$IFDEF WINDOWS} 'raylib.dll' {$IFEND}
             {$IFDEF DARWIN} 'libraylib.dylib' {$IFEND}
             {$IFDEF LINUX} 'libraylib.so' {$IFEND};
//{$ENDIF}

const
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
    id: cardinal;          // OpenGL framebuffer object id
    texture: TTexture;     // Color buffer attachment texture
    depth: TTexture;       // Depth buffer attachment texture
  end;

 // RenderTexture2D type, same as RenderTexture
  PRenderTexture2D = ^TRenderTexture;
  TRenderTexture2D = TRenderTexture;


  // N-Patch layout info
  PNPatchInfo = ^TNPatchInfo;
  TNPatchInfo =  record
    source: TRectangle;  // Texture source rectangle
    left: integer;       // Left border offset
    top: integer;        // Top border offset
    right: integer;      // Right border offset
    bottom: integer;     // Bottom border offset
    layout: integer;      // Layout of the n-patch: 3x3, 1x3 or 3x1
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
    position: TVector3;   // Camera position
    target: TVector3;     // Camera target it looks-at
    up: TVector3;         // Camera up vector (rotation over its axis)
    fovy: single;         // Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
    projection: integer;  // Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
  end;

  // Camera type fallback, defaults to Camera3D
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
    Value: Single;         // Material map value
  end;

  // Material type (generic)
  PMaterial = ^TMaterial;
  TMaterial =  record
    shader: TShader;       // Material shader
    maps: PMaterialMap;   // Material maps array (MAX_MATERIAL_MAPS)
    params: array[0..3] of Single;       // Material generic parameters (if required)
   // params: PSingle;       // Material generic parameters (if required)
  end;




  // Transformation Properties
 // PPTransform = ^PTransform;
  PTransform = ^TTransform;
  TTransform =  record
    translation: TVector3; // Translation
    rotation: TQuaternion; // Rotation
    scale: TVector3;       // Scale
  end;

  // Bone Information
  PBoneInfo = ^TBoneInfo;
  TBoneInfo =  record
    name: array[0..31] of char; // Bone name
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
  //  framePoses: PPTransform;    // Poses array by frame
    framePoses : ^PTransform;
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
    hResolution: integer;                        // Horizontal resolution in pixels
    vResolution: integer;                        // Vertical resolution in pixels
    hScreenSize: single;                         // Horizontal size in meters
    vScreenSize: single;                         // Vertical size in meters
    vScreenCenter: single;                       // Screen center in meters
    eyeToScreenDistance: single;                 // Distance between eye and display in meters
    lensSeparationDistance: single;              // Lens separation distance in meters
    interpupillaryDistance: single;              // IPD (distance between pupils) in meters
    lensDistortionValues: array[0..3] of single; // Lens distortion constant parameters
    chromaAbCorrection: array[0..3] of single;   // Chromatic aberration correction parameters
  end;

  // VR Stereo rendering configuration for simulator
  PVrStereoConfig = ^TVrStereoConfig;
  TVrStereoConfig = record
    leftLensCenter:    array[0..1] of single; // VR left lens center
    rightLensCenter:   array[0..1] of single; // VR right lens center
    leftScreenCenter:  array[0..1] of single; // VR left screen center
    rightScreenCenter: array[0..1] of single; // VR right screen center
    scale:             array[0..1] of single; // VR distortion scale
    scaleIn:           array[0..1] of single; // VR distortion scale in
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

    KEY_NULL            = 0;
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

    // functionkeys
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

  // Android key buttons
  KEY_BACK = 4;
  KEY_MENU = 82;
  KEY_VOLUME_UP = 24;
  KEY_VOLUME_DOWN = 25;

  // Mouse buttons
  MOUSE_LEFT_BUTTON = 0;
  MOUSE_RIGHT_BUTTON = 1;
  MOUSE_MIDDLE_BUTTON = 2;

  // Mouse cursor
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

  // Gamepad Number     look is removed ????
 { GAMEPAD_PLAYER1 = 0;
  GAMEPAD_PLAYER2 = 1;
  GAMEPAD_PLAYER3 = 2;
  GAMEPAD_PLAYER4 = 3;}

 // Gamepad buttons
 // This is here just for error checking
  GAMEPAD_BUTTON_UNKNOWN                    = 0;
 // This is normally a DPAD
  GAMEPAD_BUTTON_LEFT_FACE_UP               = 1;
  GAMEPAD_BUTTON_LEFT_FACE_RIGHT            = 2;
  GAMEPAD_BUTTON_LEFT_FACE_DOWN             = 3;
  GAMEPAD_BUTTON_LEFT_FACE_LEFT             = 4;
  // This normally corresponds with PlayStation and Xbox controllers
  // XBOX: [Y,X,A,B]
  // PS3: [Triangle,Square,Cross,Circle]
  // No support for 6 button controllers though..
  GAMEPAD_BUTTON_RIGHT_FACE_UP              = 5;
  GAMEPAD_BUTTON_RIGHT_FACE_RIGHT           = 6;
  GAMEPAD_BUTTON_RIGHT_FACE_DOWN            = 7;
  GAMEPAD_BUTTON_RIGHT_FACE_LEFT            = 8;
  // Triggers
  GAMEPAD_BUTTON_LEFT_TRIGGER_1             = 9;
  GAMEPAD_BUTTON_LEFT_TRIGGER_2             = 10;
  GAMEPAD_BUTTON_RIGHT_TRIGGER_1            = 11;
  GAMEPAD_BUTTON_RIGHT_TRIGGER_2            = 12;

  // These are buttons in the center of the gamepad
  GAMEPAD_BUTTON_MIDDLE_LEFT                = 13;     //PS3 Select
  GAMEPAD_BUTTON_MIDDLE                     = 14;          //PS Button/XBOX Button
  GAMEPAD_BUTTON_MIDDLE_RIGHT               = 15;    //PS3 Start

  // These are the joystick press in buttons
  GAMEPAD_BUTTON_LEFT_THUMB                 = 16;
  GAMEPAD_BUTTON_RIGHT_THUMB                = 17;

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

  // Mesh vertex attributes
  MESH_VERTEX_POSITION    = 1;
  MESH_VERTEX_TEXCOORD1   = 2;
  MESH_VERTEX_TEXCOORD2   = 4;
  MESH_VERTEX_NORMAL      = 8;
  MESH_VERTEX_TANGENT     = 16;
  MESH_VERTEX_COLOR       = 32;
  MESH_VERTEX_INDEX       = 64;

  // Material map index
  MATERIAL_MAP_ALBEDO     = 0;       // MATERIAL_MAP_DIFFUSE
  MATERIAL_MAP_METALNESS  = 1;       // MATERIAL_MAP_SPECULAR
  MATERIAL_MAP_NORMAL     = 2;
  MATERIAL_MAP_ROUGHNESS  = 4;
  MATERIAL_MAP_OCCLUSION  = 5;
  MATERIAL_MAP_EMISSION   = 6;
  MATERIAL_MAP_HEIGHT     = 7;
  MATERIAL_MAP_BRDG       = 8;
  MATERIAL_MAP_CUBEMAP    = 9;            // NOTE: Uses GL_TEXTURE_CUBE_MAP
  MATERIAL_MAP_IRRADIANCE = 10;          // NOTE: Uses GL_TEXTURE_CUBE_MAP
  MATERIAL_MAP_PREFILTER  = 11;          // NOTE: Uses GL_TEXTURE_CUBE_MAP

  MATERIAL_MAP_DIFFUSE  = MATERIAL_MAP_ALBEDO;
  MATERIAL_MAP_SPECULAR = MATERIAL_MAP_METALNESS;

  // Shader location points
  SHADER_LOC_VERTEX_POSITION        = 0;
  SHADER_LOC_VERTEX_TEXCOORD01      = 1;
  SHADER_LOC_VERTEX_TEXCOORD02      = 2;
  SHADER_LOC_VERTEX_NORMAL          = 3;
  SHADER_LOC_VERTEX_TANGENT         = 4;
  SHADER_LOC_VERTEX_COLOR           = 5;
  SHADER_LOC_MATRIX_MVP             = 6;
  SHADER_LOC_MATRIX_MODEL           = 7;
  SHADER_LOC_MATRIX_NORMAL          = 8;
  SHADER_LOC_MATRIX_VIEW            = 9;
  SHADER_LOC_MATRIX_PROJECTION      = 10;
  SHADER_LOC_VECTOR_VIEW            = 11;
  SHADER_LOC_COLOR_DIFFUSE          = 12;
  SHADER_LOC_COLOR_SPECULAR         = 13;
  SHADER_LOC_COLOR_AMBIENT          = 14;
  SHADER_LOC_MAP_ALBEDO             = 15;       // LOC_MAP_DIFFUSE
  SHADER_LOC_MAP_METALNESS          = 16;       // LOC_MAP_SPECULAR
  SHADER_LOC_MAP_NORMAL             = 17;
  SHADER_LOC_MAP_ROUGHNESS          = 18;
  SHADER_LOC_MAP_OCCLUSION          = 19;
  SHADER_LOC_MAP_EMISSION           = 20;
  SHADER_LOC_MAP_HEIGHT             = 21;
  SHADER_LOC_MAP_CUBEMAP            = 22;
  SHADER_LOC_MAP_IRRADIANCE         = 23;
  SHADER_LOC_MAP_PREFILTER          = 24;
  SHADER_LOC_MAP_BRDF               = 25;

  SHADER_LOC_MAP_DIFFUSE = SHADER_LOC_MAP_ALBEDO;
  SHADER_LOC_MAP_SPECULAR = SHADER_LOC_MAP_METALNESS;

  // Shader uniform data types
  SHADER_UNIFORM_FLOAT = 0;
  SHADER_UNIFORM_VEC2 = 1;
  SHADER_UNIFORM_VEC3 = 2;
  SHADER_UNIFORM_VEC4 = 3;
  SHADER_UNIFORM_INT = 4;
  SHADER_UNIFORM_IVEC2 = 5;
  SHADER_UNIFORM_IVEC3 = 6;
  SHADER_UNIFORM_IVEC4 = 7;
  SHADER_UNIFORM_SAMPLER2D = 8;

  // Pixel formats
  // NOTE: Support depends on OpenGL version and platform
  PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1;        // 8 bit per pixel (no alpha)
  PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2;       // 8*2 bpp (2 channels)
  PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3;           // 16 bpp
  PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4;           // 24 bpp
  PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5;         // 16 bpp (1 bit alpha)
  PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6;         // 16 bpp (4 bit alpha)
  PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7;         // 32 bpp
  PIXELFORMAT_UNCOMPRESSED_R32 = 8;              // 32 bpp (1 channel - float)
  PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9;        // 32*3 bpp (3 channels - float)
  PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10;    // 32*4 bpp (4 channels - float)
  PIXELFORMAT_COMPRESSED_DXT1_RGB = 11;          // 4 bpp (no alpha)
  PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12;         // 4 bpp (1 bit alpha)
  PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13;         // 8 bpp
  PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14;         // 8 bpp
  PIXELFORMAT_COMPRESSED_ETC1_RGB = 15;          // 4 bpp
  PIXELFORMAT_COMPRESSED_ETC2_RGB = 16;          // 4 bpp
  PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17;     // 8 bpp
  PIXELFORMAT_COMPRESSED_PVRT_RGB = 18;          // 4 bpp
  PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19;         // 8 bpp
  PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20;     // 8 bpp
  PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21;     // 2 bpp

  // Texture parameters: filter mode
  // NOTE 1: Filtering considers mipmaps if available in the texture
  // NOTE 2: Filter is accordingly set for minification and magnification
  TEXTURE_FILTER_POINT = 0;                  // No filter, just pixel aproximation
  TEXTURE_FILTER_BILINEAR = 1;               // Linear filtering
  TEXTURE_FILTER_TRILINEAR = 2;              // Trilinear filtering (linear with mipmaps)
  TEXTURE_FILTER_ANISOTROPIC_4X = 3;         // Anisotropic filtering 4x
  TEXTURE_FILTER_ANISOTROPIC_8X = 4;         // Anisotropic filtering 8x
  TEXTURE_FILTER_ANISOTROPIC_16X = 5;        // Anisotropic filtering 16x

  // Texture parameters: wrap mode
  TEXTURE_WRAP_REPEAT = 0;                   // Repeats texture in tiled mode
  TEXTURE_WRAP_CLAMP = 1;                    // Clamps texture to edge pixel in tiled mode
  TEXTURE_WRAP_MIRROR_REPEAT = 2;            // Mirrors and repeats the texture in tiled mode
  TEXTURE_WRAP_MIRROR_CLAMP = 3;             // Mirrors and clamps to border the texture in tiled mode

  // Cubemap layout type
  CUBEMAPP_LAYOUT_AUTO_DETECT = 0;            // Automatically detect layout type
  CUBEMAPP_LAYOUT_LINE_VERTICAL = 1;          // Layout is defined by a vertical line with faces
  CUBEMAPP_LAYOUT_LINE_HORIZONTAL = 2;        // Layout is defined by an horizontal line with faces
  CUBEMAPP_LAYOUT_CROSS_THREE_BY_FOUR = 3;    // Layout is defined by a 3x4 cross with cubemap faces
  CUBEMAPP_LAYOUT_CROSS_FOUR_BY_THREE = 4;    // Layout is defined by a 4x3 cross with cubemap faces
  CUBEMAPP_LAYOUT_PANORAMA = 5;               // Layout is defined by a panorama image (equirectangular map)

  // Font type, defines generation method
  FONT_DEFAULT = 0;             // Default font generation, anti-aliased
  FONT_BITMAP = 1;              // Bitmap font generation, no anti-aliasing
  FONT_SDF = 2;                 // SDF font generation, requires external shader

  // Color blending modes (pre-defined)
  BLEND_ALPHA = 0;              // Blend textures considering alpha (default)
  BLEND_ADDITIVE = 1;           // Blend textures adding colors
  BLEND_MULTIPLIED = 2;         // Blend textures multiplying colors
  BLEND_SUBTRACT_COLORS = 3;      // Blend textures subtracting colors (alternative)
  BLEND_CUSTOM = 4;               // Belnd textures using custom src/dst factors (use rlSetBlendMode())

  // Gestures
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

  // Camera projection
  CAMERA_PERSPECTIVE = 0;
  CAMERA_ORTHOGRAPHIC = 1;

  // N-patch layout
  NPATCH_NINE_PATCH = 0;              // Npatch layout: 3x3 tiles
  NPATCH_THREE_PATCH_VERTICAL = 1;    // Npatch layout: 1x3 tiles
  NPATCH_THREE_PATCH_HORIZONTAL =2;   // Npatch layout: 3x1 tiles

type
   // Callbacks to hook some internal functions
  // WARNING: This callbacks are intended for advance users
  TTraceLogCallback     = procedure(aLogLevel: integer; aText, aArgs: PAnsiChar); cdecl; // Logging: Redirect trace log messages
  TLoadFileDataCallback = function(aFileName: PAnsiChar; aBytesRead: Cardinal): Byte; cdecl;   // FileIO: Load binary data
  TSaveFileDataCallback = procedure(aFilename: PAnsiChar; aData: Pointer; aBytesToWrite: Byte); cdecl; // FileIO: Save binary data
  TLoadFileTextCallback = function(aFilename: PAnsiChar): PAnsiChar; cdecl; // FileIO: Load text data
  TSaveFileTextCallback = procedure(aFilename, aText: PAnsiChar); cdecl;

//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

// Window-related functions
procedure InitWindow(aWidth: integer; aHeight: integer; aTitle: PAnsiChar);cdecl;external cDllName; // Initialize window and OpenGL context
function WindowShouldClose(): boolean;cdecl;external cDllName; // Check if KEY_ESCAPE pressed or Close icon pressed
procedure CloseWindow();cdecl;external cDllName; // Close window and unload OpenGL context
function IsWindowReady(): boolean;cdecl;external cDllName; // Check if window has been initialized successfully                                              // Check if window has been initialized successfully
function IsWindowFullscreen(): boolean;cdecl;external cDllName; // Check if window is currently fullscreen
function IsWindowHidden(): boolean;cdecl;external cDllName; // Check if window is currently hidden (only PLATFORM_DESKTOP)
function IsWindowMinimized(): boolean;cdecl;external cDllName; // Check if window is currently minimized (only PLATFORM_DESKTOP)
function IsWindowMaximized(): boolean;cdecl;external cDllName; // Check if window is currently maximized (only PLATFORM_DESKTOP)
function IsWindowFocused(): boolean;cdecl;external cDllName; // Check if window is currently focused (only PLATFORM_DESKTOP)
function IsWindowResized(): boolean;cdecl;external cDllName; // Check if window has been resized last frame
function IsWindowState(aFlag: cardinal): boolean;cdecl;external cDllName; // Check if one specific window flag is enabled
procedure SetWindowState(aFlags: cardinal);cdecl;external cDllName; // Set window configuration state using flags
procedure ClearWindowState(aFlags: cardinal);cdecl;external cDllName; // Clear window configuration state flags
procedure ToggleFullscreen();cdecl;external cDllName; // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
procedure MaximizeWindow();cdecl;external cDllName; // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
procedure MinimizeWindow();cdecl;external cDllName; // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
procedure RestoreWindow();cdecl;external cDllName; // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
procedure SetWindowIcon(aImage: TImage);cdecl;external cDllName; // Set icon for window (only PLATFORM_DESKTOP)
procedure SetWindowTitle(aTitle: PAnsiChar);cdecl;external cDllName; // Set title for window (only PLATFORM_DESKTOP)
procedure SetWindowPosition(aX: integer; aY: integer);cdecl;external cDllName; // Set window position on screen (only PLATFORM_DESKTOP)
procedure SetWindowMonitor(aMonitor: integer);cdecl;external cDllName; // Set monitor for the current window (fullscreen mode)
procedure SetWindowMinSize(aWidth: integer; aHeight: integer);cdecl;external cDllName; // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
procedure SetWindowSize(aWidth: integer; aHeight: integer);cdecl;external cDllName; // Set window dimensions
function GetWindowHandle(): Pointer;cdecl;external cDllName; // Get native window handle
function GetScreenWidth(): integer;cdecl;external cDllName;  // Get current screen width
function GetScreenHeight(): integer;cdecl;external cDllName; // Get current screen height
function GetMonitorCount(): integer;cdecl;external cDllName; // Get number of connected monitors
function GetCurrentMonitor():integer;cdecl;external cDllName;// Get current connected monitor
function GetMonitorPosition(aMonitor: integer): TVector2;cdecl;external cDllName; // Get specified monitor position
function GetMonitorWidth(aMonitor: integer): integer;cdecl;external cDllName; // Get specified monitor width (max available by monitor)
function GetMonitorHeight(aMonitor: integer): integer;cdecl;external cDllName; // Get specified monitor height (max available by monitor)
function GetMonitorPhysicalWidth(aMonitor: integer): integer;cdecl;external cDllName; // Get specified monitor physical width in millimetres
function GetMonitorPhysicalHeight(aMonitor: integer): integer;cdecl;external cDllName; // Get specified monitor physical height in millimetres
function GetMonitorRefreshRate(aMonitor: integer): integer;cdecl;external cDllName; // Get specified monitor refresh rate
function GetWindowPosition(): TVector2;cdecl;external cDllName; // Get window position XY on monitor
function GetWindowScaleDPI(): TVector2;cdecl;external cDllName; // Get window scale DPI factor
function GetMonitorName(aMonitor: integer): PAnsiChar;cdecl;external cDllName; // Get the human-readable, UTF-8 encoded name of the primary monitor
procedure SetClipboardText(aText: PAnsiChar);cdecl;external cDllName; // Set clipboard text content
function GetClipboardText(): PAnsiChar;cdecl;external cDllName; // Get clipboard text content

// Cursor-related functions
procedure ShowCursor();cdecl;external cDllName; // Shows cursor
procedure HideCursor();cdecl;external cDllName; // Hides cursor
function IsCursorHidden(): boolean;cdecl;external cDllName; // Check if cursor is not visible
procedure EnableCursor();cdecl;external cDllName; // Enables cursor (unlock cursor)
procedure DisableCursor();cdecl;external cDllName; // Disables cursor (lock cursor)
function IsCursorOnScreen(): boolean;cdecl;external cDllName; // Check if cursor is on the current screen.

// Drawing-related functions
procedure ClearBackground(aColor: TColor);cdecl;external cDllName; // Set background color (framebuffer clear color)
procedure BeginDrawing();cdecl;external cDllName; // Setup canvas (framebuffer) to start drawing
procedure EndDrawing();cdecl;external cDllName; // End canvas drawing and swap buffers (double buffering)
procedure BeginMode2D(aCamera: TCamera2D);cdecl;external cDllName; // Initialize 2D mode with custom camera (2D)
procedure EndMode2D();cdecl;external cDllName; // Ends 2D mode with custom camera
procedure BeginMode3D(aCamera: TCamera3D);cdecl;external cDllName; // Initializes 3D mode with custom camera (3D)
procedure EndMode3D();cdecl;external cDllName; // Ends 3D mode and returns to default 2D orthographic mode
procedure BeginTextureMode(aTarget: TRenderTexture2D);cdecl;external cDllName; // Initializes render texture for drawing
procedure EndTextureMode();cdecl;external cDllName; // Ends drawing to render texture
procedure BeginScissorMode(aX, aY, aWidth, aHeight: integer);cdecl;external cDllName; // Begin scissor mode (define screen area for following drawing)
procedure EndScissorMode();cdecl;external cDllName; // End scissor mode

// Screen-space-related functions
function GetMouseRay(aMousePosition: TVector2; aCamera: TCamera): TRay;cdecl;external cDllName; // Returns a ray trace from mouse position
function GetCameraMatrix(aCamera: TCamera): TMatrix;cdecl;external cDllName; // Returns camera transform matrix (view matrix)
function GetCameraMatrix2D(aCamera: TCamera2D): TMatrix;cdecl;external cDllName; // Returns camera 2d transform matrix
function GetWorldToScreen(aPosition: TVector3; aCamera: TCamera): TVector2;cdecl;external cDllName; // Returns the screen space position for a 3d world space position
function GetWorldToScreenEx(aPosition: TVector3; aCamera: TCamera; aWidth, aHeight: integer): TVector2;cdecl;external cDllName; // Returns size position for a 3d world space position
function GetWorldToScreen2D(aPosition: TVector2; aCamera: TCamera2D): TVector2;cdecl;external cDllName; // Returns the screen space position for a 2d camera world space position
function GetScreenToWorld2D(aPosition: TVector2; aCamera: TCamera2D): TVector2;cdecl;external cDllName; // Returns the world space position for a 2d camera screen space position

// Timming-related functions
procedure SetTargetFPS(aFPS: integer);cdecl;external cDllName; // Set target FPS (maximum)
function GetFPS(): integer;cdecl;external cDllName; // Returns current FPS
function GetFrameTime(): single;cdecl;external cDllName; // Returns time in seconds for last frame drawn (delta time)
function GetTime(): double;cdecl;external cDllName; // Returns elapsed time in seconds since InitWindow()

// Misc. functions
function GetRandomValue(aMin, aMax: integer): integer;cdecl;external cDllName; // Returns a random value between min and max (both included)
procedure TakeScreenshot(aFileName: PAnsiChar);cdecl;external cDllName; // Takes a screenshot of current screen (filename extension defines format)
procedure SetConfigFlags(aFlags: cardinal);cdecl;external cDllName; // Setup init configuration flags (view FLAGS)

procedure TraceLog(aLogLevel: integer; aText: PAnsiChar);cdecl; external cDllName; // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
procedure SetTraceLogLevel(aLogLevel: integer);cdecl;external cDllName; // Set the current threshold (minimum) log level
function MemAlloc(aSize:integer): Pointer;cdecl;external cDllName;   // Internal memory allocator
procedure MemRealloc(aPtr:Pointer; aSize: integer);cdecl;external cDllName; // Internal memory reallocator
procedure MemFree(aPtr:Pointer);cdecl;external cDllName; // Internal memory free

// Set custom callbacks
// WARNING: Callbacks setup is intended for advance users
procedure SetTraceLogCallback(aCallback:TTraceLogCallback);cdecl;external cDllName;       // Set custom trace log
procedure SetLoadFileDataCallback(aCallback: TLoadFileDataCallback);cdecl;external cDllName; // Set custom file binary data loader
procedure SetSaveFileDataCallback(aCallback: TSaveFileDataCallback);cdecl;external cDllName;  // Set custom file binary data saver
procedure SetLoadFileTextCallback(aCallback: TLoadFileTextCallback);cdecl;external cDllName;  // Set custom file text data loader
procedure SetSaveFileTextCallback(aCallback: TSaveFileTextCallback);cdecl;external cDllName;  // Set custom file text data saver

// Files management functions
function LoadFileData(aFileName: PAnsiChar; bytesRead: PCardinal): PAnsiChar;cdecl;external; // Load file data as byte array (read)
procedure UnloadFileData(aData: PAnsiChar);cdecl;external; // Unload file data allocated by LoadFileData()
function SaveFileData(aFileName: PAnsiChar; aData: Pointer; bytesToWrite: cardinal): Boolean;cdecl;external; // Save data to file from byte array (write), returns true on success
function LoadFileText(aFileName: PAnsiChar): PAnsiChar;cdecl;external cDllName; // Load text data from file (read), returns a '\0' terminated string
function SaveFileText(aFileName: PAnsiChar; aText: PAnsiChar): boolean;cdecl;external cDllName; // Save text data to file (write), string must be '\0' terminated
function FileExists(aFilename: PAnsiChar): boolean;cdecl;external cDllName; // Check if file exists
function IsFileExtension(aFilename: PAnsiChar; aExt: PAnsiChar): boolean;cdecl;external cDllName; // Check file extension
function DirectoryExists(aDirPath: PAnsiChar): boolean;cdecl;external cDllName; // Check if a directory path exists
function GetFileExtension(aFilename: PAnsiChar): PAnsiChar;cdecl;external cDllName; // Get pointer to extension for a filename string (includes dot: ".png")
function GetFileName(aFilepath: PAnsiChar): PAnsiChar;cdecl;external cDllName;  // Get pointer to filename for a path string
function GetFileNameWithoutExt(aFilepath: PAnsiChar): PAnsiChar;cdecl;external cDllName; // Get filename string without extension (uses static string)
function GetDirectoryPath(aFilename: PAnsiChar): PAnsiChar;cdecl;external cDllName; // Get full path for a given fileName with path (uses static string)
function GetPrevDirectoryPath(aDirPath: PAnsiChar): PAnsiChar;cdecl;external cDllName; // Get previous directory path for a given path (uses static string)
function GetWorkingDirectory(): PAnsiChar;cdecl;external cDllName; // Get current working directory (uses static string)
function GetDirectoryFiles(aDirpath: PAnsiChar; aCount: PInteger): PPAnsiChar;cdecl;external cDllName; // Get filenames in a directory path (memory should be freed)
procedure ClearDirectoryFiles();cdecl;external cDllName; // Clear directory files paths buffers (free memory)
function ChangeDirectory(aDir: PAnsiChar): boolean;cdecl;external cDllName; // Change working directory, return true on success
function IsFileDropped(): boolean;cdecl;external cDllName;  // Check if a file has been dropped into window
function GetDroppedFiles(aCount: PInteger): PPAnsiChar;cdecl;external cDllName; // Get dropped files names (memory should be freed)
procedure ClearDroppedFiles;cdecl;external cDllName; // Clear dropped files paths buffer (free memory)
function GetFileModTime(aFilename: PAnsiChar): longint;cdecl;external cDllName; // Get file modification time (last write time)

function CompressData(aData: PByte; aDataLength: integer; aCompDataLength: PInteger): PByte;cdecl;external cDllName; // Compress data (DEFLATE algorythm)
function DecompressData(aCompData: PByte; aCompDataLength: integer; aDataLength: PInteger): PByte;cdecl;external cDllName; // Decompress data (DEFLATE algorythm)

// Persistent storage management
function SaveStorageValue(aPosition: cardinal ; aValue: integer): boolean;cdecl;external cDllName; // Save integer value to storage file (to defined position)
function LoadStorageValue(aPosition: cardinal): integer;cdecl;external cDllName;

procedure OpenURL(aUrl: PAnsiChar);cdecl;external cDllName; // Open URL with default system browser (if available)

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
function IsKeyPressed(aKey: integer): boolean;cdecl;external cDllName;  // Detect if a key has been pressed once
function IsKeyDown(aKey: integer): boolean;cdecl;external cDllName; // Detect if a key is being pressed
function IsKeyReleased(aKey: integer): boolean;cdecl;external cDllName; // Detect if a key has been released once
function IsKeyUp(aKey: integer): boolean;cdecl;external cDllName; // Detect if a key is NOT being pressed
procedure SetExitKey(aKey: integer);cdecl;external cDllName; // Set a custom key to exit program (default is ESC)
function GetKeyPressed(): integer;cdecl;external cDllName; // Get key pressed (keycode), call it multiple times for keys queued
function GetCharPressed(): integer;cdecl;external cDllName; // Get char pressed (unicode), call it multiple times for chars queued

// Input-related functions: gamepads
function IsGamepadAvailable(aGamepad: integer): boolean;cdecl;external cDllName; // Detect if a gamepad is available
function IsGamepadName(aGamepad: integer; aName: PAnsiChar): boolean;cdecl;external cDllName; // Check gamepad name (if available)
function GetGamepadName(aGamepad: integer): PAnsiChar;cdecl;external cDllName; // Return gamepad internal name id
function IsGamepadButtonPressed(aGamepad: integer; aButton: integer): boolean;cdecl;external cDllName; // Detect if a gamepad button has been pressed once
function IsGamepadButtonDown(aGamepad: integer; aButton: integer): boolean;cdecl;external cDllName; // Detect if a gamepad button is being pressed
function IsGamepadButtonReleased(aGamepad: integer; aButton: integer): boolean;cdecl;external cDllName; // Detect if a gamepad button has been released once
function IsGamepadButtonUp(aGamepad: integer; aButton: integer): boolean;cdecl;external cDllName; // Detect if a gamepad button is NOT being pressed
function GetGamepadButtonPressed(): integer;cdecl;external cDllName; // Get the last gamepad button pressed
function GetGamepadAxisCount(aGamepad: integer): integer;cdecl;external cDllName; // Return gamepad axis count for a gamepad
function GetGamepadAxisMovement(aGamepad: integer; aAxis: integer): single;cdecl;external cDllName; // Return axis movement value for a gamepad axis
function SetGamepadMappings(aMappings: PAnsiChar): integer;cdecl;external cDllName; // Set internal gamepad mappings (SDL_GameControllerDB)

// Input-related functions: mouse
function IsMouseButtonPressed(aButton: integer): boolean;cdecl;external cDllName; // Detect if a mouse button has been pressed once
function IsMouseButtonDown(aButton: integer): boolean;cdecl;external cDllName; // Detect if a mouse button is being pressed
function IsMouseButtonReleased(aButton: integer): boolean;cdecl;external cDllName; // Detect if a mouse button has been released once
function IsMouseButtonUp(aButton: integer): boolean;cdecl;external cDllName; // Detect if a mouse button is NOT being pressed
function GetMouseX(): integer;cdecl;external cDllName; // Returns mouse position X
function GetMouseY(): integer;cdecl;external cDllName; // Returns mouse position Y
function GetMousePosition(): TVector2;cdecl;external cDllName; // Returns mouse position XY
procedure SetMousePosition(aPosition: TVector2);cdecl;external cDllName; // Set mouse position XY
procedure SetMouseOffset(aOffsetX, aOffsetY: integer);cdecl;external cDllName; // Set mouse offset
procedure SetMouseScale(aScaleX, aScaleY: single);cdecl;external cDllName; // Set mouse scaling
function GetMouseWheelMove(): single;cdecl;external cDllName; // Returns mouse wheel movement Y
procedure SetMouseCursor(aCurrsor: integer);cdecl;external cDllName; // Set mouse cursor

// Input-related functions: touch
function GetTouchX(): integer;cdecl;external cDllName; // Returns touch position X for touch point 0 (relative to screen size)
function GetTouchY(): integer;cdecl;external cDllName; // Returns touch position Y for touch point 0 (relative to screen size)
function GetTouchPosition(aIndex: integer): TVector2;cdecl;external cDllName; // Returns touch position XY for a touch point index (relative to screen size)

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------

procedure SetGesturesEnabled(aFlags: cardinal);cdecl;external cDllName; // Enable a set of gestures using flags
function IsGestureDetected(aGesture: integer): boolean;cdecl;external cDllName; // Check if a gesture have been detected
function GetGestureDetected(): integer;cdecl;external cDllName; // Get latest detected gesture
function GetTouchPointsCount(): integer;cdecl;external cDllName; // Get touch points count
function GetGestureHoldDuration(): single;cdecl;external cDllName; // Get gesture hold time in milliseconds
function GetGestureDragVector(): TVector2;cdecl;external cDllName; // Get gesture drag vector
function GetGestureDragAngle(): single;cdecl;external cDllName; // Get gesture drag angle
function GetGesturePinchVector(): TVector2;cdecl;external cDllName; // Get gesture pinch delta
function GetGesturePinchAngle(): single;cdecl;external cDllName; // Get gesture pinch angle

//------------------------------------------------------------------------------------
// TCamera System Functions (Module: TCamera)
//------------------------------------------------------------------------------------

procedure SetCameraMode(aCamera: TCamera; aMode: integer);cdecl;external cDllName; // Set camera mode (multiple camera modes available)
procedure UpdateCamera(aCamera: PCamera);cdecl;external cDllName; // Update camera position for selected mode

procedure SetCameraPanControl(aKeyPan: integer);cdecl;external cDllName; // Set camera pan key to combine with mouse movement (free camera)
procedure SetCameraAltControl(aKeyPan: integer);cdecl;external cDllName; // Set camera alt key to combine with mouse movement (free camera)
procedure SetCameraSmoothZoomControl(aKeySmoothZoom: integer);cdecl;external cDllName; // Set camera smooth zoom key to combine with mouse (free camera)
procedure SetCameraMoveControls(aKeyFront: integer; aKeyBack: integer; aKeyRight: integer; aKeyLeft: integer; aKeyUp: integer; aKeyDown: integer);cdecl;external cDllName; // Set camera move controls (1st person and 3rd person cameras)

//------------------------------------------------------------------------------------
// VR Simulator Functions (Module: core)
//------------------------------------------------------------------------------------

procedure InitVrSimulator(aDevice: TVrDeviceInfo);cdecl;external cDllName; // Init VR simulator for selected device parameters
procedure CloseVrSimulator();cdecl;external cDllName; // Close VR simulator for current device
function IsVrSimulatorReady(): boolean;cdecl;external cDllName; // Detect if VR simulator is ready
procedure UpdateVrTracking(aCamera: PCamera);cdecl;external cDllName; // Update VR tracking (position and orientation) and camera
procedure BeginVrDrawing(aTarget: TRenderTexture2D);cdecl;external cDllName; // Begin VR simulator stereo rendering (using provided fbo)
procedure EndVrDrawing();cdecl;external cDllName; // End VR simulator stereo rendering
function GetVrConfig(aDevice: TVrDeviceInfo): TVrStereoConfig;cdecl;external cDllName; // Get stereo rendering configuration parameters

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

// Basic shapes drawing functions
procedure DrawPixel(aPosX: integer; aPosY: integer; aColor: TColor);cdecl;external cDllName; // Draw a pixel
procedure DrawPixelV(aPosition: TVector2; TColor: TColor);cdecl;external cDllName; // Draw a pixel (Vector version)
procedure DrawLine(aStartPosX: integer; aStartPosY: integer; aEndPosX: integer; aEndPosY: integer; aColor: TColor);cdecl;external cDllName; // Draw a line
procedure DrawLineV(aStartPos: TVector2; aEndPos: TVector2; aColor: TColor);cdecl;external cDllName; // Draw a line (Vector version)
procedure DrawLineEx(aStartPos: TVector2; aEndPos: TVector2; aThick: single; aColor: TColor);cdecl;external cDllName; // Draw a line defining thickness
procedure DrawLineBezier(aStartPos: TVector2; aEndPos: TVector2; aThick: single; aColor: TColor);cdecl;external cDllName; // Draw a line using cubic-bezier curves in-out
procedure DrawLineBezierQuad(aStartPos, aEndPos, aControlPos: TVector2; aThink: single; aColor: TColor);cdecl;external cDllName; //Draw line using quadratic bezier curves with a control point
procedure DrawLineStrip(aPoints: PVector2; aPointsCount: integer; aColor: TColor);cdecl;external cDllName; // Draw lines sequence
procedure DrawCircle(aCenterX: integer; aCenterY: integer; aRadius: single; aColor: TColor);cdecl;external cDllName; // Draw a color-filled circle
procedure DrawCircleSector(aCenter: TVector2; aRadius: single; aStartAngle, aEndAngle: single; aSegments: integer; aColor: TColor);cdecl;external cDllName; // Draw a piece of a circle
procedure DrawCircleSectorLines(aCenter: TVector2; aRadius: single; aStartAngle, aEndAngle: single; aSegments: integer; aColor: TColor);cdecl;external cDllName; // Draw circle sector outline
procedure DrawCircleGradient(aCenterX: integer; aCenterY: integer; aRadius: single; aColor1: TColor; aColor2: TColor);cdecl;external cDllName; // Draw a gradient-filled circle
procedure DrawCircleV(aCenter: TVector2; aRadius: single; TColor: TColor);cdecl;external cDllName; // Draw a color-filled circle (Vector version)
procedure DrawCircleLines(aCenterX: integer; aCenterY: integer; aRadius: single; aColor: TColor);cdecl;external cDllName; // Draw circle outline

procedure DrawEllipse(aCenterX: integer; aCenterY: integer; aRadiusH: single; aRadiusV: single; aColor: TColor);cdecl;external cDllName; // Draw ellipse
procedure DrawEllipseLines(aCenterX: integer; aCenterY: integer; aRadiusH: single; aRadiusV: single; aColor: TColor);cdecl;external cDllName; // Draw ellipse outline

procedure DrawRing(aCenter: TVector2; aInnerRadius, aOuterRadius: single; aStartAngle, aEndAngle: single; aSegments: integer; aColor: TColor);cdecl;external cDllName; // Draw ring
procedure DrawRingLines(aCenter: TVector2; aInnerRadius, aOuterRadius: single; aStartAngle, aEndAngle: single; aSegments: integer; aColor: TColor);cdecl;external cDllName; // Draw ring outline

procedure DrawRectangle(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor: TColor);cdecl;external cDllName; // Draw a color-filled rectangle
procedure DrawRectangleV(aPosition: TVector2; size: TVector2; TColor: TColor);cdecl;external cDllName; // Draw a color-filled rectangle (Vector version)
procedure DrawRectangleRec(aRect: TRectangle; aColor: TColor);cdecl;external cDllName; // Draw a color-filled rectangle
procedure DrawRectanglePro(aRect: TRectangle; origin: TVector2; aRotation: single; aColor: TColor);cdecl;external cDllName; // Draw a color-filled rectangle with pro parameters
procedure DrawRectangleGradientV(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor1: TColor; aColor2: TColor);cdecl;external cDllName; // Draw a vertical-gradient-filled rectangle
procedure DrawRectangleGradientH(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; aColor1: TColor; aColor2: TColor);cdecl;external cDllName; // Draw a horizontal-gradient-filled rectangle
procedure DrawRectangleGradientEx(aRect: TRectangle; aCol1: TColor; aCol2: TColor; aCol3: TColor; aCol4: TColor);cdecl;external cDllName; // Draw a gradient-filled rectangle with custom vertex colors
procedure DrawRectangleLines(aPosX: integer; aPosY: integer; aWidth: integer; aHeight: integer; TColor: TColor);cdecl;external cDllName; // Draw rectangle outline
procedure DrawRectangleLinesEx(aRect: TRectangle; aLineThick: integer; aColor: TColor);cdecl;external cDllName; // Draw rectangle outline with extended parameters

procedure DrawRectangleRounded(aRec: TRectangle; aRoundness: single; aSegments: integer; aColor: TColor);cdecl;external cDllName;  // Draw rectangle with rounded edges
procedure DrawRectangleRoundedLines(aRec: TRectangle; aRoundness: single; aSegments: integer; aLineThick: integer; aColor: TColor);cdecl;external cDllName; // Draw rectangle with rounded edges outline

procedure DrawTriangle(aVec1: TVector2; aVec2: TVector2; aVec3: TVector2; aColor: TColor);cdecl;external cDllName; // Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleLines(aVec1: TVector2; aVec2: TVector2; aVec3: TVector2; aColor: TColor);cdecl;external cDllName; // Draw triangle outline (vertex in counter-clockwise order!)
procedure DrawTriangleFan(aPoints: PVector2; aPointsCount: integer; aColor: TColor);cdecl;external cDllName; // Draw a triangle fan defined by points (first vertex is the center)
procedure DrawTriangleStrip(aPoints: PVector2; aPointsCount: integer; aColor: TColor);cdecl;external cDllName; // Draw a triangle strip defined by points
procedure DrawPoly(aCenter: TVector2; aSides: integer; aRadius: single; aRotation: single; aColor: TColor);cdecl;external cDllName; // Draw a regular polygon (Vector version)
procedure DrawPolyLines(aCenter: TVector2; aSides: integer; aRadius: single; aRotation: single; aColor: TColor);cdecl;external cDllName; // Draw a polygon outline of n sides

// Basic shapes collision detection functions
function CheckCollisionRecs(aRect1: TRectangle; aRect2: TRectangle): boolean;cdecl;external cDllName; // Check collision between two rectangles
function CheckCollisionCircles(aCenter1: TVector2; aRadius1: single; aCenter2: TVector2; aRadius2: single): boolean;cdecl;external cDllName; // Check collision between two circles
function CheckCollisionCircleRec(aCenter: TVector2; aRadius: single; aRect: TRectangle): boolean;cdecl;external cDllName; // Check collision between circle and rectangle
function CheckCollisionPointRec(aPoint: TVector2; aRect: TRectangle): boolean;cdecl;external cDllName; // Check if point is inside rectangle
function CheckCollisionPointCircle(aPoint: TVector2; aCenter: TVector2; aRadius: single): boolean;cdecl;external cDllName; // Check if point is inside circle
function CheckCollisionPointTriangle(aPoint: TVector2; aP1: TVector2; aP2: TVector2; aP3: TVector2): boolean;cdecl;external cDllName; // Check if point is inside a triangle
function CheckCollisionLines( aStartPos1: TVector2; aEndPos1: TVector2; aStartPos2: TVector2; aEndPos2: TVector2; aCollisionPoint: PVector2): boolean;cdecl;external cDllName; // Check the collision between two lines defined by two points each, returns collision point by reference
function GetCollisionRec(aRect1: TRectangle; aRect2: TRectangle): TRectangle;cdecl;external cDllName; // Get collision rectangle for two rectangles collision

//------------------------------------------------------------------------------------
// Texture Loading and Drawing Functions (Module: textures)
//------------------------------------------------------------------------------------

// Image loading functions
// NOTE: This functions do not require GPU access
function LoadImage(fileName:Pchar):TImage;cdecl;external cDllName; // Load image from file into CPU memory (RAM)
function LoadImageRaw(fileName:Pchar; width:longint; height:longint; format:longint; headerSize:longint):TImage;cdecl;external cDllName; // Load image from RAW file data
function LoadImageAnim(fileName:Pchar; var frames:longint):TImage;cdecl;external cDllName; // Load image sequence from file (frames appended to image.data)
function LoadImageFromMemory(fileType:Pchar; var fileData:byte; dataSize:longint):TImage;cdecl;external cDllName; // Load image from memory buffer, fileType refers to extension: i.e. "png"
procedure UnloadImage(image:TImage);cdecl;external cDllName; // Unload image from CPU memory (RAM)
function ExportImage(image:TImage; fileName:Pchar):boolean;cdecl;external cDllName; // Export image data to file
function ExportImageAsCode(image:TImage; fileName:Pchar):boolean;cdecl;external cDllName; // Export image as code file defining an array of bytes

// TImage generation functions
function GenImageColor(width:longint; height:longint; color:TColor):TImage;cdecl;external cDllName; // Generate image: plain color
function GenImageGradientV(width:longint; height:longint; top:TColor; bottom:TColor):TImage;cdecl;external cDllName; // Generate image: vertical gradient
function GenImageGradientH(width:longint; height:longint; left:TColor; right:TColor):TImage;cdecl;external cDllName; // Generate image: horizontal gradient
function GenImageGradientRadial(width:longint; height:longint; density:single; inner:TColor; outer:TColor):TImage;cdecl;external cDllName; // Generate image: radial gradient
function GenImageChecked(width:longint; height:longint; checksX:longint; checksY:longint; col1:TColor; col2:TColor):TImage;cdecl;external cDllName; // Generate image: checked
function GenImageWhiteNoise(width:longint; height:longint; factor:single):TImage;cdecl;external cDllName; // Generate image: white noise
function GenImagePerlinNoise(width:longint; height:longint; offsetX:longint; offsetY:longint; scale:single):TImage;cdecl;external cDllName; // Generate image: perlin noise
function GenImageCellular(width:longint; height:longint; tileSize:longint):TImage;cdecl;external cDllName; // Generate image: cellular algorithm. Bigger tileSize means bigger cells

// TImage manipulation functions
function ImageCopy(image:TImage):TImage;cdecl;external cDllName; // Create an image duplicate (useful for transformations)
function ImageFromImage(image:TImage; rec:TRectangle):TImage;cdecl;external cDllName; // Create an image from another image piece
function ImageText(text:Pchar; fontSize:longint; color:TColor):TImage;cdecl;external cDllName; // Create an image from text (default font)
function ImageTextEx(font:TFont; text:Pchar; fontSize:single; spacing:single; tint:TColor):TImage;cdecl;external cDllName; // Create an image from text (custom sprite font)
procedure ImageFormat(var image:TImage; newFormat:longint);cdecl;external cDllName; // Convert image data to desired format
procedure ImageToPOT(var image:TImage; fill:TColor);cdecl;external cDllName; // Convert image to POT (power-of-two)
procedure ImageCrop(var image:TImage; crop:TRectangle);cdecl;external cDllName; // Crop an image to a defined rectangle
procedure ImageAlphaCrop(var image:TImage; threshold:single);cdecl;external cDllName; // Crop image depending on alpha value
procedure ImageAlphaClear(var image:TImage; color:TColor; threshold:single);cdecl;external cDllName; // Clear alpha channel to desired color
procedure ImageAlphaMask(var image:TImage; alphaMask:TImage);cdecl;external cDllName; // Apply alpha mask to image
procedure ImageAlphaPremultiply(var image:TImage);cdecl;external cDllName; // Premultiply alpha channel
procedure ImageResize(var image:TImage; newWidth:longint; newHeight:longint);cdecl;external cDllName; // Resize image (Bicubic scaling algorithm)
procedure ImageResizeNN(var image:TImage; newWidth:longint; newHeight:longint);cdecl;external cDllName; // Resize image (Nearest-Neighbor scaling algorithm)
procedure ImageResizeCanvas(var image:TImage; newWidth:longint; newHeight:longint; offsetX:longint; offsetY:longint; fill:TColor);cdecl;external cDllName; // Resize canvas and fill with color
procedure ImageMipmaps(var image:TImage);cdecl;external cDllName; // Generate all mipmap levels for a provided image
procedure ImageDither(var image:TImage; rBpp:longint; gBpp:longint; bBpp:longint; aBpp:longint);cdecl;external cDllName; // Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
procedure ImageFlipVertical(var image:TImage);cdecl;external cDllName; // Flip image vertically
procedure ImageFlipHorizontal(var image:TImage);cdecl;external cDllName; // Flip image horizontally
procedure ImageRotateCW(var image:TImage);cdecl;external cDllName; // Rotate image clockwise 90deg
procedure ImageRotateCCW(var image:TImage);cdecl;external cDllName; // Rotate image counter-clockwise 90deg
procedure ImageColorTint(var image:TImage; color:TColor);cdecl;external cDllName; // Modify image color: tint
procedure ImageColorInvert(var image:TImage);cdecl;external cDllName; // Modify image color: invert
procedure ImageColorGrayscale(var image:TImage);cdecl;external cDllName; // Modify image color: grayscale
procedure ImageColorContrast(var image:TImage; contrast:single);cdecl;external cDllName; // Modify image color: contrast (-100 to 100)
procedure ImageColorBrightness(var image:TImage; brightness:longint);cdecl;external cDllName; // Modify image color: brightness (-255 to 255)
procedure ImageColorReplace(var image:TImage; color:TColor; replace:TColor);cdecl;external cDllName; // Modify image color: replace color
function LoadImageColors(image:TImage):PColor;cdecl;external cDllName; // Load color data from image as a Color array (RGBA - 32bit)
function LoadImagePalette(image:TImage; maxPaletteSize:longint; var colorsCount:longint):PColor;cdecl;external cDllName; // Load colors palette from image as a Color array (RGBA - 32bit)
procedure UnloadImageColors(var colors:TColor);cdecl;external cDllName; // Unload color data loaded with LoadImageColors()
procedure UnloadImagePalette(var colors:TColor);cdecl;external cDllName; // Unload colors palette loaded with LoadImagePalette()
function GetImageAlphaBorder(image:TImage; threshold:single):TRectangle;cdecl;external cDllName; // Get image alpha border rectangle

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
procedure ImageClearBackground(var dst:TImage; color:TColor);cdecl;external cDllName; // Clear image background with given color
procedure ImageDrawPixel(var dst:TImage; posX:longint; posY:longint; color:TColor);cdecl;external cDllName; // Draw pixel within an image
procedure ImageDrawPixelV(var dst:TImage; position:TVector2; color:TColor);cdecl;external cDllName; // Draw pixel within an image (Vector version)
procedure ImageDrawLine(var dst:TImage; startPosX:longint; startPosY:longint; endPosX:longint; endPosY:longint; color:TColor);cdecl;external cDllName; // Draw line within an image
procedure ImageDrawLineV(var dst:TImage; start:TVector2; _end:TVector2; color:TColor);cdecl;external cDllName; // Draw line within an image (Vector version)
procedure ImageDrawCircle(var dst:TImage; centerX:longint; centerY:longint; radius:longint; color:TColor);cdecl;external cDllName; // Draw circle within an image
procedure ImageDrawCircleV(var dst:TImage; center:TVector2; radius:longint; color:TColor);cdecl;external cDllName; // Draw circle within an image (Vector version)
procedure ImageDrawRectangle(var dst:TImage; posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName; // Draw rectangle within an image
procedure ImageDrawRectangleV(var dst:TImage; position:TVector2; size:TVector2; color:TColor);cdecl;external cDllName; // Draw rectangle within an image (Vector version)
procedure ImageDrawRectangleRec(var dst:TImage; rec:TRectangle; color:TColor);cdecl;external cDllName; // Draw rectangle within an image
procedure ImageDrawRectangleLines(var dst:TImage; rec:TRectangle; thick:longint; color:TColor);cdecl;external cDllName; // Draw rectangle lines within an image
procedure ImageDraw(var dst:TImage; src:TImage; srcRec:TRectangle; dstRec:TRectangle; tint:TColor);cdecl;external cDllName; // Draw a source image within a destination image (tint applied to source)
procedure ImageDrawText(var dst:TImage; text:Pchar; posX:longint; posY:longint; fontSize:longint; color:TColor);cdecl;external cDllName; // Draw text (using default font) within an image (destination)
procedure ImageDrawTextEx(var dst:TImage; font:TFont; text:Pchar; position:TVector2; fontSize:single; spacing:single; tint:TColor);cdecl;external cDllName; // Draw text (custom sprite font) within an image (destination)

// Texture loading functions
// NOTE: These functions require GPU access
function LoadTexture(fileName:Pchar):TTexture2D;cdecl;external cDllName; // Load texture from file into GPU memory (VRAM)
function LoadTextureFromImage(image:TImage):TTexture2D;cdecl;external cDllName; // Load texture from image data
function LoadTextureCubemap(image:TImage; layout:longint): TTextureCubemap;cdecl;external cDllName; // Load cubemap from image, multiple image cubemap layouts supported
function LoadRenderTexture(width:longint; height:longint):TRenderTexture2D;cdecl;external cDllName; // Load texture for rendering (framebuffer)
procedure UnloadTexture(texture:TTexture2D);cdecl;external cDllName; // Unload texture from GPU memory (VRAM)
procedure UnloadRenderTexture(target:TRenderTexture2D);cdecl;external cDllName; // Unload render texture from GPU memory (VRAM)
procedure UpdateTexture(texture:TTexture2D; pixels:pointer);cdecl;external cDllName; // Update GPU texture with new data
procedure UpdateTextureRec(texture:TTexture2D; rec:TRectangle; pixels:pointer);cdecl;external cDllName; // Update GPU texture rectangle with new data
function GetTextureData(texture:TTexture2D):TImage;cdecl;external cDllName; // Get pixel data from GPU texture and return an Image
function GetScreenData:TImage;cdecl;external cDllName; // Get pixel data from screen buffer and return an Image (screenshot)
procedure SetShapesTexture(texture:TTexture2D; source:TRectangle);cdecl;external cDllName; // Define default texture used to draw shapes

// TTexture2D configuration functions
procedure GenTextureMipmaps(var texture:TTexture2D);cdecl;external cDllName; // Generate GPU mipmaps for a texture
procedure SetTextureFilter(texture:TTexture2D; filter:longint);cdecl;external cDllName; // Set texture scaling filter mode
procedure SetTextureWrap(texture:TTexture2D; wrap:longint);cdecl;external cDllName; // Set texture wrapping mode

// TTexture2D drawing functions
procedure DrawTexture(texture:TTexture2D; posX:longint; posY:longint; tint:TColor);cdecl;external cDllName; // Draw a Texture2D
procedure DrawTextureV(texture:TTexture2D; position:TVector2; tint:TColor);cdecl;external cDllName; // Draw a Texture2D with position defined as Vector2
procedure DrawTextureEx(texture:TTexture2D; position:TVector2; rotation:single; scale:single; tint:TColor);cdecl;external cDllName; // Draw a Texture2D with extended parameters
procedure DrawTextureRec(texture:TTexture2D; source:TRectangle; position:TVector2; tint:TColor);cdecl;external cDllName; // Draw a part of a texture defined by a rectangle
procedure DrawTextureQuad(texture:TTexture2D; tiling:TVector2; offset:TVector2; quad:TRectangle; tint:TColor);cdecl;external cDllName; // Draw texture quad with tiling and offset parameters
procedure DrawTextureTiled(texture:TTexture2D; source:TRectangle; dest:TRectangle; origin:TVector2; rotation:single; scale:single; tint:TColor);cdecl;external cDllName; // Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
procedure DrawTexturePro(texture:TTexture2D; source:TRectangle; dest:TRectangle; origin:TVector2; rotation:single; tint:TColor);cdecl;external cDllName; // Draw a part of a texture defined by a rectangle with 'pro' parameters
procedure DrawTextureNPatch(texture:TTexture2D; nPatchInfo:TNPatchInfo; dest:TRectangle; origin:TVector2; rotation:single; tint:TColor);cdecl;external cDllName; // Draws a texture (or part of it) that stretches or shrinks nicely
procedure DrawTexturePoly(texture:TTexture2D; center:TVector2; var points:TVector2; var texcoords:TVector2; pointsCount:longint; tint:TColor);cdecl;external cDllName; // Draw a textured polygon

// Color/pixel related functions
function Fade(color:TColor; alpha:single):TColor;cdecl;external cDllName; /// Returns color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorToInt(color:TColor):longint;cdecl;external cDllName; // Returns hexadecimal value for a Color
function ColorNormalize(color:TColor):TVector4;cdecl;external cDllName; // Returns color normalized as float [0..1]
function ColorFromNormalized(normalized:TVector4):TColor;cdecl;external cDllName; // Returns color from normalized values [0..1]
function ColorToHSV(color:TColor):TVector3;cdecl;external cDllName; // Returns HSV values for a Color
function ColorFromHSV(hue:single; saturation:single; value:single):TColor;cdecl;external cDllName; // Returns a Color from HSV values
function ColorAlpha(color:TColor; alpha:single):TColor;cdecl;external cDllName; // Returns color with alpha applied, alpha goes from 0.0f to 1.0f
function ColorAlphaBlend(dst:TColor; src:TColor; tint:TColor):TColor;cdecl;external cDllName; // Returns src alpha-blended into dst color with tint
function GetColor(hexValue:longint):TColor;cdecl;external cDllName; // Get Color structure from hexadecimal value
function GetPixelColor(srcPtr:pointer; format:longint):TColor;cdecl;external cDllName; // Get Color from a source pixel pointer of certain format
procedure SetPixelColor(dstPtr:pointer; color:TColor; format:longint);cdecl;external cDllName; // Set color formatted into destination pixel pointer
function GetPixelDataSize(width:longint; height:longint; format:longint):longint;cdecl;external cDllName; // Get pixel data size in bytes (image or texture)

//------------------------------------------------------------------------------------
// TFont Loading and Text Drawing Functions (Module: text)
//------------------------------------------------------------------------------------

// TFont loading/unloading functions
function GetFontDefault:TFont;cdecl;external cDllName; // Get the default Font
function LoadFont(fileName:Pchar):TFont;cdecl;external cDllName; // Load font from file into GPU memory (VRAM)
function LoadFontEx(fileName:Pchar; fontSize:longint; var fontChars:longint; charsCount:longint): TFont;cdecl;external cDllName; // Load font from file with extended parameters
function LoadFontFromImage(image:TImage; key:TColor; firstChar:longint):TFont;cdecl;external cDllName; // Load font from Image (XNA style)
function LoadFontFromMemory(fileType:Pchar; var fileData:byte; dataSize:longint; fontSize:longint; var fontChars:longint; charsCount:longint):TFont;cdecl;external cDllName; // Load font from memory buffer, fileType refers to extension: i.e. "ttf"
function LoadFontData(var fileData:byte; dataSize:longint; fontSize:longint; var fontChars:longint; charsCount:longint; _type:longint): PCharInfo;cdecl;external cDllName; // Load font data for further use
function GenImageFontAtlas(chars:PCharInfo; var recs:PRectangle; charsCount:longint; fontSize:longint; padding:longint; packMethod:longint):TImage;cdecl;external cDllName; // Generate image font atlas using chars info
procedure UnloadFontData(chars:PCharInfo; charsCount:longint);cdecl;external cDllName; // Unload font chars info data (RAM)
procedure UnloadFont(font:TFont);cdecl;external cDllName; // Unload Font from GPU memory (VRAM)

// aText drawing functions
procedure DrawFPS(posX:longint; posY:longint);cdecl;external cDllName; // Draw current FPS
procedure DrawText(text:Pchar; posX:longint; posY:longint; fontSize:longint; color:TColor);cdecl;external cDllName; // Draw text (using default font)
procedure DrawTextEx(font:TFont; text:Pchar; position:TVector2; fontSize:single; spacing:single; tint:TColor);cdecl;external cDllName; // Draw text using font and additional parameters
procedure DrawTextRec(font:TFont; text:Pchar; rec:TRectangle; fontSize:single; spacing:single; wordWrap:boolean; tint:TColor);cdecl;external cDllName; // Draw text using font inside rectangle limits
procedure DrawTextRecEx(font:TFont; text:Pchar; rec:TRectangle; fontSize:single; spacing:single; wordWrap:boolean; tint:TColor; selectStart:longint; selectLength:longint; selectTint:TColor; selectBackTint:TColor);cdecl;external cDllName; // Draw text using font inside rectangle limits with support for text selection
procedure DrawTextCodepoint(font:TFont; codepoint:longint; position:TVector2; fontSize:single; tint:TColor);cdecl;external cDllName; // Draw one character (codepoint)

// Text misc. functions
function MeasureText(text:Pchar; fontSize:longint):longint;cdecl;external cDllName; // Measure string width for default font
function MeasureTextEx(font:TFont; text:Pchar; fontSize:single; spacing:single):TVector2;cdecl;external cDllName; // Measure string size for Font
function GetGlyphIndex(font:TFont; codepoint:longint):longint;cdecl;external cDllName; // Get index position for a unicode character on font

// Text strings management functions (no utf8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
function TextCopy(dst:Pchar; src:Pchar):longint;cdecl;external cDllName; // Copy one string to another, returns bytes copied
function TextIsEqual(text1:Pchar; text2:Pchar):boolean;cdecl;external cDllName; // Check if two text string are equal
function TextLength(text:Pchar):dword;cdecl;external cDllName; // Get text length, checks for '\0' ending
function TextFormat(text:Pchar; args: array of const):Pchar;cdecl;external cDllName; //// Text formatting with variables (sprintf style)
//function TextFormat(text:Pchar):Pchar;cdecl;external cDllName; // Text formatting with variables (sprintf style)
// чет запуталься нахрен
function TextSubtext(text:Pchar; position:longint; length:longint):Pchar;cdecl;external cDllName; // Get a piece of a text string
function TextReplace(text:Pchar; replace:Pchar; by:Pchar):Pchar;cdecl;external cDllName; // Replace text string (memory must be freed!)
function TextInsert(text:Pchar; insert:Pchar; position:longint):Pchar;cdecl;external cDllName; // Insert text in a position (memory should be freed!)
function TextJoin(textList:PPchar; count:longint; delimiter:Pchar):Pchar;cdecl;external cDllName; // Join text strings with delimiter
{ TODO : проверить ppchar / ^PChar}
function TextSplit(text:Pchar; delimiter:char; var count:longint):PPchar;cdecl;external cDllName; // Split text into multiple strings
procedure TextAppend(text:Pchar; append:Pchar; var position:longint);cdecl;external cDllName; // Append text at specific position and move cursor!
function TextFindIndex(text:Pchar; find:Pchar):longint;cdecl;external cDllName; // Find first text occurrence within a string
function TextToUpper(text:Pchar):Pchar;cdecl;external cDllName; // Get upper case version of provided string
function TextToLower(text:Pchar):Pchar;cdecl;external cDllName; // Get lower case version of provided string
function TextToPascal(text:Pchar):Pchar;cdecl;external cDllName; // Get Pascal case notation version of provided string
function TextToInteger(text:Pchar):longint;cdecl;external cDllName; // Get integer value from text (negative values not supported)
function TextToUtf8(var codepoints:longint; length:longint):Pchar;cdecl;external cDllName; // Encode text codepoint into utf8 text (memory must be freed!)

// UTF8 text strings management functions
function GetCodepoints(text:Pchar; var count:longint):Plongint;cdecl;external cDllName;// Get all codepoints in a string, codepoints count returned by parameters
function GetCodepointsCount(text:Pchar):longint;cdecl;external cDllName;// Get total number of characters (codepoints) in a UTF8 encoded string
function GetNextCodepoint(text:Pchar; var bytesProcessed:longint):longint;cdecl;external cDllName;// Returns next codepoint in a UTF8 encoded string; 0x3f('?') is returned on failure
function CodepointToUtf8(codepoint:longint; var byteLength:longint):PChar;cdecl;external cDllName;// Encode codepoint into utf8 text (char array length returned as parameter)

//------------------------------------------------------------------------------------
// Basic 3d Shapes Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// Basic geometric 3D shapes drawing functions
procedure DrawLine3D(startPos:TVector3; endPos:TVector3; color:TColor);cdecl;external cDllName;  // Draw a line in 3D world space
procedure DrawPoint3D(position:TVector3; color:TColor);cdecl;external cDllName; // Draw a point in 3D space, actually a small line
procedure DrawCircle3D(center:TVector3; radius:single; rotationAxis:TVector3; rotationAngle:single; color:TColor);cdecl;external cDllName; // Draw a circle in 3D world space
procedure DrawTriangle3D(v1:TVector3; v2:TVector3; v3:TVector3; color:TColor);cdecl;external cDllName; // Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleStrip3D(var points:TVector3; pointsCount:longint; color:TColor);cdecl;external cDllName; // Draw a triangle strip defined by points
procedure DrawCube(position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName; // Draw cube
procedure DrawCubeV(position:TVector3; size:TVector3; color:TColor);cdecl;external cDllName; // Draw cube (Vector version)
procedure DrawCubeWires(position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName; // Draw cube wires
procedure DrawCubeWiresV(position:TVector3; size:TVector3; color:TColor);cdecl;external cDllName; // Draw cube wires (Vector version)
procedure DrawCubeTexture(texture:TTexture2D; position:TVector3; width:single; height:single; length:single; color:TColor);cdecl;external cDllName; // Draw cube textured
procedure DrawSphere(centerPos:TVector3; radius:single; color:TColor);cdecl;external cDllName; // Draw sphere
procedure DrawSphereEx(centerPos:TVector3; radius:single; rings:longint; slices:longint; color:TColor);cdecl;external cDllName; // Draw sphere with extended parameters
procedure DrawSphereWires(centerPos:TVector3; radius:single; rings:longint; slices:longint; color:TColor);cdecl;external cDllName; // Draw sphere wires
procedure DrawCylinder(position:TVector3; radiusTop:single; radiusBottom:single; height:single; slices:longint; color:TColor);cdecl;external cDllName; // Draw a cylinder/cone
procedure DrawCylinderWires(position:TVector3; radiusTop:single; radiusBottom:single; height:single; slices:longint;color:TColor);cdecl;external cDllName; // Draw a cylinder/cone wires
procedure DrawPlane(centerPos:TVector3; size:TVector2; color:TColor);cdecl;external cDllName; // Draw a plane XZ
procedure DrawRay(ray:TRay; color:TColor);cdecl;external cDllName; // Draw a ray line
procedure DrawGrid(slices: longint; spacing:single);cdecl;external cDllName; // Draw a grid (centered at (0, 0, 0))

//------------------------------------------------------------------------------------
// TModel 3d Loading and Drawing Functions (Module: models)
//------------------------------------------------------------------------------------

// TModel loading/unloading functions
function LoadModel(filename: PChar): TModel;cdecl;external cDllName; // Load model from files (meshes and materials)
function LoadModelFromMesh(mesh: TMesh): TModel;cdecl;external cDllName; // Load model from generated mesh (default material)
procedure UnloadModel(model: TModel);cdecl;external cDllName; // Unload model from memory (RAM and/or VRAM)
procedure UnloadModelKeepMeshes(model:TModel);cdecl;external cDllName; // Unload model (but not meshes) from memory (RAM and/or VRAM)

// TMesh loading/unloading functions
procedure UploadMesh(var mesh:TMesh; dynamic_: boolean);cdecl;external cDllName; // Upload vertex data into GPU and provided VAO/VBO ids
procedure DrawMesh(mesh:TMesh; material:TMaterial; transform:TMatrix);cdecl;external cDllName; // Draw a 3d mesh with material and transform
procedure DrawMeshInstanced(mesh:TMesh; material:TMaterial; var transforms:TMatrix; instances:longint);cdecl;external cDllName;  // Draw multiple mesh instances with material and different transforms
procedure UnloadMesh(mesh:TMesh);cdecl;external cDllName; // Unload mesh data from CPU and GPU
function ExportMesh(mesh:TMesh; fileName:Pchar):boolean;cdecl;external cDllName;  // Export mesh data to file, returns true on success

// Material loading/unloading functions
function LoadMaterials(fileName:Pchar; var materialCount:longint): PMaterial;cdecl;external cDllName; // Load materials from model file
function LoadMaterialDefault:TMaterial;cdecl;external cDllName; // Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
procedure UnloadMaterial(material:TMaterial);cdecl;external cDllName; // Unload material from GPU memory (VRAM)
procedure SetMaterialTexture(var material:TMaterial; mapType:longint; texture:TTexture2D);cdecl;external cDllName; // Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
procedure SetModelMeshMaterial(var model:TModel; meshId:longint; materialId:longint);cdecl;external cDllName; // Check model animation skeleton match

// Model animations loading/unloading functions
function LoadModelAnimations(fileName:Pchar; var animsCount:longint):PModelAnimation;cdecl;external cDllName; // Load model animations from file
procedure UpdateModelAnimation(model:TModel; anim:TModelAnimation; frame:longint);cdecl;external cDllName ;// Update model animation pose
procedure UnloadModelAnimation(anim:TModelAnimation);cdecl;external cDllName; // Unload animation data
procedure UnloadModelAnimations(var animations:TModelAnimation; count:dword);cdecl; external cDllName; // Unload animation array data
function IsModelAnimationValid(model:TModel; anim:TModelAnimation):boolean;cdecl;external cDllName; // Check model animation skeleton match

// TMesh generation functions
function GenMeshCustom(vertexCount:longint; flags:longint):TMesh;cdecl;external cDllName; // Generate custom empty mesh (data initialized to 0)
function GenMeshPoly(sides:longint; radius:single):TMesh;cdecl;external cDllName; // Generate polygonal mesh
function GenMeshPlane(width:single; length:single; resX:longint; resZ:longint):TMesh;cdecl;external cDllName; // Generate plane mesh (with subdivisions)
function GenMeshCube(width:single; height:single; length:single):TMesh;cdecl;external cDllName; // Generate cuboid mesh
function GenMeshSphere(radius:single; rings:longint; slices:longint):TMesh;cdecl;external cDllName; // Generate sphere mesh (standard sphere)
function GenMeshHemiSphere(radius:single; rings:longint; slices:longint):TMesh;cdecl;external cDllName; // Generate half-sphere mesh (no bottom cap)
function GenMeshCylinder(radius:single; height:single; slices:longint):TMesh;cdecl;external cDllName; // Generate cylinder mesh
function GenMeshTorus(radius:single; size:single; radSeg:longint; sides:longint):TMesh;cdecl;external cDllName; // Generate torus mesh
function GenMeshKnot(radius:single; size:single; radSeg:longint; sides:longint):TMesh;cdecl;external cDllName; // Generate trefoil knot mesh
function GenMeshHeightmap(heightMap:TImage; size:TVector3):TMesh;cdecl;external cDllName; // Generate heightmap mesh from image data
function GenMeshCubicmap(cubicmap:TImage; cubeSize:TVector3):TMesh;cdecl;external cDllName; // Generate cubes-based map mesh from image data

// TMesh manipulation functions
function MeshBoundingBox(mesh:TMesh): TBoundingBox;cdecl;external cDllName;// Compute mesh bounding box limits
procedure MeshTangents(var mesh:TMesh);cdecl;external cDllName; // Compute mesh tangents
procedure MeshBinormals(var mesh:TMesh);cdecl;external cDllName; // Compute mesh binormals

// TModel drawing functions
procedure DrawModel(model:TModel; position:TVector3; scale:single; tint:TColor);cdecl;external cDllName; // Draw a model (with texture if set)
procedure DrawModelEx(model:TModel; position:TVector3; rotationAxis:TVector3; rotationAngle:single; scale:TVector3; tint:TColor);cdecl;external cDllName; // Draw a model with extended parameters
procedure DrawModelWires(model:TModel; position:TVector3; scale:single; tint:TColor);cdecl;external cDllName;  // Draw a model wires (with texture if set)
procedure DrawModelWiresEx(model:TModel; position:TVector3; rotationAxis:TVector3; rotationAngle:single; scale:TVector3; tint:TColor);cdecl;external cDllName; // Draw a model wires (with texture if set) with extended parameters
procedure DrawBoundingBox(box:TBoundingBox; color:TColor);cdecl;external cDllName; // Draw bounding box (wires)
procedure DrawBillboard(camera:TCamera; texture:TTexture2D; center:TVector3; size:single; tint:TColor);cdecl;external cDllName; // Draw a billboard texture
procedure DrawBillboardRec(camera:TCamera; texture:TTexture2D; source:TRectangle; center:TVector3; size:single; tint:TColor);cdecl;external cDllName; // Draw a billboard texture defined by sourceRec

// Collision detection functions
function CheckCollisionSpheres(center1:TVector3; radius1:single; center2:TVector3; radius2:single): boolean;cdecl;external cDllName; // Detect collision between two spheres
function CheckCollisionBoxes(box1:TBoundingBox; box2:TBoundingBox): boolean;cdecl;external cDllName; // Detect collision between two bounding boxes
function CheckCollisionBoxSphere(box:TBoundingBox; center:TVector3; radius:single): boolean;cdecl;external cDllName; // Detect collision between box and sphere
function CheckCollisionRaySphere(ray:TRay; center:TVector3; radius:single): boolean;cdecl;external cDllName; // Detect collision between ray and sphere
function CheckCollisionRaySphereEx(ray:TRay; center:TVector3; radius:single; var collisionPoint:TVector3): boolean;cdecl;external cDllName; // Detect collision between ray and sphere, returns collision point
function CheckCollisionRayBox(ray:TRay; box: TBoundingBox): boolean;cdecl;external cDllName; // Detect collision between ray and box
function GetCollisionRayMesh(ray:TRay; mesh:TMesh; transform:TMatrix):TRayHitInfo;cdecl;external cDllName; // Get collision info between ray and mesh
function GetCollisionRayModel(ray:TRay; model:TModel): TRayHitInfo;cdecl;external cDllName; // Get collision info between ray and model
function GetCollisionRayTriangle(ray:TRay; p1:TVector3; p2:TVector3; p3:TVector3): TRayHitInfo;cdecl;external cDllName; // Get collision info between ray and triangle
function GetCollisionRayGround(ray: TRay; groundHeight: single): TRayHitInfo;cdecl;external cDllName; // Get collision info between ray and ground plane (Y-normal plane)

//------------------------------------------------------------------------------------
// Shaders System Functions (Module: rlgl)
// NOTE: This functions are useless when using OpenGL 1.1
//------------------------------------------------------------------------------------
// Shading begin/end functions
procedure BeginShaderMode(shader:TShader);cdecl;external cDllName;// Begin custom shader drawing
procedure EndShaderMode;cdecl;external cDllName;// End custom shader drawing (use default shader)
procedure BeginBlendMode(mode: longint);cdecl;external cDllName;// Begin blending mode (alpha, additive, multiplied)
procedure EndBlendMode;cdecl;external cDllName;// End blending mode (reset to default: alpha blending)

// Shader management functions
function LoadShader(vsFileName:Pchar; fsFileName:Pchar): TShader;cdecl;external cDllName; // Load shader from files and bind default locations
function LoadShaderFromMemory(vsCode:Pchar; fsCode:Pchar):TShader; cdecl;external cDllName; // Load shader from code strings and bind default locations
procedure UnloadShader(shader:TShader);cdecl;external cDllName; // Unload shader from GPU memory (VRAM)
function GetShaderLocation(shader:TShader; uniformName:Pchar):longint;cdecl;external cDllName; // Get shader uniform location
function GetShaderLocationAttrib(shader:TShader; attribName:Pchar):longint;cdecl;external cDllName; // Get shader attribute location
procedure SetShaderValue(shader:TShader; locIndex:longint; value:pointer; uniformType:longint);cdecl;external cDllName; // Set shader uniform value
procedure SetShaderValueV(shader:TShader; locIndex:longint; value:pointer; uniformType:longint; count:longint);cdecl;external cDllName; // Set shader uniform value vector
procedure SetShaderValueMatrix(shader:TShader; locIndex:longint; mat:TMatrix);cdecl;external cDllName; // Set shader uniform value (matrix 4x4)
procedure SetShaderValueTexture(shader:TShader; locIndex:longint; texture:TTexture2D);cdecl;external cDllName; // Set shader uniform value for texture

//------------------------------------------------------------------------------------
// Audio Loading and Playing Functions (Module: audio)
//------------------------------------------------------------------------------------

// Audio device management functions
procedure InitAudioDevice;cdecl;external cDllName; // Initialize audio device and context
procedure CloseAudioDevice;cdecl;external cDllName; // Close the audio device and context
function IsAudioDeviceReady: boolean;cdecl;external cDllName; // Check if audio device has been initialized successfully
procedure SetMasterVolume(volume:single);cdecl;external cDllName; // Set master volume (listener)

// TWave/TSound loading/unloading functions
function LoadWave(fileName:Pchar):TWave;cdecl; external cDllName; // Load wave data from file
function LoadWaveFromMemory(fileType:Pchar; var fileData:byte; dataSize:longint): TWave;cdecl;external cDllName; // Load wave from memory buffer, fileType refers to extension: i.e. "wav"
function LoadSound(fileName:Pchar): TSound;cdecl;external cDllName; // Load sound from file
function LoadSoundFromWave(wave: TWave): TSound;cdecl;external cDllName; // Load sound from wave data
procedure UpdateSound(sound:TSound; data:pointer; samplesCount:longint);cdecl;external cDllName; // Update sound buffer with new data
procedure UnloadWave(wave: TWave);cdecl;external cDllName; // Unload wave data
procedure UnloadSound(sound: TSound);cdecl;external cDllName; // Unload sound
function ExportWave(wave:TWave; fileName:Pchar): boolean;cdecl;external cDllName;// Export wave data to file
function ExportWaveAsCode(wave: TWave; fileName: Pchar):boolean ;cdecl;external cDllName; // Export wave sample data to code (.h)

// TWave/TSound management functions
procedure PlaySound(sound: TSound);cdecl;external cDllName; // Play a sound
procedure StopSound(sound: TSound);cdecl;external cDllName; // Stop playing a sound
procedure PauseSound(sound: TSound);cdecl;external cDllName; // Pause a sound
procedure ResumeSound(sound: TSound);cdecl;external cDllName; // Resume a paused sound
procedure PlaySoundMulti(sound: TSound);cdecl;external cDllName; // Play a sound (using multichannel buffer pool)
procedure StopSoundMulti;cdecl;external cDllName; // Stop any sound playing (using multichannel buffer pool)
function GetSoundsPlaying: longint;cdecl;external cDllName; // Get number of sounds playing in the multichannel
function IsSoundPlaying(sound: TSound): boolean;cdecl;external cDllName; // Check if a sound is currently playing
procedure SetSoundVolume(sound:TSound; volume:single);cdecl;external cDllName; // Set volume for a sound (1.0 is max level)
procedure SetSoundPitch(sound:TSound; pitch:single);cdecl;external cDllName; // Set pitch for a sound (1.0 is base level)
procedure WaveFormat(var wave:TWave; sampleRate:longint; sampleSize:longint; channels:longint);cdecl;external cDllName; // Convert wave data to desired format
function WaveCopy(wave: TWave): TWave;cdecl;external cDllName; // Copy a wave to a new wave
procedure WaveCrop(var wave:TWave; initSample:longint; finalSample:longint);cdecl;external cDllName; // Crop a wave to defined samples range
function LoadWaveSamples(wave: TWave): PSingle;cdecl;external cDllName; /// Load samples data from wave as a floats array
procedure UnloadWaveSamples(var samples:single);cdecl;external cDllName; // Unload samples data loaded with LoadWaveSamples()

// Music management functions
function LoadMusicStream(fileName:Pchar): TMusic;cdecl;external cDllName; // Load music stream from file
function LoadMusicStreamFromMemory(fileType:Pchar; var data:byte; dataSize:longint): TMusic;cdecl;external cDllName; // Load module music from data
procedure UnloadMusicStream(music: TMusic);cdecl;external cDllName; // Unload music stream
procedure PlayMusicStream(music: TMusic);cdecl;external cDllName; // Start music playing
function IsMusicPlaying(music: TMusic): boolean;cdecl;external cDllName; // Check if music is playing
procedure UpdateMusicStream(music: TMusic);cdecl;external cDllName; // Updates buffers for music streaming
procedure StopMusicStream(music: TMusic);cdecl;external cDllName; // Stop music playing
procedure PauseMusicStream(music: TMusic);cdecl;external cDllName; // Pause music playing
procedure ResumeMusicStream(music: TMusic);cdecl;external cDllName; // Resume playing paused music
procedure SetMusicVolume(music: TMusic; volume: single);cdecl;external cDllName; // Set volume for music (1.0 is max level)
procedure SetMusicPitch(music: TMusic; pitch: single);cdecl;external cDllName; // Set pitch for a music (1.0 is base level)
function GetMusicTimeLength(music: TMusic): single;cdecl;external cDllName; // Get music time length (in seconds)
function GetMusicTimePlayed(music: TMusic): single;cdecl;external cDllName; // Get current music time played (in seconds)

// TAudioStream management functions
function InitAudioStream(sampleRate:dword; sampleSize:dword; channels: dword): TAudioStream;cdecl;external cDllName; // Init audio stream (to stream raw audio pcm data)
procedure UpdateAudioStream(stream: TAudioStream; data: Pointer; samplesCount: longint);cdecl;external cDllName; // Update audio stream buffers with data
procedure CloseAudioStream(stream: TAudioStream);cdecl;external cDllName; // Close audio stream and free memory
function IsAudioStreamProcessed(stream: TAudioStream): boolean;cdecl;external cDllName; // Check if any audio stream buffers requires refill
procedure PlayAudioStream(stream: TAudioStream);cdecl;external cDllName; // Play audio stream
procedure PauseAudioStream(stream: TAudioStream);cdecl;external cDllName; // Pause audio stream
procedure ResumeAudioStream(stream: TAudioStream);cdecl;external cDllName; // Resume audio stream
function IsAudioStreamPlaying(stream: TAudioStream): boolean;cdecl;external cDllName; // Check if audio stream is playing
procedure StopAudioStream(stream: TAudioStream);cdecl;external cDllName; // Stop audio stream
procedure SetAudioStreamVolume(stream: TAudioStream; volume: single);cdecl;external cDllName; // Set volume for audio stream (1.0 is max level)
procedure SetAudioStreamPitch(stream: TAudioStream; pitch: single);cdecl;external cDllName; // Set pitch for audio stream (1.0 is base level)
procedure SetAudioStreamBufferSizeDefault(size:longint);cdecl;external cDllName; // Default size for new audio streams

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

function TCamera3DCreate(aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer): TCamera3D;
begin
  Result.position := aPosition;
  Result.target := aTarget;
  Result.up := aUp;
  Result.fovy := aFOVY;
  Result.projection := aType;
end;

procedure TCamera3DSet(aCam: PCamera3D; aPosition, aTarget, aUp: TVector3; aFOVY: single; aType: integer);
begin
  aCam^.position := aPosition;
  aCam^.target := aTarget;
  aCam^.up := aUp;
  aCam^.fovy := aFOVY;
  aCam^.projection := aType;
end;


initialization

end.
