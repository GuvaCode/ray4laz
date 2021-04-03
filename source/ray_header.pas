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

type
     PVector2 = ^TVector2;
     TVector2 = record
         x : single;
         y : single;
       end;

     PVector3 = ^TVector3;
     TVector3 = record
         x : single;
         y : single;
         z : single;
       end;

     PVector4 = ^TVector4;
     TVector4 = record
         x : single;
         y : single;
         z : single;
         w : single;
       end;

     PQuaternion = ^TQuaternion;
     TQuaternion = TVector4;

     PMatrix = ^TMatrix;
     TMatrix = record
         m0 : single;
         m4 : single;
         m8 : single;
         m12 : single;
         m1 : single;
         m5 : single;
         m9 : single;
         m13 : single;
         m2 : single;
         m6 : single;
         m10 : single;
         m14 : single;
         m3 : single;
         m7 : single;
         m11 : single;
         m15 : single;
       end;



     PRectangle = ^TRectangle;
     TRectangle = record
         x : single;
         y : single;
         width : single;
         height : single;
       end;

     PImage = ^TImage;
     TImage = record
         data : pointer;
         width : longint;
         height : longint;
         mipmaps : longint;
         format : longint;
       end;

     PTexture = ^TTexture;
     TTexture = record
         id : dword;
         width : longint;
         height : longint;
         mipmaps : longint;
         format : longint;
       end;

     PTexture2D = ^TTexture2D;
     TTexture2D = TTexture;

     PTextureCubemap = ^TTextureCubemap;
     TTextureCubemap = TTexture;

     PRenderTexture = ^TRenderTexture;
     TRenderTexture = record
         id : dword;
         texture : TTexture;
         depth : TTexture;
       end;

     PRenderTexture2D = ^TRenderTexture2D;
     TRenderTexture2D = TRenderTexture;

     PNPatchInfo = ^TNPatchInfo;
     TNPatchInfo = record
         source : TRectangle;
         left : longint;
         top : longint;
         right : longint;
         bottom : longint;
         layout : longint;
       end;

     PCharInfo = ^TCharInfo;
     TCharInfo = record
         value : longint;
         offsetX : longint;
         offsetY : longint;
         advanceX : longint;
         image : TImage;
       end;

     PFont = ^TFont;
     TFont = record
         baseSize : longint;
         charsCount : longint;
         charsPadding : longint;
         texture : TTexture2D;
         recs : PRectangle;
         chars : PCharInfo;
       end;


     SpriteFont = TFont;

   type
     PCamera3D = ^TCamera3D;
     TCamera3D = record
         position : TVector3;
         target : TVector3;
         up : TVector3;
         fovy : single;
         projection : longint;
       end;

     PCamera = ^TCamera;
     TCamera = TCamera3D;

     PCamera2D = ^TCamera2D;
     TCamera2D = record
         offset : TVector2;
         target : TVector2;
         rotation : single;
         zoom : single;
       end;

     PMesh = ^TMesh;
     TMesh = record
         vertexCount : longint;
         triangleCount : longint;
         vertices : Psingle;
         texcoords : Psingle;
         texcoords2 : Psingle;
         normals : Psingle;
         tangents : Psingle;
         colors : Pbyte;
         indices : Pword;
         animVertices : Psingle;
         animNormals : Psingle;
         boneIds : Plongint;
         boneWeights : Psingle;
         vaoId : dword;
         vboId : Pdword;
       end;

     PShader = ^TShader;
     TShader = record
         id : dword;
         locs : Plongint;
       end;

     PMaterialMap = ^TMaterialMap;
     TMaterialMap = record
         texture : TTexture2D;
         color : TColor;
         value : single;
       end;

     PMaterial = ^TMaterial;
     TMaterial = record
         shader : TShader;
         maps : PMaterialMap;
         params : array[0..3] of single;
       end;

     PTransform = ^TTransform;
     TTransform = record
         translation : TVector3;
         rotation : TQuaternion;
         scale : TVector3;
       end;

     PBoneInfo = ^TBoneInfo;
     TBoneInfo = record
         name : array[0..31] of char;
         parent : longint;
       end;

     PModel = ^TModel;
     TModel = record
         transform : TMatrix;
         meshCount : longint;
         materialCount : longint;
         meshes : PMesh;
         materials : PMaterial;
         meshMaterial : Plongint;
         boneCount : longint;
         bones : PBoneInfo;
         bindPose : PTransform;
       end;

     PModelAnimation = ^TModelAnimation;
     TModelAnimation = record
         boneCount : longint;
         frameCount : longint;
         bones : PBoneInfo;
         framePoses : ^PTransform;
       end;

     PRay = ^TRay;
     TRay = record
         position : TVector3;
         direction : TVector3;
       end;

     PRayHitInfo = ^TRayHitInfo;
     TRayHitInfo = record
         hit : boolean;
         distance : single;
         position : TVector3;
         normal : TVector3;
       end;

     PBoundingBox = ^TBoundingBox;
     TBoundingBox = record
         min : TVector3;
         max : TVector3;
       end;

     PWave = ^TWave;
     TWave = record
         sampleCount : dword;
         sampleRate : dword;
         sampleSize : dword;
         channels : dword;
         data : pointer;
       end;

     PrAudioBuffer = ^TrAudioBuffer;
     TrAudioBuffer = record
     end;

     PAudioStream = ^TAudioStream;
     TAudioStream = record
         buffer : PrAudioBuffer;
         sampleRate : dword;
         sampleSize : dword;
         channels : dword;
       end;

     PSound = ^TSound;
     TSound = record
         stream : TAudioStream;
         sampleCount : dword;
       end;

     PMusic = ^TMusic;
     TMusic = record
         stream : TAudioStream;
         sampleCount : dword;
         looping : boolean;
         ctxType : longint;
         ctxData : pointer;
       end;

     PVrDeviceInfo = ^TVrDeviceInfo;
     TVrDeviceInfo = record
         hResolution : longint;
         vResolution : longint;
         hScreenSize : single;
         vScreenSize : single;
         vScreenCenter : single;
         eyeToScreenDistance : single;
         lensSeparationDistance : single;
         interpupillaryDistance : single;
         lensDistortionValues : array[0..3] of single;
         chromaAbCorrection : array[0..3] of single;
       end;

     PVrStereoConfig = ^TVrStereoConfig;
     TVrStereoConfig = record
         leftLensCenter : array[0..1] of single;
         rightLensCenter : array[0..1] of single;
         leftScreenCenter : array[0..1] of single;
         rightScreenCenter : array[0..1] of single;
         scale : array[0..1] of single;
         scaleIn : array[0..1] of single;
       end;

     PConfigFlags = ^TConfigFlags;
     TConfigFlags =  Longint;
     Const
       FLAG_VSYNC_HINT = $00000040;
       FLAG_FULLSCREEN_MODE = $00000002;
       FLAG_WINDOW_RESIZABLE = $00000004;
       FLAG_WINDOW_UNDECORATED = $00000008;
       FLAG_WINDOW_HIDDEN = $00000080;
       FLAG_WINDOW_MINIMIZED = $00000200;
       FLAG_WINDOW_MAXIMIZED = $00000400;
       FLAG_WINDOW_UNFOCUSED = $00000800;
       FLAG_WINDOW_TOPMOST = $00001000;
       FLAG_WINDOW_ALWAYS_RUN = $00000100;
       FLAG_WINDOW_TRANSPARENT = $00000010;
       FLAG_WINDOW_HIGHDPI = $00002000;
       FLAG_MSAA_4X_HINT = $00000020;
       FLAG_INTERLACED_HINT = $00010000;

   type
     PTraceLogLevel = ^TTraceLogLevel;
     TTraceLogLevel =  Longint;
     Const
       LOG_ALL = 0;
       LOG_TRACE = 1;
       LOG_DEBUG = 2;
       LOG_INFO = 3;
       LOG_WARNING = 4;
       LOG_ERROR = 5;
       LOG_FATAL = 6;
       LOG_NONE = 7;

   type
     PKeyboardKey = ^TKeyboardKey;
     TKeyboardKey =  Longint;
     Const
       KEY_NULL = 0;
       KEY_APOSTROPHE = 39;
       KEY_COMMA = 44;
       KEY_MINUS = 45;
       KEY_PERIOD = 46;
       KEY_SLASH = 47;
       KEY_ZERO = 48;
       KEY_ONE = 49;
       KEY_TWO = 50;
       KEY_THREE = 51;
       KEY_FOUR = 52;
       KEY_FIVE = 53;
       KEY_SIX = 54;
       KEY_SEVEN = 55;
       KEY_EIGHT = 56;
       KEY_NINE = 57;
       KEY_SEMICOLON = 59;
       KEY_EQUAL = 61;
       KEY_A = 65;
       KEY_B = 66;
       KEY_C = 67;
       KEY_D = 68;
       KEY_E = 69;
       KEY_F = 70;
       KEY_G = 71;
       KEY_H = 72;
       KEY_I = 73;
       KEY_J = 74;
       KEY_K = 75;
       KEY_L = 76;
       KEY_M = 77;
       KEY_N = 78;
       KEY_O = 79;
       KEY_P = 80;
       KEY_Q = 81;
       KEY_R = 82;
       KEY_S = 83;
       KEY_T = 84;
       KEY_U = 85;
       KEY_V = 86;
       KEY_W = 87;
       KEY_X = 88;
       KEY_Y = 89;
       KEY_Z = 90;
       KEY_SPACE = 32;
       KEY_ESCAPE = 256;
       KEY_ENTER = 257;
       KEY_TAB = 258;
       KEY_BACKSPACE = 259;
       KEY_INSERT = 260;
       KEY_DELETE = 261;
       KEY_RIGHT = 262;
       KEY_LEFT = 263;
       KEY_DOWN = 264;
       KEY_UP = 265;
       KEY_PAGE_UP = 266;
       KEY_PAGE_DOWN = 267;
       KEY_HOME = 268;
       KEY_END = 269;
       KEY_CAPS_LOCK = 280;
       KEY_SCROLL_LOCK = 281;
       KEY_NUM_LOCK = 282;
       KEY_PRINT_SCREEN = 283;
       KEY_PAUSE = 284;
       KEY_F1 = 290;
       KEY_F2 = 291;
       KEY_F3 = 292;
       KEY_F4 = 293;
       KEY_F5 = 294;
       KEY_F6 = 295;
       KEY_F7 = 296;
       KEY_F8 = 297;
       KEY_F9 = 298;
       KEY_F10 = 299;
       KEY_F11 = 300;
       KEY_F12 = 301;
       KEY_LEFT_SHIFT = 340;
       KEY_LEFT_CONTROL = 341;
       KEY_LEFT_ALT = 342;
       KEY_LEFT_SUPER = 343;
       KEY_RIGHT_SHIFT = 344;
       KEY_RIGHT_CONTROL = 345;
       KEY_RIGHT_ALT = 346;
       KEY_RIGHT_SUPER = 347;
       KEY_KB_MENU = 348;
       KEY_LEFT_BRACKET = 91;
       KEY_BACKSLASH = 92;
       KEY_RIGHT_BRACKET = 93;
       KEY_GRAVE = 96;
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
       KEY_BACK = 4;
       KEY_MENU = 82;
       KEY_VOLUME_UP = 24;
       KEY_VOLUME_DOWN = 25;

   type
     PMouseButton = ^TMouseButton;
     TMouseButton =  Longint;
     Const
       MOUSE_LEFT_BUTTON = 0;
       MOUSE_RIGHT_BUTTON = 1;
       MOUSE_MIDDLE_BUTTON = 2;

   type
     PMouseCursor = ^TMouseCursor;
     TMouseCursor =  Longint;
     Const
       MOUSE_CURSOR_DEFAULT = 0;
       MOUSE_CURSOR_ARROW = 1;
       MOUSE_CURSOR_IBEAM = 2;
       MOUSE_CURSOR_CROSSHAIR = 3;
       MOUSE_CURSOR_POINTING_HAND = 4;
       MOUSE_CURSOR_RESIZE_EW = 5;
       MOUSE_CURSOR_RESIZE_NS = 6;
       MOUSE_CURSOR_RESIZE_NWSE = 7;
       MOUSE_CURSOR_RESIZE_NESW = 8;
       MOUSE_CURSOR_RESIZE_ALL = 9;
       MOUSE_CURSOR_NOT_ALLOWED = 10;

   type
     PGamepadButton = ^TGamepadButton;
     TGamepadButton =  Longint;
     Const
       GAMEPAD_BUTTON_UNKNOWN = 0;
       GAMEPAD_BUTTON_LEFT_FACE_UP = 1;
       GAMEPAD_BUTTON_LEFT_FACE_RIGHT = 2;
       GAMEPAD_BUTTON_LEFT_FACE_DOWN = 3;
       GAMEPAD_BUTTON_LEFT_FACE_LEFT = 4;
       GAMEPAD_BUTTON_RIGHT_FACE_UP = 5;
       GAMEPAD_BUTTON_RIGHT_FACE_RIGHT = 6;
       GAMEPAD_BUTTON_RIGHT_FACE_DOWN = 7;
       GAMEPAD_BUTTON_RIGHT_FACE_LEFT = 8;
       GAMEPAD_BUTTON_LEFT_TRIGGER_1 = 9;
       GAMEPAD_BUTTON_LEFT_TRIGGER_2 = 10;
       GAMEPAD_BUTTON_RIGHT_TRIGGER_1 = 11;
       GAMEPAD_BUTTON_RIGHT_TRIGGER_2 = 12;
       GAMEPAD_BUTTON_MIDDLE_LEFT = 13;
       GAMEPAD_BUTTON_MIDDLE = 14;
       GAMEPAD_BUTTON_MIDDLE_RIGHT = 15;
       GAMEPAD_BUTTON_LEFT_THUMB = 16;
       GAMEPAD_BUTTON_RIGHT_THUMB = 17;

   type
     PGamepadAxis = ^TGamepadAxis;
     TGamepadAxis =  Longint;
     Const
       GAMEPAD_AXIS_LEFT_X = 0;
       GAMEPAD_AXIS_LEFT_Y = 1;
       GAMEPAD_AXIS_RIGHT_X = 2;
       GAMEPAD_AXIS_RIGHT_Y = 3;
       GAMEPAD_AXIS_LEFT_TRIGGER = 4;
       GAMEPAD_AXIS_RIGHT_TRIGGER = 5;

   type
     PMeshVertexAttributes = ^TMeshVertexAttributes;
     TMeshVertexAttributes =  Longint;
     Const
       MESH_VERTEX_POSITION = 1;
       MESH_VERTEX_TEXCOORD1 = 2;
       MESH_VERTEX_TEXCOORD2 = 4;
       MESH_VERTEX_NORMAL = 8;
       MESH_VERTEX_TANGENT = 16;
       MESH_VERTEX_COLOR = 32;
       MESH_VERTEX_INDEX = 64;

   type
     PMaterialMapIndex = ^TMaterialMapIndex;
     TMaterialMapIndex =  Longint;
     Const
       MATERIAL_MAP_ALBEDO = 0;
       MATERIAL_MAP_METALNESS = 1;
       MATERIAL_MAP_NORMAL = 2;
       MATERIAL_MAP_ROUGHNESS = 3;
       MATERIAL_MAP_OCCLUSION = 4;
       MATERIAL_MAP_EMISSION = 5;
       MATERIAL_MAP_HEIGHT = 6;
       MATERIAL_MAP_BRDG = 7;
       MATERIAL_MAP_CUBEMAP = 8;
       MATERIAL_MAP_IRRADIANCE = 9;
       MATERIAL_MAP_PREFILTER = 10;
       MATERIAL_MAP_DIFFUSE = MATERIAL_MAP_ALBEDO;
       MATERIAL_MAP_SPECULAR = MATERIAL_MAP_METALNESS;

   type
     PShaderLocationIndex = ^TShaderLocationIndex;
     TShaderLocationIndex =  Longint;
     Const
       SHADER_LOC_VERTEX_POSITION = 0;
       SHADER_LOC_VERTEX_TEXCOORD01 = 1;
       SHADER_LOC_VERTEX_TEXCOORD02 = 2;
       SHADER_LOC_VERTEX_NORMAL = 3;
       SHADER_LOC_VERTEX_TANGENT = 4;
       SHADER_LOC_VERTEX_COLOR = 5;
       SHADER_LOC_MATRIX_MVP = 6;
       SHADER_LOC_MATRIX_VIEW = 7;
       SHADER_LOC_MATRIX_PROJECTION = 8;
       SHADER_LOC_MATRIX_MODEL = 9;
       SHADER_LOC_MATRIX_NORMAL = 10;
       SHADER_LOC_VECTOR_VIEW = 11;
       SHADER_LOC_COLOR_DIFFUSE = 12;
       SHADER_LOC_COLOR_SPECULAR = 13;
       SHADER_LOC_COLOR_AMBIENT = 14;
       SHADER_LOC_MAP_ALBEDO = 15;
       SHADER_LOC_MAP_METALNESS = 16;
       SHADER_LOC_MAP_NORMAL = 17;
       SHADER_LOC_MAP_ROUGHNESS = 18;
       SHADER_LOC_MAP_OCCLUSION = 19;
       SHADER_LOC_MAP_EMISSION = 20;
       SHADER_LOC_MAP_HEIGHT = 21;
       SHADER_LOC_MAP_CUBEMAP = 22;
       SHADER_LOC_MAP_IRRADIANCE = 23;
       SHADER_LOC_MAP_PREFILTER = 24;
       SHADER_LOC_MAP_BRDF = 25;
       SHADER_LOC_MAP_DIFFUSE = SHADER_LOC_MAP_ALBEDO;
       SHADER_LOC_MAP_SPECULAR = SHADER_LOC_MAP_METALNESS;

   type
     PShaderUniformDataType = ^TShaderUniformDataType;
     TShaderUniformDataType =  Longint;
     Const
       SHADER_UNIFORM_FLOAT = 0;
       SHADER_UNIFORM_VEC2 = 1;
       SHADER_UNIFORM_VEC3 = 2;
       SHADER_UNIFORM_VEC4 = 3;
       SHADER_UNIFORM_INT = 4;
       SHADER_UNIFORM_IVEC2 = 5;
       SHADER_UNIFORM_IVEC3 = 6;
       SHADER_UNIFORM_IVEC4 = 7;
       SHADER_UNIFORM_SAMPLER2D = 8;

   type
     PPixelFormat = ^TPixelFormat;
     TPixelFormat =  Longint;
     Const
       PIXELFORMAT_UNCOMPRESSED_GRAYSCALE = 1;
       PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA = 2;
       PIXELFORMAT_UNCOMPRESSED_R5G6B5 = 3;
       PIXELFORMAT_UNCOMPRESSED_R8G8B8 = 4;
       PIXELFORMAT_UNCOMPRESSED_R5G5B5A1 = 5;
       PIXELFORMAT_UNCOMPRESSED_R4G4B4A4 = 6;
       PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 = 7;
       PIXELFORMAT_UNCOMPRESSED_R32 = 8;
       PIXELFORMAT_UNCOMPRESSED_R32G32B32 = 9;
       PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 = 10;
       PIXELFORMAT_COMPRESSED_DXT1_RGB = 11;
       PIXELFORMAT_COMPRESSED_DXT1_RGBA = 12;
       PIXELFORMAT_COMPRESSED_DXT3_RGBA = 13;
       PIXELFORMAT_COMPRESSED_DXT5_RGBA = 14;
       PIXELFORMAT_COMPRESSED_ETC1_RGB = 15;
       PIXELFORMAT_COMPRESSED_ETC2_RGB = 16;
       PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA = 17;
       PIXELFORMAT_COMPRESSED_PVRT_RGB = 18;
       PIXELFORMAT_COMPRESSED_PVRT_RGBA = 19;
       PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA = 20;
       PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA = 21;

   type
     PTextureFilter = ^TTextureFilter;
     TTextureFilter =  Longint;
     Const
       TEXTURE_FILTER_POINT = 0;
       TEXTURE_FILTER_BILINEAR = 1;
       TEXTURE_FILTER_TRILINEAR = 2;
       TEXTURE_FILTER_ANISOTROPIC_4X = 3;
       TEXTURE_FILTER_ANISOTROPIC_8X = 4;
       TEXTURE_FILTER_ANISOTROPIC_16X = 5;

   type
     PTextureWrap = ^TTextureWrap;
     TTextureWrap =  Longint;
     Const
       TEXTURE_WRAP_REPEAT = 0;
       TEXTURE_WRAP_CLAMP = 1;
       TEXTURE_WRAP_MIRROR_REPEAT = 2;
       TEXTURE_WRAP_MIRROR_CLAMP = 3;

   type
     PCubemapLayout = ^TCubemapLayout;
     TCubemapLayout =  Longint;
     Const
       CUBEMAP_LAYOUT_AUTO_DETECT = 0;
       CUBEMAP_LAYOUT_LINE_VERTICAL = 1;
       CUBEMAP_LAYOUT_LINE_HORIZONTAL = 2;
       CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR = 3;
       CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE = 4;
       CUBEMAP_LAYOUT_PANORAMA = 5;

   type
     PFontType = ^TFontType;
     TFontType =  Longint;
     Const
       FONT_DEFAULT = 0;
       FONT_BITMAP = 1;
       FONT_SDF = 2;

   type
     PBlendMode = ^TBlendMode;
     TBlendMode =  Longint;
     Const
       BLEND_ALPHA = 0;
       BLEND_ADDITIVE = 1;
       BLEND_MULTIPLIED = 2;
       BLEND_ADD_COLORS = 3;
       BLEND_SUBTRACT_COLORS = 4;
       BLEND_CUSTOM = 5;

   type
     PGestures = ^TGestures;
     TGestures =  Longint;
     Const
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

   type
     PCameraMode = ^TCameraMode;
     TCameraMode =  Longint;
     Const
       CAMERA_CUSTOM = 0;
       CAMERA_FREE = 1;
       CAMERA_ORBITAL = 2;
       CAMERA_FIRST_PERSON = 3;
       CAMERA_THIRD_PERSON = 4;

   type
     PCameraProjection = ^TCameraProjection;
     TCameraProjection =  Longint;
     Const
       CAMERA_PERSPECTIVE = 0;
       CAMERA_ORTHOGRAPHIC = 1;

     type
      PNPatchLayout = ^TNPatchLayout;
      TNPatchLayout =  Longint;

      Const
        NPATCH_NINE_PATCH = 0;              // Npatch layout: 3x3 tiles
        NPATCH_THREE_PATCH_VERTICAL = 1;   // Npatch layout: 1x3 tiles
        NPATCH_THREE_PATCH_HORIZONTAL = 2; // Npatch layout: 3x1 tiles

type
   // Callbacks to hook some internal functions
  // WARNING: This callbacks are intended for advance users

      //TTraceLogCallback = procedure (logLevel:longint; text:Pchar; args:Tva_list);cdecl;
      TTraceLogCallback = procedure (logLevel:longint; text:Pchar; args: PChar);cdecl;
      PLoadFileDataCallback = ^TLoadFileDataCallback;
      TLoadFileDataCallback = function (fileName:Pchar; var bytesRead:dword):Pbyte;cdecl;
      TSaveFileDataCallback = procedure (fileName:Pchar; data:pointer; bytesToWrite:dword);cdecl;

      PLoadFileTextCallback = ^TLoadFileTextCallback;
      TLoadFileTextCallback = function (fileName:Pchar):Pchar;cdecl;
      TSaveFileTextCallback = procedure (fileName:Pchar; text:Pchar);cdecl;


//------------------------------------------------------------------------------------
// Global Variables Definition
//------------------------------------------------------------------------------------
// It's lonely here...
//------------------------------------------------------------------------------------
// Window and Graphics Device Functions (Module: core)
//------------------------------------------------------------------------------------

// Window-related functions
procedure InitWindow(aWidth: integer; aHeight: integer; aTitle: PAnsiChar);cdecl;external cDllName; // Initialize window and OpenGL context
function WindowShouldClose:boolean;cdecl;external cDllName; // Check if KEY_ESCAPE pressed or Close icon pressed
procedure CloseWindow;cdecl;external cDllName; // Close window and unload OpenGL context
function IsWindowReady:boolean;cdecl;external cDllName; // Check if window has been initialized successfully                                              // Check if window has been initialized successfully
function IsWindowFullscreen:boolean;cdecl;external cDllName; // Check if window is currently fullscreen
function IsWindowHidden:boolean;cdecl;external cDllName; // Check if window is currently hidden (only PLATFORM_DESKTOP)
function IsWindowMinimized:boolean;cdecl;external cDllName; // Check if window is currently minimized (only PLATFORM_DESKTOP)
function IsWindowMaximized:boolean;cdecl;external cDllName; // Check if window is currently maximized (only PLATFORM_DESKTOP)
function IsWindowFocused:boolean;cdecl;external cDllName; // Check if window is currently focused (only PLATFORM_DESKTOP)
function IsWindowResized:boolean;cdecl;external cDllName; // Check if window has been resized last frame
function IsWindowState(flag:dword):boolean;cdecl;external cDllName; // Check if one specific window flag is enabled
procedure SetWindowState(flags:dword);cdecl;external cDllName; // Set window configuration state using flags
procedure ClearWindowState(flags:dword);cdecl;external cDllName; // Clear window configuration state flags
procedure ToggleFullscreen;cdecl;external cDllName; // Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
procedure MaximizeWindow;cdecl;external cDllName; // Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
procedure MinimizeWindow;cdecl;external cDllName; // Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
procedure RestoreWindow;cdecl;external cDllName; // Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
procedure SetWindowIcon(image:TImage);cdecl;external cDllName; // Set icon for window (only PLATFORM_DESKTOP)
procedure SetWindowTitle(title:Pchar);cdecl;external cDllName; // Set title for window (only PLATFORM_DESKTOP)
procedure SetWindowPosition(x:longint; y:longint);cdecl;external cDllName; // Set window position on screen (only PLATFORM_DESKTOP)
procedure SetWindowMonitor(monitor:longint);cdecl;external cDllName; // Set monitor for the current window (fullscreen mode)
procedure SetWindowMinSize(width:longint; height:longint);cdecl;external cDllName; // Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
procedure SetWindowSize(width:longint; height:longint);cdecl;external cDllName; // Set window dimensions
function GetWindowHandle:pointer;cdecl;external cDllName; // Get native window handle
function GetScreenWidth:longint;cdecl;external cDllName;  // Get current screen width
function GetScreenHeight:longint;cdecl;external cDllName; // Get current screen height
function GetMonitorCount:longint;cdecl;external cDllName; // Get number of connected monitors
function GetCurrentMonitor:longint;cdecl;external cDllName;// Get current connected monitor
function GetMonitorPosition(aMonitor:longint):TVector2;cdecl;external cDllName; // Get specified monitor position
function GetMonitorWidth(monitor:longint):longint;cdecl;external cDllName; // Get specified monitor width (max available by monitor)
function GetMonitorHeight(monitor:longint):longint;cdecl;external cDllName; // Get specified monitor height (max available by monitor)
function GetMonitorPhysicalWidth(monitor:longint):longint;cdecl;external cDllName; // Get specified monitor physical width in millimetres
function GetMonitorPhysicalHeight(monitor:longint):longint;cdecl;external cDllName; // Get specified monitor physical height in millimetres
function GetMonitorRefreshRate(monitor:longint):longint;cdecl;external cDllName; // Get specified monitor refresh rate
function GetWindowPosition:TVector2;cdecl;external cDllName; // Get window position XY on monitor
function GetWindowScaleDPI:TVector2;cdecl;external cDllName; // Get window scale DPI factor
function GetMonitorName(monitor:longint):Pchar;cdecl;external cDllName; // Get the human-readable, UTF-8 encoded name of the primary monitor
procedure SetClipboardText(text:Pchar);cdecl;external cDllName; // Set clipboard text content
function GetClipboardText:Pchar;cdecl;external cDllName; // Get clipboard text content

// Cursor-related functions
procedure ShowCursor;cdecl;external cDllName; // Shows cursor
procedure HideCursor;cdecl;external cDllName; // Hides cursor
function IsCursorHidden:boolean;cdecl;external cDllName; // Check if cursor is not visible
procedure EnableCursor;cdecl;external cDllName; // Enables cursor (unlock cursor)
procedure DisableCursor;cdecl;external cDllName; // Disables cursor (lock cursor)
function IsCursorOnScreen:boolean;cdecl;external cDllName; // Check if cursor is on the current screen.

// Drawing-related functions
procedure ClearBackground(color:TColor);cdecl;external cDllName; // Set background color (framebuffer clear color)
procedure BeginDrawing;cdecl;external cDllName; // Setup canvas (framebuffer) to start drawing
procedure EndDrawing;cdecl;external cDllName; // End canvas drawing and swap buffers (double buffering)
procedure BeginMode2D(camera:TCamera2D);cdecl;external cDllName; // Initialize 2D mode with custom camera (2D)
procedure EndMode2D;cdecl;external cDllName; // Ends 2D mode with custom camera
procedure BeginMode3D(camera:TCamera3D);cdecl;external cDllName; // Initializes 3D mode with custom camera (3D)
procedure EndMode3D;cdecl;external cDllName; // Ends 3D mode and returns to default 2D orthographic mode
procedure BeginTextureMode(target:TRenderTexture2D);cdecl;external cDllName; // Initializes render texture for drawing
procedure EndTextureMode;cdecl;external cDllName; // Ends drawing to render texture
procedure BeginScissorMode(x:longint; y:longint; width:longint; height:longint);cdecl;external cDllName; // Begin scissor mode (define screen area for following drawing)
procedure EndScissorMode;cdecl;external cDllName; // End scissor mode

// Screen-space-related functions
function GetMouseRay(mousePosition:TVector2; camera:TCamera):TRay;cdecl;external cDllName; // Returns a ray trace from mouse position
function GetCameraMatrix(camera:TCamera):TMatrix;cdecl;external cDllName; // Returns camera transform matrix (view matrix)
function GetCameraMatrix2D(camera:TCamera2D):TMatrix;cdecl;external cDllName; // Returns camera 2d transform matrix
function GetWorldToScreen(position:TVector3; camera:TCamera):TVector2;cdecl;external cDllName; // Returns the screen space position for a 3d world space position
function GetWorldToScreenEx(position:TVector3; camera:TCamera; width:longint; height:longint): TVector2;cdecl;external cDllName; // Returns size position for a 3d world space position
function GetWorldToScreen2D(position:TVector2; camera:TCamera2D):TVector2;cdecl;external cDllName; // Returns the screen space position for a 2d camera world space position
function GetScreenToWorld2D(position:TVector2; camera:TCamera2D):TVector2;cdecl;external cDllName; // Returns the world space position for a 2d camera screen space position

// Timming-related functions
procedure SetTargetFPS(fps:longint);cdecl;external cDllName; // Set target FPS (maximum)
function GetFPS:longint;cdecl;external cDllName; // Returns current FPS
function GetFrameTime:single;cdecl;external cDllName; // Returns time in seconds for last frame drawn (delta time)
function GetTime:double;cdecl;external cDllName; // Returns elapsed time in seconds since InitWindow()

// Misc. functions
function GetRandomValue(min:longint; max:longint):longint;cdecl;external cDllName; // Returns a random value between min and max (both included)
procedure TakeScreenshot(fileName:Pchar);cdecl;external cDllName; // Takes a screenshot of current screen (filename extension defines format)
procedure SetConfigFlags(flags:dword);cdecl;external cDllName; // Setup init configuration flags (view FLAGS)
procedure TraceLog(logLevel:longint; text:Pchar; args:array of const);cdecl;external cDllName; // Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR)
procedure SetTraceLogLevel(logLevel:longint);cdecl;external cDllName; // Set the current threshold (minimum) log level
function MemAlloc(size:longint):pointer;cdecl;external cDllName;   // Internal memory allocator
function MemRealloc(ptr:pointer; size:longint):pointer;cdecl;external cDllName; // Internal memory reallocator
procedure MemFree(ptr:pointer);cdecl;external cDllName; // Internal memory free

// Set custom callbacks
// WARNING: Callbacks setup is intended for advance users
procedure SetTraceLogCallback(callback:TTraceLogCallback);cdecl;external cDllName;       // Set custom trace log
procedure SetLoadFileDataCallback(callback:TLoadFileDataCallback);cdecl;external cDllName; // Set custom file binary data loader
procedure SetSaveFileDataCallback(callback:TSaveFileDataCallback);cdecl;external cDllName;  // Set custom file binary data saver
procedure SetLoadFileTextCallback(callback:TLoadFileTextCallback);cdecl;external cDllName;  // Set custom file text data loader
procedure SetSaveFileTextCallback(callback:TSaveFileTextCallback);cdecl;external cDllName;  // Set custom file text data saver

// Files management functions
function LoadFileData(fileName:Pchar; var bytesRead:dword):Pbyte;cdecl;external; // Load file data as byte array (read)
procedure UnloadFileData(var data:byte);cdecl;external; // Unload file data allocated by LoadFileData()
function SaveFileData(fileName:Pchar; data:pointer; bytesToWrite:dword):boolean;cdecl;external; // Save data to file from byte array (write), returns true on success
function LoadFileText(fileName:Pchar):Pchar;cdecl;external cDllName; // Load text data from file (read), returns a '\0' terminated string
procedure UnloadFileText(var text:byte);cdecl;external cDllName; // Unload file text data allocated by LoadFileText()
function SaveFileText(fileName:Pchar; text:Pchar):boolean;cdecl;external cDllName; // Save text data to file (write), string must be '\0' terminated
function FileExists(fileName:Pchar):boolean;cdecl;external cDllName; // Check if file exists
function DirectoryExists(dirPath:Pchar):boolean;cdecl;external cDllName; // Check if a directory path exists
function IsFileExtension(fileName:Pchar; ext:Pchar):boolean;cdecl;external cDllName; // Check file extension
function GetFileExtension(fileName:Pchar):Pchar;cdecl;external cDllName; // Get pointer to extension for a filename string (includes dot: ".png")
function GetFileName(filePath:Pchar):Pchar;cdecl;external cDllName;  // Get pointer to filename for a path string
function GetFileNameWithoutExt(filePath:Pchar):Pchar;cdecl;external cDllName; // Get filename string without extension (uses static string)
function GetDirectoryPath(filePath:Pchar):Pchar;cdecl;external cDllName; // Get full path for a given fileName with path (uses static string)
function GetPrevDirectoryPath(dirPath:Pchar):Pchar;cdecl;external cDllName; // Get previous directory path for a given path (uses static string)
function GetWorkingDirectory:Pchar;cdecl;external cDllName; // Get current working directory (uses static string)
function GetDirectoryFiles(dirPath:Pchar; var count:longint):PPchar;cdecl;external cDllName; // Get filenames in a directory path (memory should be freed)
procedure ClearDirectoryFiles;cdecl;external cDllName; // Clear directory files paths buffers (free memory)
function ChangeDirectory(dir:Pchar):boolean;cdecl;external cDllName; // Change working directory, return true on success
function IsFileDropped:boolean;cdecl;external cDllName;  // Check if a file has been dropped into window
function GetDroppedFiles(var count:longint):PPchar;cdecl;external cDllName; // Get dropped files names (memory should be freed)
procedure ClearDroppedFiles;cdecl;external cDllName; // Clear dropped files paths buffer (free memory)
function GetFileModTime(fileName:Pchar):longint;cdecl;external cDllName; // Get file modification time (last write time)
function CompressData(var data:byte; dataLength:longint; var compDataLength:longint):Pbyte;cdecl;external cDllName; // Compress data (DEFLATE algorythm)
function DecompressData(var compData:byte; compDataLength:longint; var dataLength:longint):Pbyte;cdecl;external cDllName; // Decompress data (DEFLATE algorythm)

// Persistent storage management
function SaveStorageValue(position:dword; value:longint):boolean;cdecl;external cDllName; // Save integer value to storage file (to defined position)
function LoadStorageValue(position:dword):longint;cdecl;external cDllName;
procedure OpenURL(url:Pchar);cdecl;external cDllName; // Open URL with default system browser (if available)

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------
// Input-related functions: keyboard
function IsKeyPressed(key: longint):boolean;cdecl;external cDllName;  // Detect if a key has been pressed once
function IsKeyDown(key: longint):boolean;cdecl;external cDllName; // Detect if a key is being pressed
function IsKeyReleased(key:longint):boolean;cdecl;external cDllName; // Detect if a key has been released once
function IsKeyUp(key:longint):boolean;cdecl;external cDllName; // Detect if a key is NOT being pressed
procedure SetExitKey(key:longint);cdecl;external cDllName; // Set a custom key to exit program (default is ESC)
function GetKeyPressed:longint;cdecl;external cDllName; // Get key pressed (keycode), call it multiple times for keys queued
function GetCharPressed:longint;cdecl;external cDllName; // Get char pressed (unicode), call it multiple times for chars queued

// Input-related functions: gamepads
function IsGamepadAvailable(gamepad:longint):boolean;cdecl;external cDllName; // Detect if a gamepad is available
function IsGamepadName(gamepad:longint; name:Pchar):boolean;cdecl;external cDllName; // Check gamepad name (if available)
function GetGamepadName(gamepad:longint):Pchar;cdecl;external cDllName; // Return gamepad internal name id
function IsGamepadButtonPressed(gamepad:longint; button:longint):boolean;cdecl;external cDllName; // Detect if a gamepad button has been pressed once
function IsGamepadButtonDown(gamepad:longint; button:longint):boolean;cdecl;external cDllName; // Detect if a gamepad button is being pressed
function IsGamepadButtonReleased(gamepad:longint; button:longint):boolean;cdecl;external cDllName; // Detect if a gamepad button has been released once
function IsGamepadButtonUp(gamepad:longint; button:longint):boolean;cdecl;external cDllName; // Detect if a gamepad button is NOT being pressed
function GetGamepadButtonPressed:longint;cdecl;external cDllName; // Get the last gamepad button pressed
function GetGamepadAxisCount(gamepad:longint):longint;cdecl;external cDllName; // Return gamepad axis count for a gamepad
function GetGamepadAxisMovement(gamepad:longint; axis:longint):single;cdecl;external cDllName; // Return axis movement value for a gamepad axis
function SetGamepadMappings(mappings:Pchar):longint;cdecl;external cDllName; // Set internal gamepad mappings (SDL_GameControllerDB)

// Input-related functions: mouse
function IsMouseButtonPressed(button:longint):boolean;cdecl;external cDllName; // Detect if a mouse button has been pressed once
function IsMouseButtonDown(button:longint):boolean;cdecl;external cDllName; // Detect if a mouse button is being pressed
function IsMouseButtonReleased(button:longint):boolean;cdecl;external cDllName; // Detect if a mouse button has been released once
function IsMouseButtonUp(button:longint):boolean;cdecl;external cDllName; // Detect if a mouse button is NOT being pressed
function GetMouseX: longint;cdecl;external cDllName; // Returns mouse position X
function GetMouseY: longint;cdecl;external cDllName; // Returns mouse position Y
function GetMousePosition:TVector2;cdecl;external cDllName; // Returns mouse position XY
procedure SetMousePosition(x:longint; y:longint);cdecl;external cDllName; // Set mouse position XY
procedure SetMouseOffset(offsetX:longint; offsetY:longint);cdecl;external cDllName; // Set mouse offset
procedure SetMouseScale(scaleX:single; scaleY:single);cdecl;external cDllName; // Set mouse scaling
function GetMouseWheelMove:single;cdecl;external cDllName; // Returns mouse wheel movement Y
procedure SetMouseCursor(cursor:longint);cdecl;external cDllName; // Set mouse cursor

// Input-related functions: touch
function GetTouchX: longint;cdecl;external cDllName; // Returns touch position X for touch point 0 (relative to screen size)
function GetTouchY: longint;cdecl;external cDllName; // Returns touch position Y for touch point 0 (relative to screen size)
function GetTouchPosition(index:longint):TVector2;cdecl;external cDllName; // Returns touch position XY for a touch point index (relative to screen size)

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: gestures)
//------------------------------------------------------------------------------------

procedure SetGesturesEnabled(flags:dword);cdecl;external cDllName; // Enable a set of gestures using flags
function IsGestureDetected(gesture:longint):boolean;cdecl;external cDllName; // Check if a gesture have been detected
function GetGestureDetected:longint;cdecl;external cDllName; // Get latest detected gesture
function GetTouchPointsCount:longint;cdecl;external cDllName; // Get touch points count
function GetGestureHoldDuration:single;cdecl;external cDllName; // Get gesture hold time in milliseconds
function GetGestureDragVector:TVector2;cdecl;external cDllName; // Get gesture drag vector
function GetGestureDragAngle:single;cdecl;external cDllName; // Get gesture drag angle
function GetGesturePinchVector:TVector2;cdecl;external cDllName; // Get gesture pinch delta
function GetGesturePinchAngle:single;cdecl;external cDllName; // Get gesture pinch angle

//------------------------------------------------------------------------------------
// TCamera System Functions (Module: TCamera)
//------------------------------------------------------------------------------------

procedure SetCameraMode(camera:TCamera; mode:longint);cdecl;external cDllName; // Set camera mode (multiple camera modes available)
procedure UpdateCamera(var camera:TCamera);cdecl;external cDllName; // Update camera position for selected mode
procedure SetCameraPanControl(keyPan:longint);cdecl;external cDllName; // Set camera pan key to combine with mouse movement (free camera)
procedure SetCameraAltControl(keyAlt:longint);cdecl;external cDllName; // Set camera alt key to combine with mouse movement (free camera)
procedure SetCameraSmoothZoomControl(keySmoothZoom:longint);cdecl;external cDllName; // Set camera smooth zoom key to combine with mouse (free camera)
procedure SetCameraMoveControls(keyFront:longint; keyBack:longint; keyRight:longint; keyLeft:longint; keyUp:longint; keyDown:longint);cdecl;external cDllName; // Set camera move controls (1st person and 3rd person cameras)

//------------------------------------------------------------------------------------
// VR Simulator Functions (Module: core)
//------------------------------------------------------------------------------------
procedure InitVrSimulator(device:TVrDeviceInfo);cdecl;external cDllName; // Init VR simulator for selected device parameters
procedure CloseVrSimulator;cdecl;external cDllName; // Close VR simulator for current device
function IsVrSimulatorReady:boolean;cdecl;external cDllName; // Detect if VR simulator is ready
procedure UpdateVrTracking(var camera:TCamera);cdecl;external cDllName; // Update VR tracking (position and orientation) and camera
procedure BeginVrDrawing(target:TRenderTexture2D);cdecl;external cDllName; // Begin VR simulator stereo rendering (using provided fbo)
procedure EndVrDrawing;cdecl;external cDllName; // End VR simulator stereo rendering
function GetVrConfig(device:TVrDeviceInfo):TVrStereoConfig;cdecl;external cDllName; // Get stereo rendering configuration parameters

//------------------------------------------------------------------------------------
// Basic Shapes Drawing Functions (Module: shapes)
//------------------------------------------------------------------------------------

// Basic shapes drawing functions
procedure DrawPixel(posX:longint; posY:longint; color:TColor);cdecl;external cDllName; // Draw a pixel
procedure DrawPixelV(position:TVector2; color:TColor);cdecl;external cDllName; // Draw a pixel (Vector version)
procedure DrawLine(startPosX:longint; startPosY:longint; endPosX:longint; endPosY:longint; color:TColor);cdecl;external cDllName; // Draw a line
procedure DrawLineV(startPos:TVector2; endPos:TVector2; color:TColor);cdecl;external cDllName; // Draw a line (Vector version)
procedure DrawLineEx(startPos:TVector2; endPos:TVector2; thick:single; color:TColor);cdecl;external cDllName; // Draw a line defining thickness
procedure DrawLineBezier(startPos:TVector2; endPos:TVector2; thick:single; color:TColor);cdecl;external cDllName; // Draw a line using cubic-bezier curves in-out
procedure DrawLineBezierQuad(startPos:TVector2; endPos:TVector2; controlPos:TVector2; thick:single; color:TColor);cdecl;external cDllName; //Draw line using quadratic bezier curves with a control point
procedure DrawLineStrip(var points:TVector2; pointsCount:longint; color:TColor);cdecl;external cDllName; // Draw lines sequence
procedure DrawCircle(centerX:longint; centerY:longint; radius:single; color:TColor);cdecl;external cDllName; // Draw a color-filled circle
procedure DrawCircleSector(center:TVector2; radius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName; // Draw a piece of a circle
procedure DrawCircleSectorLines(center:TVector2; radius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName; // Draw circle sector outline
procedure DrawCircleGradient(centerX:longint; centerY:longint; radius:single; color1:TColor; color2:TColor);cdecl;external cDllName; // Draw a gradient-filled circle
procedure DrawCircleV(center:TVector2; radius:single; color:TColor);cdecl;external cDllName; // Draw a color-filled circle (Vector version)
procedure DrawCircleLines(centerX:longint; centerY:longint; radius:single; color:TColor);cdecl;external cDllName; // Draw circle outline
procedure DrawEllipse(centerX:longint; centerY:longint; radiusH:single; radiusV:single; color:TColor);cdecl;external cDllName; // Draw ellipse
procedure DrawEllipseLines(centerX:longint; centerY:longint; radiusH:single; radiusV:single; color:TColor);cdecl;external cDllName; // Draw ellipse outline
procedure DrawRing(center:TVector2; innerRadius:single; outerRadius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName; // Draw ring
procedure DrawRingLines(center:TVector2; innerRadius:single; outerRadius:single; startAngle:single; endAngle:single; segments:longint; color:TColor);cdecl;external cDllName; // Draw ring outline
procedure DrawRectangle(posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName; // Draw a color-filled rectangle
procedure DrawRectangleV(position:TVector2; size:TVector2; color:TColor);cdecl;external cDllName; // Draw a color-filled rectangle (Vector version)
procedure DrawRectangleRec(rec:TRectangle; color:TColor);cdecl;external cDllName; // Draw a color-filled rectangle
procedure DrawRectanglePro(rec:TRectangle; origin:TVector2; rotation:single; color:TColor);cdecl;external cDllName; // Draw a color-filled rectangle with pro parameters
procedure DrawRectangleGradientV(posX:longint; posY:longint; width:longint; height:longint; color1:TColor; color2:TColor);cdecl;external cDllName; // Draw a vertical-gradient-filled rectangle
procedure DrawRectangleGradientH(posX:longint; posY:longint; width:longint; height:longint; color1:TColor; color2:TColor);cdecl;external cDllName; // Draw a horizontal-gradient-filled rectangle
procedure DrawRectangleGradientEx(rec:TRectangle; col1:TColor; col2:TColor; col3:TColor; col4:TColor);cdecl;external cDllName; // Draw a gradient-filled rectangle with custom vertex colors
procedure DrawRectangleLines(posX:longint; posY:longint; width:longint; height:longint; color:TColor);cdecl;external cDllName; // Draw rectangle outline
procedure DrawRectangleLinesEx(rec:TRectangle; lineThick:longint; color:TColor);cdecl;external cDllName; // Draw rectangle outline with extended parameters
procedure DrawRectangleRounded(rec:TRectangle; roundness:single; segments:longint; color:TColor);cdecl;external cDllName;  // Draw rectangle with rounded edges
procedure DrawRectangleRoundedLines(rec:TRectangle; roundness:single; segments:longint; lineThick:longint; color:TColor);cdecl;external cDllName; // Draw rectangle with rounded edges outline
procedure DrawTriangle(v1:TVector2; v2:TVector2; v3:TVector2; color:TColor);cdecl;external cDllName; // Draw a color-filled triangle (vertex in counter-clockwise order!)
procedure DrawTriangleLines(v1:TVector2; v2:TVector2; v3:TVector2; color:TColor);cdecl;external cDllName; // Draw triangle outline (vertex in counter-clockwise order!)
procedure DrawTriangleFan(var points:TVector2; pointsCount:longint; color:TColor);cdecl;external cDllName; // Draw a triangle fan defined by points (first vertex is the center)
procedure DrawTriangleStrip(var points:TVector2; pointsCount:longint; color:TColor);cdecl;external cDllName; // Draw a triangle strip defined by points
procedure DrawPoly(center:TVector2; sides:longint; radius:single; rotation:single; color:TColor);cdecl;external cDllName; // Draw a regular polygon (Vector version)
procedure DrawPolyLines(center:TVector2; sides:longint; radius:single; rotation:single; color:TColor);cdecl;external cDllName; // Draw a polygon outline of n sides

// Basic shapes collision detection functions
function CheckCollisionRecs(rec1:TRectangle; rec2:TRectangle):boolean;cdecl;external cDllName; // Check collision between two rectangles
function CheckCollisionCircles(center1:TVector2; radius1:single; center2:TVector2; radius2:single):boolean;cdecl;external cDllName; // Check collision between two circles
function CheckCollisionCircleRec(center:TVector2; radius:single; rec:TRectangle):boolean;cdecl;external cDllName; // Check collision between circle and rectangle
function CheckCollisionPointRec(point:TVector2; rec:TRectangle):boolean;cdecl;external cDllName; // Check if point is inside rectangle
function CheckCollisionPointCircle(point:TVector2; center:TVector2; radius:single):boolean;cdecl;external cDllName; // Check if point is inside circle
function CheckCollisionPointTriangle(point:TVector2; p1:TVector2; p2:TVector2; p3:TVector2):boolean;cdecl;external cDllName; // Check if point is inside a triangle
function CheckCollisionLines(startPos1:TVector2; endPos1:TVector2; startPos2:TVector2; endPos2:TVector2; var collisionPoint:TVector2):boolean;cdecl;external cDllName; // Check the collision between two lines defined by two points each, returns collision point by reference
function GetCollisionRec(rec1:TRectangle; rec2:TRectangle):TRectangle;cdecl;external cDllName; // Get collision rectangle for two rectangles collision

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
