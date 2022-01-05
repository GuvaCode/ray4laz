{**********************************************************************************************
*
*   Physac v1.1 - 2D Physics library for videogames
*
*   DESCRIPTION:
*
*   Physac is a small 2D physics engine written in pure C. The engine uses a fixed time-step thread loop
*   to simluate physics. A physics step contains the following phases: get collision information,
*   apply dynamics, collision solving and position correction. It uses a very simple struct for physic
*   bodies with a position vector to be used in any 3D rendering API.
*
*   CONFIGURATION:
*
*   #define PHYSAC_IMPLEMENTATION
*       Generates the implementation of the library into the included file.
*       If not defined, the library is in header only mode and can be included in other headers
*       or source files without problems. But only ONE file should hold the implementation.
*
*   #define PHYSAC_DEBUG
*       Show debug traces log messages about physic bodies creation/destruction, physic system errors,
*       some calculations results and NULL reference exceptions.
*
*   #define PHYSAC_AVOID_TIMMING_SYSTEM
*       Disables internal timming system, used by UpdatePhysics() to launch timmed physic steps,
*       it allows just running UpdatePhysics() automatically on a separate thread at a desired time step.
*       In case physics steps update needs to be controlled by user with a custom timming mechanism,
*       just define this flag and the internal timming mechanism will be avoided, in that case,
*       timming libraries are neither required by the module.
*
*   #define PHYSAC_MALLOC()
*   #define PHYSAC_CALLOC()
*   #define PHYSAC_FREE()
*       You can define your own malloc/free implementation replacing stdlib.h malloc()/free() functions.
*       Otherwise it will include stdlib.h and use the C standard library malloc()/free() function.
*
*   COMPILATION:
*
*   Use the following code to compile with GCC:
*       gcc -o $(NAME_PART).exe $(FILE_NAME) -s -static -lraylib -lopengl32 -lgdi32 -lwinmm -std=c99
*
*   VERSIONS HISTORY:
*       1.1 (20-Jan-2021) @raysan5: Library general revision
*               Removed threading system (up to the user)
*               Support MSVC C++ compilation using CLITERAL()
*               Review DEBUG mechanism for TRACELOG() and all TRACELOG() messages
*               Review internal variables/functions naming for consistency
*               Allow option to avoid internal timming system, to allow app manage the steps
*       1.0 (12-Jun-2017) First release of the library
*
*
*   LICENSE: zlib/libpng
*
*   Copyright (c) 2016-2021 Victor Fisac (@victorfisac) and Ramon Santamaria (@raysan5)
*   pascal header (c) 2021 Ginko Vadim (@GuvaCode)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************}
unit physac;

{$mode ObjFPC}{$H+}

interface

uses
  raylib;

//----------------------------------------------------------------------------------
// Defines and Macros
//----------------------------------------------------------------------------------
const
  PHYSAC_MAX_BODIES               = 64;          // Maximum number of physic bodies supported
  PHYSAC_MAX_MANIFOLDS            = 4096;        // Maximum number of physic bodies interactions (64x64)
  PHYSAC_MAX_VERTICES             = 23;          // Maximum number of vertex for polygons shapes
  PHYSAC_DEFAULT_CIRCLE_VERTICES  = 23;          // Default number of vertices for circle shapes

  PHYSAC_COLLISION_ITERATIONS     = 100;
  PHYSAC_PENETRATION_ALLOWANCE    = 0.05;
  PHYSAC_PENETRATION_CORRECTION   = 0.4;

  PHYSAC_PI                       = 3.14159265358979323846;
  PHYSAC_DEG2RAD                  = (PHYSAC_PI/180.0);

//----------------------------------------------------------------------------------
// Data Types Structure Definition
//----------------------------------------------------------------------------------

type
// Matrix2x2 type (used for polygon shape rotation matrix)
  PMatrix2x2 = ^TMatrix2x2;
  TMatrix2x2 = record
    m00: single;
    m01: single;
    m10: single;
    m11: single;
  end;


type
  PPhysicsShapeType = ^TPhysicsShapeType;
  TPhysicsShapeType = Longint;
  const
    PHYSICS_CIRCLE   = 0;
    PHYSICS_POLYGON  = 1;

type
  PPhysicsVertexData = ^TPhysicsVertexData;
  TPhysicsVertexData = record
    vertexCount: dword;                                    // Vertex count (positions and normals)
    positions: array [0..PHYSAC_MAX_VERTICES] of TVector2; // Vertex positions vectors
    normals: array[0..PHYSAC_MAX_VERTICES] of TVector2;    // Vertex normals vectors
  end;

  // Previously defined to be used in PhysicsShape struct as circular dependencies
  PPhysicsBody = ^TPhysicsBody;
  TPhysicsBody = ^TPhysicsBodyData;

  PPhysicsShape = ^TPhysicsShape;
  TPhysicsShape = record
    type_: TPhysicsShapeType;                     // Shape type (circle or polygon)
    body: TPhysicsBody;                           // Shape physics body data pointer
    vertexData: TPhysicsVertexData;               // Shape vertices data (used for polygon shapes)
    radius: single;                               // Shape radius (used for circle shapes)
    transform: TMatrix2x2;                        // Vertices transform matrix 2x2
  end;

   PPhysicsBodyData = ^TPhysicsBodyData;
   TPhysicsBodyData = record
     id: dword ;                                   // Unique identifier
     enabled: boolean;                             // Enabled dynamics state (collisions are calculated anyway)
     position: TVector2;                           // Physics body shape pivot
     velocity: TVector2;                           // Current linear velocity applied to position
     force: TVector2;                              // Current linear force (reset to 0 every step)
     angularVelocity: single;                      // Current angular velocity applied to orient
     torque: single;                               // Current angular force (reset to 0 every step)
     orient: single;                               // Rotation in radians
     inertia: single;                              // Moment of inertia
     inverseInertia: single;                       // Inverse value of inertia
     mass: single;                                 // Physics body mass
     inverseMass: single;                          // Inverse value of mass
     staticFriction: single;                       // Friction when the body has not movement (0 to 1)
     dynamicFriction: single;                      // Friction when the body has movement (0 to 1)
     restitution: single;                          // Restitution coefficient of the body (0 to 1)
     useGravity: boolean;                          // Apply gravity force to dynamics
     isGrounded: boolean;                          // Physics grounded on other body state
     freezeOrient: boolean;                        // Physics rotation constraint
     shape: TPhysicsShape;                         // Physics body shape information (type, radius, vertices, transform)
   end;

   PPhysicsManifoldData = ^TPhysicsManifoldData;
   TPhysicsManifoldData = record
     id: dword;                                    // Unique identifier
     bodyA: TPhysicsBody;                          // Manifold first physics body reference
     bodyB: TPhysicsBody;                          // Manifold second physics body reference
     penetration: single;                          // Depth of penetration from collision
     normal: TVector2;                             // Normal direction vector from 'a' to 'b'
     contacts: array [0..1] of TVector2;           // Points of contact during collision
     contactsCount: dword;                         // Current collision number of contacts
     restitution: single;                          // Mixed restitution during collision
     dynamicFriction: single;                      // Mixed dynamic friction during collision
     staticFriction: single;                       // Mixed static friction during collision
   end;

   TPhysicsManifold = PPhysicsManifoldData;

//----------------------------------------------------------------------------------
// Module Functions Declaration
//----------------------------------------------------------------------------------

// Physics system management
procedure InitPhysics; cdecl;external cDllName;// Initializes physics system
procedure UpdatePhysics; cdecl;external cDllName;// Update physics system
procedure ResetPhysics; cdecl;external cDllName;// Reset physics system (global variables)
procedure ClosePhysics; cdecl;external cDllName;// Close physics system and unload used memory
procedure SetPhysicsTimeStep(delta: single); cdecl;external cDllName;// Sets physics fixed time step in milliseconds. 1.666666 by default
procedure SetPhysicsGravity(x,y: single); cdecl;external cDllName;// Sets physics global gravity force

// Physic body creation/destroy
function CreatePhysicsBodyCircle(pos: TVector2; radius: single; density: single): TPhysicsBody; cdecl;external cDllName;// Creates a new circle physics body with generic parameters
function CreatePhysicsBodyRectangle(pos: TVector2; width: single; height: single; density: single): TPhysicsBody; cdecl;external cDllName;// Creates a new rectangle physics body with generic parameters
function CreatePhysicsBodyPolygon(pos: TVector2; radius: single; sides:longint; density: single): TPhysicsBody; cdecl;external cDllName;// Creates a new polygon physics body with generic parameters
procedure DestroyPhysicsBody(body: TPhysicsBody); cdecl;external cDllName;// Destroy a physics body

// Physic body forces
procedure PhysicsAddForce(body: TPhysicsBody; force: TVector2); cdecl;external cDllName;// Adds a force to a physics body
procedure PhysicsAddTorque(body: TPhysicsBody; amount: single); cdecl;external cDllName;// Adds an angular force to a physics body
procedure PhysicsShatter(body: TPhysicsBody; position: TVector2; force: single); cdecl;external cDllName;// Shatters a polygon shape physics body to little physics bodies with explosion force
procedure SetPhysicsBodyRotation(body: TPhysicsBody; radians: single); cdecl;external cDllName;// Sets physics body shape transform based on radians parameter

// Query physics info
function GetPhysicsBody(index: longint): TPhysicsBody; cdecl;external cDllName;// Returns a physics body of the bodies pool at a specific index
function GetPhysicsBodiesCount: longint; cdecl;external cDllName;// Returns the current amount of created physics bodies
function GetPhysicsShapeType(index: longint): longint; cdecl;external cDllName;// Returns the physics body shape type (PHYSICS_CIRCLE or PHYSICS_POLYGON)
function GetPhysicsShapeVerticesCount(index: longint): longint; cdecl;external cDllName;// Returns the amount of vertices of a physics body shape
function GetPhysicsShapeVertex(body: TPhysicsBody; vertex: longint): TVector2; cdecl;external cDllName;// Returns transformed position of a body shape (body position + vertex transformed position)

implementation

end.

