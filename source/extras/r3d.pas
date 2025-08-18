unit r3d;
(*
 * r3d header for pascal 2025 Gunko Vadim @guvacode
 * this is part of ray4laz project
 * original code r3d by Le Juez Victor
 * https://github.com/Bigfoot71/r3d
 *
 * This software is provided "as-is", without any express or implied warranty. In no event
 * will the authors be held liable for any damages arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose, including commercial
 * applications, and to alter it and redistribute it freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented; you must not claim that you
 *   wrote the original software. If you use this software in a product, an acknowledgment
 *   in the product documentation would be appreciated but is not required.
 *
 *   2. Altered source versions must be plainly marked as such, and must not be misrepresented
 *   as being the original software.
 *
 *   3. This notice may not be removed or altered from any source distribution.
 *)


{$mode objfpc}{$H+}



// Include configuration file
{$I raylib.inc}

{$IFDEF RAY_STATIC}
   {$Warning At the moment, it is possible to use only a dynamic library.}
{$ENDIF}

interface

uses
  raylib;


const
  r3dName =
    {$IFDEF WINDOWS} 'libr3d.dll'; {$IFEND}
    {$IFDEF LINUX} 'libr3d.so'; {$IFEND}

// --------------------------------------------
//                   ENUMS
// --------------------------------------------

{$REGION 'ENUMS'}

type (* Flags to configure the rendering engine behavior.              *)
     (* These flags control various aspects of the rendering pipeline. *)
  R3D_Flags = Cardinal;
const
  R3D_FLAG_NONE                  = R3D_Flags(0);       // No special rendering flags
  R3D_FLAG_FXAA                  = R3D_Flags(1 shl 0); // Enables Fast Approximate Anti-Aliasing (FXAA)
  R3D_FLAG_BLIT_LINEAR           = R3D_Flags(1 shl 1); // Uses linear filtering when blitting the final image
  R3D_FLAG_ASPECT_KEEP           = R3D_Flags(1 shl 2); // Maintains the aspect ratio of the internal resolution when blitting the final image
  R3D_FLAG_STENCIL_TEST          = R3D_Flags(1 shl 3); // Performs a stencil test on each rendering pass affecting geometry
  R3D_FLAG_DEPTH_PREPASS         = R3D_Flags(1 shl 4); // Performs a depth pre-pass before forward rendering, improving desktop GPU performance but unnecessary on mobile
  R3D_FLAG_8_BIT_NORMALS         = R3D_Flags(1 shl 5); // Use 8-bit precision for the normals buffer (deferred); default is 16-bit float
  R3D_FLAG_FORCE_FORWARD         = R3D_Flags(1 shl 6); // Used to force forward rendering for opaque objects, useful for tile-based devices. Be careful, this flag should not be set when rendering, or you may get incorrect sorting of draw calls.
  R3D_FLAG_NO_FRUSTUM_CULLING    = R3D_Flags(1 shl 7); // Disables internal frustum culling. Manual culling is allowed, but may break shadow visibility if objects casting shadows are skipped.
  R3D_FLAG_TRANSPARENT_SORTING   = R3D_Flags(1 shl 8); // Back-to-front sorting of transparent objects for correct blending of non-discarded fragments. Be careful, in 'force forward' mode this flag will also sort opaque objects in 'near-to-far' but in the same sorting pass.
  R3D_FLAG_OPAQUE_SORTING        = R3D_Flags(1 shl 9); // Front-to-back sorting of opaque objects to optimize depth testing at the cost of additional sorting. Please note, in 'force forward' mode this flag has no effect, see transparent sorting.
  R3D_FLAG_LOW_PRECISION_BUFFERS = R3D_Flags(1 shl 10); // Use 32-bit HDR formats like R11G11B10F for intermediate color buffers instead of full 16-bit floats. Saves memory and bandwidth.
 
type (* Blend modes for rendering.                                                                     *)
     (* Defines common blending modes used in 3D rendering to combine source and destination colors.   *)
     (* note: The blend mode is applied only if you are in forward rendering mode or auto-detect mode. *)
  R3D_BlendMode = Integer;
  const
    R3D_BLEND_OPAQUE   = R3D_BlendMode(0); // No blending, the source color fully replaces the destination color.
    R3D_BLEND_ALPHA    = R3D_BlendMode(1); // Alpha blending: source color is blended with the destination based on alpha value.
    R3D_BLEND_ADDITIVE = R3D_BlendMode(2); // Additive blending: source color is added to the destination, making bright effects.
    R3D_BLEND_MULTIPLY = R3D_BlendMode(3); // Multiply blending: source color is multiplied with the destination, darkening the image.

type
  R3D_CullMode = Integer;
  const
    R3D_CULL_NONE  = R3D_CullMode(0);
    R3D_CULL_BACK  = R3D_CullMode(1);
    R3D_CULL_FRONT = R3D_CullMode(2);

type (* Defines the shadow casting mode for objects in the scene.                  *)
     (* Determines how an object contributes to shadow mapping, which can affect   *)
     (* performance and visual accuracy depending on the rendering technique used. *)
  R3D_ShadowCastMode = Integer;
  const
    R3D_SHADOW_CAST_DISABLED    = R3D_ShadowCastMode(0); // The object does not cast shadows.
    R3D_SHADOW_CAST_FRONT_FACES = R3D_ShadowCastMode(1); // Only front-facing polygons cast shadows.
    R3D_SHADOW_CAST_BACK_FACES  = R3D_ShadowCastMode(2); // Only back-facing polygons cast shadows.
    R3D_SHADOW_CAST_ALL_FACES   = R3D_ShadowCastMode(3); // Both front and back-facing polygons cast shadows.

type (* Defines billboard modes for 3D objects.                                               *)
     (* This enumeration defines how a 3D object aligns itself relative to the camera.        *)
     (* It provides options to disable billboarding or to enable specific modes of alignment. *)
  R3D_BillboardMode = Integer;
  const
    R3D_BILLBOARD_DISABLED = R3D_BillboardMode(0); // Billboarding is disabled; the object retains its original orientation.
    R3D_BILLBOARD_FRONT    = R3D_BillboardMode(1); // Full billboarding; the object fully faces the camera, rotating on all axes.
    R3D_BILLBOARD_Y_AXIS   = R3D_BillboardMode(2); // Y-axis constrained billboarding; the object rotates only around the Y-axis,
    // keeping its "up" orientation fixed. This is suitable for upright objects like characters or signs.

type (* Types of lights supported by the rendering engine.    *)
     (* Each light type has different behaviors and use cases *)
  R3D_LightType = Integer;
  const
    R3D_LIGHT_DIR  = R3D_LightType(0); // Directional light, affects the entire scene with parallel rays.
    R3D_LIGHT_SPOT = R3D_LightType(1); // Spot light, emits light in a cone shape.
    R3D_LIGHT_OMNI = R3D_LightType(2); // Omni light, emits light in all directions from a single point.

type (* Modes for updating shadow maps.                     *)
     (* Determines how often the shadow maps are refreshed. *)
  R3D_ShadowUpdateMode = Integer;
  const
    R3D_SHADOW_UPDATE_MANUAL     = R3D_ShadowUpdateMode(0); // Shadow maps update only when explicitly requested.
    R3D_SHADOW_UPDATE_INTERVAL   = R3D_ShadowUpdateMode(1); // Shadow maps update at defined time intervals.
    R3D_SHADOW_UPDATE_CONTINUOUS = R3D_ShadowUpdateMode(2); // Shadow maps update every frame for real-time accuracy.

type (* Bloom effect modes.                                                                *)
     (* Specifies different post-processing bloom techniques that can be applied           *)
     (* to the rendered scene. Bloom effects enhance the appearance of bright areas        *)
     (* by simulating light bleeding, contributing to a more cinematic and realistic look. *)
  R3D_Bloom = Integer;
  const
    R3D_BLOOM_DISABLED = R3D_Bloom(0); // Bloom effect is disabled. The scene is rendered without any glow enhancement.
    R3D_BLOOM_MIX      = R3D_Bloom(1); // Blends the bloom effect with the original scene using linear interpolation (Lerp).
    R3D_BLOOM_ADDITIVE = R3D_Bloom(2); // Adds the bloom effect additively to the scene, intensifying bright regions.
    R3D_BLOOM_SCREEN   = R3D_Bloom(3); // Combines the scene and bloom using screen blending, which brightens highlights

type (* Fog effect modes.                                                                      *)
     (* Determines how fog is applied to the scene, affecting depth perception and atmosphere. *)
  R3D_Fog = Integer;
  const
    R3D_FOG_DISABLED         = R3D_Fog(0); // Fog effect is disabled.
    R3D_FOG_LINEAR           = R3D_Fog(1); // Fog density increases linearly with distance from the camera.
    R3D_FOG_EXP2             = R3D_Fog(2); // Exponential fog (exp2), where density increases exponentially with distance.
    R3D_FOG_EXP              = R3D_Fog(3); // Exponential fog, similar to EXP2 but with a different rate of increase.

type (* Tone mapping modes.                                                                             *)
     (* Controls how high dynamic range (HDR) colors are mapped to low dynamic range (LDR) for display. *)
  R3D_Tonemap = Integer;
  const
    R3D_TONEMAP_LINEAR       = R3D_Tonemap(0); // Simple linear mapping of HDR values.
    R3D_TONEMAP_REINHARD     = R3D_Tonemap(1); // Reinhard tone mapping, a balanced method for compressing HDR values.
    R3D_TONEMAP_FILMIC       = R3D_Tonemap(2); // Filmic tone mapping, mimicking the response of photographic film.
    R3D_TONEMAP_ACES         = R3D_Tonemap(3); // ACES tone mapping, a high-quality cinematic rendering technique.
    R3D_TONEMAP_AGX          = R3D_Tonemap(4); // AGX tone mapping, a modern technique designed to preserve both highlight and shadow details for HDR rendering.
    R3D_TONEMAP_COUNT        = R3D_Tonemap(5); // Number of tone mapping modes (used internally)


  type (* Depth of field effect modes.                                                                  *)
       (* Controls how depth of field is applied to the scene, affecting the focus and blur of objects. *)
  R3D_Dof = Integer;
  const
    R3D_DOF_DISABLED = R3D_Dof(0); // Depth of field effect is disabled.
    R3D_DOF_ENABLED  = R3D_Dof(1); // Depth of field effect is enabled.

{$ENDREGION}

// --------------------------------------------
//                   TYPES
// --------------------------------------------

{$REGION 'TYPES'}

type (* Represents a vertex and all its attributes for a mesh. *)
  PR3D_Vertex = ^TR3D_Vertex;
  TR3D_Vertex = record
    position: TVector3;               // The 3D position of the vertex in object space.
    texcoord: TVector2;               // The 2D texture coordinates (UV) for mapping textures.
    normal:   TVector3;               // The normal vector used for lighting calculations.
    color:    TVector4;               // Vertex color, typically RGBA.
    tangent:  TVector4;               // The tangent vector, used in normal mapping (often with a handedness in w).
    boneIds:  array[0..3] of Integer; // Indices of up to 4 bones that influence this vertex (for GPU skinning).
    weights:  array[0..3] of Single;  // Corresponding bone weights (should sum to 1.0). Defines the influence of each bone.
  end;

type (* Represents a mesh with its geometry data and GPU buffers. *)
     (* Contains vertex/index data, GPU buffer handles, and bounding volume. *)
  PR3D_Mesh = ^TR3D_Mesh;
  TR3D_Mesh = record
    vertices:     PR3D_Vertex;  // Pointer to the array of vertices.
    indices:      PCardinal;    // Pointer to the array of indices.
    vertexCount:  Integer;      // Number of vertices.
    indexCount:   Integer;      // Number of indices.
    vbo:          Cardinal;     // Vertex Buffer Object (GPU handle).
    ebo:          Cardinal;     // Element Buffer Object (GPU handle).
    vao:          Cardinal;     // Vertex Array Object (GPU handle).
    boneMatrices: PMatrix;      // Cached animation matrices for all passes.
    aabb:         TBoundingBox; // Axis-Aligned Bounding Box in local space.
  end;

type (* Represents a material with textures, parameters, and rendering modes. *)
     (* Combines multiple texture maps and settings used during shading.      *)
  PR3D_MapAlbedo = ^TR3D_MapAlbedo;
  TR3D_MapAlbedo = record
    texture: TTexture2D; // Albedo (base color) texture.
    color:   TColorB;    // Albedo color multiplier.
  end;

  PR3D_MapEmission = ^TR3D_MapEmission;
  TR3D_MapEmission = record
    texture: TTexture2D; // Emission texture.
    color:   TColorB;    // Emission color.
    energy:  Single;     // Emission energy multiplier.
  end;

  PR3D_MapNormal = ^TR3D_MapNormal;
  TR3D_MapNormal = record
    texture: TTexture2D; // Normal map texture.
    scale:   Single;     // Normal scale.
  end;

  PR3D_MapORM = ^TR3D_MapORM;
  TR3D_MapORM = record
    texture:   TTexture2D; // Combined Occlusion-Roughness-Metalness texture.
    occlusion: Single;     // Occlusion multiplier.
    roughness: Single;     // Roughness multiplier.
    metalness: Single;     // Metalness multiplier.
  end;

  PR3D_Material = ^TR3D_Material;
  TR3D_Material = record
    albedo:         TR3D_MapAlbedo;
    emission:       TR3D_MapEmission;
    normal:         TR3D_MapNormal;
    orm:            TR3D_MapORM;
    blendMode:      R3D_BlendMode;      // Blend mode used for rendering the material.
    cullMode:       R3D_CullMode;       // Face culling mode used for the material.
    shadowCastMode: R3D_ShadowCastMode; // Shadow casting mode for the object.
    billboardMode:  R3D_BillboardMode;  // Billboard mode applied to the object.
    uvOffset:       TVector2;           // UV offset applied to the texture coordinates. For models, this can be set manually.
                                        // For sprites, this value is overridden automatically.
    uvScale:        TVector2;           // UV scale factor applied to the texture coordinates.
                                        // For models, this can be set manually.
                                        // For sprites, this value is overridden automatically.
    alphaCutoff:    Single;             // Alpha threshold below which fragments are discarded.
  end;

type (* Represents a skeletal animation for a model.                 *)
     (* This structure holds the animation data for a skinned model, *)
     (* including per-frame bone transformation poses.               *)
  PR3D_ModelAnimation = ^TR3D_ModelAnimation;
  TR3D_ModelAnimation = record
    boneCount:  Integer;   // Number of bones in the skeleton affected by this animation.
    frameCount: Integer;   // Total number of frames in the animation sequence.
    bones:      PBoneInfo; // Array of bone metadata (name, parent index, etc.) that defines the skeleton hierarchy.
    framePoses: PMatrix;   // 2D array of transformation matrices: [frame][bone]. Each matrix represents the pose of a bone in a specific frame, typically in local space.
    name: array[0..31] of Char; // Name identifier for the animation (e.g., "Walk", "Jump", etc.).
  end;

type (* Represents a complete 3D model with meshes and materials.                                 *)
     (* Contains multiple meshes and their associated materials, along with bounding information. *)
  PR3D_Model = ^TR3D_Model;
  TR3D_Model = record
    meshes:        PR3D_Mesh;           // Array of meshes composing the model.
    materials:     PR3D_Material;       // Array of materials used by the model.
    meshMaterials: PInteger;            // Array of material indices, one per mesh.
    meshCount:     Integer;             // Number of meshes.
    materialCount: Integer;             // Number of materials.
    aabb:          TBoundingBox;        // Axis-Aligned Bounding Box encompassing the whole model.
    boneOffsets:   PMatrix;             // Array of offset (inverse bind) matrices, one per bone. Transforms mesh-space vertices to bone space. Used in skinning.
    bones:         PBoneInfo;           // Bones information (skeleton). Defines the hierarchy and names of bones.
    boneCount:     Integer;             // Number of bones.
    anim:          PR3D_ModelAnimation; // Pointer to the currently assigned animation for this model (optional).
    animFrame:     Integer;             // Current animation frame index. Used for sampling bone poses from the animation.
  end;

type (* Represents a unique identifier for a light in R3D.                                 *)
     (* This ID is used to reference a specific light when calling R3D lighting functions. *)
  PR3D_Light = ^TR3D_Light;
  TR3D_Light = Cardinal;  // Cardinal is an unsigned 32-bit integer in Pascal

type (* Structure representing a skybox and its related textures for lighting.   *)
     (* This structure contains textures used for rendering a skybox, as well as *)
     (* precomputed lighting textures used for image-based lighting (IBL).       *)
  PR3D_Skybox = ^TR3D_Skybox;
  TR3D_Skybox = record
    cubemap:    TTextureCubemap; // The skybox cubemap texture for the background and reflections.
    irradiance: TTexture2D;      // The irradiance cubemap for diffuse ambient lighting.
    prefilter:  TTexture2D;      // The prefiltered cubemap for specular reflections with mipmaps.
  end;

type (* Represents a 3D sprite with billboard rendering and animation support. *)
     (* This structure defines a 3D sprite, which by default is rendered as a billboard around the Y-axis. *)
     (* The sprite supports frame-based animations and can be configured to use various billboard modes.   *)
     (* Warning: The shadow mode does not handle transparency. If shadows are enabled,                     *)
     (* the entire quad will be rendered in the shadow map,                                                *)
     (* potentially causing undesired visual artifacts for semi-transparent sprites.                       *)
  PR3D_Sprite = ^TR3D_Sprite;
  TR3D_Sprite = record
    material:     TR3D_Material; // The material used for rendering the sprite, including its texture and shading properties.
    currentFrame: Single;        // The current animation frame, represented as a floating-point value to allow smooth interpolation.
    frameSize:    TVector2;      // The size of a single animation frame, in texture coordinates (width and height).
    xFrameCount:  Integer;       // The number of frames along the horizontal (X) axis of the texture.
    yFrameCount:  Integer;       // The number of frames along the vertical (Y) axis of the texture.
  end;

type (* Represents a keyframe in an interpolation curve.                                                                       *)
     (* A keyframe contains two values: the time at which the keyframe occurs and the value of the interpolation at that time. *)
     (* The time is normalized between 0.0 and 1.0, where 0.0 represents the start of the curve and 1.0 represents the end.    *)
  PR3D_Keyframe = ^TR3D_Keyframe;
  TR3D_Keyframe = record
    time:  Single; // Normalized time of the keyframe, ranging from 0.0 to 1.0.
    value: Single; // The value of the interpolation at this keyframe.
  end;

type (* Represents an interpolation curve composed of keyframes.                                                            *)
     (* This structure contains an array of keyframes and metadata about the array, such as the current number of keyframes *)
     (* and the allocated capacity. The keyframes define a curve that can be used for smooth interpolation between values   *)
     (* over a normalized time range (0.0 to 1.0).                                                                          *)
  PR3D_InterpolationCurve = ^TR3D_InterpolationCurve;
  TR3D_InterpolationCurve = record
    keyframes: PR3D_Keyframe; // Dynamic array of keyframes defining the interpolation curve.
    capacity:  Cardinal;      // Allocated size of the keyframes array.
    count:     Cardinal;      // Current number of keyframes in the array.
  end;

type (* Represents a particle in a 3D particle system, with properties *)
     (* such as position, velocity, rotation, and color modulation.    *)
  PR3D_Particle = ^TR3D_Particle;
  TR3D_Particle = record
    lifetime:            Single;   // Duration of the particle's existence in seconds.
    transform:           TMatrix;  // The particle's current transformation matrix in 3D space.
    position:            TVector3; // The current position of the particle in 3D space.
    rotation:            TVector3; // The current rotation of the particle in 3D space (Euler angles).
    scale:               TVector3; // The current scale of the particle in 3D space.
    color:               TColor;   // The current color of the particle, representing its color modulation.
    velocity:            TVector3; // The current velocity of the particle in 3D space.
    angularVelocity:     TVector3; // The current angular velocity of the particle in radians (Euler angles).
    baseScale:           TVector3; // The initial scale of the particle in 3D space.
    baseVelocity:        TVector3; // The initial velocity of the particle in 3D space.
    baseAngularVelocity: TVector3; // The initial angular velocity of the particle in radians (Euler angles).
    baseOpacity:         Byte;     // The initial opacity of the particle, ranging from 0 (fully transparent) to 255 (fully opaque).
  end;

  PR3D_ParticleSystem = ^TR3D_ParticleSystem;
  TR3D_ParticleSystem = record
    particles:                   PR3D_Particle; // Pointer to the array of particles in the system.
    capacity:                    Integer;  // The maximum number of particles the system can manage.
    count:                       Integer;  // The current number of active particles in the system.
    position:                    TVector3; // The initial position of the particle system. Default: (0, 0, 0).
    gravity:                     TVector3; // The gravity applied to the particles. Default: (0, -9.81, 0)
    initialScale:                TVector3; // The initial scale of the particles. Default: (1, 1, 1).
    scaleVariance:               Single;   // The variance in particle scale. Default: 0.0f.
    initialRotation:             TVector3; // The initial rotation of the particles in Euler angles (degrees). Default: (0, 0, 0).
    rotationVariance:            TVector3; // The variance in particle rotation in Euler angles (degrees). Default: (0, 0, 0).
    initialColor:                TColor;   // The initial color of the particles. Default: WHITE.
    colorVariance:               TColor;   // The variance in particle color. Default: BLANK.
    initialVelocity:             TVector3; // The initial velocity of the particles. Default: (0, 0, 0).
    velocityVariance:            TVector3; // The variance in particle velocity. Default: (0, 0, 0).
    initialAngularVelocity:      TVector3; // The initial angular velocity of the particles in Euler angles (degrees). Default: (0, 0, 0).
    angularVelocityVariance:     TVector3; // The variance in angular velocity. Default: (0, 0, 0).
    lifetime:                    Single;   // The lifetime of the particles in seconds. Default: 1.0.
    lifetimeVariance:            Single;   // The variance in lifetime in seconds. Default: 0.0.
    emissionTimer:               Single;   // Use to control automatic emission, should not be modified manually.
    emissionRate:                Single;   // The rate of particle emission in particles per second. Default: 10.0
    spreadAngle:                 Single;   // The angle of propagation of the particles in a cone (degrees). Default: 0.0
    scaleOverLifetime:           PR3D_InterpolationCurve; // Curve controlling the scale evolution of the particles over their lifetime. Default: NULL.
    speedOverLifetime:           PR3D_InterpolationCurve; // Curve controlling the speed evolution of the particles over their lifetime. Default: NULL.
    opacityOverLifetime:         PR3D_InterpolationCurve; // Curve controlling the opacity evolution of the particles over their lifetime. Default: NULL.
    angularVelocityOverLifetime: PR3D_InterpolationCurve; // Curve controlling the angular velocity evolution of the particles over their lifetime. Default: NULL.
    aabb:                        TBoundingBox; // For frustum culling. Defaults to a large AABB; compute manually via `R3D_CalculateParticleSystemBoundingBox` after setup.
    autoEmission:                Boolean; // Indicates whether particle emission is automatic when calling `R3D_UpdateParticleSystem`.
                                          // If false, emission is manual using `R3D_EmitParticle`. Default: true.
  end;

{$ENDREGION}

// --------------------------------------------
// CORE: Init And Config Functions
// --------------------------------------------

(*
 * @brief Initializes the rendering engine.
 *
 * This function sets up the internal rendering system with the provided resolution
 * and state flags, which define the internal behavior. These flags can be modified
 * later via R3D_SetState.
 *
 * @param resWidth Width of the internal resolution.
 * @param resHeight Height of the internal resolution.
 * @param flags Flags indicating internal behavior (modifiable via R3D_SetState).
 *)
procedure R3D_Init(resWidth, resHeight: Integer; flags: R3D_Flags); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_Init';

(*
 * @brief Closes the rendering engine and deallocates all resources.
 *
 * This function shuts down the rendering system and frees all allocated memory,
 * including the resources associated with the created lights.
 *)
procedure R3D_Close; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_Close';

(*
 * @brief Checks if a specific internal state flag is set.
 *
 * @param flag The state flag to check.
 * @return True if the flag is set, false otherwise.
 *)
function R3D_HasState(flag: R3D_Flags): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_HasState';

(*
 * @brief Sets internal state flags for the rendering engine.
 *
 * This function modifies the behavior of the rendering engine by setting one or more
 * state flags. Flags can be later cleared with R3D_ClearState.
 *
 * @param flags The flags to set.
 *)
procedure R3D_SetState(flags: R3D_Flags); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetState';

(*
 * @brief Clears specific internal state flags.
 *
 * This function clears one or more previously set state flags, modifying the
 * behavior of the rendering engine accordingly.
 *
 * @param flags The flags to clear.
 *)
procedure R3D_ClearState(flags: R3D_Flags); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ClearState';

(*
 * @brief Gets the current internal resolution.
 *
 * This function retrieves the current internal resolution being used by the
 * rendering engine.
 *
 * @param width Pointer to store the width of the internal resolution.
 * @param height Pointer to store the height of the internal resolution.
 *)
procedure R3D_GetResolution(width, height: PInteger); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetResolution';

(*
 * @brief Updates the internal resolution.
 *
 * This function changes the internal resolution of the rendering engine. Note that
 * this process destroys and recreates all framebuffers, which may be a slow operation.
 *
 * @param width The new width for the internal resolution.
 * @param height The new height for the internal resolution.
 *
 * @warning This function may be slow due to the destruction and recreation of framebuffers.
 *)
procedure R3D_UpdateResolution(width, height: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateResolution';

(*
 * @brief Sets a custom render target.
 *
 * This function allows rendering to a custom framebuffer instead of the main one.
 * Passing `NULL` will revert back to rendering to the main framebuffer.
 *
 * @param target The custom render target (can be NULL to revert to the default framebuffer).
 *)
procedure R3D_SetRenderTarget(target: PRenderTexture); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetRenderTarget';

(*
 * @brief Defines the bounds of the scene for directional light calculations.
 *
 * This function sets the scene bounds used to determine which areas should be illuminated
 * by directional lights. It is the user's responsibility to calculate and provide the
 * correct bounds.
 *
 * @param sceneBounds The bounding box defining the scene's dimensions.
 *)
procedure R3D_SetSceneBounds(sceneBounds: TBoundingBox); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSceneBounds';

(*
 * @brief Sets the default texture filtering mode.
 *
 * This function defines the default texture filter that will be applied to all subsequently
 * loaded textures, including those used in materials, sprites, and other resources.
 *
 * If a trilinear or anisotropic filter is selected, mipmaps will be automatically generated
 * for the textures, but they will not be generated when using nearest or bilinear filtering.
 *
 * The default texture filter mode is `TEXTURE_FILTER_TRILINEAR`.
 *
 * @param filter The texture filtering mode to be applied by default.
 *)
procedure R3D_SetTextureFilter(filter: TTextureFilter); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetTextureFilter';

// --------------------------------------------
// CORE: Drawing Functions
// --------------------------------------------

(*
 * @brief Begins a rendering session for a 3D camera.
 *
 * This function starts a rendering session, preparing the engine to handle subsequent
 * draw calls using the provided camera settings.
 *
 * @param camera The camera to use for rendering the scene.
 *)
procedure R3D_Begin(camera: TCamera3D); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_Begin';

(*
 * @brief Ends the current rendering session.
 *
 * This function signals the end of a rendering session, at which point the engine
 * will process all necessary render passes and output the final result to the main
 * or custom framebuffer.
 *)
procedure R3D_End; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_End';

(*
 * @brief Draws a mesh with a specified material and transformation.
 *
 * This function renders a mesh with the provided material and transformation matrix.
 *
 * @param mesh A pointer to the mesh to render. Cannot be NULL.
 * @param material A pointer to the material to apply to the mesh. Can be NULL, default material will be used.
 * @param transform The transformation matrix to apply to the mesh.
 *)
procedure R3D_DrawMesh(mesh: PR3D_Mesh; material: PR3D_Material; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMesh';

(*
 * @brief Draws a mesh with instancing support.
 *
 * This function renders a mesh multiple times with different transformation matrices
 * for each instance.
 *
 * @param mesh A pointer to the mesh to render. Cannot be NULL.
 * @param material A pointer to the material to apply to the mesh. Can be NULL, default material will be used.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawMeshInstanced(mesh: PR3D_Mesh; material: PR3D_Material; instanceTransforms: PMatrix; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstanced';

(*
 * @brief Draws a mesh with instancing support and different colors per instance.
 *
 * This function renders a mesh multiple times with different transformation matrices
 * and different colors for each instance.
 *
 * @param mesh A pointer to the mesh to render. Cannot be NULL.
 * @param material A pointer to the material to apply to the mesh. Can be NULL, default material will be used.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceColors Array of colors for each instance. Can be NULL if no per-instance colors are needed.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawMeshInstancedEx(mesh: PR3D_Mesh; material: PR3D_Material; instanceTransforms: PMatrix; instanceColors: PColor; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstancedEx';

(*
 * @brief Draws a mesh with instancing support, a global transformation, and different colors per instance.
 *
 * This function renders a mesh multiple times using instancing, with a global transformation
 * applied to all instances, and individual transformation matrices and colors for each instance.
 * Each instance can have its own position, rotation, scale, and color while sharing the same mesh
 * and material.
 *
 * @param mesh A pointer to the mesh to render. Cannot be NULL.
 * @param material A pointer to the material to apply to the mesh. Can be NULL, default material will be used.
 * @param globalAabb Optional bounding box encompassing all instances, in local space. Used for frustum culling.
 *                   Can be NULL to disable culling. Will be transformed by the global matrix if necessary.
 * @param globalTransform The global transformation matrix applied to all instances.
 * @param instanceTransforms Pointer to an array of transformation matrices for each instance, allowing unique transformations. Cannot be NULL.
 * @param transformsStride The stride (in bytes) between consecutive transformation matrices in the array.
 *                         Set to 0 if the matrices are tightly packed (stride equals sizeof(Matrix)).
 *                         If matrices are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceColors Pointer to an array of colors for each instance, allowing unique colors. Can be NULL if no per-instance colors are needed.
 * @param colorsStride The stride (in bytes) between consecutive colors in the array.
 *                     Set to 0 if the colors are tightly packed (stride equals sizeof(Color)).
 *                     If colors are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawMeshInstancedPro(mesh: PR3D_Mesh; material: PR3D_Material; globalAabb: PBoundingBox; globalTransform: TMatrix; instanceTransforms: PMatrix; transformsStride: Integer; instanceColors: PColor; colorsStride: Integer; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstancedPro';

(*
 * @brief Draws a model at a specified position and scale.
 *
 * This function renders a model at the given position with the specified scale factor.
 *
 * @param model A pointer to the model to render.
 * @param position The position to place the model at.
 * @param scale The scale factor to apply to the model.
 *)
procedure R3D_DrawModel(model: PR3D_Model; position: TVector3; scale: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModel';

(*
 * @brief Draws a model with advanced transformation options.
 *
 * This function renders a model with a specified position, rotation axis, rotation
 * angle, and scale. It provides more control over how the model is transformed before
 * rendering.
 *
 * @param model A pointer to the model to render.
 * @param position The position to place the model at.
 * @param rotationAxis The axis of rotation for the model.
 * @param rotationAngle The angle to rotate the model.
 * @param scale The scale factor to apply to the model.
 *)
procedure R3D_DrawModelEx(model: PR3D_Model; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelEx';

(*
 * @brief Draws a model using a transformation matrix.
 *
 * This function renders a model using a custom transformation matrix, allowing full control
 * over the model's position, rotation, scale, and skew. It is intended for advanced rendering
 * scenarios where a single matrix defines the complete transformation.
 *
 * @param model A pointer to the model to render.
 * @param transform A transformation matrix that defines how to position, rotate, and scale the model.
 *)
procedure R3D_DrawModelPro(model: PR3D_Model; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelPro';

(*
 * @brief Draws a model with instancing support.
 *
 * This function renders a model multiple times with different transformation matrices
 * for each instance.
 *
 * @param model A pointer to the model to render. Cannot be NULL.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawModelInstanced(const model: PR3D_Model; const instanceTransforms: PMatrix; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelInstanced';

(*
 * @brief Draws a model with instancing support and different colors per instance.
 *
 * This function renders a model multiple times with different transformation matrices
 * and different colors for each instance.
 *
 * @param model A pointer to the model to render. Cannot be NULL.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceColors Array of colors for each instance. Can be NULL if no per-instance colors are needed.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawModelInstancedEx(const model: PR3D_Model; const instanceTransforms: PMatrix; const instanceColors: PColorB; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelInstancedEx';

(*
 * @brief Draws a model with instancing support, a global transformation, and different colors per instance.
 *
 * This function renders a model multiple times using instancing, with a global transformation
 * applied to all instances, and individual transformation matrices and colors for each instance.
 * Each instance can have its own position, rotation, scale, and color while sharing the same model.
 *
 * @param model A pointer to the model to render. Cannot be NULL.
 * @param globalAabb Optional bounding box encompassing all instances, in local space. Used for frustum culling.
 *                   Can be NULL to disable culling. Will be transformed by the global matrix if necessary.
 * @param globalTransform The global transformation matrix applied to all instances.
 * @param instanceTransforms Pointer to an array of transformation matrices for each instance, allowing unique transformations. Cannot be NULL.
 * @param transformsStride The stride (in bytes) between consecutive transformation matrices in the array.
 *                         Set to 0 if the matrices are tightly packed (stride equals sizeof(Matrix)).
 *                         If matrices are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceColors Pointer to an array of colors for each instance, allowing unique colors. Can be NULL if no per-instance colors are needed.
 * @param colorsStride The stride (in bytes) between consecutive colors in the array.
 *                     Set to 0 if the colors are tightly packed (stride equals sizeof(Color)).
 *                     If colors are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawModelInstancedPro(const model: PR3D_Model;
                                      const globalAabb: PBoundingBox; globalTransform: TMatrix;
                                      const instanceTransforms: PMatrix; transformsStride: Integer;
                                      const instanceColors: PColorB; colorsStride: Integer;
                                      instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelInstancedPro';

(*
 * @brief Draws a sprite at a specified position.
 *
 * This function renders a sprite in 3D space at the given position.
 * It supports negative scaling to flip the sprite.
 *
 * @param sprite A pointer to the sprite to render.
 * @param position The position to place the sprite at.
 *)
procedure R3D_DrawSprite(sprite: PR3D_Sprite; position: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSprite';

(*
 * @brief Draws a sprite with size and rotation options.
 *
 * This function allows rendering a sprite with a specified size and rotation.
 * It supports negative size values for flipping the sprite.
 *
 * @param sprite A pointer to the sprite to render.
 * @param position The position to place the sprite at.
 * @param size The size of the sprite (negative values flip the sprite).
 * @param rotation The rotation angle in degrees.
 *)
procedure R3D_DrawSpriteEx(sprite: PR3D_Sprite; position: TVector3; size: TVector2; rotation: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpriteEx';

(*
 * @brief Draws a sprite with full transformation control.
 *
 * This function provides advanced transformation options, allowing
 * customization of size, rotation axis, and rotation angle.
 * It supports all billboard modes, or can be drawn without billboarding.
 *
 * @param sprite A pointer to the sprite to render.
 * @param position The position to place the sprite at.
 * @param size The size of the sprite (negative values flip the sprite).
 * @param rotationAxis The axis around which the sprite rotates.
 * @param rotationAngle The angle to rotate the sprite around the given axis.
 *)
procedure R3D_DrawSpritePro(sprite: PR3D_Sprite; position, size: TVector2; rotationAxis: TVector3; rotationAngle: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpritePro';

(*
 * @brief Draws a 3D sprite with instancing support.
 *
 * This function renders a 3D sprite multiple times with different transformation matrices
 * for each instance.
 *
 * @param sprite A pointer to the sprite to render. Cannot be NULL.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawSpriteInstanced(sprite: PR3D_Sprite; instanceTransforms: PMatrix; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpriteInstanced';

(*
 * @brief Draws a 3D sprite with instancing support and different colors per instance.
 *
 * This function renders a 3D sprite multiple times with different transformation matrices
 * and different colors for each instance.
 *
 * @param sprite A pointer to the sprite to render. Cannot be NULL.
 * @param instanceTransforms Array of transformation matrices for each instance. Cannot be NULL.
 * @param instanceColors Array of colors for each instance. Can be NULL if no per-instance colors are needed.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawSpriteInstancedEx(sprite: PR3D_Sprite; instanceTransforms: PMatrix; instanceColors: PColor; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpriteInstancedEx';

(*
 * @brief Draws a 3D sprite with instancing support, a global transformation, and different colors per instance.
 *
 * This function renders a 3D sprite multiple times using instancing, with a global transformation
 * applied to all instances, and individual transformation matrices and colors for each instance.
 * Each instance can have its own position, rotation, scale, and color while sharing the same sprite.
 *
 * @param sprite A pointer to the sprite to render. Cannot be NULL.
 * @param globalAabb Optional bounding box encompassing all instances, in local space. Used for frustum culling.
 *                   Can be NULL to disable culling. Will be transformed by the global matrix if provided.
 * @param globalTransform The global transformation matrix applied to all instances.
 * @param instanceTransforms Pointer to an array of transformation matrices for each instance, allowing unique transformations. Cannot be NULL.
 * @param transformsStride The stride (in bytes) between consecutive transformation matrices in the array.
 *                         Set to 0 if the matrices are tightly packed (stride equals sizeof(Matrix)).
 *                         If matrices are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceColors Pointer to an array of colors for each instance, allowing unique colors. Can be NULL if no per-instance colors are needed.
 * @param colorsStride The stride (in bytes) between consecutive colors in the array.
 *                     Set to 0 if the colors are tightly packed (stride equals sizeof(Color)).
 *                     If colors are embedded in a struct, set to the size of the struct or the actual byte offset between elements.
 * @param instanceCount The number of instances to render. Must be greater than 0.
 *)
procedure R3D_DrawSpriteInstancedPro(sprite: PR3D_Sprite; globalAabb: PBoundingBox; globalTransform: TMatrix; instanceTransforms: PMatrix; transformsStride: Integer; instanceColors: PColor; colorsStride: Integer; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpriteInstancedPro';

(*
 * @brief Renders the current state of a CPU-based particle system.
 *
 * This function draws the particles of a CPU-simulated particle system
 * in their current state. It does not modify the simulation or update
 * particle properties such as position, velocity, or lifetime.
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be rendered.
 *               The particle system must be properly initialized and updated
 *               to the desired state before calling this function.
 * @param mesh A pointer to the mesh used to represent each particle. Cannot be NULL.
 * @param material A pointer to the material applied to the particle mesh. Can be NULL, default material will be used.
 *)
procedure R3D_DrawParticleSystem(system: PR3D_ParticleSystem; mesh: PR3D_Mesh; material: PR3D_Material); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawParticleSystem';

(*
 * @brief Renders the current state of a CPU-based particle system with a global transformation.
 *
 * This function is similar to `R3D_DrawParticleSystem`, but it applies an additional
 * global transformation to all particles. This is useful for rendering particle effects
 * in a transformed space (e.g., attached to a moving object).
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be rendered.
 *               The particle system must be properly initialized and updated
 *               to the desired state before calling this function.
 * @param mesh A pointer to the mesh used to represent each particle. Cannot be NULL.
 * @param material A pointer to the material applied to the particle mesh. Can be NULL, default material will be used.
 * @param transform A transformation matrix applied to all particles.
 *)
procedure R3D_DrawParticleSystemEx(system: PR3D_ParticleSystem; mesh: PR3D_Mesh; material: PR3D_Material; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawParticleSystemEx';

// --------------------------------------------
// MODEL: Mesh Functions
// --------------------------------------------

(*
 * @brief Generate a polygon mesh with specified number of sides.
 *
 * Creates a regular polygon mesh centered at the origin in the XY plane.
 * The polygon is generated with vertices evenly distributed around a circle.
 *
 * @param sides Number of sides for the polygon (minimum 3).
 * @param radius Radius of the circumscribed circle.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated polygon mesh structure.
 *)
function R3D_GenMeshPoly(sides: Integer; radius: Single; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshPoly';

(*
 * @brief Generate a plane mesh with specified dimensions and resolution.
 *
 * Creates a flat plane mesh in the XZ plane, centered at the origin.
 * The mesh can be subdivided for higher resolution or displacement mapping.
 *
 * @param width Width of the plane along the X axis.
 * @param length Length of the plane along the Z axis.
 * @param resX Number of subdivisions along the X axis.
 * @param resZ Number of subdivisions along the Z axis.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated plane mesh structure.
 *)
function R3D_GenMeshPlane(width, length: Single; resX, resZ: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshPlane';

(*
 * @brief Generate a cube mesh with specified dimensions.
 *
 * Creates a cube mesh centered at the origin with the specified width, height, and length.
 * Each face consists of two triangles with proper normals and texture coordinates.
 *
 * @param width Width of the cube along the X axis.
 * @param height Height of the cube along the Y axis.
 * @param length Length of the cube along the Z axis.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated cube mesh structure.
 *)
function R3D_GenMeshCube(width, height, length: Single; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshCube';

(*
 * @brief Generate a sphere mesh with specified parameters.
 *
 * Creates a UV sphere mesh centered at the origin using latitude-longitude subdivision.
 * Higher ring and slice counts produce smoother spheres but with more vertices.
 *
 * @param radius Radius of the sphere.
 * @param rings Number of horizontal rings (latitude divisions).
 * @param slices Number of vertical slices (longitude divisions).
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated sphere mesh structure.
 *)
function R3D_GenMeshSphere(radius: Single; rings, slices: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshSphere';

(*
 * @brief Generate a hemisphere mesh with specified parameters.
 *
 * Creates a half-sphere mesh (dome) centered at the origin, extending upward in the Y axis.
 * Uses the same UV sphere generation technique as R3D_GenMeshSphere but only the upper half.
 *
 * @param radius Radius of the hemisphere.
 * @param rings Number of horizontal rings (latitude divisions).
 * @param slices Number of vertical slices (longitude divisions).
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated hemisphere mesh structure.
 *)
function R3D_GenMeshHemiSphere(radius: Single; rings, slices: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshHemiSphere';

(*
 * @brief Generate a cylinder mesh with specified parameters.
 *
 * Creates a cylinder mesh centered at the origin, extending along the Y axis.
 * The cylinder includes both top and bottom caps and smooth side surfaces.
 *
 * @param radius Radius of the cylinder base.
 * @param height Height of the cylinder along the Y axis.
 * @param slices Number of radial subdivisions around the cylinder.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated cylinder mesh structure.
 *)
function R3D_GenMeshCylinder(radius, height: Single; slices: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshCylinder';

(*
 * @brief Generate a cone mesh with specified parameters.
 *
 * Creates a cone mesh with its base centered at the origin and apex pointing upward along the Y axis.
 * The cone includes a circular base and smooth tapered sides.
 *
 * @param radius Radius of the cone base.
 * @param height Height of the cone along the Y axis.
 * @param slices Number of radial subdivisions around the cone base.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated cone mesh structure.
 *)
function R3D_GenMeshCone(radius, height: Single; slices: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshCone';

(*
 * @brief Generate a torus mesh with specified parameters.
 *
 * Creates a torus (donut shape) mesh centered at the origin in the XZ plane.
 * The torus is defined by a major radius (distance from center to tube center)
 * and a minor radius (tube thickness).
 *
 * @param radius Major radius of the torus (distance from center to tube center).
 * @param size Minor radius of the torus (tube thickness/radius).
 * @param radSeg Number of segments around the major radius.
 * @param sides Number of sides around the tube cross-section.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated torus mesh structure.
 *)
function R3D_GenMeshTorus(radius, size: Single; radSeg, sides: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshTorus';

(*
 * @brief Generate a trefoil knot mesh with specified parameters.
 *
 * Creates a trefoil knot mesh, which is a mathematical knot shape.
 * Similar to a torus but with a twisted, knotted topology.
 *
 * @param radius Major radius of the knot.
 * @param size Minor radius (tube thickness) of the knot.
 * @param radSeg Number of segments around the major radius.
 * @param sides Number of sides around the tube cross-section.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated trefoil knot mesh structure.
 *)
function R3D_GenMeshKnot(radius, size: Single; radSeg, sides: Integer; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshKnot';

(*
 * @brief Generate a terrain mesh from a heightmap image.
 *
 * Creates a terrain mesh by interpreting the brightness values of a heightmap image
 * as height values. The resulting mesh represents a 3D terrain surface.
 *
 * @param heightmap Image containing height data (grayscale values represent elevation).
 * @param size 3D vector defining the terrain dimensions (width, max height, depth).
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated heightmap terrain mesh structure.
 *)
function R3D_GenMeshHeightmap(heightmap: TImage; size: TVector3; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshHeightmap';

(*
 * @brief Generate a voxel-style mesh from a cubicmap image.
 *
 * Creates a mesh composed of cubes based on a cubicmap image, where each pixel
 * represents the presence or absence of a cube at that position. Useful for
 * creating voxel-based or block-based geometry.
 *
 * @param cubicmap Image where pixel values determine cube placement.
 * @param cubeSize 3D vector defining the size of each individual cube.
 * @param upload If true, automatically uploads the mesh to GPU memory.
 *
 * @return Generated cubicmap mesh structure.
 *)
function R3D_GenMeshCubicmap(cubicmap: TImage; cubeSize: TVector3; upload: Boolean): TR3D_Mesh; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GenMeshCubicmap';

(*
 * @brief Free mesh data from both RAM and VRAM.
 *
 * Releases all memory associated with a mesh, including vertex data in RAM
 * and GPU buffers (VAO, VBO, EBO) if the mesh was uploaded to VRAM.
 * After calling this function, the mesh should not be used.
 *
 * @param mesh Pointer to the mesh structure to be freed.
 *)
procedure R3D_UnloadMesh(mesh: PR3D_Mesh); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadMesh';

(*
 * @brief Upload a mesh to GPU memory.
 *
 * This function uploads a mesh's vertex and (optional) index data to the GPU.
 * It creates and configures a VAO, VBO, and optionally an EBO if indices are provided.
 * All vertex attributes are interleaved in a single VBO.
 *
 * This function must only be called once per mesh. For updates, use R3D_UpdateMesh().
 *
 * @param mesh Pointer to the mesh structure containing vertex and index data.
 * @param dynamic If true, allocates buffers with GL_DYNAMIC_DRAW for later updates.
 * If false, uses GL_STATIC_DRAW for optimized static meshes.
 *
 * @return true if upload succeeded, false on error (e.g. invalid input or already uploaded).
 *)
function R3D_UploadMesh(mesh: PR3D_Mesh; dynamic: Boolean): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UploadMesh';

(*
 * @brief Update an already uploaded mesh on the GPU.
 *
 * This function updates the GPU-side data of a mesh previously uploaded with R3D_UploadMesh().
 * It replaces the vertex buffer contents using glBufferSubData.
 * If index data is present, it also updates or creates the index buffer (EBO).
 *
 * This function assumes the mesh was uploaded with the `dynamic` flag set to true.
 *
 * @param mesh Pointer to the mesh structure with updated vertex and/or index data.
 *
 * @return true if update succeeded, false on error (e.g. mesh not uploaded or invalid data).
 *)
function R3D_UpdateMesh(mesh: PR3D_Mesh): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateMesh';

(*
 * @brief Recalculate the bounding box of a mesh.
 *
 * Computes and updates the axis-aligned bounding box (AABB) of the mesh
 * by examining all vertex positions. This is useful after mesh deformation
 * or when the bounding box needs to be refreshed.
 *
 * @param mesh Pointer to the mesh structure whose bounding box will be updated.
 *)
procedure R3D_UpdateMeshBoundingBox(mesh: PR3D_Mesh); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateMeshBoundingBox';

// --------------------------------------------
// MODEL: Material Functions
// --------------------------------------------

(*
 * @brief Get the default material configuration.
 *
 * Returns a default material with standard properties and default textures.
 * This material can be used as a fallback or starting point for custom materials.
 *
 * @return Default material structure with standard properties.
 *)
function R3D_GetDefaultMaterial: TR3D_Material; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDefaultMaterial';

(*
 * @brief Unload a material and its associated textures.
 *
 * Frees all memory associated with a material, including its textures.
 * This function will unload all textures that are not default textures.
 *
 * @warning Only call this function if you are certain that the textures
 * are not shared with other materials or objects, as this will permanently
 * free the texture data.
 *
 * @param material Pointer to the material structure to be unloaded.
 *)
procedure R3D_UnloadMaterial(material: PR3D_Material); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadMaterial';

// --------------------------------------------
// MODEL: Model Functions
// --------------------------------------------

(*
 * @brief Load a 3D model from a file.
 *
 * Loads a 3D model from the specified file path. Supports various 3D file formats
 * and automatically parses meshes, materials, and texture references.
 *
 * @param filePath Path to the 3D model file to load.
 *
 * @return Loaded model structure containing meshes and materials.
 *)
function R3D_LoadModel(filePath: PChar): TR3D_Model; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadModel';

(*
 * @brief Load a 3D model from memory buffer.
 *
 * Loads a 3D model from a memory buffer containing the file data.
 * Useful for loading models from embedded resources or network streams.
 *
 * @param fileType String indicating the file format (e.g., "obj", "fbx", "gltf").
 * @param data Pointer to the memory buffer containing the model data.
 * @param size Size of the data buffer in bytes.
 *
 * @return Loaded model structure containing meshes and materials.
 *
 * @note External dependencies (e.g., textures or linked resources) are not supported.
 *       The model data must be fully self-contained. Use embedded formats like .glb to ensure compatibility.
 *)
function R3D_LoadModelFromMemory(fileType: PChar; data: Pointer; size: Cardinal): TR3D_Model; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadModelFromMemory';

(*
 * @brief Create a model from a single mesh.
 *
 * Creates a model structure containing a single mesh with a default material.
 * This is useful for procedurally generated meshes or simple geometry.
 *
 * @warning The model's bounding box calculation assumes that the mesh's
 * bounding box has already been computed. Call R3D_UpdateMeshBoundingBox()
 * on the mesh before using this function if needed.
 *
 * @param mesh Pointer to the mesh to be wrapped in a model structure.
 *
 * @return Model structure containing the specified mesh.
 *)
function R3D_LoadModelFromMesh(mesh: PR3D_Mesh): TR3D_Model; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadModelFromMesh';

(*
 * @brief Unload a model and optionally its materials.
 *
 * Frees all memory associated with a model, including its meshes.
 * Materials can be optionally unloaded as well.
 *
 * @param model Pointer to the model structure to be unloaded.
 * @param unloadMaterials If true, also unloads all materials associated with the model.
 * Set to false if textures are still being used elsewhere to avoid freeing shared resources.
 *)
procedure R3D_UnloadModel(model: PR3D_Model; unloadMaterials: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadModel';

(*
 * @brief Update the bounding box of a model.
 *
 * Recalculates the axis-aligned bounding box (AABB) of the entire model
 * by examining all meshes within the model. Optionally updates individual
 * mesh bounding boxes as well.
 *
 * @param model Pointer to the model structure whose bounding box will be updated.
 * @param updateMeshBoundingBoxes If true, also updates the bounding box of each
 * individual mesh within the model before calculating the model's overall bounding box.
 *)
procedure R3D_UpdateModelBoundingBox(model: PR3D_Model; updateMeshBoundingBoxes: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateModelBoundingBox';

(*
 * @brief Loads model animations from a supported file format (e.g., GLTF, IQM).
 *
 * This function parses animation data from the given model file and returns an array
 * of R3D_ModelAnimation structs. The caller is responsible for freeing the returned data
 * using R3D_UnloadModelAnimations().
 *
 * @param fileName Path to the model file containing animation(s).
 * @param animCount Pointer to an integer that will receive the number of animations loaded.
 * @param targetFrameRate Desired frame rate (FPS) to sample the animation at. For example, 30 or 60.
 * @return Pointer to a dynamically allocated array of R3D_ModelAnimation. NULL on failure.
 *)
function R3D_LoadModelAnimations(fileName: PChar; animCount: PInteger; targetFrameRate: Integer): PR3D_ModelAnimation; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadModelAnimations';

(*
 * @brief Loads model animations from memory data in a supported format (e.g., GLTF, IQM).
 *
 * This function parses animation data from the given memory buffer and returns an array
 * of R3D_ModelAnimation structs. The caller is responsible for freeing the returned data
 * using R3D_UnloadModelAnimations().
 *
 * @param fileType File format hint (e.g., "gltf", "iqm", ".gltf"). The leading dot is optional.
 * @param data Pointer to the model data in memory.
 * @param size Size of the data buffer in bytes.
 * @param animCount Pointer to an integer that will receive the number of animations loaded.
 * @param targetFrameRate Desired frame rate (FPS) to sample the animation at. For example, 30 or 60.
 * @return Pointer to a dynamically allocated array of R3D_ModelAnimation. NULL on failure.
 *)
function R3D_LoadModelAnimationsFromMemory(const fileType: PChar; const data: Pointer; size: Cardinal; animCount: PInteger; targetFrameRate: integer): PR3D_ModelAnimation; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadModelAnimationsFromMemory';

(*
 * @brief Frees memory allocated for model animations.
 *
 * This should be called after you're done using animations loaded via R3D_LoadModelAnimations().
 *
 * @param animations Pointer to the animation array to free.
 * @param animCount Number of animations in the array.
 *)
procedure R3D_UnloadModelAnimations(animations: PR3D_ModelAnimation; animCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadModelAnimations';

(*
 * @brief Finds and returns a pointer to a named animation within the array.
 *
 * Searches the given array of animations for one that matches the specified name.
 *
 * @param animations Array of animations to search.
 * @param animCount Number of animations in the array.
 * @param name Name of the animation to find (case-sensitive).
 * @return Pointer to the matching animation, or NULL if not found.
 *)
function R3D_GetModelAnimation(animations: PR3D_ModelAnimation; animCount: Integer; name: PChar): PR3D_ModelAnimation; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetModelAnimation';

(*
 * @brief Logs the names of all animations in the array (for debugging or inspection).
 *
 * Prints the animation names (and possibly other info) to the standard output or debug console.
 *
 * @param animations Array of animations to list.
 * @param animCount Number of animations in the array.
 *)
procedure R3D_ListModelAnimations(animations: PR3D_ModelAnimation; animCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ListModelAnimations';

(*
 * @brief Sets the scaling factor applied to models on loading.
 *
 * The functions sets the scaling factor to be used when loading models. This value
 * is only applied to models loaded after this value is set.
 *
 * @param value Scaling factor to be used (i.e. 0.01 for meters to centimeters).
 *)
procedure R3D_SetModelImportScale(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetModelImportScale';

// --------------------------------------------
// LIGHTING: Lights Config Functions
// --------------------------------------------

(*
 * @brief Creates a new light of the specified type.
 *
 * This function creates a light of the given type. The light must be destroyed
 * manually when no longer needed by calling `R3D_DestroyLight`.
 *
 * @param type The type of light to create (directional, spot or omni-directional).
 * @return The ID of the created light.
 *)
function R3D_CreateLight(lightType: R3D_LightType): TR3D_Light; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_CreateLight';

(*
 * @brief Destroys the specified light.
 *
 * This function deallocates the resources associated with the light and makes
 * the light ID invalid. It must be called after the light is no longer needed.
 *
 * @param id The ID of the light to destroy.
 *)
procedure R3D_DestroyLight(id: TR3D_Light); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DestroyLight';

(*
 * @brief Checks if a light exists.
 *
 * This function checks if the specified light ID is valid and if the light exists.
 *
 * @param id The ID of the light to check.
 * @return True if the light exists, false otherwise.
 *)
function R3D_IsLightExist(id: TR3D_Light): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsLightExist';

(*
 * @brief Gets the type of a light.
 *
 * This function returns the type of the specified light (directional, spot or omni-directional).
 *
 * @param id The ID of the light.
 * @return The type of the light.
 *)
function R3D_GetLightType(id: TR3D_Light): R3D_LightType; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightType';

(*
 * @brief Checks if a light is active.
 *
 * This function checks whether the specified light is currently active (enabled or disabled).
 *
 * @param id The ID of the light to check.
 * @return True if the light is active, false otherwise.
 *)
function R3D_IsLightActive(id: TR3D_Light): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsLightActive';

(*
 * @brief Toggles the state of a light (active or inactive).
 *
 * This function toggles the state of the specified light, turning it on if it is off,
 * or off if it is on.
 *
 * @param id The ID of the light to toggle.
 *)
procedure R3D_ToggleLight(id: TR3D_Light); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ToggleLight';

(*
 * @brief Sets the active state of a light.
 *
 * This function allows manually turning a light on or off by specifying its active state.
 *
 * @param id The ID of the light to set the active state for.
 * @param active True to activate the light, false to deactivate it.
 *)
procedure R3D_SetLightActive(id: TR3D_Light; active: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightActive';

(*
 * @brief Gets the color of a light.
 *
 * This function retrieves the color of the specified light as a `Color` structure.
 *
 * @param id The ID of the light.
 * @return The color of the light as a `Color` structure.
 *)
function R3D_GetLightColor(id: TR3D_Light): TColor; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightColor';

(*
 * @brief Gets the color of a light as a `Vector3`.
 *
 * This function retrieves the color of the specified light as a `Vector3`, where each
 * component (x, y, z) represents the RGB values of the light.
 *
 * @param id The ID of the light.
 * @return The color of the light as a `Vector3`.
 *)
function R3D_GetLightColorV(id: TR3D_Light): TVector3; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightColorV';

(*
 * @brief Sets the color of a light.
 *
 * This function sets the color of the specified light using a `Color` structure.
 *
 * @param id The ID of the light.
 * @param color The new color to set for the light.
 *)
procedure R3D_SetLightColor(id: TR3D_Light; color: TColor); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightColor';

(*
 * @brief Sets the color of a light using a `Vector3`.
 *
 * This function sets the color of the specified light using a `Vector3`, where each
 * component (x, y, z) represents the RGB values of the light.
 *
 * @param id The ID of the light.
 * @param color The new color to set for the light as a `Vector3`.
 *)
procedure R3D_SetLightColorV(id: TR3D_Light; color: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightColorV';

(*
 * @brief Gets the position of a light.
 *
 * This function retrieves the position of the specified light.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The position of the light as a `Vector3`.
 *)
function R3D_GetLightPosition(id: TR3D_Light): TVector3; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightPosition';

(*
 * @brief Sets the position of a light.
 *
 * This function sets the position of the specified light.
 * Only applicable to spot lights or omni-lights.
 *
 * @note Has no effect for directional lights.
 *       If called on a directional light,
 *       a warning will be logged.
 *
 * @param id The ID of the light.
 * @param position The new position to set for the light.
 *)
procedure R3D_SetLightPosition(id: TR3D_Light; position: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightPosition';

(*
 * @brief Gets the direction of a light.
 *
 * This function retrieves the direction of the specified light.
 * Only applicable to directional lights or spot lights.
 *
 * @param id The ID of the light.
 * @return The direction of the light as a `Vector3`.
 *)
function R3D_GetLightDirection(id: TR3D_Light): TVector3; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightDirection';

(*
 * @brief Sets the direction of a light.
 *
 * This function sets the direction of the specified light.
 * Only applicable to directional lights or spot lights.
 *
 * @note Has no effect for omni-directional lights.
 *       If called on an omni-directional light,
 *       a warning will be logged.
 *
 * @param id The ID of the light.
 * @param direction The new direction to set for the light.
 *                  The vector is automatically normalized.
 *)
procedure R3D_SetLightDirection(id: TR3D_Light; direction: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightDirection';

(*
 * @brief Sets the position and direction of a light to look at a target point.
 *
 * This function sets both the position and the direction of the specified light,
 * causing it to "look at" a given target point.
 *
 * @note - For directional lights, only the direction is updated (position is ignored).
 *       - For omni-directional lights, only the position is updated (direction is not calculated).
 *       - For spot lights, both position and direction are set accordingly.
 *       - This function does **not** emit any warning or log message.
 *
 * @param id The ID of the light.
 * @param position The position to set for the light.
 * @param target The point the light should look at.
 *)
procedure R3D_LightLookAt(id: TR3D_Light; position, target: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LightLookAt';

(*
 * @brief Gets the energy level of a light.
 *
 * This function retrieves the energy level (intensity) of the specified light.
 * Energy typically affects the brightness of the light.
 *
 * @param id The ID of the light.
 * @return The energy level of the light.
 *)
function R3D_GetLightEnergy(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightEnergy';

(*
 * @brief Sets the energy level of a light.
 *
 * This function sets the energy (intensity) of the specified light.
 * A higher energy value will result in a brighter light.
 *
 * @param id The ID of the light.
 * @param energy The new energy value to set for the light.
 *)
procedure R3D_SetLightEnergy(id: TR3D_Light; energy: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightEnergy';

(*
 * @brief Gets the specular intensity of a light.
 *
 * This function retrieves the current specular intensity of the specified light.
 * Specular intensity affects how shiny surfaces appear when reflecting the light.
 *
 * @param id The ID of the light.
 * @return The current specular intensity of the light.
 *)
function R3D_GetLightSpecular(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightSpecular';

(*
 * @brief Sets the specular intensity of a light.
 *
 * This function sets the specular intensity of the specified light.
 * Higher specular values result in stronger and sharper highlights on reflective surfaces.
 *
 * @param id The ID of the light.
 * @param specular The new specular intensity value to set for the light.
 *)
procedure R3D_SetLightSpecular(id: TR3D_Light; specular: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightSpecular';

(*
 * @brief Gets the range of a light.
 *
 * This function retrieves the range of the specified light, which determines how far the light can affect.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The range of the light.
 *)
function R3D_GetLightRange(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightRange';

(*
 * @brief Sets the range of a light.
 *
 * This function sets the range of the specified light.
 * The range determines how far the light can illuminate the scene before it fades out.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @param range The new range to set for the light.
 *)
procedure R3D_SetLightRange(id: TR3D_Light; range: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightRange';

(*
 * @brief Gets the attenuation factor of a light.
 *
 * This function retrieves the attenuation factor of the specified light.
 * Attenuation controls how the intensity of a light decreases with distance.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The attenuation factor of the light.
 *)
function R3D_GetLightAttenuation(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightAttenuation';

(*
 * @brief Sets the attenuation factor of a light.
 *
 * This function sets the attenuation factor of the specified light.
 * A higher attenuation value causes the light to lose intensity more quickly as the distance increases.
 * For a realistic effect, an attenuation factor of 2.0f is typically used.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @param attenuation The new attenuation factor to set for the light.
 *)
procedure R3D_SetLightAttenuation(id: TR3D_Light; attenuation: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightAttenuation';

(*
 * @brief Gets the inner cutoff angle of a spotlight.
 *
 * This function retrieves the inner cutoff angle of a spotlight.
 * The inner cutoff defines the cone of light where the light is at full intensity.
 *
 * @param id The ID of the light.
 * @return The inner cutoff angle in degrees of the spotlight.
 *)
function R3D_GetLightInnerCutOff(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightInnerCutOff';

(*
 * @brief Sets the inner cutoff angle of a spotlight.
 *
 * This function sets the inner cutoff angle of a spotlight.
 * The inner cutoff angle defines the cone where the light is at full intensity.
 * Anything outside this cone starts to fade.
 *
 * @param id The ID of the light.
 * @param degrees The new inner cutoff angle in degrees.
 *)
procedure R3D_SetLightInnerCutOff(id: TR3D_Light; degrees: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightInnerCutOff';

(*
 * @brief Gets the outer cutoff angle of a spotlight.
 *
 * This function retrieves the outer cutoff angle of a spotlight.
 * The outer cutoff defines the outer boundary of the light's cone, where the light starts to fade.
 *
 * @param id The ID of the light.
 * @return The outer cutoff angle in degrees of the spotlight.
 *)
function R3D_GetLightOuterCutOff(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightOuterCutOff';

(*
 * @brief Sets the outer cutoff angle of a spotlight.
 *
 * This function sets the outer cutoff angle of a spotlight.
 * The outer cutoff defines the boundary of the light's cone where the light intensity starts to gradually decrease.
 *
 * @param id The ID of the light.
 * @param degrees The new outer cutoff angle in degrees.
 *)
procedure R3D_SetLightOuterCutOff(id: TR3D_Light; degrees: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightOuterCutOff';

// --------------------------------------------
// LIGHTING: Shadow Config Functions
// --------------------------------------------

(*
 * @brief Enables shadow casting for a light and sets the resolution of its shadow map.
 *
 * This function enables shadow casting for a specified light and allocates a shadow map with the specified resolution.
 * Shadows can be rendered from the light based on this shadow map.
 *
 * @param id The ID of the light for which shadows should be enabled.
 * @param resolution The resolution of the shadow map to be used by the light.
 *)
procedure R3D_EnableShadow(id: TR3D_Light; resolution: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_EnableShadow';

(*
 * @brief Disables shadow casting for a light and optionally destroys its shadow map.
 *
 * This function disables shadow casting for the specified light and optionally frees the memory
 * used by its shadow map. If `destroyMap` is true, the shadow map will be destroyed, otherwise,
 * the map will be retained but the light will no longer cast shadows.
 *
 * @param id The ID of the light for which shadows should be disabled.
 * @param destroyMap Whether or not to destroy the shadow map associated with the light.
 *)
procedure R3D_DisableShadow(id: TR3D_Light; destroyMap: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DisableShadow';

(*
 * @brief Checks if shadow casting is enabled for a light.
 *
 * This function checks if shadow casting is currently enabled for the specified light.
 *
 * @param id The ID of the light.
 * @return True if shadow casting is enabled, false otherwise.
 *)
function R3D_IsShadowEnabled(id: TR3D_Light): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsShadowEnabled';

(*
 * @brief Checks if a light has an associated shadow map.
 *
 * This function checks if the specified light has a shadow map allocated for it.
 *
 * @param id The ID of the light.
 * @return True if the light has a shadow map, false otherwise.
 *)
function R3D_HasShadowMap(id: TR3D_Light): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_HasShadowMap';

(*
 * @brief Gets the shadow map update mode of a light.
 *
 * This function retrieves the current mode for updating the shadow map of a light. The mode can be:
 * - Interval: Updates the shadow map at a fixed interval.
 * - Continuous: Updates the shadow map continuously.
 * - Manual: Updates the shadow map manually (via explicit function calls).
 *
 * @param id The ID of the light.
 * @return The shadow map update mode.
 *)
function R3D_GetShadowUpdateMode(id: TR3D_Light): R3D_ShadowUpdateMode; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetShadowUpdateMode';

(*
 * @brief Sets the shadow map update mode of a light.
 *
 * This function sets the mode for updating the shadow map of the specified light.
 * The update mode controls when and how often the shadow map is refreshed.
 *
 * @param id The ID of the light.
 * @param mode The update mode to set for the shadow map (Interval, Continuous, or Manual).
 *)
procedure R3D_SetShadowUpdateMode(id: TR3D_Light; mode: R3D_ShadowUpdateMode); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetShadowUpdateMode';

(*
 * @brief Gets the frequency of shadow map updates for the interval update mode.
 *
 * This function retrieves the frequency (in milliseconds) at which the shadow map should be updated when
 * the interval update mode is enabled. This function is only relevant if the shadow map update mode is set
 * to "Interval".
 *
 * @param id The ID of the light.
 * @return The frequency in milliseconds at which the shadow map is updated.
 *)
function R3D_GetShadowUpdateFrequency(id: TR3D_Light): Integer; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetShadowUpdateFrequency';

(*
 * @brief Sets the frequency of shadow map updates for the interval update mode.
 *
 * This function sets the frequency (in milliseconds) at which the shadow map should be updated when
 * the interval update mode is enabled. This function is only relevant if the shadow map update mode is set
 * to "Interval".
 *
 * @param id The ID of the light.
 * @param msec The frequency in milliseconds at which to update the shadow map.
 *)
procedure R3D_SetShadowUpdateFrequency(id: TR3D_Light; msec: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetShadowUpdateFrequency';

(*
 * @brief Forces an immediate update of the shadow map during the next rendering pass.
 *
 * This function forces the shadow map of the specified light to be updated during the next call to `R3D_End`.
 * This is primarily used for the manual update mode, but may also work for the interval mode.
 *
 * @param id The ID of the light.
 *)
procedure R3D_UpdateShadowMap(id: TR3D_Light); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateShadowMap';

(*
 * @brief Retrieves the softness factor used to simulate penumbra in shadows.
 *
 * This function returns the current softness factor for the specified light's shadow map.
 * A higher softness value will produce softer shadow edges, simulating a broader penumbra,
 * while a lower value results in sharper shadows.
 *
 * @param id The ID of the light.
 * @return The softness factor currently set for the shadow (typically in the range [0.0, 1.0]).
 *)
function R3D_GetShadowSoftness(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetShadowSoftness';

(*
 * @brief Sets the softness factor used to simulate penumbra in shadows.
 *
 * This function adjusts the softness of the shadow edges for the specified light.
 * Increasing the softness value creates more diffuse, penumbra-like shadows.
 *
 * @param id The ID of the light.
 * @param softness The softness factor to apply (typically in the range [0.0, 1.0]).
 *)
procedure R3D_SetShadowSoftness(id: TR3D_Light; softness: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetShadowSoftness';

(*
 * @brief Gets the shadow bias of a light.
 *
 * This function retrieves the shadow bias value for the specified light. The shadow bias helps prevent shadow artifacts,
 * such as shadow acne, by slightly offsetting the depth comparisons used in shadow mapping.
 *
 * @param id The ID of the light.
 * @return The shadow bias value.
 *)
function R3D_GetShadowBias(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetShadowBias';

(*
 * @brief Sets the shadow bias of a light.
 *
 * This function sets the shadow bias value for the specified light. Adjusting the shadow bias can help avoid shadow
 * artifacts such as shadow acne by modifying the depth comparisons used in shadow mapping.
 *
 * @param id The ID of the light.
 * @param value The shadow bias value to set.
 *)
procedure R3D_SetShadowBias(id: TR3D_Light; value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetShadowBias';

// --------------------------------------------
// LIGHTING: Light Helper Functions
// --------------------------------------------

(*
 * @brief Returns the bounding box encompassing the light's area of influence.
 *
 * This function computes the axis-aligned bounding box (AABB) that encloses the
 * volume affected by the specified light, based on its type:
 *
 * - For spotlights, the bounding box encloses the light cone.
 * - For omni-directional lights, it encloses a sphere representing the light's range.
 * - For directional lights, it returns an infinite bounding box to represent global influence.
 *
 * This bounding box is primarily useful for spatial partitioning, culling, or visual debugging.
 *
 * @param light The light for which to compute the bounding box.
 *
 * @return A BoundingBox struct that encloses the light's influence volume.
 *)
function R3D_GetLightBoundingBox(light: TR3D_Light): TBoundingBox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightBoundingBox';

(*
 * @brief Draws the area of influence of the light in 3D space.
 *
 * This function visualizes the area affected by a light in 3D space.
 * It draws the light's influence, such as the cone for spotlights or the volume for omni-lights.
 * This function is only relevant for spotlights and omni-lights.
 *
 * @note This function should be called while using the default 3D rendering mode of raylib,
 *       not with R3D's rendering mode. It uses raylib's 3D drawing functions to render the light's shape.
 *
 * @param id The ID of the light.
 *)
procedure R3D_DrawLightShape(id: TR3D_Light); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawLightShape';

// --------------------------------------------
// PARTICLES: Particle System Functions
// --------------------------------------------

(*
 * @brief Loads a particle emitter system for the CPU.
 *
 * This function initializes a particle emitter system on the CPU with a specified maximum
 * number of particles. It prepares the necessary data structures and allocates memory
 * for managing the particles.
 *
 * @param maxParticles The maximum number of particles the system can handle at once.
 *                     This value determines the memory allocation and performance constraints.
 * @return A newly initialized `R3D_ParticleSystem` structure.
 *         The caller is responsible for properly managing and freeing the system when no longer needed.
 *)
function R3D_LoadParticleSystem(maxParticles: Integer): TR3D_ParticleSystem; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadParticleSystem';

(*
 * @brief Unloads the particle emitter system and frees allocated memory.
 *
 * This function deallocates the memory used by the particle emitter system and clears the associated resources.
 * It should be called when the particle system is no longer needed to prevent memory leaks.
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be unloaded.
 *)
procedure R3D_UnloadParticleSystem(system: PR3D_ParticleSystem); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadParticleSystem';

(*
 * @brief Emits a particle in the particle system.
 *
 * This function triggers the emission of a new particle in the particle system. It handles the logic of adding a new
 * particle to the system and initializing its properties based on the current state of the system.
 *
 * @param system A pointer to the `R3D_ParticleSystemCPU` where the particle will be emitted.
 * @return `true` if the particle was successfully emitted, `false` if the system is at full capacity and cannot emit more particles.
 *)
function R3D_EmitParticle(system: PR3D_ParticleSystem): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_EmitParticle';

(*
 * @brief Updates the particle emitter system by advancing particle positions.
 *
 * This function updates the positions and properties of particles in the system based on the elapsed time. It handles
 * simulation of particle movement, gravity, and other physics-based calculations.
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be updated.
 * @param deltaTime The time elapsed since the last update (in seconds).
 *)
procedure R3D_UpdateParticleSystem(system: PR3D_ParticleSystem; deltaTime: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateParticleSystem';

(*
 * @brief Computes and updates the AABB (Axis-Aligned Bounding Box) of a particle system.
 *
 * This function simulates the particle system to estimate the region of space it occupies.
 * It considers particle positions at mid-life and end-of-life to approximate the AABB,
 * which is then stored in the system's `aabb` field. This is useful for enabling frustum culling,
 * especially when the bounds are not known beforehand.
 *
 * @param system Pointer to the `R3D_ParticleSystem` to update.
 *)
procedure R3D_CalculateParticleSystemBoundingBox(system: PR3D_ParticleSystem); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_CalculateParticleSystemBoundingBox';

// --------------------------------------------
// CURVES: Interpolation Curves Functions
// --------------------------------------------

(*
 * @brief Load a sprite from a texture.
 *
 * This function creates a `R3D_Sprite` using the provided texture.
 * The texture will be used as the albedo of the sprite's material.
 * The default billboard mode applied to the material is `R3D_BILLBOARD_Y_AXIS`.
 *
 * @warning The lifetime of the provided texture is managed by the caller.
 *
 * @param texture The `Texture2D` to be used for the sprite.
 * @param xFrameCount The number of frames in the horizontal direction.
 * @param yFrameCount The number of frames in the vertical direction.
 *
 * @return A `R3D_Sprite` object initialized with the given texture and frame configuration.
 *)
function R3D_LoadSprite(texture: TTexture2D; xFrameCount, yFrameCount: Integer): TR3D_Sprite; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSprite';

(*
 * @brief Unload a sprite and free associated resources.
 *
 * This function releases the resources allocated for a `R3D_Sprite`.
 * It should be called when the sprite is no longer needed.
 *
 * @warning This function only unloads non-default textures from the sprite's material,
 * so make sure these textures are not shared with other material instances elsewhere.
 *
 * @param sprite A pointer to the `R3D_Sprite` to be unloaded.
 *)
procedure R3D_UnloadSprite(sprite: PR3D_Sprite); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadSprite';

(*
 * @brief Updates the animation of a sprite.
 *
 * This function updates the current frame of the sprite's animation based on the provided speed. The animation frames are read from
 * right to left, advancing the cursor to the next row after completing a line.
 *
 * @note The `speed` parameter can be calculated as the number of frames per second multiplied by `GetFrameTime()`.
 *
 * @param sprite A pointer to the `R3D_Sprite` to update.
 * @param speed The speed at which the animation progresses, in frames per second.
 *)
procedure R3D_UpdateSprite(sprite: PR3D_Sprite; speed: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateSprite';

(*
 * @brief Updates the animation of a sprite with specified frame boundaries.
 *
 * This function updates the current frame of the sprite's animation while restricting it between `firstFrame` and `lastFrame`.
 * This is useful for spritesheets containing multiple animations.
 *
 * @note The frames are read from right to left, and the cursor moves to the next row after completing a line.
 * @note The `speed` parameter can be calculated as the number of frames per second multiplied by `GetFrameTime()`.
 *
 * @param sprite A pointer to the `R3D_Sprite` to update.
 * @param firstFrame The first frame of the animation loop.
 * @param lastFrame The last frame of the animation loop.
 * @param speed The speed at which the animation progresses, in frames per second.
 *)
procedure R3D_UpdateSpriteEx(sprite: PR3D_Sprite; firstFrame, lastFrame: Integer; speed: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UpdateSpriteEx';

// --------------------------------------------
// CURVES: Interpolation Curves Functions
// --------------------------------------------

(*
 * @brief Loads an interpolation curve with a specified initial capacity.
 *
 * This function initializes an interpolation curve with the given capacity. The capacity represents the initial size of
 * the memory allocated for the curve. You can add keyframes to the curve using `R3D_AddKeyframe`. If adding a keyframe
 * exceeds the initial capacity, the system will automatically reallocate memory and double the initial capacity.
 *
 * @param capacity The initial capacity (size) of the interpolation curve. This is the number of keyframes that can be added
 *                 before a reallocation occurs.
 * @return An initialized interpolation curve with the specified capacity.
 *)
function R3D_LoadInterpolationCurve(capacity: Integer): TR3D_InterpolationCurve; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadInterpolationCurve';

(*
 * @brief Unloads the interpolation curve and frees the allocated memory.
 *
 * This function deallocates the memory associated with the interpolation curve and clears any keyframes stored in it.
 * It should be called when the curve is no longer needed to avoid memory leaks.
 *
 * @param curve The interpolation curve to be unloaded.
 *)
procedure R3D_UnloadInterpolationCurve(curve: TR3D_InterpolationCurve); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadInterpolationCurve';

(*
 * @brief Adds a keyframe to the interpolation curve.
 *
 * This function adds a keyframe to the given interpolation curve at a specific time and value. If the addition of the
 * keyframe requires reallocating memory and the reallocation fails, the previously allocated memory and keyframes are
 * preserved, but the new keyframe is not added.
 *
 * @param curve A pointer to the interpolation curve to which the keyframe will be added.
 * @param time The time at which the keyframe will be added.
 * @param value The value associated with the keyframe.
 * @return `true` if the keyframe was successfully added, or `false` if the reallocation failed.
 *)
function R3D_AddKeyframe(curve: PR3D_InterpolationCurve; time, value: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_AddKeyframe';

(*
 * @brief Evaluates the interpolation curve at a specific time.
 *
 * This function evaluates the value of the interpolation curve at a given time. The curve will interpolate between
 * keyframes based on the time provided.
 *
 * @param curve The interpolation curve to be evaluated.
 * @param time The time at which to evaluate the curve.
 * @return The value of the curve at the specified time.
 *)
function R3D_EvaluateCurve(curve: TR3D_InterpolationCurve; time: Single): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_EvaluateCurve';

// --------------------------------------------
// ENVIRONMENT: Background And Ambient
// --------------------------------------------

(*
 * @brief Sets the background color when no skybox is enabled.
 *
 * This function defines the background color to be used when no skybox is active.
 * The color will be used for the clear color of the scene.
 *
 * @param color The color to set as the background color.
 *)
procedure R3D_SetBackgroundColor(color: TColor); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBackgroundColor';

(*
 * @brief Sets the ambient light color when no skybox is enabled.
 *
 * This function defines the ambient light color to be used when no skybox is active.
 * It affects the overall lighting of the scene when no skybox is present.
 *
 * @param color The color to set for ambient light.
 *)
procedure R3D_SetAmbientColor(color: TColor); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetAmbientColor';

(*
 * @brief Enables a skybox for the scene.
 *
 * This function enables a skybox in the scene, replacing the default background with
 * a 3D environment. The skybox is defined by the specified skybox asset.
 *
 * @param skybox The skybox to enable.
 *)
procedure R3D_EnableSkybox(skybox: TR3D_Skybox); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_EnableSkybox';

(*
 * @brief Disables the skybox in the scene.
 *
 * This function disables the skybox, reverting back to the default background
 * color (or no background if none is set). It should be called to remove the skybox
 * from the scene.
 *)
procedure R3D_DisableSkybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DisableSkybox';

(*
 * @brief Sets the rotation of the skybox.
 *
 * This function allows you to specify the rotation of the skybox along the
 * pitch, yaw, and roll axes, which allows the skybox to be rotated in the scene.
 *
 * @param pitch The rotation angle around the X-axis (in degrees).
 * @param yaw The rotation angle around the Y-axis (in degrees).
 * @param roll The rotation angle around the Z-axis (in degrees).
 *)
procedure R3D_SetSkyboxRotation(pitch, yaw, roll: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSkyboxRotation';

(*
 * @brief Gets the current rotation of the skybox.
 *
 * This function returns the current rotation of the skybox as a vector containing
 * the pitch, yaw, and roll values in degrees.
 *
 * @return A vector containing the current pitch, yaw, and roll of the skybox.
 *)
function R3D_GetSkyboxRotation: TVector3; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSkyboxRotation';

(*
 * @brief Sets the intensity scaling values used for the environment's skybox.
 *
 * This function controls the intensity of both the rendered skybox as well as
 * the light that is generated from the skybox.
 *
 * @param background The intensity of the skybox rendered as the background.
 *                   A value of 0.0 will disable rendering the skybox but
 *                   allow any generated lighting to still be applied.
 * @param ambient The intensity of ambient light produced by the skybox.
 * @param reflection The intensity of reflections of the skybox in reflective materials.
 *)
procedure R3D_SetSkyboxIntensity(background: Single; ambient: Single; reflection: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSkyboxIntensity';

(*
 * @brief Gets the intensity scaling values used for the environment's skybox.
 *
 * This function returns the intensity values for the rendered skybox as well
 * the light that is generated from the skybox.
 *
 * @param background Pointer to store the intensity value for the rendered skybox.
 * @param ambient Pointer to store the intensity value for ambient light produced by the skybox.
 * @param reflection Pointer to store the intensity value for reflections from the skybox.
 *)
procedure R3D_GetSkyboxIntensity(background: PSingle; ambient: PSingle; reflection: PSingle); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSkyboxIntensity';


// --------------------------------------------
// ENVIRONMENT: SSAO Config Functions
// --------------------------------------------

(*
 * @brief Enables or disables Screen Space Ambient Occlusion (SSAO).
 *
 * This function toggles the SSAO effect. When enabled, SSAO enhances the realism
 * of the scene by simulating ambient occlusion, darkening areas where objects
 * are close together or in corners.
 *
 * @param enabled Whether to enable or disable SSAO.
 *)
procedure R3D_SetSSAO(enabled: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSSAO';

(*
 * @brief Gets the current state of SSAO.
 *
 * This function checks if SSAO is currently enabled or disabled.
 *
 * @return True if SSAO is enabled, false otherwise.
 *)
function R3D_GetSSAO: Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAO';

(*
 * @brief Sets the radius for SSAO effect.
 *
 * This function sets the radius used by the SSAO effect to calculate occlusion.
 * A higher value will affect a larger area around each pixel, while a smaller value
 * will create sharper and more localized occlusion.
 *
 * @param value The radius value to set for SSAO.
 *)
procedure R3D_SetSSAORadius(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSSAORadius';

(*
 * @brief Gets the current SSAO radius.
 *
 * This function retrieves the current radius value used by the SSAO effect.
 *
 * @return The radius value for SSAO.
 *)
function R3D_GetSSAORadius: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAORadius';

(*
 * @brief Sets the bias for SSAO effect.
 *
 * This function sets the bias used by the SSAO effect to adjust how much occlusion
 * is applied to the scene. A higher value can reduce artifacts, but may also
 * result in less pronounced ambient occlusion.
 *
 * @param value The bias value for SSAO.
 *)
procedure R3D_SetSSAOBias(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSSAOBias';

(*
 * @brief Gets the current SSAO bias.
 *
 * This function retrieves the current bias value used by the SSAO effect.
 *
 * @return The SSAO bias value.
 *)
function R3D_GetSSAOBias: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAOBias';

(*
 * @brief Sets the number of blur iterations for the SSAO effect.
 *
 * This function sets the number of blur iterations applied to the SSAO effect.
 * By default, one iteration is performed, using a total of 12 samples for the
 * Gaussian blur. Increasing the number of iterations results in a smoother
 * ambient occlusion but may impact performance.
 *
 * @param value The number of blur iterations for SSAO.
 *)
procedure R3D_SetSSAOIterations(value: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSSAOIterations';

(*
 * @brief Gets the current number of blur iterations for the SSAO effect.
 *
 * This function retrieves the current number of blur iterations applied to the SSAO effect.
 *
 * @return The number of blur iterations for SSAO.
 *)
function R3D_GetSSAOIterations: Integer; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAOIterations';

// --------------------------------------------
// ENVIRONMENT: Bloom Config Functions
// --------------------------------------------

(*
 * @brief Sets the bloom mode.
 *
 * This function configures the bloom effect mode, which determines how the bloom
 * effect is applied to the rendered scene.
 *
 * @param mode The bloom mode to set.
 *)
procedure R3D_SetBloomMode(mode: R3D_Bloom); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomMode';

(*
 * @brief Gets the current bloom mode.
 *
 * This function retrieves the bloom mode currently applied to the scene.
 *
 * @return The current bloom mode.
 *)
function R3D_GetBloomMode: R3D_Bloom; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomMode';

(*
 * @brief Sets the bloom intensity.
 *
 * This function controls the strength of the bloom effect. Higher values result
 * in a more intense glow effect on bright areas of the scene.
 *
 * @param value The intensity value for bloom.
 *)
procedure R3D_SetBloomIntensity(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomIntensity';

(*
 * @brief Gets the current bloom intensity.
 *
 * This function retrieves the intensity value of the bloom effect.
 *
 * @return The current bloom intensity.
 *)
function R3D_GetBloomIntensity: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomIntensity';

(*
 * @brief Sets the bloom filter radius.
 *
 * Controls the radius of the blur filter applied during the upscaling stage
 * of the bloom effect. A larger radius results in a wider glow around bright
 * objects, creating a softer and more diffuse bloom. A value of 0 disables
 * the filtering effect, preserving sharp bloom highlights.
 *
 * @param value The radius of the bloom filter (in pixels or arbitrary units depending on implementation).
 *)
procedure R3D_SetBloomFilterRadius(value: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomFilterRadius';

(*
 * @brief Gets the current bloom filter radius.
 *
 * Retrieves the current radius used for the bloom filter. This value determines
 * how far the glow effect extends around bright areas in the scene.
 *
 * @return The current bloom filter radius.
 *)
function R3D_GetBloomFilterRadius: Integer; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomFilterRadius';

(*
 * @brief Sets the bloom brightness threshold.
 *
 * Controls the brightness cutoff used during the downsampling stage of the
 * bloom effect. If the color channel with the brightest value is below the
 * set threshold the pixel will not be included in the bloom effect.
 *
 * @param value The lowest value to be included the bloom effect (in color value depending on implementation).
 *)
procedure R3D_SetBloomThreshold(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomThreshold';

(*
 * @brief Gets the bloom brightness threshold.
 *
 * Retrieves the current brightness cutoff used for the bloom effect. This value
 * determines if a pixel will be included in the bloom effect based on it's brightness.
 *
 * @return The current bloom brightness cutoff threshold.
 *)
function R3D_GetBloomThreshold: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomThreshold';

(*
 * @brief Sets the bloom brightness threshold's softness.
 *
 * Controls the softness of the cutoff between being include or excluded in the
 * bloom effect. A value of 0 will result in a hard transition between being
 * included or excluded, while larger values will give an increasingly
 * softer transition.
 *
 * @param value The value of of the bloom brightness threshold's softness.
 *)
procedure R3D_SetBloomSoftThreshold(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomSoftThreshold';

(*
 * @brief Gets the current bloom brightness threshold's softness.
 *
 * Retrieves the softness of the brightness cutoff for the bloom effect.
 * This value determines the softness of the transition between being
 * included or excluded in the bloom effect
 *
 * @return The current bloom brightness threshold's softness.
 *)
function R3D_GetBloomSoftThreshold: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomSoftThreshold';

// --------------------------------------------
// ENVIRONMENT: Fog Config Functions
// --------------------------------------------

(*
 * @brief Sets the fog mode.
 *
 * This function defines the type of fog effect applied to the scene.
 * Different modes may provide linear, exponential, or volumetric fog effects.
 *
 * @param mode The fog mode to set.
 *)
procedure R3D_SetFogMode(mode: R3D_Fog); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogMode';

(*
 * @brief Gets the current fog mode.
 *
 * This function retrieves the fog mode currently applied to the scene.
 *
 * @return The current fog mode.
 *)
function R3D_GetFogMode: R3D_Fog; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogMode';

(*
 * @brief Sets the color of the fog.
 *
 * This function defines the color of the fog effect applied to the scene.
 * The fog color blends with objects as they are affected by fog.
 *
 * @param color The color to set for the fog.
 *)
procedure R3D_SetFogColor(color: TColor); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogColor';

(*
 * @brief Gets the current fog color.
 *
 * This function retrieves the color currently used for the fog effect.
 *
 * @return The current fog color.
 *)
function R3D_GetFogColor: TColor; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogColor';

(*
 * @brief Sets the start distance of the fog.
 *
 * This function defines the distance from the camera at which fog begins to appear.
 * Objects closer than this distance will not be affected by fog.
 *
 * @param value The start distance for the fog effect.
 *)
procedure R3D_SetFogStart(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogStart';

(*
 * @brief Gets the current fog start distance.
 *
 * This function retrieves the distance at which the fog begins to be applied.
 *
 * @return The current fog start distance.
 *)
function R3D_GetFogStart: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogStart';

(*
 * @brief Sets the end distance of the fog.
 *
 * This function defines the distance from the camera at which fog reaches full intensity.
 * Objects beyond this distance will be completely covered by fog.
 *
 * @param value The end distance for the fog effect.
 *)
procedure R3D_SetFogEnd(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogEnd';

(*
 * @brief Gets the current fog end distance.
 *
 * This function retrieves the distance at which the fog is fully applied.
 *
 * @return The current fog end distance.
 *)
function R3D_GetFogEnd: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogEnd';

(*
 * @brief Sets the density of the fog.
 *
 * This function controls how thick the fog appears. Higher values result in
 * denser fog, making objects fade out more quickly.
 *
 * @param value The density of the fog (higher values increase fog thickness).
 *)
procedure R3D_SetFogDensity(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogDensity';

(*
 * @brief Gets the current fog density.
 *
 * This function retrieves the current density of the fog.
 *
 * @return The current fog density.
 *)
function R3D_GetFogDensity: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogDensity';

// --------------------------------------------
// ENVIRONMENT: Tonemap Config Functions
// --------------------------------------------

(*
 * @brief Sets the tonemapping mode.
 *
 * This function defines the tonemapping algorithm applied to the final rendered image.
 * Different tonemap modes affect color balance, brightness compression, and overall
 * scene appearance.
 *
 * @param mode The tonemap mode to set.
 *)
procedure R3D_SetTonemapMode(mode: R3D_Tonemap); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetTonemapMode';

(*
 * @brief Gets the current tonemapping mode.
 *
 * This function retrieves the tonemap mode currently applied to the scene.
 *
 * @return The current tonemap mode.
 *)
function R3D_GetTonemapMode: R3D_Tonemap; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapMode';

(*
 * @brief Sets the exposure level for tonemapping.
 *
 * This function adjusts the exposure level used in tonemapping, affecting
 * the overall brightness of the rendered scene.
 *
 * @param value The exposure value (higher values make the scene brighter).
 *)
procedure R3D_SetTonemapExposure(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetTonemapExposure';

(*
 * @brief Gets the current tonemap exposure level.
 *
 * This function retrieves the current exposure setting used in tonemapping.
 *
 * @return The current tonemap exposure value.
 *)
function R3D_GetTonemapExposure: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapExposure';

(*
 * @brief Sets the white point for tonemapping.
 *
 * This function defines the reference white level, which determines how bright
 * areas of the scene are mapped to the final output.
 *
 * @param value The white point value.
 *)
procedure R3D_SetTonemapWhite(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetTonemapWhite';

(*
 * @brief Gets the current tonemap white point.
 *
 * This function retrieves the white point setting used in tonemapping.
 *
 * @return The current tonemap white value.
 *)
function R3D_GetTonemapWhite: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapWhite';

// --------------------------------------------
// ENVIRONMENT: Color Adjustment Functions
// --------------------------------------------

(*
 * @brief Sets the global brightness adjustment.
 *
 * This function controls the brightness of the final rendered image.
 * Higher values make the image brighter, while lower values darken it.
 *
 * @param value The brightness adjustment value.
 *)
procedure R3D_SetBrightness(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBrightness';

(*
 * @brief Gets the current brightness level.
 *
 * This function retrieves the brightness setting applied to the scene.
 *
 * @return The current brightness value.
 *)
function R3D_GetBrightness: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBrightness';

(*
 * @brief Sets the global contrast adjustment.
 *
 * This function controls the contrast of the final rendered image.
 * Higher values increase the difference between dark and bright areas.
 *
 * @param value The contrast adjustment value.
 *)
procedure R3D_SetContrast(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetContrast';

(*
 * @brief Gets the current contrast level.
 *
 * This function retrieves the contrast setting applied to the scene.
 *
 * @return The current contrast value.
 *)
function R3D_GetContrast: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetContrast';

(*
 * @brief Sets the global saturation adjustment.
 *
 * This function controls the color intensity of the final rendered image.
 * Higher values make colors more vibrant, while lower values desaturate them.
 *
 * @param value The saturation adjustment value.
 *)
procedure R3D_SetSaturation(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSaturation';

(*
 * @brief Gets the current saturation level.
 *
 * This function retrieves the saturation setting applied to the scene.
 *
 * @return The current saturation value.
 *)
function R3D_GetSaturation: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSaturation';

// --------------------------------------------
// ENVIRONMENT: Depth of Field (DoF) Functions
// --------------------------------------------

(*
 * @brief Enables or disables the depth of field post-process.
 *
 * @param mode The depth of field mode to set.
 *)
procedure R3D_SetDofMode(mode: R3D_Dof); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetDofMode';

(*
 * @brief Gets the current depth of field mode.
 *
 * @return The current depth of field mode.
 *)
function R3D_GetDofMode: R3D_Dof; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDofMode';

(*
 * @brief Sets the focus point in world space.
 *
 * This function defines the distance (in meters) from the camera where
 * objects will be in perfect focus. Objects closer or farther will be blurred.
 *
 * @param value The focus point distance in meters.
 *)
procedure R3D_SetDofFocusPoint(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetDofFocusPoint';

(*
 * @brief Gets the current focus point.
 *
 * @return The focus point distance in meters.
 *)
function R3D_GetDofFocusPoint: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDofFocusPoint';

(*
 * @brief Sets the focus scale.
 *
 * This function controls how shallow the depth of field effect is.
 * Lower values create a shallower depth of field with more blur,
 * while higher values create a deeper depth of field with less blur.
 *
 * @param value The focus scale value.
 *)
procedure R3D_SetDofFocusScale(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetDofFocusScale';

(*
 * @brief Gets the current focus scale.
 *
 * @return The current focus scale value.
 *)
function R3D_GetDofFocusScale: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDofFocusScale';

(*
 * @brief Sets the maximum blur size.
 *
 * This function controls the maximum amount of blur applied to out-of-focus
 * areas. This value is similar to the lens aperture size, larger values
 * create more pronounced blur effects.
 *
 * @param value The maximum blur size value.
 *)
procedure R3D_SetDofMaxBlurSize(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetDofMaxBlurSize';

(*
 * @brief Gets the current maximum blur size.
 *
 * @return The current maximum blur size value.
 *)
function R3D_GetDofMaxBlurSize: Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDofMaxBlurSize';

(*
 * @brief Enables or disables depth-of-field debug mode.
 *
 * In debug mode, the scene uses color coding:
 * - Green: near blur
 * - Black: sharp areas
 * - Blue: far blur
 *
 * @param enabled true to enable, false to disable.
 *)
procedure R3D_SetDofDebugMode(enabled: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetDofDebugMode';

(*
 * @brief Gets the current debug mode state.
 *
 * @return True if debug mode is enabled, false otherwise.
 *)
function R3D_GetDofDebugMode: Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetDofDebugMode';


// --------------------------------------------
// SKYBOX: Skybox Loading Functions
// --------------------------------------------

(*
 * @brief Loads a skybox from a texture file.
 *
 * This function loads a skybox from a texture file using a specified cubemap layout.
 * The layout defines how the six faces of the cubemap are arranged within the texture.
 *
 * @param fileName The path to the texture file.
 * @param layout The cubemap layout format.
 * @return The loaded skybox object.
 *)
function R3D_LoadSkybox(fileName: PChar; layout: TCubemapLayout): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkybox';

(*
 * @brief Loads a skybox from an image in memory.
 *
 * This function loads a skybox cubemap from an image already loaded in memory,
 * using a specified cubemap layout to map the six faces.
 *
 * @param image The source image in memory.
 * @param layout The cubemap layout format.
 * @return The loaded skybox object.
 *)
function R3D_LoadSkyboxFromMemory(image: TImage; layout: TCubemapLayout): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkyboxFromMemory';

(*
 * @brief Loads a skybox from a panorama texture file.
 *
 * This function loads a skybox from a panorama (equirectangular) texture file,
 * and converts it into a cubemap with the specified resolution.
 *
 * @param fileName The path to the panorama texture file.
 * @param size The resolution of the generated cubemap (e.g., 512, 1024).
 * @return The loaded skybox object.
 *)
function R3D_LoadSkyboxPanorama(const fileName: Pchar; size: Integer): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkyboxPanorama';

(*
 * @brief Loads a skybox from a panorama image in memory.
 *
 * This function loads a skybox from a panorama (equirectangular) image already loaded in memory,
 * and converts it into a cubemap with the specified resolution.
 *
 * @param image The panorama image in memory.
 * @param size The resolution of the generated cubemap (e.g., 512, 1024).
 * @return The loaded skybox object.
 *)
function R3D_LoadSkyboxPanoramaFromMemory(image: TImage; size: Integer): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkyboxPanoramaFromMemory';

(*
 * @brief Unloads a skybox and frees its resources.
 *
 * This function removes a previously loaded skybox from memory.
 * It should be called when the skybox is no longer needed to prevent memory leaks.
 *
 * @param sky The skybox to unload.
 *)
procedure R3D_UnloadSkybox(sky: TR3D_Skybox); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadSkybox';

// --------------------------------------------
// CULLING: Frustum Test Functions
// --------------------------------------------

(*
 * @brief Checks if a point is inside the view frustum.
 *
 * Tests whether a 3D point lies within the camera's frustum by checking against all six planes.
 * Call this only between `R3D_Begin` and `R3D_End`.
 *
 * Useful when automatic frustum culling is disabled and you're using a custom spatial structure
 * (e.g., octree, BVH, etc.).
 *
 * @param position The 3D point to test.
 * @return `true` if inside the frustum, `false` otherwise.
 *
 * @note This performs an exact plane-point test. Slower than bounding box tests.
 * @warning Frustum culling may incorrectly discard objects casting visible shadows.
 * @todo Improve shadow-aware culling in future versions.
 *
 * @see R3D_IsPointInFrustumBoundingBox()
 *)
function R3D_IsPointInFrustum(position: TVector3): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsPointInFrustum';

(*
 * @brief Checks if a sphere is inside the view frustum.
 *
 * Tests whether a sphere intersects the camera's frustum using plane-sphere tests.
 * Call this only between `R3D_Begin` and `R3D_End`.
 *
 * Useful when managing visibility manually.
 *
 * @param position The center of the sphere.
 * @param radius The sphere's radius (must be positive).
 * @return `true` if at least partially inside the frustum, `false` otherwise.
 *
 * @note More accurate but slower than bounding box approximations.
 * @warning May cause visual issues with shadow casters being culled too early.
 * @todo Add support for shadow-aware visibility.
 *
 * @see R3D_IsSphereInFrustumBoundingBox()
 *)
function R3D_IsSphereInFrustum(position: TVector3; radius: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsSphereInFrustum';

(*
 * @brief Checks if an AABB is inside the view frustum.
 *
 * Determines whether an axis-aligned bounding box intersects the frustum.
 * Call between `R3D_Begin` and `R3D_End`.
 *
 * For use in custom culling strategies or spatial partitioning systems.
 *
 * @param aabb The bounding box to test.
 * @return `true` if at least partially inside the frustum, `false` otherwise.
 *
 * @note Exact but more costly than AABB pre-tests.
 * @warning May prematurely cull objects casting visible shadows.
 * @todo Add support for light-aware visibility tests.
 *
 * @see R3D_IsAABBInFrustumBoundingBox()
 *)
function R3D_IsAABBInFrustum(aabb: TBoundingBox): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsAABBInFrustum';

(*
 * @brief Checks if an OBB is inside the view frustum.
 *
 * Tests an oriented bounding box (transformed AABB) for frustum intersection.
 * Must be called between `R3D_Begin` and `R3D_End`.
 *
 * Use this for objects with transformations when doing manual culling.
 *
 * @param aabb Local-space bounding box.
 * @param transform World-space transform matrix.
 * @return `true` if the transformed box intersects the frustum, `false` otherwise.
 *
 * @note More expensive than AABB checks due to matrix operations.
 * @warning May incorrectly cull shadow casters.
 * @todo Consider shadow-aware culling improvements.
 *
 * @see R3D_IsAABBInFrustum()
 *)
function R3D_IsOBBInFrustum(aabb: TBoundingBox; transform: TMatrix): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsOBBInFrustum';

(*
 * @brief Fast pre-filtering test for point inside frustum bounding box.
 *
 * Performs an AABB check using the frustum's bounding volume.
 * Useful for quick rejection before precise tests.
 *
 * @param position The 3D point to test.
 * @return `true` if inside the frustum AABB, `false` otherwise.
 *
 * @note May return false positives, never false negatives.
 * @warning Only checks against a loose AABB, not actual frustum planes.
 * @see R3D_IsPointInFrustum()
 *)
function R3D_IsPointInFrustumBoundingBox(position: TVector3): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsPointInFrustumBoundingBox';

(*
 * @brief Fast pre-filtering test for sphere inside frustum bounding box.
 *
 * Performs a quick check using the frustum's AABB to approximate intersection.
 *
 * @param position The center of the sphere.
 * @param radius Radius of the sphere.
 * @return `true` if possibly intersecting the frustum AABB, `false` otherwise.
 *
 * @note Faster but less accurate than full frustum testing.
 * @warning May produce false positives.
 * @see R3D_IsSphereInFrustum()
 *)
function R3D_IsSphereInFrustumBoundingBox(position: TVector3; radius: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsSphereInFrustumBoundingBox';

(*
 * @brief Fast pre-filtering test for AABB inside frustum bounding box.
 *
 * Performs a bounding box vs bounding box intersection to quickly eliminate non-visible objects.
 * Useful as an initial coarse check before calling full frustum tests.
 *
 * @param aabb The bounding box to test.
 * @return `true` if intersecting the frustum AABB, `false` otherwise.
 *
 * @note False positives possible, but never false negatives.
 * @warning Does not use actual frustum planes.
 * @note No OBB variant exists due to computational cost.
 * @see R3D_IsAABBInFrustum()
 *)
function R3D_IsAABBInFrustumBoundingBox(aabb: TBoundingBox): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsAABBInFrustumBoundingBox';

// --------------------------------------------
// UTILS: Default Texture Retrieval Functions
// --------------------------------------------

(*
 * @brief Retrieves a default white texture.
 *
 * This texture is fully white (1,1,1,1), useful for default material properties.
 *
 * @return A white texture.
 *)
function R3D_GetWhiteTexture: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetWhiteTexture';

(*
 * @brief Retrieves a default black texture.
 *
 * This texture is fully black (0,0,0,1), useful for masking or default values.
 *
 * @return A black texture.
 *)
function R3D_GetBlackTexture: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBlackTexture';

(*
 * @brief Retrieves a default normal map texture.
 *
 * This texture represents a neutral normal map (0.5, 0.5, 1.0), which applies no normal variation.
 *
 * @return A neutral normal texture.
 *)
function R3D_GetNormalTexture: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetNormalTexture';

// --------------------------------------------
// UTILS: Render Texture Retrieval Functions
// --------------------------------------------

(*
 * @brief Retrieves the final scene color buffer.
 *
 * This texture stores the final rendered scene as a 24-bit RGB buffer.
 *
 * @return The final color buffer texture.
 *)
function R3D_GetBufferColor: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBufferColor';

(*
 * @brief Retrieves the buffer containing the scene's normal data.
 *
 * This texture stores octahedral-compressed normals using two 16-bit per-channel RG components.
 *
 * @note You can find the decoding functions in the embedded shaders, such as 'screen/lighting.fs.glsl'.
 *
 * @return The normal buffer texture.
 *)
function R3D_GetBufferNormal: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBufferNormal';

(*
 * @brief Retrieves the final depth buffer.
 *
 * This texture contains the depth stored in 24 bits and a stencil buffer where each value is 0 or 1, indicating the presence of geometry.
 * It is useful for post-processing effects outside of R3D.
 *
 * @note If you modify the texture parameters to sample the stencil instead of the depth,
 * make sure to reset the parameters afterward.
 *
 * @return The final depth buffer texture.
 *)
function R3D_GetBufferDepth: TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBufferDepth';

// --------------------------------------------
// UTILS: Camera Matrices Retrieval Functions
// --------------------------------------------

(*
 * @brief Retrieves the view matrix.
 *
 * This matrix represents the camera's transformation from world space to view space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current view matrix.
 *)
function R3D_GetMatrixView: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetMatrixView';

(*
 * @brief Retrieves the inverse view matrix.
 *
 * This matrix transforms coordinates from view space back to world space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current inverse view matrix.
 *)
function R3D_GetMatrixInvView: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetMatrixInvView';

(*
 * @brief Retrieves the projection matrix.
 *
 * This matrix defines the transformation from view space to clip space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current projection matrix.
 *)
function R3D_GetMatrixProjection: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetMatrixProjection';

(*
 * @brief Retrieves the inverse projection matrix.
 *
 * This matrix transforms coordinates from clip space back to view space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current inverse projection matrix.
 *)
function R3D_GetMatrixInvProjection: TMatrix; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetMatrixInvProjection';

// --------------------------------------------
// UTILS: Debug Buffer Rendering Functions
// --------------------------------------------

(*
 * @brief Renders the internal albedo buffer to the screen.
 *
 * This function displays the albedo (diffuse) buffer as a 2D texture.
 * It must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferAlbedo(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferAlbedo';

(*
 * @brief Renders the internal emission buffer to the screen.
 *
 * Displays the emission buffer, which contains emissive lighting data.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferEmission(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferEmission';

(*
 * @brief Renders the internal normal buffer to the screen.
 *
 * Displays the normal buffer, showing world-space normal data as colors.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferNormal(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferNormal';

(*
 * @brief Renders the ORM (Occlusion, Roughness, Metalness) buffer to the screen.
 *
 * Displays the ORM buffer, where each channel stores different material properties:
 * - Red: Ambient occlusion
 * - Green: Roughness
 * - Blue: Metalness
 *
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferORM(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferORM';

(*
 * @brief Renders the SSAO (Screen Space Ambient Occlusion) buffer to the screen.
 *
 * Displays the SSAO buffer, showing ambient occlusion data in grayscale.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferSSAO(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferSSAO';

(*
 * @brief Renders the bloom buffer to the screen.
 *
 * Displays the bloom effect buffer, showing the extracted bright areas after blur processing.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferBloom(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferBloom';



implementation



end.

