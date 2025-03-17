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
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I raylib.inc}
interface

uses
  raylib;

{$IFNDEF RAY_STATIC}
const
  r3dName =
    {$IFDEF WINDOWS} 'libr3d.dll'; {$IFEND}
    {$IFDEF LINUX} 'libr3d.so'; {$IFEND}
{$ENDIF}

type
  R3D_Flags = Integer;
  const
    R3D_FLAG_NONE           = R3D_Flags(0);        //< No special rendering flags.
    R3D_FLAG_FXAA           = R3D_Flags(1 shl 0);  //< Enables Fast Approximate Anti-Aliasing (FXAA).
    R3D_FLAG_BLIT_LINEAR    = R3D_Flags(1 shl 1);  //< Uses linear filtering when blitting the final image.
    R3D_FLAG_ASPECT_KEEP    = R3D_Flags(1 shl 2);  //< Maintains the aspect ratio when rendering.
    R3D_FLAG_STENCIL_TEST   = R3D_Flags(1 shl 3);  //< Performs a stencil test on each rendering pass affecting geometry, useful for outdoor scenes where the sky is dominant.
    R3D_FLAG_DEPTH_PREPASS  = R3D_Flags(1 shl 4);  //< Performs a depth pre-pass before forward rendering, improving desktop GPU performance but unnecessary on mobile.

type
  R3D_RenderMode = Integer;
  const
    {*< Automatically determines the rendering mode based on the material,
    for example, by analyzing the albedo texture formats or the alpha
    value of albedo colors. This is the default mode. *}
    R3D_RENDER_AUTO_DETECT = R3D_RenderMode(0);
    R3D_RENDER_DEFERRED    = R3D_RenderMode(1);  //< Optimized for desktop GPUs, but does not support transparency.
    R3D_RENDER_FORWARD     = R3D_RenderMode(2);  //< Works well on tile-based renderers, supports transparency.

type
  R3D_BlendMode = Integer;
  const
    R3D_BLEND_OPAQUE   = R3D_BlendMode(0);  //< No blending, the source color fully replaces the destination color.
    R3D_BLEND_ALPHA    = R3D_BlendMode(1);  //< Alpha blending: source color is blended with the destination based on alpha value.
    R3D_BLEND_ADDITIVE = R3D_BlendMode(2);  //< Additive blending: source color is added to the destination, making bright effects.
    R3D_BLEND_MULTIPLY = R3D_BlendMode(3);  //< Multiply blending: source color is multiplied with the destination, darkening the image.

type
  R3D_ShadowCastMode = Integer;
  const
    R3D_SHADOW_CAST_DISABLED    = R3D_ShadowCastMode(0);  //< The object does not cast shadows.
    R3D_SHADOW_CAST_FRONT_FACES = R3D_ShadowCastMode(1);  //< Only front-facing polygons cast shadows.
    R3D_SHADOW_CAST_BACK_FACES  = R3D_ShadowCastMode(2);  //< Only back-facing polygons cast shadows.
    R3D_SHADOW_CAST_ALL_FACES   = R3D_ShadowCastMode(3);  //< Both front and back-facing polygons cast shadows.

type
  R3D_BillboardMode = Integer;
  const
    R3D_BILLBOARD_DISABLED = R3D_BillboardMode(0);  //< Billboarding is disabled; the object retains its original orientation.
    R3D_BILLBOARD_FRONT    = R3D_BillboardMode(1);  //< Full billboarding; the object fully faces the camera, rotating on all axes.
    R3D_BILLBOARD_Y_AXIS   = R3D_BillboardMode(2);
    {*< Y-axis constrained billboarding; the object rotates only around the Y-axis,
    keeping its "up" orientation fixed. This is suitable for upright objects like characters or signs. *}
type
 R3D_LightType = Integer;
 const
    R3D_LIGHT_DIR  = R3D_LightType(0);  //< Directional light, affects the entire scene with parallel rays.
    R3D_LIGHT_SPOT = R3D_LightType(1);  //< Spot light, emits light in a cone shape.
    R3D_LIGHT_OMNI = R3D_LightType(2); //< Omni light, emits light in all directions from a single point.

type
  R3D_ShadowUpdateMode = Integer;
  const
    R3D_SHADOW_UPDATE_MANUAL = R3D_ShadowUpdateMode(0);     //< Shadow maps update only when explicitly requested.
    R3D_SHADOW_UPDATE_INTERVAL = R3D_ShadowUpdateMode(1);   //< Shadow maps update at defined time intervals.
    R3D_SHADOW_UPDATE_CONTINUOUS = R3D_ShadowUpdateMode(2); //< Shadow maps update every frame for real-time accuracy.

type
  R3D_Bloom = Integer;
  const
    R3D_BLOOM_DISABLED = R3D_Bloom(0);         //< Bloom effect is disabled.
    R3D_BLOOM_ADDITIVE = R3D_Bloom(1);         //< Enhances bright areas by adding light to them (stronger glow effect).
    R3D_BLOOM_SOFT_LIGHT = R3D_Bloom(2);       //< Creates a softer, more diffused glow around bright areas.

type
  R3D_Fog = Integer;
  const
    R3D_FOG_DISABLED = R3D_Fog(0);  //< Fog effect is disabled.
    R3D_FOG_LINEAR = R3D_Fog(1);    //< Fog density increases linearly with distance from the camera.
    R3D_FOG_EXP2 = R3D_Fog(2);      //< Exponential fog (exp2), where density increases exponentially with distance.
    R3D_FOG_EXP = R3D_Fog(3);       //< Exponential fog, similar to EXP2 but with a different rate of increase.

type
  R3D_Tonemap = Integer;
  const
    R3D_TONEMAP_LINEAR = R3D_Tonemap(0);         //< Simple linear mapping of HDR values.
    R3D_TONEMAP_REINHARD = R3D_Tonemap(1);       //< Reinhard tone mapping, a balanced method for compressing HDR values.
    R3D_TONEMAP_FILMIC = R3D_Tonemap(2);         //< Filmic tone mapping, mimicking the response of photographic film.
    R3D_TONEMAP_ACES = R3D_Tonemap(3);           //< ACES tone mapping, a high-quality cinematic rendering technique.
    R3D_TONEMAP_AGX   = R3D_Tonemap(4);          //< AGX tone mapping, a modern technique designed to preserve both highlight and shadow details for HDR rendering.


type
  PR3D_Light = ^TR3D_Light;
  TR3D_Light = Cardinal;  // Cardinal is an unsigned 32-bit integer in Pascal

  PR3D_Skybox = ^TR3D_Skybox;
  TR3D_Skybox = record
    Cubemap: TTextureCubemap;  //< The skybox cubemap texture for the background and reflections.
    Irradiance: TTexture2D;    //< The irradiance cubemap for diffuse ambient lighting.
    Prefilter: TTexture2D;     //< The prefiltered cubemap for specular reflections with mipmaps.
  end;

  PR3D_Sprite= ^TR3D_Sprite;
  TR3D_Sprite = record
    Material: TMaterial;            //< The material used for rendering the sprite, including its texture and shading properties.
    CurrentFrame: Single;           //< The current animation frame, represented as a floating-point value to allow smooth interpolation.
    FrameSize: TVector2;            //< The size of a single animation frame, in texture coordinates (width and height).
    XFrameCount: Integer;           //< The number of frames along the horizontal (X) axis of the texture.
    YFrameCount: Integer;           //< The number of frames along the vertical (Y) axis of the texture.
  end;

  PR3D_Keyframe = ^TR3D_Keyframe;
  TR3D_Keyframe = record
    Time: Single;   //< Normalized time of the keyframe, ranging from 0.0 to 1.0.
    Value: Single;  //< The value of the interpolation at this keyframe.
  end;

  PR3D_InterpolationCurve = ^TR3D_InterpolationCurve;
  TR3D_InterpolationCurve = record
    Keyframes: PR3D_Keyframe;           //< Dynamic array of keyframes defining the interpolation curve. **todo
    Capacity: Cardinal;                 //< Allocated size of the keyframes array.
    Count: Cardinal;                    //< Current number of keyframes in the array.
  end;

  PR3D_Particle = ^TR3D_Particle;
  TR3D_Particle = record
    Lifetime: Single;               //< Duration of the particle's existence in seconds.

    Transform: TMatrix;             //< The particle's current transformation matrix in 3D space.

    Position: TVector3;             //< The current position of the particle in 3D space.
    Rotation: TVector3;             //< The current rotation of the particle in 3D space (Euler angles).
    Scale: TVector3;                //< The current scale of the particle in 3D space.
    Color: TColorB;                  //< The current color of the particle, representing its color modulation.

    Velocity: TVector3;             //< The current velocity of the particle in 3D space.
    AngularVelocity: TVector3;      //< The current angular velocity of the particle in radians (Euler angles).

    BaseScale: TVector3;            //< The initial scale of the particle in 3D space.
    BaseVelocity: TVector3;         //< The initial velocity of the particle in 3D space.
    BaseAngularVelocity: TVector3;  //< The initial angular velocity of the particle in radians (Euler angles).
    BaseOpacity: Byte;              //< The initial opacity of the particle, ranging from 0 (fully transparent) to 255 (fully opaque).
  end;

  PR3D_ParticleSystem = ^TR3D_ParticleSystem;
  TR3D_ParticleSystem = record
    Particles: PR3D_Particle;         //< Dynamic array of particles in the system.
    Capacity: Integer;                //< The maximum number of particles the system can manage.
    Count: Integer;                   //< The current number of active particles in the system.

    Position: TVector3;               //< The initial position of the particle system. Default: (0, 0, 0).
    Gravity: TVector3;                //< The gravity applied to the particles. Default: (0, -9.81, 0).

    InitialScale: TVector3;           //< The initial scale of the particles. Default: (1, 1, 1).
    ScaleVariance: Single;            //< The variance in particle scale. Default: 0.0.

    InitialRotation: TVector3;        //< The initial rotation of the particles in Euler angles (degrees). Default: (0, 0, 0).
    RotationVariance: TVector3;       //< The variance in particle rotation in Euler angles (degrees). Default: (0, 0, 0).

    InitialColor: TColorB;             //< The initial color of the particles. Default: WHITE.
    ColorVariance: TColorB;            //< The variance in particle color. Default: BLANK.

    InitialVelocity: TVector3;        //< The initial velocity of the particles. Default: (0, 0, 0).
    VelocityVariance: TVector3;       //< The variance in particle velocity. Default: (0, 0, 0).

    InitialAngularVelocity: TVector3; //< The initial angular velocity of the particles in Euler angles (degrees). Default: (0, 0, 0).
    AngularVelocityVariance: TVector3;//< The variance in angular velocity. Default: (0, 0, 0).

    Lifetime: Single;                 //< The lifetime of the particles in seconds. Default: 1.0.
    LifetimeVariance: Single;         //< The variance in lifetime in seconds. Default: 0.0.

    EmissionTimer: Single;            //< Used to control automatic emission, should not be modified manually.
    EmissionRate: Single;             //< The rate of particle emission in particles per second. Default: 10.0.
    SpreadAngle: Single;              //< The angle of propagation of the particles in a cone (degrees). Default: 0.0.

    ScaleOverLifetime: PR3D_InterpolationCurve;  //< Curve controlling the scale evolution of the particles over their lifetime. Default: nil.
    SpeedOverLifetime: PR3D_InterpolationCurve;  //< Curve controlling the speed evolution of the particles over their lifetime. Default: nil.
    OpacityOverLifetime: PR3D_InterpolationCurve;//< Curve controlling the opacity evolution of the particles over their lifetime. Default: nil.
    AngularVelocityOverLifetime: PR3D_InterpolationCurve; //< Curve controlling the angular velocity evolution of the particles over their lifetime. Default: nil.

    AutoEmission: Boolean;            //< Indicates whether particle emission is automatic when calling `R3D_UpdateParticleSystem`.
                                      //< If false, emission is manual using `R3D_EmitParticle`. Default: true.
  end;

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
procedure R3D_Close(); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_Close';

(*
 * @brief Checks if a specific internal state flag is set.
 *
 * @param flag The state flag to check.
 * @return True if the flag is set, false otherwise.
 *)
function R3D_HasState(flag: R3D_Flags): boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_HasState';

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

// --------------------------------------------
// CORE: Rendering Config Functions
// --------------------------------------------

(*
 * @brief Applies a render mode (Deferred or Forward).
 *
 * This function sets the current render mode to either deferred or forward. It can
 * be called at any time, including between `R3D_Begin` and `R3D_End`. The set mode
 * will apply to all subsequent draw calls.
 *
 * @param mode The render mode to apply.
 *)
procedure R3D_ApplyRenderMode(mode: R3D_RenderMode); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ApplyRenderMode';

(*
 * @brief Sets the active blend mode for rendering.
 *
 * This function sets the current blend mode, which determines how the colors of
 * the current object are blended with the colors of the background or other objects
 * in the scene. It can be called at any time, including between `R3D_Begin` and `R3D_End`.
 * The set blend mode will apply to all subsequent draw calls.
 *
 * @note The blend mode is applied only if you are in forward rendering mode or auto-detect mode.
 *
 * @param mode The blend mode to apply.
 *)
procedure R3D_ApplyBlendMode(mode: R3D_BlendMode); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ApplyBlendMode';

(*
 * @brief Sets the shadow casting mode for meshes.
 *
 * This function controls how meshes cast shadows in the scene. It can be
 * called at any time, including between `R3D_Begin` and `R3D_End`. The selected mode
 * will apply to all subsequent draw calls.
 *
 * @param mode The shadow casting mode to apply.
 *)
procedure R3D_ApplyShadowCastMode(mode: R3D_ShadowCastMode); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ApplyShadowCastMode';

(*
 * @brief Applies a billboard mode to sprites or meshes.
 *
 * This function sets the current billboard mode, determining how objects orient
 * themselves relative to the camera. It can be called at any time, including
 * between `R3D_Begin` and `R3D_End`. The set mode will apply to all subsequent
 * draw calls.
 *
 * @param mode The billboard mode to apply.
 *)
procedure R3D_ApplyBillboardMode(mode: R3D_BillboardMode); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ApplyBillboardMode';

(*
 * @brief Sets an alpha threshold for forward rendering.
 *
 * This function defines an alpha scissor threshold, determining the minimum alpha
 * value required for a fragment to be rendered. Fragments with an alpha value below
 * the threshold will be discarded.
 *
 * @param threshold The alpha value threshold (usually from 0.0 to 1.0).
 *)
procedure R3D_ApplyAlphaScissorThreshold(threshold: float); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_ApplyAlphaScissorThreshold';


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
procedure R3D_End(); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_End';

(*
 * @brief Draws a mesh with a specified material and transformation.
 *
 * This function renders a mesh with the provided material and transformation matrix.
 *
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param transform The transformation matrix to apply to the mesh.
 *)
procedure R3D_DrawMesh(mesh: TMesh; material: TMaterial; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMesh';

(*
 * @brief Draws a mesh with instancing support.
 *
 * This function renders a mesh multiple times with different transformation matrices
 * for each instance.
 *
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param instanceTransforms Array of transformation matrices for each instance.
 * @param instanceCount The number of instances to render.
 *)
procedure R3D_DrawMeshInstanced(mesh: TMesh; material: TMaterial; instanceTransforms: PMatrix; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstanced';

(*
 * @brief Draws a mesh with instancing support and different colors per instance.
 *
 * This function renders a mesh multiple times with different transformation matrices
 * and different colors for each instance.
 *
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param instanceTransforms Array of transformation matrices for each instance.
 * @param instanceColors Array of colors for each instance.
 * @param instanceCount The number of instances to render.
 *)
procedure R3D_DrawMeshInstancedEx(mesh: TMesh; material: TMaterial; instanceTransforms: PMatrix; instanceColors: PColorB; instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstancedEx';

(*
 * @brief Draws a mesh with instancing support, a global transformation, and different colors per instance.
 *
 * This function renders a mesh multiple times using instancing, with a global transformation
 * applied to all instances, and individual transformation matrices and colors for each instance.
 * Each instance can have its own position, rotation, scale, and color while sharing the same mesh
 * and material.
 *
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param transform The global transformation matrix applied to all instances.
 * @param instanceTransforms Pointer to an array of transformation matrices for each instance, allowing unique transformations.
 * @param transformsStride The stride (in bytes) between consecutive transformation matrices in the array.
 * @param instanceColors Pointer to an array of colors for each instance, allowing unique colors.
 * @param colorsStride The stride (in bytes) between consecutive colors in the array.
 * @param instanceCount The number of instances to render.
 *)
procedure R3D_DrawMeshInstancedPro(mesh: TMesh; material: TMaterial; transform: TMatrix;
                                     instanceTransforms: PMatrix; transformsStride: Integer;
                                     instanceColors: PColorB; colorsStride: Integer;
                                     instanceCount: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawMeshInstancedPro';

(*
 * @brief Draws a model at a specified position and scale.
 *
 * This function renders a model at the given position with the specified scale factor.
 *
 * @param model The model to render.
 * @param position The position to place the model at.
 * @param scale The scale factor to apply to the model.
 *)
procedure R3D_DrawModel(model: TModel; position: TVector3; scale: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModel';

(*
 * @brief Draws a model with advanced transformation options.
 *
 * This function renders a model with a specified position, rotation axis, rotation
 * angle, and scale. It provides more control over how the model is transformed before
 * rendering.
 *
 * @param model The model to render.
 * @param position The position to place the model at.
 * @param rotationAxis The axis of rotation for the model.
 * @param rotationAngle The angle to rotate the model.
 * @param scale The scale factor to apply to the model.
 *)
procedure R3D_DrawModelEx(model: TModel; position, rotationAxis: TVector3; rotationAngle: Single; scale: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawModelEx';

(*
 * @brief Draws a sprite at a specified position.
 *
 * This function renders a sprite in 3D space at the given position.
 * It supports negative scaling to flip the sprite.
 *
 * @param sprite The sprite to render.
 * @param position The position to place the sprite at.
 *)
procedure R3D_DrawSprite(sprite: TR3D_Sprite; position: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSprite';

(*
 * @brief Draws a sprite with size and rotation options.
 *
 * This function allows rendering a sprite with a specified size and rotation.
 * It supports negative size values for flipping the sprite.
 *
 * @param sprite The sprite to render.
 * @param position The position to place the sprite at.
 * @param size The size of the sprite (negative values flip the sprite).
 * @param rotation The rotation angle in degrees.
 *)
procedure R3D_DrawSpriteEx(sprite: TR3D_Sprite; position: TVector3; size: TVector2; rotation: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpriteEx';

(*
 * @brief Draws a sprite with full transformation control.
 *
 * This function provides advanced transformation options, allowing
 * customization of size, rotation axis, and rotation angle.
 * It supports all billboard modes, or can be drawn without billboarding.
 *
 * @param sprite The sprite to render.
 * @param position The position to place the sprite at.
 * @param size The size of the sprite (negative values flip the sprite).
 * @param rotationAxis The axis around which the sprite rotates.
 * @param rotationAngle The angle to rotate the sprite around the given axis.
 *)
procedure R3D_DrawSpritePro(sprite: TR3D_Sprite; position: TVector3; size: TVector2; rotationAxis: TVector3; rotationAngle: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawSpritePro';

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
 * @param mesh The mesh used to represent each particle.
 * @param material The material applied to the particle mesh.
 *)
procedure R3D_DrawParticleSystem(const system: PR3D_ParticleSystem; mesh: Tmesh; material: Tmaterial); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawParticleSystem';

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
 * @param mesh The mesh used to represent each particle.
 * @param material The material applied to the particle mesh.
 * @param transform A transformation matrix applied to all particles.
 *)
procedure R3D_DrawParticleSystemEx(const system: PR3D_ParticleSystem; mesh: TMesh; material: TMaterial; transform: TMatrix); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawParticleSystemEx';

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
function R3D_CreateLight(type_: R3D_LightType): TR3D_Light; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_CreateLight';

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
function R3D_IsLightExist(id: TR3D_Light): boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsLightExist';

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
function R3D_IsLightActive(id: TR3D_Light): boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsLightActive';

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
procedure R3D_SetLightActive(id: TR3D_Light;  active: Boolean); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightActive';

(*
 * @brief Gets the color of a light.
 *
 * This function retrieves the color of the specified light as a `Color` structure.
 *
 * @param id The ID of the light.
 * @return The color of the light as a `Color` structure.
 *)
function R3D_GetLightColor(id: TR3D_Light): TColorB; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightColor';

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
procedure R3D_SetLightColor(id: TR3D_Light; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightColor';

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
 * @param id The ID of the light.
 * @param direction The new direction to set for the light.
 *)
procedure R3D_SetLightDirection(id: TR3D_Light; direction: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightDirection';

(*
 * @brief Sets the target of a directional light.
 *
 * This function sets the target that a directional light will point towards.
 * Only applicable to directional lights or spot lights.
 *
 * @param id The ID of the light.
 * @param target The target position the light should focus on.
 *)
procedure R3D_SetLightTarget(id: TR3D_Light; target: TVector3); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightTarget';

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
 * @brief Gets the size of a light source.
 *
 * This function retrieves the size of the specified light source, which is used for PCSS.
 * The size affects how shadows are computed and how soft or sharp they appear.
 *
 * @param id The ID of the light.
 * @return The size of the light.
 *)
function R3D_GetLightSize(id: TR3D_Light): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetLightSize';

(*
 * @brief Sets the size of a light source.
 *
 * This function sets the size of the specified light source.
 * The size influences how shadows are rendered, with larger sizes creating softer shadows.
 *
 * @param id The ID of the light.
 * @param size The new size to set for the light.
 *)
procedure R3D_SetLightSize(id: TR3D_Light; size: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetLightSize';

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
 * @brief Computes and returns the AABB (Axis-Aligned Bounding Box) of the particle emitter system.
 *
 * This function performs a simulation of the particle system to estimate the AABB of the particles. It calculates the
 * possible positions of each particle at both half of their lifetime and at the end of their lifetime. The resulting
 * AABB approximates the region of space the particle system occupies, which helps in determining if the system should
 * be rendered or not based on camera frustum culling.
 *
 * @param system A pointer to the `R3D_ParticleSystem` whose AABB is to be computed.
 * @return The computed `BoundingBox` of the particle system.
 *)
function R3D_GetParticleSystemBoundingBox(system: PR3D_ParticleSystem): TBoundingBox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetParticleSystemBoundingBox';

// --------------------------------------------
// CURVES: Interpolation Curves Functions
// --------------------------------------------

(*
 * @brief Load a sprite from a texture.
 *
 * This function creates a `R3D_Sprite` using the provided texture. The texture will be used as the albedo of the sprite's material.
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
 * It should be called when the sprite is no longer needed to avoid memory leaks.
 *
 * @warning This function does not free the texture associated with the sprite.
 * The caller is responsible for managing the texture's lifetime.
 *
 * @param sprite The `R3D_Sprite` to be unloaded.
 *)
procedure R3D_UnloadSprite(sprite: TR3D_Sprite); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_UnloadSprite';

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
procedure R3D_SetBackgroundColor(color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBackgroundColor';

(*
 * @brief Sets the ambient light color when no skybox is enabled.
 *
 * This function defines the ambient light color to be used when no skybox is active.
 * It affects the overall lighting of the scene when no skybox is present.
 *
 * @param color The color to set for ambient light.
 *)
procedure R3D_SetAmbientColor(color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetAmbientColor';

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
procedure R3D_DisableSkybox(); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DisableSkybox';

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
function R3D_GetSkyboxRotation(): TVector3; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSkyboxRotation';

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
function R3D_GetSSAO(): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAO';

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
function R3D_GetSSAORadius(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAORadius';

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
function R3D_GetSSAOBias(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAOBias';

(*
 * @brief Sets the number of iterations for SSAO effect.
 *
 * This function sets the number of iterations (or samples) used to calculate the
 * SSAO effect. A higher value will result in a more accurate and smoother effect,
 * but may also be more performance-intensive.
 *
 * @param value The number of iterations for SSAO.
 *)
procedure R3D_SetSSAOIterations(value: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetSSAOIterations';

(*
 * @brief Gets the current number of SSAO iterations.
 *
 * This function retrieves the current number of iterations (or samples) used to
 * calculate the SSAO effect.
 *
 * @return The number of SSAO iterations.
 *)
function R3D_GetSSAOIterations(): Integer; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSSAOIterations';

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
function R3D_GetBloomMode(): R3D_Bloom; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomMode';

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
function R3D_GetBloomIntensity(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomIntensity';

(*
 * @brief Sets the HDR threshold for bloom.
 *
 * This function defines the brightness threshold above which pixels contribute
 * to the bloom effect. Lower values will make more areas of the image glow.
 *
 * @param value The HDR threshold for bloom.
 *)
procedure R3D_SetBloomHdrThreshold(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomHdrThreshold';

(*
 * @brief Gets the current HDR threshold for bloom.
 *
 * This function retrieves the brightness threshold above which pixels contribute
 * to the bloom effect.
 *
 * @return The current HDR threshold for bloom.
 *)
function R3D_GetBloomHdrThreshold(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomHdrThreshold';

(*
 * @brief Sets the HDR threshold for the sky in bloom.
 *
 * This function defines a separate HDR threshold for the sky when applying bloom.
 * This allows finer control over the intensity of the bloom effect on sky elements.
 *
 * @param value The HDR threshold for bloom on the sky.
 *)
procedure R3D_SetBloomSkyHdrThreshold(value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomSkyHdrThreshold';

(*
 * @brief Gets the current HDR threshold for bloom on the sky.
 *
 * This function retrieves the HDR threshold specifically applied to the sky for bloom.
 *
 * @return The current HDR threshold for sky bloom.
 *)
function R3D_GetBloomSkyHdrThreshold(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomSkyHdrThreshold';

(*
 * @brief Sets the number of iterations for the bloom effect.
 *
 * This function defines how many iterations are performed to blur the bright areas.
 * Higher values result in a smoother and more pronounced bloom effect but may
 * impact performance.
 *
 * @param value The number of bloom iterations.
 *)
procedure R3D_SetBloomIterations(value: Integer); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetBloomIterations';

(*
 * @brief Gets the current number of bloom iterations.
 *
 * This function retrieves the number of iterations used to process the bloom effect.
 *
 * @return The current number of bloom iterations.
 *)
function R3D_GetBloomIterations(): Integer; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBloomIterations';

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
function R3D_GetFogMode(): R3D_Fog; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogMode';

(*
 * @brief Sets the color of the fog.
 *
 * This function defines the color of the fog effect applied to the scene.
 * The fog color blends with objects as they are affected by fog.
 *
 * @param color The color to set for the fog.
 *)
procedure R3D_SetFogColor(color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetFogColor';

(*
 * @brief Gets the current fog color.
 *
 * This function retrieves the color currently used for the fog effect.
 *
 * @return The current fog color.
 *)
function R3D_GetFogColor(): TColorB; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogColor';

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
function R3D_GetFogStart(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogStart';

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
function R3D_GetFogEnd(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogEnd';

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
function R3D_GetFogDensity(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetFogDensity';

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
function R3D_GetTonemapMode(): R3D_Tonemap; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapMode';

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
function R3D_GetTonemapExposure(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapExposure';

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
function R3D_GetTonemapWhite(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetTonemapWhite';

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
function R3D_GetBrightness(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBrightness';

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
function R3D_GetContrast(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetContrast';

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
function R3D_GetSaturation(): Single; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetSaturation';

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
function  R3D_LoadSkybox(const fileName: PChar; layout: TCubemapLayout): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkybox';

(*
 * @brief Loads a skybox from a high dynamic range (HDR) image.
 *
 * This function loads a skybox from an HDR image and converts it into a cubemap.
 * The size parameter determines the resolution of the generated cubemap.
 *
 * @param fileName The path to the HDR image file.
 * @param size The resolution of the cubemap (e.g., 512, 1024).
 * @return The loaded skybox object.
 *)
function R3D_LoadSkyboxHDR(const fileName: PChar; size: Integer): TR3D_Skybox; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_LoadSkyboxHDR';

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
 * This function determines whether a given 3D point is within the current camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param position The position of the point to check.
 * @return `true` if the point is inside the frustum, `false` otherwise.
 *)
function R3D_IsPointInFrustum(position: TVector3): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsPointInFrustum';

(*
 * @brief Checks if a point is inside the view frustum (alternative XYZ version).
 *
 * This function performs the same check as `R3D_IsPointInFrustum`, but allows specifying
 * the point coordinates separately instead of using a `Vector3`.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param x The X coordinate of the point.
 * @param y The Y coordinate of the point.
 * @param z The Z coordinate of the point.
 * @return `true` if the point is inside the frustum, `false` otherwise.
 *)
function R3D_IsPointInFrustumXYZ(x, y, z: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsPointInFrustumXYZ';

(*
 * @brief Checks if a sphere is inside the view frustum.
 *
 * This function tests whether a sphere, defined by a center position and radius,
 * is at least partially inside the camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param position The center of the sphere.
 * @param radius The radius of the sphere.
 * @return `true` if the sphere is at least partially inside the frustum, `false` otherwise.
 *)
function R3D_IsSphereInFrustum(position: TVector3; radius: Single): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsSphereInFrustum';

(*
 * @brief Checks if an axis-aligned bounding box (AABB) is inside the view frustum.
 *
 * This function determines whether an AABB is at least partially visible within the camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param aabb The bounding box to test.
 * @return `true` if any part of the bounding box is inside the frustum, `false` otherwise.
 *)
function R3D_IsBoundingBoxInFrustum(aabb: TBoundingBox): Boolean; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_IsBoundingBoxInFrustum';

// --------------------------------------------
// UTILS: Material Configuration Functions
// --------------------------------------------

(*
 * @brief Sets the albedo (diffuse color) properties of a material.
 *
 * This function assigns an albedo texture and color to a material.
 * If a texture is provided, it is used as the albedo map. The color is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional albedo texture (set to NULL for none).
 * @param color Albedo color to apply.
 *)
procedure R3D_SetMaterialAlbedo(material: PMaterial; texture: PTexture2D; color: TColorB); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetMaterialAlbedo';

(*
 * @brief Sets the ambient occlusion properties of a material.
 *
 * This function assigns an ambient occlusion (AO) texture and intensity value to a material.
 * If a texture is provided, it is used as the AO map. The intensity controls the effect strength.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional occlusion texture (set to NULL for none).
 * @param value Occlusion strength (0.0 to 1.0).
 *)
procedure R3D_SetMaterialOcclusion(material: PMaterial; texture: PTexture2D; value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetMaterialOcclusion';

(*
 * @brief Sets the roughness properties of a material.
 *
 * This function assigns a roughness texture and scalar value to a material.
 * If a texture is provided, it is used as the roughness map. The scalar value is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional roughness texture (set to NULL for none).
 * @param value Roughness factor (0.0 = smooth, 1.0 = rough).
 *)
procedure R3D_SetMaterialRoughness(material: PMaterial; texture: PTexture2D; value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetMaterialRoughness';

(*
 * @brief Sets the metalness properties of a material.
 *
 * This function assigns a metalness texture and scalar value to a material.
 * If a texture is provided, it is used as the metalness map. The scalar value is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional metalness texture (set to NULL for none).
 * @param value Metalness factor (0.0 = non-metallic, 1.0 = metallic).
 *)
procedure R3D_SetMaterialMetalness(material: PMaterial; texture: PTexture2D; value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetMaterialMetalness';

(*
 * @brief Sets the emission properties of a material.
 *
 * This function assigns an emission texture, emission color, and intensity to a material.
 * If a texture is provided, it is used as the emission map. The color and intensity control
 * the final emission effect.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional emission texture (set to NULL for none).
 * @param color Emission color.
 * @param value Emission intensity.
 *)
procedure R3D_SetMaterialEmission(material: PMaterial; texture: PTexture2D; color: TColorB; value: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_SetMaterialEmission';



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
function R3D_GetWhiteTexture(): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetWhiteTexture';

(*
 * @brief Retrieves a default black texture.
 *
 * This texture is fully black (0,0,0,1), useful for masking or default values.
 *
 * @return A black texture.
 *)
function R3D_GetBlackTexture(): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBlackTexture';

(*
 * @brief Retrieves a default normal map texture.
 *
 * This texture represents a neutral normal map (0.5, 0.5, 1.0), which applies no normal variation.
 *
 * @return A neutral normal texture.
 *)
function R3D_GetNormalTexture(): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetNormalTexture';

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
function R3D_GetBufferColor(): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBufferColor';

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
function R3D_GetBufferDepth(): TTexture2D; cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_GetBufferDepth';

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
 * @brief Renders the bright colors buffer to the screen.
 *
 * Displays the bright color buffer, which is used for bloom effects.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 *)
procedure R3D_DrawBufferBrightColors(x, y, w, h: Single); cdecl; external {$IFNDEF RAY_STATIC}r3dName{$ENDIF} name 'R3D_DrawBufferBrightColors';

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

{$IFDEF linux}
{$IFDEF RAY_STATIC}
 {$linklib c}
 {$linklib m}
 {$linklib dl}
 {$linklib libr3d.a}
{$endif}
{$endif}



end.

