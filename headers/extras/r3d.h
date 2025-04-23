/*
 * Copyright (c) 2025 Le Juez Victor
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
 */

#ifndef R3D_H
#define R3D_H

#include <raylib.h>


 // --------------------------------------------
 //                   DEFINES
 // --------------------------------------------

#if defined(_WIN32)
#   if defined(__TINYC__)
#       define __declspec(x) __attribute__((x))
#   endif
#   if defined(R3D_BUILD_SHARED)
#       define R3DAPI __declspec(dllexport)
#   elif defined(R3D_USE_SHARED)
#       define R3DAPI __declspec(dllimport)
#   endif
#else
#   if defined(R3D_BUILD_SHARED)
#       define R3DAPI __attribute__((visibility("default")))
#   endif
#endif

#ifndef R3DAPI
#   define R3DAPI extern
#endif



 // --------------------------------------------
 //                   ENUMS
 // --------------------------------------------

/**
 * @brief Flags to configure the rendering engine behavior.
 *
 * These flags control various aspects of the rendering pipeline.
 */
typedef unsigned int R3D_Flags;

#define R3D_FLAG_NONE           0           /*< No special rendering flags */
#define R3D_FLAG_FXAA           (1 << 0)    /*< Enables Fast Approximate Anti-Aliasing (FXAA) */
#define R3D_FLAG_BLIT_LINEAR    (1 << 1)    /*< Uses linear filtering when blitting the final image */
#define R3D_FLAG_ASPECT_KEEP    (1 << 2)    /*< Maintains the aspect ratio of the internal resolution when blitting the final image */
#define R3D_FLAG_STENCIL_TEST   (1 << 3)    /*< Performs a stencil test on each rendering pass affecting geometry */
#define R3D_FLAG_DEPTH_PREPASS  (1 << 4)    /*< Performs a depth pre-pass before forward rendering, improving desktop GPU performance but unnecessary on mobile */
#define R3D_FLAG_8_BIT_NORMALS  (1 << 5)    /*< Use 8-bit precision for the normals buffer (deferred); default is 16-bit float */

/**
 * @brief Defines the rendering mode used in the pipeline.
 *
 * Each mode has its own advantages depending on the hardware and rendering needs.
 */
typedef enum {
    R3D_RENDER_AUTO_DETECT = 0,         /**< Automatically determines the rendering mode based on the material,
                                             for example, by analyzing the albedo texture formats or the alpha 
                                             value of albedo colors. This is the default mode. */
    R3D_RENDER_DEFERRED = 1,            ///< Optimized for desktop GPUs, but does not support transparency.
    R3D_RENDER_FORWARD = 2,             ///< Works well on tile-based renderers, supports transparency.
} R3D_RenderMode;

/**
 * @brief Blend modes for rendering.
 *
 * Defines common blending modes used in 3D rendering to combine source and destination colors.
 * @note The blend mode is applied only if you are in forward rendering mode or auto-detect mode.
 */
typedef enum {
    R3D_BLEND_OPAQUE,          ///< No blending, the source color fully replaces the destination color.
    R3D_BLEND_ALPHA,           ///< Alpha blending: source color is blended with the destination based on alpha value.
    R3D_BLEND_ADDITIVE,        ///< Additive blending: source color is added to the destination, making bright effects.
    R3D_BLEND_MULTIPLY         ///< Multiply blending: source color is multiplied with the destination, darkening the image.
} R3D_BlendMode;

/**
 * @brief Defines the shadow casting mode for objects in the scene.
 *
 * Determines how an object contributes to shadow mapping, which can affect
 * performance and visual accuracy depending on the rendering technique used.
 */
typedef enum {
    R3D_SHADOW_CAST_DISABLED,     ///< The object does not cast shadows.
    R3D_SHADOW_CAST_FRONT_FACES,  ///< Only front-facing polygons cast shadows.
    R3D_SHADOW_CAST_BACK_FACES,   ///< Only back-facing polygons cast shadows.
    R3D_SHADOW_CAST_ALL_FACES     ///< Both front and back-facing polygons cast shadows.
} R3D_ShadowCastMode;

/**
 * @brief Defines billboard modes for 3D objects.
 *
 * This enumeration defines how a 3D object aligns itself relative to the camera.
 * It provides options to disable billboarding or to enable specific modes of alignment.
 */
typedef enum {
    R3D_BILLBOARD_DISABLED,     ///< Billboarding is disabled; the object retains its original orientation.
    R3D_BILLBOARD_FRONT,        ///< Full billboarding; the object fully faces the camera, rotating on all axes.
    R3D_BILLBOARD_Y_AXIS        /**< Y-axis constrained billboarding; the object rotates only around the Y-axis,
                                     keeping its "up" orientation fixed. This is suitable for upright objects like characters or signs. */
} R3D_BillboardMode;

/**
 * @brief Types of lights supported by the rendering engine.
 *
 * Each light type has different behaviors and use cases.
 */
typedef enum {
    R3D_LIGHT_DIR,                      ///< Directional light, affects the entire scene with parallel rays.
    R3D_LIGHT_SPOT,                     ///< Spot light, emits light in a cone shape.
    R3D_LIGHT_OMNI                      ///< Omni light, emits light in all directions from a single point.
} R3D_LightType;

/**
 * @brief Modes for updating shadow maps.
 *
 * Determines how often the shadow maps are refreshed.
 */
typedef enum {
    R3D_SHADOW_UPDATE_MANUAL,           ///< Shadow maps update only when explicitly requested.
    R3D_SHADOW_UPDATE_INTERVAL,         ///< Shadow maps update at defined time intervals.
    R3D_SHADOW_UPDATE_CONTINUOUS        ///< Shadow maps update every frame for real-time accuracy.
} R3D_ShadowUpdateMode;

/**
 * @brief Bloom effect modes.
 *
 * Specifies different post-processing bloom techniques that can be applied
 * to the rendered scene. Bloom effects enhance the appearance of bright areas
 * by simulating light bleeding, contributing to a more cinematic and realistic look.
 */
 typedef enum {
    R3D_BLOOM_DISABLED,     ///< Bloom effect is disabled. The scene is rendered without any glow enhancement.
    R3D_BLOOM_MIX,          ///< Blends the bloom effect with the original scene using linear interpolation (Lerp).
    R3D_BLOOM_ADDITIVE,     ///< Adds the bloom effect additively to the scene, intensifying bright regions.
    R3D_BLOOM_SCREEN        ///< Combines the scene and bloom using screen blending, which brightens highlights
} R3D_Bloom;

/**
 * @brief Fog effect modes.
 *
 * Determines how fog is applied to the scene, affecting depth perception and atmosphere.
 */
typedef enum {
    R3D_FOG_DISABLED, ///< Fog effect is disabled.
    R3D_FOG_LINEAR,   ///< Fog density increases linearly with distance from the camera.
    R3D_FOG_EXP2,     ///< Exponential fog (exp2), where density increases exponentially with distance.
    R3D_FOG_EXP       ///< Exponential fog, similar to EXP2 but with a different rate of increase.
} R3D_Fog;

/**
 * @brief Tone mapping modes.
 *
 * Controls how high dynamic range (HDR) colors are mapped to low dynamic range (LDR) for display.
 */
typedef enum {
    R3D_TONEMAP_LINEAR,   ///< Simple linear mapping of HDR values.
    R3D_TONEMAP_REINHARD, ///< Reinhard tone mapping, a balanced method for compressing HDR values.
    R3D_TONEMAP_FILMIC,   ///< Filmic tone mapping, mimicking the response of photographic film.
    R3D_TONEMAP_ACES,     ///< ACES tone mapping, a high-quality cinematic rendering technique.
    R3D_TONEMAP_AGX       ///< AGX tone mapping, a modern technique designed to preserve both highlight and shadow details for HDR rendering.
} R3D_Tonemap;



// --------------------------------------------
//                   TYPES
// --------------------------------------------

/**
 * @brief Represents a unique identifier for a light in R3D.
 *
 * This ID is used to reference a specific light when calling R3D lighting functions.
 */
typedef unsigned int R3D_Light;

/**
 * @brief Structure representing a skybox and its related textures for lighting.
 *
 * This structure contains textures used for rendering a skybox, as well as
 * precomputed lighting textures used for image-based lighting (IBL).
 */
typedef struct {
    TextureCubemap cubemap;  ///< The skybox cubemap texture for the background and reflections.
    Texture2D irradiance;    ///< The irradiance cubemap for diffuse ambient lighting.
    Texture2D prefilter;     ///< The prefiltered cubemap for specular reflections with mipmaps.
} R3D_Skybox;

/**
 * @brief Represents a 3D sprite with billboard rendering and animation support.
 *
 * This structure defines a 3D sprite, which by default is rendered as a billboard around the Y-axis.
 * The sprite supports frame-based animations and can be configured to use various billboard modes.
 *
 * @warning The shadow mode does not handle transparency. If shadows are enabled, the entire quad will be rendered in the shadow map,
 * potentially causing undesired visual artifacts for semi-transparent sprites.
 */
typedef struct {
    Material material;      ///< The material used for rendering the sprite, including its texture and shading properties.
    float currentFrame;     ///< The current animation frame, represented as a floating-point value to allow smooth interpolation.
    Vector2 frameSize;      ///< The size of a single animation frame, in texture coordinates (width and height).
    int xFrameCount;        ///< The number of frames along the horizontal (X) axis of the texture.
    int yFrameCount;        ///< The number of frames along the vertical (Y) axis of the texture.
} R3D_Sprite;

/**
 * @brief Represents a keyframe in an interpolation curve.
 *
 * A keyframe contains two values: the time at which the keyframe occurs and the value of the interpolation at that time.
 * The time is normalized between 0.0 and 1.0, where 0.0 represents the start of the curve and 1.0 represents the end.
 */
typedef struct {
    float time;             ///< Normalized time of the keyframe, ranging from 0.0 to 1.0.
    float value;            ///< The value of the interpolation at this keyframe.
} R3D_Keyframe;

/**
 * @brief Represents an interpolation curve composed of keyframes.
 *
 * This structure contains an array of keyframes and metadata about the array, such as the current number of keyframes
 * and the allocated capacity. The keyframes define a curve that can be used for smooth interpolation between values
 * over a normalized time range (0.0 to 1.0).
 */
typedef struct {
    R3D_Keyframe* keyframes;    ///< Dynamic array of keyframes defining the interpolation curve.
    unsigned int capacity;      ///< Allocated size of the keyframes array.
    unsigned int count;         ///< Current number of keyframes in the array.
} R3D_InterpolationCurve;

/**
 * @struct R3D_Particle
 * @brief Represents a particle in a 3D particle system, with properties
 *        such as position, velocity, rotation, and color modulation.
 */
typedef struct {

    float lifetime;                 ///< Duration of the particle's existence in seconds.

    Matrix transform;               ///< The particle's current transformation matrix in 3D space.

    Vector3 position;               ///< The current position of the particle in 3D space.
    Vector3 rotation;               ///< The current rotation of the particle in 3D space (Euler angles).
    Vector3 scale;                  ///< The current scale of the particle in 3D space.
    Color color;                    ///< The current color of the particle, representing its color modulation.

    Vector3 velocity;               ///< The current velocity of the particle in 3D space.
    Vector3 angularVelocity;        ///< The current angular velocity of the particle in radians (Euler angles).

    Vector3 baseScale;              ///< The initial scale of the particle in 3D space.
    Vector3 baseVelocity;           ///< The initial velocity of the particle in 3D space.
    Vector3 baseAngularVelocity;    ///< The initial angular velocity of the particle in radians (Euler angles).
    unsigned char baseOpacity;      ///< The initial opacity of the particle, ranging from 0 (fully transparent) to 255 (fully opaque).

} R3D_Particle;

/**
 * @brief Represents a CPU-based particle system with various properties and settings.
 *
 * This structure contains configuration data for a particle system, such as mesh information, initial properties,
 * curves for controlling properties over time, and settings for shadow casting, emission rate, and more.
 */
typedef struct {

    R3D_Particle* particles;            ///< Pointer to the array of particles in the system.
    int capacity;                       ///< The maximum number of particles the system can manage.
    int count;                          ///< The current number of active particles in the system.

    Vector3 position;                   ///< The initial position of the particle system. Default: (0, 0, 0).
    Vector3 gravity;                    ///< The gravity applied to the particles. Default: (0, -9.81, 0).

    Vector3 initialScale;               ///< The initial scale of the particles. Default: (1, 1, 1).
    float scaleVariance;                ///< The variance in particle scale. Default: 0.0f.

    Vector3 initialRotation;            ///< The initial rotation of the particles in Euler angles (degrees). Default: (0, 0, 0).
    Vector3 rotationVariance;           ///< The variance in particle rotation in Euler angles (degrees). Default: (0, 0, 0).

    Color initialColor;                 ///< The initial color of the particles. Default: WHITE.
    Color colorVariance;                ///< The variance in particle color. Default: BLANK.

    Vector3 initialVelocity;            ///< The initial velocity of the particles. Default: (0, 0, 0).
    Vector3 velocityVariance;           ///< The variance in particle velocity. Default: (0, 0, 0).

    Vector3 initialAngularVelocity;     ///< The initial angular velocity of the particles in Euler angles (degrees). Default: (0, 0, 0).
    Vector3 angularVelocityVariance;    ///< The variance in angular velocity. Default: (0, 0, 0).

    float lifetime;                     ///< The lifetime of the particles in seconds. Default: 1.0f.
    float lifetimeVariance;             ///< The variance in lifetime in seconds. Default: 0.0f.

    float emissionTimer;                ///< Use to control automatic emission, should not be modified manually.
    float emissionRate;                 ///< The rate of particle emission in particles per second. Default: 10.0f.
    float spreadAngle;                  ///< The angle of propagation of the particles in a cone (degrees). Default: 0.0f.

    R3D_InterpolationCurve* scaleOverLifetime;              ///< Curve controlling the scale evolution of the particles over their lifetime. Default: NULL.
    R3D_InterpolationCurve* speedOverLifetime;              ///< Curve controlling the speed evolution of the particles over their lifetime. Default: NULL.
    R3D_InterpolationCurve* opacityOverLifetime;            ///< Curve controlling the opacity evolution of the particles over their lifetime. Default: NULL.
    R3D_InterpolationCurve* angularVelocityOverLifetime;    ///< Curve controlling the angular velocity evolution of the particles over their lifetime. Default: NULL.

    bool autoEmission;               /**< Indicates whether particle emission is automatic when calling `R3D_UpdateParticleSystem`.
                                      *   If false, emission is manual using `R3D_EmitParticle`. Default: true.
                                      */

} R3D_ParticleSystem;


/* === Extern C guard === */

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

// --------------------------------------------
// CORE: Init And Config Functions
// --------------------------------------------

/**
 * @brief Initializes the rendering engine.
 * 
 * This function sets up the internal rendering system with the provided resolution 
 * and state flags, which define the internal behavior. These flags can be modified
 * later via R3D_SetState.
 * 
 * @param resWidth Width of the internal resolution.
 * @param resHeight Height of the internal resolution.
 * @param flags Flags indicating internal behavior (modifiable via R3D_SetState).
 */
R3DAPI void R3D_Init(int resWidth, int resHeight, unsigned int flags);

/**
 * @brief Closes the rendering engine and deallocates all resources.
 * 
 * This function shuts down the rendering system and frees all allocated memory, 
 * including the resources associated with the created lights.
 */
R3DAPI void R3D_Close(void);

/**
 * @brief Checks if a specific internal state flag is set.
 * 
 * @param flag The state flag to check.
 * @return True if the flag is set, false otherwise.
 */
R3DAPI bool R3D_HasState(unsigned int flag);

/**
 * @brief Sets internal state flags for the rendering engine.
 * 
 * This function modifies the behavior of the rendering engine by setting one or more 
 * state flags. Flags can be later cleared with R3D_ClearState.
 * 
 * @param flags The flags to set.
 */
R3DAPI void R3D_SetState(unsigned int flags);

/**
 * @brief Clears specific internal state flags.
 * 
 * This function clears one or more previously set state flags, modifying the 
 * behavior of the rendering engine accordingly.
 * 
 * @param flags The flags to clear.
 */
R3DAPI void R3D_ClearState(unsigned int flags);

/**
 * @brief Gets the current internal resolution.
 * 
 * This function retrieves the current internal resolution being used by the 
 * rendering engine.
 * 
 * @param width Pointer to store the width of the internal resolution.
 * @param height Pointer to store the height of the internal resolution.
 */
R3DAPI void R3D_GetResolution(int* width, int* height);

/**
 * @brief Updates the internal resolution.
 * 
 * This function changes the internal resolution of the rendering engine. Note that 
 * this process destroys and recreates all framebuffers, which may be a slow operation.
 * 
 * @param width The new width for the internal resolution.
 * @param height The new height for the internal resolution.
 * 
 * @warning This function may be slow due to the destruction and recreation of framebuffers.
 */
R3DAPI void R3D_UpdateResolution(int width, int height);

/**
 * @brief Sets a custom render target.
 * 
 * This function allows rendering to a custom framebuffer instead of the main one. 
 * Passing `NULL` will revert back to rendering to the main framebuffer.
 * 
 * @param target The custom render target (can be NULL to revert to the default framebuffer).
 */
R3DAPI void R3D_SetRenderTarget(RenderTexture* target);

/**
 * @brief Defines the bounds of the scene for directional light calculations.
 * 
 * This function sets the scene bounds used to determine which areas should be illuminated 
 * by directional lights. It is the user's responsibility to calculate and provide the 
 * correct bounds.
 * 
 * @param sceneBounds The bounding box defining the scene's dimensions.
 */
R3DAPI void R3D_SetSceneBounds(BoundingBox sceneBounds);



// --------------------------------------------
// CORE: Rendering Config Functions
// --------------------------------------------

/**
 * @brief Applies a render mode (Deferred or Forward).
 * 
 * This function sets the current render mode to either deferred or forward. It can 
 * be called at any time, including between `R3D_Begin` and `R3D_End`. The set mode 
 * will apply to all subsequent draw calls.
 * 
 * @param mode The render mode to apply.
 */
R3DAPI void R3D_ApplyRenderMode(R3D_RenderMode mode);

/**
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
 */
R3DAPI void R3D_ApplyBlendMode(R3D_BlendMode mode);

/**
 * @brief Sets the shadow casting mode for meshes.
 *
 * This function controls how meshes cast shadows in the scene. It can be
 * called at any time, including between `R3D_Begin` and `R3D_End`. The selected mode
 * will apply to all subsequent draw calls.
 *
 * @param mode The shadow casting mode to apply.
 */
R3DAPI void R3D_ApplyShadowCastMode(R3D_ShadowCastMode mode);

/**
 * @brief Applies a billboard mode to sprites or meshes.
 *
 * This function sets the current billboard mode, determining how objects orient
 * themselves relative to the camera. It can be called at any time, including
 * between `R3D_Begin` and `R3D_End`. The set mode will apply to all subsequent
 * draw calls.
 *
 * @param mode The billboard mode to apply.
 */
R3DAPI void R3D_ApplyBillboardMode(R3D_BillboardMode mode);

/**
 * @brief Sets an alpha threshold for forward rendering.
 *
 * This function defines an alpha scissor threshold, determining the minimum alpha
 * value required for a fragment to be rendered. Fragments with an alpha value below
 * the threshold will be discarded.
 *
 * @param threshold The alpha value threshold (usually from 0.0 to 1.0).
 */
R3DAPI void R3D_ApplyAlphaScissorThreshold(float threshold);


// --------------------------------------------
// CORE: Drawing Functions
// --------------------------------------------

/**
 * @brief Begins a rendering session for a 3D camera.
 * 
 * This function starts a rendering session, preparing the engine to handle subsequent 
 * draw calls using the provided camera settings.
 * 
 * @param camera The camera to use for rendering the scene.
 */
R3DAPI void R3D_Begin(Camera3D camera);

/**
 * @brief Ends the current rendering session.
 * 
 * This function signals the end of a rendering session, at which point the engine 
 * will process all necessary render passes and output the final result to the main 
 * or custom framebuffer.
 */
R3DAPI void R3D_End(void);

/**
 * @brief Draws a mesh with a specified material and transformation.
 * 
 * This function renders a mesh with the provided material and transformation matrix.
 * 
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param transform The transformation matrix to apply to the mesh.
 */
R3DAPI void R3D_DrawMesh(Mesh mesh, Material material, Matrix transform);

/**
 * @brief Draws a mesh with instancing support.
 * 
 * This function renders a mesh multiple times with different transformation matrices 
 * for each instance.
 * 
 * @param mesh The mesh to render.
 * @param material The material to apply to the mesh.
 * @param instanceTransforms Array of transformation matrices for each instance.
 * @param instanceCount The number of instances to render.
 */
R3DAPI void R3D_DrawMeshInstanced(Mesh mesh, Material material, Matrix* instanceTransforms, int instanceCount);

/**
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
 */
R3DAPI void R3D_DrawMeshInstancedEx(Mesh mesh, Material material, Matrix* instanceTransforms, Color* instanceColors, int instanceCount);

/**
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
 */
R3DAPI void R3D_DrawMeshInstancedPro(Mesh mesh, Material material, Matrix transform,
                                     Matrix* instanceTransforms, int transformsStride,
                                     Color* instanceColors, int colorsStride,
                                     int instanceCount);

/**
 * @brief Draws a model at a specified position and scale.
 * 
 * This function renders a model at the given position with the specified scale factor.
 * 
 * @param model The model to render.
 * @param position The position to place the model at.
 * @param scale The scale factor to apply to the model.
 */
R3DAPI void R3D_DrawModel(Model model, Vector3 position, float scale);

/**
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
 */
R3DAPI void R3D_DrawModelEx(Model model, Vector3 position, Vector3 rotationAxis, float rotationAngle, Vector3 scale);

/**
 * @brief Draws a sprite at a specified position.
 *
 * This function renders a sprite in 3D space at the given position.
 * It supports negative scaling to flip the sprite.
 *
 * @param sprite The sprite to render.
 * @param position The position to place the sprite at.
 */
R3DAPI void R3D_DrawSprite(R3D_Sprite sprite, Vector3 position);

/**
 * @brief Draws a sprite with size and rotation options.
 *
 * This function allows rendering a sprite with a specified size and rotation.
 * It supports negative size values for flipping the sprite.
 *
 * @param sprite The sprite to render.
 * @param position The position to place the sprite at.
 * @param size The size of the sprite (negative values flip the sprite).
 * @param rotation The rotation angle in degrees.
 */
R3DAPI void R3D_DrawSpriteEx(R3D_Sprite sprite, Vector3 position, Vector2 size, float rotation);

/**
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
 */
R3DAPI void R3D_DrawSpritePro(R3D_Sprite sprite, Vector3 position, Vector2 size, Vector3 rotationAxis, float rotationAngle);

/**
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
 */
R3DAPI void R3D_DrawParticleSystem(const R3D_ParticleSystem* system, Mesh mesh, Material material);

/**
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
 */
R3DAPI void R3D_DrawParticleSystemEx(const R3D_ParticleSystem* system, Mesh mesh, Material material, Matrix transform);



// --------------------------------------------
// LIGHTING: Lights Config Functions
// --------------------------------------------

/**
 * @brief Creates a new light of the specified type.
 *
 * This function creates a light of the given type. The light must be destroyed
 * manually when no longer needed by calling `R3D_DestroyLight`.
 *
 * @param type The type of light to create (directional, spot or omni-directional).
 * @return The ID of the created light.
 */
R3DAPI R3D_Light R3D_CreateLight(R3D_LightType type);

/**
 * @brief Destroys the specified light.
 *
 * This function deallocates the resources associated with the light and makes
 * the light ID invalid. It must be called after the light is no longer needed.
 *
 * @param id The ID of the light to destroy.
 */
R3DAPI void R3D_DestroyLight(R3D_Light id);

/**
 * @brief Checks if a light exists.
 *
 * This function checks if the specified light ID is valid and if the light exists.
 *
 * @param id The ID of the light to check.
 * @return True if the light exists, false otherwise.
 */
R3DAPI bool R3D_IsLightExist(R3D_Light id);

/**
 * @brief Gets the type of a light.
 *
 * This function returns the type of the specified light (directional, spot or omni-directional).
 *
 * @param id The ID of the light.
 * @return The type of the light.
 */
R3DAPI R3D_LightType R3D_GetLightType(R3D_Light id);

/**
 * @brief Checks if a light is active.
 *
 * This function checks whether the specified light is currently active (enabled or disabled).
 *
 * @param id The ID of the light to check.
 * @return True if the light is active, false otherwise.
 */
R3DAPI bool R3D_IsLightActive(R3D_Light id);

/**
 * @brief Toggles the state of a light (active or inactive).
 *
 * This function toggles the state of the specified light, turning it on if it is off,
 * or off if it is on.
 *
 * @param id The ID of the light to toggle.
 */
R3DAPI void R3D_ToggleLight(R3D_Light id);

/**
 * @brief Sets the active state of a light.
 *
 * This function allows manually turning a light on or off by specifying its active state.
 *
 * @param id The ID of the light to set the active state for.
 * @param active True to activate the light, false to deactivate it.
 */
R3DAPI void R3D_SetLightActive(R3D_Light id, bool active);

/**
 * @brief Gets the color of a light.
 *
 * This function retrieves the color of the specified light as a `Color` structure.
 *
 * @param id The ID of the light.
 * @return The color of the light as a `Color` structure.
 */
R3DAPI Color R3D_GetLightColor(R3D_Light id);

/**
 * @brief Gets the color of a light as a `Vector3`.
 *
 * This function retrieves the color of the specified light as a `Vector3`, where each
 * component (x, y, z) represents the RGB values of the light.
 *
 * @param id The ID of the light.
 * @return The color of the light as a `Vector3`.
 */
R3DAPI Vector3 R3D_GetLightColorV(R3D_Light id);

/**
 * @brief Sets the color of a light.
 *
 * This function sets the color of the specified light using a `Color` structure.
 *
 * @param id The ID of the light.
 * @param color The new color to set for the light.
 */
R3DAPI void R3D_SetLightColor(R3D_Light id, Color color);

/**
 * @brief Sets the color of a light using a `Vector3`.
 *
 * This function sets the color of the specified light using a `Vector3`, where each
 * component (x, y, z) represents the RGB values of the light.
 *
 * @param id The ID of the light.
 * @param color The new color to set for the light as a `Vector3`.
 */
R3DAPI void R3D_SetLightColorV(R3D_Light id, Vector3 color);

/**
 * @brief Gets the position of a light.
 *
 * This function retrieves the position of the specified light.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The position of the light as a `Vector3`.
 */
R3DAPI Vector3 R3D_GetLightPosition(R3D_Light id);

/**
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
 */
R3DAPI void R3D_SetLightPosition(R3D_Light id, Vector3 position);

/**
 * @brief Gets the direction of a light.
 *
 * This function retrieves the direction of the specified light.
 * Only applicable to directional lights or spot lights.
 *
 * @param id The ID of the light.
 * @return The direction of the light as a `Vector3`.
 */
R3DAPI Vector3 R3D_GetLightDirection(R3D_Light id);

/**
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
 */
R3DAPI void R3D_SetLightDirection(R3D_Light id, Vector3 direction);

/**
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
 */
R3DAPI void R3D_LightLookAt(R3D_Light id, Vector3 position, Vector3 target);

/**
 * @brief Gets the energy level of a light.
 *
 * This function retrieves the energy level (intensity) of the specified light.
 * Energy typically affects the brightness of the light.
 *
 * @param id The ID of the light.
 * @return The energy level of the light.
 */
R3DAPI float R3D_GetLightEnergy(R3D_Light id);

/**
 * @brief Sets the energy level of a light.
 *
 * This function sets the energy (intensity) of the specified light.
 * A higher energy value will result in a brighter light.
 *
 * @param id The ID of the light.
 * @param energy The new energy value to set for the light.
 */
R3DAPI void R3D_SetLightEnergy(R3D_Light id, float energy);

/**
 * @brief Gets the specular intensity of a light.
 *
 * This function retrieves the current specular intensity of the specified light.
 * Specular intensity affects how shiny surfaces appear when reflecting the light.
 *
 * @param id The ID of the light.
 * @return The current specular intensity of the light.
 */
R3DAPI float R3D_GetLightSpecular(R3D_Light id);

/**
 * @brief Sets the specular intensity of a light.
 *
 * This function sets the specular intensity of the specified light.
 * Higher specular values result in stronger and sharper highlights on reflective surfaces.
 *
 * @param id The ID of the light.
 * @param specular The new specular intensity value to set for the light.
 */
R3DAPI void R3D_SetLightSpecular(R3D_Light id, float specular);

/**
 * @brief Gets the range of a light.
 *
 * This function retrieves the range of the specified light, which determines how far the light can affect.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The range of the light.
 */
R3DAPI float R3D_GetLightRange(R3D_Light id);

/**
 * @brief Sets the range of a light.
 *
 * This function sets the range of the specified light.
 * The range determines how far the light can illuminate the scene before it fades out.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @param range The new range to set for the light.
 */
R3DAPI void R3D_SetLightRange(R3D_Light id, float range);

/**
 * @brief Gets the size of a light source.
 *
 * This function retrieves the size of the specified light source, which is used for PCSS.
 * The size affects how shadows are computed and how soft or sharp they appear.
 *
 * @param id The ID of the light.
 * @return The size of the light.
 */
R3DAPI float R3D_GetLightSize(R3D_Light id);

/**
 * @brief Sets the size of a light source.
 *
 * This function sets the size of the specified light source.
 * The size influences how shadows are rendered, with larger sizes creating softer shadows.
 *
 * @param id The ID of the light.
 * @param size The new size to set for the light.
 */
R3DAPI void R3D_SetLightSize(R3D_Light id, float size);

/**
 * @brief Gets the attenuation factor of a light.
 *
 * This function retrieves the attenuation factor of the specified light.
 * Attenuation controls how the intensity of a light decreases with distance.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @return The attenuation factor of the light.
 */
R3DAPI float R3D_GetLightAttenuation(R3D_Light id);

/**
 * @brief Sets the attenuation factor of a light.
 *
 * This function sets the attenuation factor of the specified light.
 * A higher attenuation value causes the light to lose intensity more quickly as the distance increases.
 * Only applicable to spot lights or omni-lights.
 *
 * @param id The ID of the light.
 * @param attenuation The new attenuation factor to set for the light.
 */
R3DAPI void R3D_SetLightAttenuation(R3D_Light id, float attenuation);

/**
 * @brief Gets the inner cutoff angle of a spotlight.
 *
 * This function retrieves the inner cutoff angle of a spotlight.
 * The inner cutoff defines the cone of light where the light is at full intensity.
 *
 * @param id The ID of the light.
 * @return The inner cutoff angle in degrees of the spotlight.
 */
R3DAPI float R3D_GetLightInnerCutOff(R3D_Light id);

/**
 * @brief Sets the inner cutoff angle of a spotlight.
 *
 * This function sets the inner cutoff angle of a spotlight.
 * The inner cutoff angle defines the cone where the light is at full intensity.
 * Anything outside this cone starts to fade.
 *
 * @param id The ID of the light.
 * @param degrees The new inner cutoff angle in degrees.
 */
R3DAPI void R3D_SetLightInnerCutOff(R3D_Light id, float degrees);

/**
 * @brief Gets the outer cutoff angle of a spotlight.
 *
 * This function retrieves the outer cutoff angle of a spotlight.
 * The outer cutoff defines the outer boundary of the light's cone, where the light starts to fade.
 *
 * @param id The ID of the light.
 * @return The outer cutoff angle in degrees of the spotlight.
 */
R3DAPI float R3D_GetLightOuterCutOff(R3D_Light id);

/**
 * @brief Sets the outer cutoff angle of a spotlight.
 *
 * This function sets the outer cutoff angle of a spotlight.
 * The outer cutoff defines the boundary of the light's cone where the light intensity starts to gradually decrease.
 *
 * @param id The ID of the light.
 * @param degrees The new outer cutoff angle in degrees.
 */
R3DAPI void R3D_SetLightOuterCutOff(R3D_Light id, float degrees);



// --------------------------------------------
// LIGHTING: Shadow Config Functions
// --------------------------------------------

/**
 * @brief Enables shadow casting for a light and sets the resolution of its shadow map.
 *
 * This function enables shadow casting for a specified light and allocates a shadow map with the specified resolution.
 * Shadows can be rendered from the light based on this shadow map.
 *
 * @param id The ID of the light for which shadows should be enabled.
 * @param resolution The resolution of the shadow map to be used by the light.
 */
R3DAPI void R3D_EnableShadow(R3D_Light id, int resolution);

/**
 * @brief Disables shadow casting for a light and optionally destroys its shadow map.
 *
 * This function disables shadow casting for the specified light and optionally frees the memory
 * used by its shadow map. If `destroyMap` is true, the shadow map will be destroyed, otherwise,
 * the map will be retained but the light will no longer cast shadows.
 *
 * @param id The ID of the light for which shadows should be disabled.
 * @param destroyMap Whether or not to destroy the shadow map associated with the light.
 */
R3DAPI void R3D_DisableShadow(R3D_Light id, bool destroyMap);

/**
 * @brief Checks if shadow casting is enabled for a light.
 *
 * This function checks if shadow casting is currently enabled for the specified light.
 *
 * @param id The ID of the light.
 * @return True if shadow casting is enabled, false otherwise.
 */
R3DAPI bool R3D_IsShadowEnabled(R3D_Light id);

/**
 * @brief Checks if a light has an associated shadow map.
 *
 * This function checks if the specified light has a shadow map allocated for it.
 *
 * @param id The ID of the light.
 * @return True if the light has a shadow map, false otherwise.
 */
R3DAPI bool R3D_HasShadowMap(R3D_Light id);

/**
 * @brief Gets the shadow map update mode of a light.
 *
 * This function retrieves the current mode for updating the shadow map of a light. The mode can be:
 * - Interval: Updates the shadow map at a fixed interval.
 * - Continuous: Updates the shadow map continuously.
 * - Manual: Updates the shadow map manually (via explicit function calls).
 *
 * @param id The ID of the light.
 * @return The shadow map update mode.
 */
R3DAPI R3D_ShadowUpdateMode R3D_GetShadowUpdateMode(R3D_Light id);

/**
 * @brief Sets the shadow map update mode of a light.
 *
 * This function sets the mode for updating the shadow map of the specified light.
 * The update mode controls when and how often the shadow map is refreshed.
 *
 * @param id The ID of the light.
 * @param mode The update mode to set for the shadow map (Interval, Continuous, or Manual).
 */
R3DAPI void R3D_SetShadowUpdateMode(R3D_Light id, R3D_ShadowUpdateMode mode);

/**
 * @brief Gets the frequency of shadow map updates for the interval update mode.
 *
 * This function retrieves the frequency (in milliseconds) at which the shadow map should be updated when
 * the interval update mode is enabled. This function is only relevant if the shadow map update mode is set
 * to "Interval".
 *
 * @param id The ID of the light.
 * @return The frequency in milliseconds at which the shadow map is updated.
 */
R3DAPI int R3D_GetShadowUpdateFrequency(R3D_Light id);

/**
 * @brief Sets the frequency of shadow map updates for the interval update mode.
 *
 * This function sets the frequency (in milliseconds) at which the shadow map should be updated when
 * the interval update mode is enabled. This function is only relevant if the shadow map update mode is set
 * to "Interval".
 *
 * @param id The ID of the light.
 * @param msec The frequency in milliseconds at which to update the shadow map.
 */
R3DAPI void R3D_SetShadowUpdateFrequency(R3D_Light id, int msec);

/**
 * @brief Forces an immediate update of the shadow map during the next rendering pass.
 *
 * This function forces the shadow map of the specified light to be updated during the next call to `R3D_End`.
 * This is primarily used for the manual update mode, but may also work for the interval mode.
 *
 * @param id The ID of the light.
 */
R3DAPI void R3D_UpdateShadowMap(R3D_Light id);

/**
 * @brief Gets the shadow bias of a light.
 *
 * This function retrieves the shadow bias value for the specified light. The shadow bias helps prevent shadow artifacts,
 * such as shadow acne, by slightly offsetting the depth comparisons used in shadow mapping.
 *
 * @param id The ID of the light.
 * @return The shadow bias value.
 */
R3DAPI float R3D_GetShadowBias(R3D_Light id);

/**
 * @brief Sets the shadow bias of a light.
 *
 * This function sets the shadow bias value for the specified light. Adjusting the shadow bias can help avoid shadow
 * artifacts such as shadow acne by modifying the depth comparisons used in shadow mapping.
 *
 * @param id The ID of the light.
 * @param value The shadow bias value to set.
 */
R3DAPI void R3D_SetShadowBias(R3D_Light id, float value);



// --------------------------------------------
// LIGHTING: Light Helper Functions
// --------------------------------------------

/**
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
 */
R3DAPI void R3D_DrawLightShape(R3D_Light id);



// --------------------------------------------
// PARTICLES: Particle System Functions
// --------------------------------------------

/**
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
 */
R3DAPI R3D_ParticleSystem R3D_LoadParticleSystem(int maxParticles);

/**
 * @brief Unloads the particle emitter system and frees allocated memory.
 *
 * This function deallocates the memory used by the particle emitter system and clears the associated resources.
 * It should be called when the particle system is no longer needed to prevent memory leaks.
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be unloaded.
 */
R3DAPI void R3D_UnloadParticleSystem(R3D_ParticleSystem* system);

/**
 * @brief Emits a particle in the particle system.
 *
 * This function triggers the emission of a new particle in the particle system. It handles the logic of adding a new
 * particle to the system and initializing its properties based on the current state of the system.
 *
 * @param system A pointer to the `R3D_ParticleSystemCPU` where the particle will be emitted.
 * @return `true` if the particle was successfully emitted, `false` if the system is at full capacity and cannot emit more particles.
 */
R3DAPI bool R3D_EmitParticle(R3D_ParticleSystem* system);

/**
 * @brief Updates the particle emitter system by advancing particle positions.
 *
 * This function updates the positions and properties of particles in the system based on the elapsed time. It handles
 * simulation of particle movement, gravity, and other physics-based calculations.
 *
 * @param system A pointer to the `R3D_ParticleSystem` to be updated.
 * @param deltaTime The time elapsed since the last update (in seconds).
 */
R3DAPI void R3D_UpdateParticleSystem(R3D_ParticleSystem* system, float deltaTime);

/**
 * @brief Computes and returns the AABB (Axis-Aligned Bounding Box) of the particle emitter system.
 *
 * This function performs a simulation of the particle system to estimate the AABB of the particles. It calculates the
 * possible positions of each particle at both half of their lifetime and at the end of their lifetime. The resulting
 * AABB approximates the region of space the particle system occupies, which helps in determining if the system should
 * be rendered or not based on camera frustum culling.
 *
 * @param system A pointer to the `R3D_ParticleSystem` whose AABB is to be computed.
 * @return The computed `BoundingBox` of the particle system.
 */
R3DAPI BoundingBox R3D_GetParticleSystemBoundingBox(R3D_ParticleSystem* system);



// --------------------------------------------
// CURVES: Interpolation Curves Functions
// --------------------------------------------

/**
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
 */
R3DAPI R3D_Sprite R3D_LoadSprite(Texture2D texture, int xFrameCount, int yFrameCount);

/**
 * @brief Unload a sprite and free associated resources.
 *
 * This function releases the resources allocated for a `R3D_Sprite`.
 * It should be called when the sprite is no longer needed to avoid memory leaks.
 *
 * @warning This function does not free the texture associated with the sprite.
 * The caller is responsible for managing the texture's lifetime.
 *
 * @param sprite The `R3D_Sprite` to be unloaded.
 */
R3DAPI void R3D_UnloadSprite(R3D_Sprite sprite);

/**
 * @brief Updates the animation of a sprite.
 *
 * This function updates the current frame of the sprite's animation based on the provided speed. The animation frames are read from
 * right to left, advancing the cursor to the next row after completing a line.
 *
 * @note The `speed` parameter can be calculated as the number of frames per second multiplied by `GetFrameTime()`.
 *
 * @param sprite A pointer to the `R3D_Sprite` to update.
 * @param speed The speed at which the animation progresses, in frames per second.
 */
R3DAPI void R3D_UpdateSprite(R3D_Sprite* sprite, float speed);

/**
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
 */
R3DAPI void R3D_UpdateSpriteEx(R3D_Sprite* sprite, int firstFrame, int lastFrame, float speed);



// --------------------------------------------
// CURVES: Interpolation Curves Functions
// --------------------------------------------

/**
 * @brief Loads an interpolation curve with a specified initial capacity.
 *
 * This function initializes an interpolation curve with the given capacity. The capacity represents the initial size of
 * the memory allocated for the curve. You can add keyframes to the curve using `R3D_AddKeyframe`. If adding a keyframe
 * exceeds the initial capacity, the system will automatically reallocate memory and double the initial capacity.
 *
 * @param capacity The initial capacity (size) of the interpolation curve. This is the number of keyframes that can be added
 *                 before a reallocation occurs.
 * @return An initialized interpolation curve with the specified capacity.
 */
R3DAPI R3D_InterpolationCurve R3D_LoadInterpolationCurve(int capacity);

/**
 * @brief Unloads the interpolation curve and frees the allocated memory.
 *
 * This function deallocates the memory associated with the interpolation curve and clears any keyframes stored in it.
 * It should be called when the curve is no longer needed to avoid memory leaks.
 *
 * @param curve The interpolation curve to be unloaded.
 */
R3DAPI void R3D_UnloadInterpolationCurve(R3D_InterpolationCurve curve);

/**
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
 */
R3DAPI bool R3D_AddKeyframe(R3D_InterpolationCurve* curve, float time, float value);

/**
 * @brief Evaluates the interpolation curve at a specific time.
 *
 * This function evaluates the value of the interpolation curve at a given time. The curve will interpolate between
 * keyframes based on the time provided.
 *
 * @param curve The interpolation curve to be evaluated.
 * @param time The time at which to evaluate the curve.
 * @return The value of the curve at the specified time.
 */
R3DAPI float R3D_EvaluateCurve(R3D_InterpolationCurve curve, float time);



// --------------------------------------------
// ENVIRONMENT: Background And Ambient
// --------------------------------------------

/**
 * @brief Sets the background color when no skybox is enabled.
 *
 * This function defines the background color to be used when no skybox is active.
 * The color will be used for the clear color of the scene.
 *
 * @param color The color to set as the background color.
 */
R3DAPI void R3D_SetBackgroundColor(Color color);

/**
 * @brief Sets the ambient light color when no skybox is enabled.
 *
 * This function defines the ambient light color to be used when no skybox is active.
 * It affects the overall lighting of the scene when no skybox is present.
 *
 * @param color The color to set for ambient light.
 */
R3DAPI void R3D_SetAmbientColor(Color color);

/**
 * @brief Enables a skybox for the scene.
 *
 * This function enables a skybox in the scene, replacing the default background with
 * a 3D environment. The skybox is defined by the specified skybox asset.
 *
 * @param skybox The skybox to enable.
 */
R3DAPI void R3D_EnableSkybox(R3D_Skybox skybox);

/**
 * @brief Disables the skybox in the scene.
 *
 * This function disables the skybox, reverting back to the default background
 * color (or no background if none is set). It should be called to remove the skybox
 * from the scene.
 */
R3DAPI void R3D_DisableSkybox(void);

/**
 * @brief Sets the rotation of the skybox.
 *
 * This function allows you to specify the rotation of the skybox along the
 * pitch, yaw, and roll axes, which allows the skybox to be rotated in the scene.
 *
 * @param pitch The rotation angle around the X-axis (in degrees).
 * @param yaw The rotation angle around the Y-axis (in degrees).
 * @param roll The rotation angle around the Z-axis (in degrees).
 */
R3DAPI void R3D_SetSkyboxRotation(float pitch, float yaw, float roll);

/**
 * @brief Gets the current rotation of the skybox.
 *
 * This function returns the current rotation of the skybox as a vector containing
 * the pitch, yaw, and roll values in degrees.
 *
 * @return A vector containing the current pitch, yaw, and roll of the skybox.
 */
R3DAPI Vector3 R3D_GetSkyboxRotation(void);



// --------------------------------------------
// ENVIRONMENT: SSAO Config Functions
// --------------------------------------------

/**
 * @brief Enables or disables Screen Space Ambient Occlusion (SSAO).
 *
 * This function toggles the SSAO effect. When enabled, SSAO enhances the realism
 * of the scene by simulating ambient occlusion, darkening areas where objects
 * are close together or in corners.
 *
 * @param enabled Whether to enable or disable SSAO.
 */
R3DAPI void R3D_SetSSAO(bool enabled);

/**
 * @brief Gets the current state of SSAO.
 *
 * This function checks if SSAO is currently enabled or disabled.
 *
 * @return True if SSAO is enabled, false otherwise.
 */
R3DAPI bool R3D_GetSSAO(void);

/**
 * @brief Sets the radius for SSAO effect.
 *
 * This function sets the radius used by the SSAO effect to calculate occlusion.
 * A higher value will affect a larger area around each pixel, while a smaller value
 * will create sharper and more localized occlusion.
 *
 * @param value The radius value to set for SSAO.
 */
R3DAPI void R3D_SetSSAORadius(float value);

/**
 * @brief Gets the current SSAO radius.
 *
 * This function retrieves the current radius value used by the SSAO effect.
 *
 * @return The radius value for SSAO.
 */
R3DAPI float R3D_GetSSAORadius(void);

/**
 * @brief Sets the bias for SSAO effect.
 *
 * This function sets the bias used by the SSAO effect to adjust how much occlusion
 * is applied to the scene. A higher value can reduce artifacts, but may also
 * result in less pronounced ambient occlusion.
 *
 * @param value The bias value for SSAO.
 */
R3DAPI void R3D_SetSSAOBias(float value);

/**
 * @brief Gets the current SSAO bias.
 *
 * This function retrieves the current bias value used by the SSAO effect.
 *
 * @return The SSAO bias value.
 */
R3DAPI float R3D_GetSSAOBias(void);

/**
 * @brief Sets the number of iterations for SSAO effect.
 *
 * This function sets the number of iterations (or samples) used to calculate the
 * SSAO effect. A higher value will result in a more accurate and smoother effect,
 * but may also be more performance-intensive.
 *
 * @param value The number of iterations for SSAO.
 */
R3DAPI void R3D_SetSSAOIterations(int value);

/**
 * @brief Gets the current number of SSAO iterations.
 *
 * This function retrieves the current number of iterations (or samples) used to
 * calculate the SSAO effect.
 *
 * @return The number of SSAO iterations.
 */
R3DAPI int R3D_GetSSAOIterations(void);



// --------------------------------------------
// ENVIRONMENT: Bloom Config Functions
// --------------------------------------------

/**
 * @brief Sets the bloom mode.
 *
 * This function configures the bloom effect mode, which determines how the bloom
 * effect is applied to the rendered scene.
 *
 * @param mode The bloom mode to set.
 */
R3DAPI void R3D_SetBloomMode(R3D_Bloom mode);

/**
 * @brief Gets the current bloom mode.
 *
 * This function retrieves the bloom mode currently applied to the scene.
 *
 * @return The current bloom mode.
 */
R3DAPI R3D_Bloom R3D_GetBloomMode(void);

/**
 * @brief Sets the bloom intensity.
 *
 * This function controls the strength of the bloom effect. Higher values result
 * in a more intense glow effect on bright areas of the scene.
 *
 * @param value The intensity value for bloom.
 */
R3DAPI void R3D_SetBloomIntensity(float value);

/**
 * @brief Gets the current bloom intensity.
 *
 * This function retrieves the intensity value of the bloom effect.
 *
 * @return The current bloom intensity.
 */
R3DAPI float R3D_GetBloomIntensity(void);

/**
 * @brief Sets the bloom filter radius.
 *
 * Controls the radius of the blur filter applied during the upscaling stage
 * of the bloom effect. A larger radius results in a wider glow around bright
 * objects, creating a softer and more diffuse bloom. A value of 0 disables 
 * the filtering effect, preserving sharp bloom highlights.
 *
 * @param value The radius of the bloom filter (in pixels or arbitrary units depending on implementation).
 */
 R3DAPI void R3D_SetBloomFilterRadius(int value);

 /**
  * @brief Gets the current bloom filter radius.
  *
  * Retrieves the current radius used for the bloom filter. This value determines
  * how far the glow effect extends around bright areas in the scene.
  *
  * @return The current bloom filter radius.
  */
 R3DAPI int R3D_GetBloomFilterRadius(void);


// --------------------------------------------
// ENVIRONMENT: Fog Config Functions
// --------------------------------------------

/**
 * @brief Sets the fog mode.
 *
 * This function defines the type of fog effect applied to the scene.
 * Different modes may provide linear, exponential, or volumetric fog effects.
 *
 * @param mode The fog mode to set.
 */
R3DAPI void R3D_SetFogMode(R3D_Fog mode);

/**
 * @brief Gets the current fog mode.
 *
 * This function retrieves the fog mode currently applied to the scene.
 *
 * @return The current fog mode.
 */
R3DAPI R3D_Fog R3D_GetFogMode(void);

/**
 * @brief Sets the color of the fog.
 *
 * This function defines the color of the fog effect applied to the scene.
 * The fog color blends with objects as they are affected by fog.
 *
 * @param color The color to set for the fog.
 */
R3DAPI void R3D_SetFogColor(Color color);

/**
 * @brief Gets the current fog color.
 *
 * This function retrieves the color currently used for the fog effect.
 *
 * @return The current fog color.
 */
R3DAPI Color R3D_GetFogColor(void);

/**
 * @brief Sets the start distance of the fog.
 *
 * This function defines the distance from the camera at which fog begins to appear.
 * Objects closer than this distance will not be affected by fog.
 *
 * @param value The start distance for the fog effect.
 */
R3DAPI void R3D_SetFogStart(float value);

/**
 * @brief Gets the current fog start distance.
 *
 * This function retrieves the distance at which the fog begins to be applied.
 *
 * @return The current fog start distance.
 */
R3DAPI float R3D_GetFogStart(void);

/**
 * @brief Sets the end distance of the fog.
 *
 * This function defines the distance from the camera at which fog reaches full intensity.
 * Objects beyond this distance will be completely covered by fog.
 *
 * @param value The end distance for the fog effect.
 */
R3DAPI void R3D_SetFogEnd(float value);

/**
 * @brief Gets the current fog end distance.
 *
 * This function retrieves the distance at which the fog is fully applied.
 *
 * @return The current fog end distance.
 */
R3DAPI float R3D_GetFogEnd(void);

/**
 * @brief Sets the density of the fog.
 *
 * This function controls how thick the fog appears. Higher values result in
 * denser fog, making objects fade out more quickly.
 *
 * @param value The density of the fog (higher values increase fog thickness).
 */
R3DAPI void R3D_SetFogDensity(float value);

/**
 * @brief Gets the current fog density.
 *
 * This function retrieves the current density of the fog.
 *
 * @return The current fog density.
 */
R3DAPI float R3D_GetFogDensity(void);



// --------------------------------------------
// ENVIRONMENT: Tonemap Config Functions
// --------------------------------------------

/**
 * @brief Sets the tonemapping mode.
 *
 * This function defines the tonemapping algorithm applied to the final rendered image.
 * Different tonemap modes affect color balance, brightness compression, and overall
 * scene appearance.
 *
 * @param mode The tonemap mode to set.
 */
R3DAPI void R3D_SetTonemapMode(R3D_Tonemap mode);

/**
 * @brief Gets the current tonemapping mode.
 *
 * This function retrieves the tonemap mode currently applied to the scene.
 *
 * @return The current tonemap mode.
 */
R3DAPI R3D_Tonemap R3D_GetTonemapMode(void);

/**
 * @brief Sets the exposure level for tonemapping.
 *
 * This function adjusts the exposure level used in tonemapping, affecting
 * the overall brightness of the rendered scene.
 *
 * @param value The exposure value (higher values make the scene brighter).
 */
R3DAPI void R3D_SetTonemapExposure(float value);

/**
 * @brief Gets the current tonemap exposure level.
 *
 * This function retrieves the current exposure setting used in tonemapping.
 *
 * @return The current tonemap exposure value.
 */
R3DAPI float R3D_GetTonemapExposure(void);

/**
 * @brief Sets the white point for tonemapping.
 *
 * This function defines the reference white level, which determines how bright
 * areas of the scene are mapped to the final output.
 *
 * @param value The white point value.
 */
R3DAPI void R3D_SetTonemapWhite(float value);

/**
 * @brief Gets the current tonemap white point.
 *
 * This function retrieves the white point setting used in tonemapping.
 *
 * @return The current tonemap white value.
 */
R3DAPI float R3D_GetTonemapWhite(void);



// --------------------------------------------
// ENVIRONMENT: Color Adjustment Functions
// --------------------------------------------

/**
 * @brief Sets the global brightness adjustment.
 *
 * This function controls the brightness of the final rendered image.
 * Higher values make the image brighter, while lower values darken it.
 *
 * @param value The brightness adjustment value.
 */
R3DAPI void R3D_SetBrightness(float value);

/**
 * @brief Gets the current brightness level.
 *
 * This function retrieves the brightness setting applied to the scene.
 *
 * @return The current brightness value.
 */
R3DAPI float R3D_GetBrightness(void);

/**
 * @brief Sets the global contrast adjustment.
 *
 * This function controls the contrast of the final rendered image.
 * Higher values increase the difference between dark and bright areas.
 *
 * @param value The contrast adjustment value.
 */
R3DAPI void R3D_SetContrast(float value);

/**
 * @brief Gets the current contrast level.
 *
 * This function retrieves the contrast setting applied to the scene.
 *
 * @return The current contrast value.
 */
R3DAPI float R3D_GetContrast(void);

/**
 * @brief Sets the global saturation adjustment.
 *
 * This function controls the color intensity of the final rendered image.
 * Higher values make colors more vibrant, while lower values desaturate them.
 *
 * @param value The saturation adjustment value.
 */
R3DAPI void R3D_SetSaturation(float value);

/**
 * @brief Gets the current saturation level.
 *
 * This function retrieves the saturation setting applied to the scene.
 *
 * @return The current saturation value.
 */
R3DAPI float R3D_GetSaturation(void);



// --------------------------------------------
// SKYBOX: Skybox Loading Functions
// --------------------------------------------

/**
 * @brief Loads a skybox from a texture file.
 *
 * This function loads a skybox from a texture file using a specified cubemap layout.
 * The layout defines how the six faces of the cubemap are arranged within the texture.
 *
 * @param fileName The path to the texture file.
 * @param layout The cubemap layout format.
 * @return The loaded skybox object.
 */
R3DAPI R3D_Skybox R3D_LoadSkybox(const char* fileName, CubemapLayout layout);

/**
 * @brief Loads a skybox from a high dynamic range (HDR) image.
 *
 * This function loads a skybox from an HDR image and converts it into a cubemap.
 * The size parameter determines the resolution of the generated cubemap.
 *
 * @param fileName The path to the HDR image file.
 * @param size The resolution of the cubemap (e.g., 512, 1024).
 * @return The loaded skybox object.
 */
R3DAPI R3D_Skybox R3D_LoadSkyboxHDR(const char* fileName, int size);

/**
 * @brief Unloads a skybox and frees its resources.
 *
 * This function removes a previously loaded skybox from memory.
 * It should be called when the skybox is no longer needed to prevent memory leaks.
 *
 * @param sky The skybox to unload.
 */
R3DAPI void R3D_UnloadSkybox(R3D_Skybox sky);



// --------------------------------------------
// CULLING: Frustum Test Functions
// --------------------------------------------

/**
 * @brief Checks if a point is inside the view frustum.
 *
 * This function determines whether a given 3D point is within the current camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param position The position of the point to check.
 * @return `true` if the point is inside the frustum, `false` otherwise.
 */
R3DAPI bool R3D_IsPointInFrustum(Vector3 position);

/**
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
 */
R3DAPI bool R3D_IsPointInFrustumXYZ(float x, float y, float z);

/**
 * @brief Checks if a sphere is inside the view frustum.
 *
 * This function tests whether a sphere, defined by a center position and radius,
 * is at least partially inside the camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param position The center of the sphere.
 * @param radius The radius of the sphere.
 * @return `true` if the sphere is at least partially inside the frustum, `false` otherwise.
 */
R3DAPI bool R3D_IsSphereInFrustum(Vector3 position, float radius);

/**
 * @brief Checks if an axis-aligned bounding box (AABB) is inside the view frustum.
 *
 * This function determines whether an AABB is at least partially visible within the camera's frustum.
 * It must be called between `R3D_Begin` and `R3D_End`.
 *
 * @param aabb The bounding box to test.
 * @return `true` if any part of the bounding box is inside the frustum, `false` otherwise.
 */
R3DAPI bool R3D_IsBoundingBoxInFrustum(BoundingBox aabb);



// --------------------------------------------
// UTILS: Material Configuration Functions
// --------------------------------------------

/**
 * @brief Sets the albedo (diffuse color) properties of a material.
 *
 * This function assigns an albedo texture and color to a material.
 * If a texture is provided, it is used as the albedo map. The color is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional albedo texture (set to NULL for none).
 * @param color Albedo color to apply.
 */
R3DAPI void R3D_SetMaterialAlbedo(Material* material, Texture2D* texture, Color color);

/**
 * @brief Sets the ambient occlusion properties of a material.
 *
 * This function assigns an ambient occlusion (AO) texture and intensity value to a material.
 * If a texture is provided, it is used as the AO map. The intensity controls the effect strength.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional occlusion texture (set to NULL for none).
 * @param value Occlusion strength (0.0 to 1.0).
 */
R3DAPI void R3D_SetMaterialOcclusion(Material* material, Texture2D* texture, float value);

/**
 * @brief Sets the roughness properties of a material.
 *
 * This function assigns a roughness texture and scalar value to a material.
 * If a texture is provided, it is used as the roughness map. The scalar value is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional roughness texture (set to NULL for none).
 * @param value Roughness factor (0.0 = smooth, 1.0 = rough).
 */
R3DAPI void R3D_SetMaterialRoughness(Material* material, Texture2D* texture, float value);

/**
 * @brief Sets the metalness properties of a material.
 *
 * This function assigns a metalness texture and scalar value to a material.
 * If a texture is provided, it is used as the metalness map. The scalar value is multiplied
 * with the texture if both are set.
 *
 * @param material Pointer to the material to modify.
 * @param texture Optional metalness texture (set to NULL for none).
 * @param value Metalness factor (0.0 = non-metallic, 1.0 = metallic).
 */
R3DAPI void R3D_SetMaterialMetalness(Material* material, Texture2D* texture, float value);

/**
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
 */
R3DAPI void R3D_SetMaterialEmission(Material* material, Texture2D* texture, Color color, float value);



// --------------------------------------------
// UTILS: Default Texture Retrieval Functions
// --------------------------------------------

/**
 * @brief Retrieves a default white texture.
 *
 * This texture is fully white (1,1,1,1), useful for default material properties.
 *
 * @return A white texture.
 */
R3DAPI Texture2D R3D_GetWhiteTexture(void);

/**
 * @brief Retrieves a default black texture.
 *
 * This texture is fully black (0,0,0,1), useful for masking or default values.
 *
 * @return A black texture.
 */
R3DAPI Texture2D R3D_GetBlackTexture(void);

/**
 * @brief Retrieves a default normal map texture.
 *
 * This texture represents a neutral normal map (0.5, 0.5, 1.0), which applies no normal variation.
 *
 * @return A neutral normal texture.
 */
R3DAPI Texture2D R3D_GetNormalTexture(void);



// --------------------------------------------
// UTILS: Render Texture Retrieval Functions
// --------------------------------------------

/**
 * @brief Retrieves the final scene color buffer.
 *
 * This texture stores the final rendered scene as a 24-bit RGB buffer.
 *
 * @return The final color buffer texture.
 */
R3DAPI Texture2D R3D_GetBufferColor(void);

/**
 * @brief Retrieves the buffer containing the scene's normal data.
 *
 * This texture stores octahedral-compressed normals using two 16-bit per-channel RG components.
 *
 * @note You can find the decoding functions in the embedded shaders, such as 'screen/lighting.fs.glsl'.
 *
 * @return The normal buffer texture.
 */
R3DAPI Texture2D R3D_GetBufferNormal(void);

/**
 * @brief Retrieves the final depth buffer.
 *
 * This texture contains the depth stored in 24 bits and a stencil buffer where each value is 0 or 1, indicating the presence of geometry.
 * It is useful for post-processing effects outside of R3D.
 *
 * @note If you modify the texture parameters to sample the stencil instead of the depth,
 * make sure to reset the parameters afterward.
 *
 * @return The final depth buffer texture.
 */
R3DAPI Texture2D R3D_GetBufferDepth(void);



// --------------------------------------------
// UTILS: Camera Matrices Retrieval Functions
// --------------------------------------------

/**
 * @brief Retrieves the view matrix.
 *
 * This matrix represents the camera's transformation from world space to view space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current view matrix.
 */
R3DAPI Matrix R3D_GetMatrixView(void);

/**
 * @brief Retrieves the inverse view matrix.
 *
 * This matrix transforms coordinates from view space back to world space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current inverse view matrix.
 */
R3DAPI Matrix R3D_GetMatrixInvView(void);

/**
 * @brief Retrieves the projection matrix.
 *
 * This matrix defines the transformation from view space to clip space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current projection matrix.
 */
R3DAPI Matrix R3D_GetMatrixProjection(void);

/**
 * @brief Retrieves the inverse projection matrix.
 *
 * This matrix transforms coordinates from clip space back to view space.
 * It is updated at the last call to 'R3D_Begin'.
 *
 * @return The current inverse projection matrix.
 */
R3DAPI Matrix R3D_GetMatrixInvProjection(void);



// --------------------------------------------
// UTILS: Debug Buffer Rendering Functions
// --------------------------------------------

/**
 * @brief Renders the internal albedo buffer to the screen.
 *
 * This function displays the albedo (diffuse) buffer as a 2D texture.
 * It must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 */
R3DAPI void R3D_DrawBufferAlbedo(float x, float y, float w, float h);

/**
 * @brief Renders the internal emission buffer to the screen.
 *
 * Displays the emission buffer, which contains emissive lighting data.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 */
R3DAPI void R3D_DrawBufferEmission(float x, float y, float w, float h);

/**
 * @brief Renders the internal normal buffer to the screen.
 *
 * Displays the normal buffer, showing world-space normal data as colors.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 */
R3DAPI void R3D_DrawBufferNormal(float x, float y, float w, float h);

/**
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
 */
R3DAPI void R3D_DrawBufferORM(float x, float y, float w, float h);

/**
 * @brief Renders the SSAO (Screen Space Ambient Occlusion) buffer to the screen.
 *
 * Displays the SSAO buffer, showing ambient occlusion data in grayscale.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 */
R3DAPI void R3D_DrawBufferSSAO(float x, float y, float w, float h);

/**
 * @brief Renders the bloom buffer to the screen.
 *
 * Displays the bloom effect buffer, showing the extracted bright areas after blur processing.
 * Must be called outside of `R3D_Begin` and `R3D_End`.
 *
 * @param x X position to draw the buffer.
 * @param y Y position to draw the buffer.
 * @param w Width of the drawn buffer.
 * @param h Height of the drawn buffer.
 */
R3DAPI void R3D_DrawBufferBloom(float x, float y, float w, float h);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // R3D_H
