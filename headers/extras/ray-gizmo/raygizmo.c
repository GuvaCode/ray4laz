/***************************************************************************************************
*
*   LICENSE: zlib
*
*   Copyright (c) 2024 Claudio Z. (@cloudofoz)
*
*   This software is provided "as-is," without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
***************************************************************************************************/

//--------------------------------------------------------------------------------------------------
// Includes
//--------------------------------------------------------------------------------------------------

#include <raygizmo.h>
#include <raylib.h>
#include <raymath.h>
#include <rlgl.h>


//---------------------------------------------------------------------------------------------------
// Enumerators Definition
//---------------------------------------------------------------------------------------------------

/**
 * Bitwise flags for the actively transforming axis.
 * Used in combination with one of the GizmoAction values.
 */
typedef enum
{
	GZ_ACTIVE_X = 1 << 0,               // Active transformation on the X-axis
	GZ_ACTIVE_Y = 1 << 1,               // Active transformation on the Y-axis
	GZ_ACTIVE_Z = 1 << 2,               // Active transformation on the Z-axis
	GZ_ACTIVE_XYZ = GZ_ACTIVE_X | GZ_ACTIVE_Y | GZ_ACTIVE_Z  // Active transformation on all axes
} GizmoActiveAxis;

/**
 * Active transformation types for the gizmo.
 */
typedef enum
{
	GZ_ACTION_NONE = 0,                 // No active transformation
	GZ_ACTION_TRANSLATE,                // Translation (movement) transformation
	GZ_ACTION_SCALE,                    // Scaling transformation
	GZ_ACTION_ROTATE                    // Rotation transformation
} GizmoAction;

/**
 * Indices of the axes.
 * Useful for indexing and iteration.
 */
enum
{
	GZ_AXIS_X = 0,                      // Index of the X-axis
	GZ_AXIS_Y = 1,                      // Index of the Y-axis
	GZ_AXIS_Z = 2,                      // Index of the Z-axis

	GIZMO_AXIS_COUNT = 3                // Total number of axes
};


//---------------------------------------------------------------------------------------------------
// Types and Structures Definition
//---------------------------------------------------------------------------------------------------

/**
 * Axis-related data used by gizmos.
 */
typedef struct GizmoAxis
{
	Vector3 normal;    // Direction of the axis.
	Color color;       // Color used to represent the axis.
} GizmoAxis;

/**
 * Global variables used by the gizmos.
 * These are shared across all gizmos to maintain consistent behavior and appearance.
 */
typedef struct GizmoGlobals
{
	GizmoAxis axisCfg[GIZMO_AXIS_COUNT];  // Data related to the 3 axes, globally oriented.

	float gizmoSize;                      // Size of the gizmos. All other metrics are expressed as fractions of this measure.
	float lineWidth;                      // Width of the lines used to draw the gizmos.

	float trArrowWidthFactor;             // Width of the arrows (and cubes) as a fraction of gizmoSize.
	float trArrowLengthFactor;            // Length of the arrows as a fraction of gizmoSize.
	float trPlaneOffsetFactor;            // Offset of the gizmo planes from the center as a fraction of gizmoSize.
	float trPlaneSizeFactor;              // Size of the planes (quad representation) as a fraction of gizmoSize.
	float trCircleRadiusFactor;           // Radius of the central circle as a fraction of gizmoSize.
	Color trCircleColor;                  // Color of the central circle.

	int curAction;                        // Currently active GizmoAction.
	int activeAxis;                       // Active axis (a combination of GizmoActiveAxis flags) for the current action.
	Transform startTransform;             // Backup Transform saved before the transformation begins.
	Transform* activeTransform;           // Pointer to the active Transform to update during transformation.
	Vector3 startWorldMouse;              // Position of the mouse in world space at the start of the transformation.
} GizmoGlobals;

/**
 * Temporary data associated with a gizmo.
 * This data is recalculated at each call to DrawGizmo3D(), in immediate-mode style.
 */
typedef struct GizmoData
{
	Matrix invViewProj;                   // Inverted View-Projection matrix.
	Transform* curTransform;              // Pointer to the current Transform. Only one can be the "activeTransform" at a time.
	Vector3 axis[GIZMO_AXIS_COUNT];       // Current axes used for transformations (may differ from global axes).
										  // Axes can be in global, view, or local mode depending on configuration.

	float gizmoSize;                      // Actual gizmo size, adjusted to maintain camera-independent scaling.
	Vector3 camPos;                       // Position of the camera, extracted during rendering.
	Vector3 right, up, forward;           // Local orientation vectors: right, up, and forward.
	int flags;                            // Configuration flags for the gizmo.
} GizmoData;


//---------------------------------------------------------------------------------------------------
// Global Variables Definition
//---------------------------------------------------------------------------------------------------

// Initialize GIZMO with default values
static GizmoGlobals GIZMO = {

	.axisCfg = {
		{.normal = {1, 0, 0}, .color = {229, 72, 91, 255}},
		{.normal = {0, 1, 0}, .color = {131, 205, 56, 255}},
		{.normal = {0, 0, 1}, .color = {69, 138, 242, 255}}
	},

	.gizmoSize = 1.5f,
	.lineWidth = 2.5f,

	.trArrowLengthFactor = 0.15f,
	.trArrowWidthFactor = 0.1f,
	.trPlaneOffsetFactor = 0.3f,
	.trPlaneSizeFactor = 0.15f,
	.trCircleRadiusFactor = 0.1f,
	.trCircleColor = {255, 255, 255, 200},

	.curAction = GZ_ACTION_NONE,
	.activeAxis = 0
};


//---------------------------------------------------------------------------------------------------
// Function Declarations - Helper Functions
//---------------------------------------------------------------------------------------------------

/**
 * Compute the axis orientation for a specific gizmo.
 * Determines whether the axes are oriented globally, locally, or in view mode.
 * @param gizmoData Pointer to the data associated with the gizmo.
 */
static void ComputeAxisOrientation(GizmoData* gizmoData);

/**
 * Check if a specific axis is actively used for the current transformation.
 * @param axis The axis to check (GZ_AXIS_X, GZ_AXIS_Y, or GZ_AXIS_Z).
 * @return true if the axis is actively used; false otherwise.
 */
static bool IsGizmoAxisActive(int axis);

/**
 * Check if a gizmo is compatible with a specific transformation type.
 * @param data Pointer to the data associated with the gizmo.
 * @param type A combination of GizmoFlags values specifying the transformation type.
 * @return true if the gizmo supports the requested transformation type; false otherwise.
 */
static bool CheckGizmoType(const GizmoData* data, int type);

/**
 * Check if any gizmo is currently transforming.
 * @return true if a gizmo is active and transforming; false otherwise.
 */
static bool IsGizmoTransforming(void);

/**
 * Check if the current gizmo is the one actively transforming.
 * @param data Pointer to the data associated with the current gizmo.
 * @return true if this gizmo is the one actively transforming; false otherwise.
 */
static bool IsThisGizmoTransforming(const GizmoData* data);

/**
 * Check if the active transformation is of scaling type.
 * @return true if a gizmo is actively scaling; false otherwise.
 */
static bool IsGizmoScaling(void);

/**
 * Check if the active transformation is of translation type.
 * @return true if a gizmo is actively translating; false otherwise.
 */
static bool IsGizmoTranslating(void);

/**
 * Check if the active transformation is of rotation type.
 * @return true if a gizmo is actively rotating; false otherwise.
 */
static bool IsGizmoRotating(void);

/**
 * Convert a screen-space vector to world space.
 * @param source The screen-space Vector3 to convert.
 * @param matViewProjInv Pointer to the inverted view-projection matrix.
 * @return The input vector transformed to world space.
 * @note This function does not depend on a Camera. Refer to raylib's Vector3Unproject() for the original implementation.
 */
static Vector3 Vec3ScreenToWorld(Vector3 source, const Matrix* matViewProjInv);

/**
 * Generate a world-space ray from a screen-space position.
 * @param position The screen-space position (Vector2) from which to emit the ray.
 * @param matViewProjInv Pointer to the inverted view-projection matrix.
 * @return A Ray representing the world-space ray emitted from the screen position.
 * @note This function does not depend on a Camera. Refer to raylib's GetScreenToWorldRayEx() for the original implementation.
 */
static Ray Vec3ScreenToWorldRay(Vector2 position, const Matrix* matViewProjInv);


//---------------------------------------------------------------------------------------------------
// Functions Declaration - Drawing functions
//---------------------------------------------------------------------------------------------------

/**
 * Helper function used to draw a scaling gizmo cube on one of the axis
 * @param data data associated with the current gizmo
 * @param axis GZ_AXIS_X, GZ_AXIS_Y or GZ_AXIS_Z
 */
static void DrawGizmoCube(const GizmoData* data, int axis);

/**
 * Helper function used to draw a plane representing two axis together
 * @param data data associated with the current gizmo
 * @param index of the third (locked) axis: GZ_AXIS_X, GZ_AXIS_Y or GZ_AXIS_Z
 */
static void DrawGizmoPlane(const GizmoData* data, int index);

/**
 * Helper function used to draw a translating gizmo arrow on one of the axis
 * @param data data associated with the current gizmo
 * @param axis GZ_AXIS_X, GZ_AXIS_Y or GZ_AXIS_Z
 */
static void DrawGizmoArrow(const GizmoData* data, int axis);

/**
 * Helper function used to draw the gizmo center as a circle always pointing to the camera
 * @param data data associated with the current gizmo
 */
static void DrawGizmoCenter(const GizmoData* data);

/**
 * Helper function used to draw a rotating gizmo circle on one of the axis planes.
 * @param data data associated with the current gizmo
 * @param axis GZ_AXIS_X, GZ_AXIS_Y or GZ_AXIS_Z
 */
static void DrawGizmoCircle(const GizmoData* data, int axis);


//---------------------------------------------------------------------------------------------------
// Function Declarations - Mouse Ray to Gizmo Intersections
//---------------------------------------------------------------------------------------------------

/**
 * Check for intersection between a ray and an Oriented Bounding Box (OBB).
 * @param data Pointer to the data associated with the current gizmo.
 * @param ray Ray emitted from the camera.
 * @param obbCenter Center of the oriented bounding box.
 * @param obbHalfSize Half-size of the oriented bounding box along each axis.
 * @return true if the ray intersects the oriented bounding box; false otherwise.
 */
static bool CheckOrientedBoundingBox(const GizmoData* data, Ray ray, Vector3 obbCenter, Vector3 obbHalfSize);

/**
 * Check for intersection between a ray and one of the axes of the gizmo.
 * @param data Pointer to the data associated with the current gizmo.
 * @param axis The axis to check (GZ_AXIS_X, GZ_AXIS_Y, or GZ_AXIS_Z).
 * @param ray Ray emitted from the camera.
 * @param type Type of the axis (GIZMO_TRANSLATE or GIZMO_SCALE), needed to differentiate computation when both are present.
 * @return true if the ray intersects the gizmo axis; false otherwise.
 */
static bool CheckGizmoAxis(const GizmoData* data, int axis, Ray ray, int type);

/**
 * Check for intersection between a ray and one of the gizmo planes.
 * Planes are represented by small quads and are associated with translating or scaling along two axes.
 * @param data Pointer to the data associated with the current gizmo.
 * @param axis The axis associated with the plane (GZ_AXIS_X, GZ_AXIS_Y, or GZ_AXIS_Z).
 * @param ray Ray emitted from the camera.
 * @return true if the ray intersects the gizmo plane; false otherwise.
 */
static bool CheckGizmoPlane(const GizmoData* data, int axis, Ray ray);

/**
 * Check for intersection between a ray and one of the gizmo's rotation circles.
 * @param data Pointer to the data associated with the current gizmo.
 * @param index The axis index (GZ_AXIS_X, GZ_AXIS_Y, or GZ_AXIS_Z) corresponding to the rotation circle.
 * @param ray Ray emitted from the camera.
 * @return true if the ray intersects the rotation circle; false otherwise.
 */
static bool CheckGizmoCircle(const GizmoData* data, int index, Ray ray);

/**
 * Check for intersection between a ray and the gizmo center.
 * The gizmo center is represented as a circle always facing the viewer and treated as a sphere.
 * @param data Pointer to the data associated with the current gizmo.
 * @param ray Ray emitted from the camera.
 * @return true if the ray intersects the gizmo center; false otherwise.
 */
static bool CheckGizmoCenter(const GizmoData* data, Ray ray);


//---------------------------------------------------------------------------------------------------
// Function Declarations - Input Handling
//---------------------------------------------------------------------------------------------------

/**
 * Convert the mouse position to a world-space vector.
 * The viewer's distance to the gizmo is used to determine the depth at which the world-space position is computed.
 * @param data Pointer to the data of the current gizmo.
 * @return A Vector3 representing the mouse position in world space.
 */
static Vector3 GetWorldMouse(const GizmoData* data);


/**
 * Handle all input interactions for the gizmo.
 * Processes input such as mouse and keyboard events to update gizmo states and transformations.
 * @param data Pointer to the data of the current gizmo.
 * @note This function may be modified in future iterations.
 */
static void GizmoHandleInput(const GizmoData* data);


//---------------------------------------------------------------------------------------------------
// Functions Definitions - GIZMO API
//---------------------------------------------------------------------------------------------------

bool DrawGizmo3D(int flags, Transform* transform)
{
	//------------------------------------------------------------------------

	if (flags == GIZMO_DISABLED) return false;

	//------------------------------------------------------------------------

	GizmoData data = {0};

	//------------------------------------------------------------------------

	const Matrix matProj = rlGetMatrixProjection();
	const Matrix matView = rlGetMatrixModelview();
	const Matrix invMat = MatrixInvert(matView);

	data.invViewProj = MatrixMultiply(MatrixInvert(matProj), invMat);

	data.camPos = (Vector3){invMat.m12, invMat.m13, invMat.m14};

	data.right = (Vector3){matView.m0, matView.m4, matView.m8};
	data.up = (Vector3){matView.m1, matView.m5, matView.m9};
	data.forward = Vector3Normalize(Vector3Subtract(transform->translation, data.camPos));

	data.curTransform = transform;

	data.gizmoSize = GIZMO.gizmoSize * Vector3Distance(data.camPos, transform->translation) * 0.1f;

	data.flags = flags;

	ComputeAxisOrientation(&data);

	//------------------------------------------------------------------------

	rlDrawRenderBatchActive();
	const float prevLineWidth = rlGetLineWidth();
	rlSetLineWidth(GIZMO.lineWidth);
	rlDisableBackfaceCulling();
	rlDisableDepthTest();
	rlDisableDepthMask();

	//------------------------------------------------------------------------

	for (int i = 0; i < GIZMO_AXIS_COUNT; ++i)
	{
		if (data.flags & GIZMO_TRANSLATE)
		{
			DrawGizmoArrow(&data, i);
		}
		if (data.flags & GIZMO_SCALE)
		{
			DrawGizmoCube(&data, i);
		}
		if ((data.flags & (GIZMO_SCALE | GIZMO_TRANSLATE)) != 0)
		{
			DrawGizmoPlane(&data, i);
		}
		if (data.flags & GIZMO_ROTATE)
		{
			DrawGizmoCircle(&data, i);
		}		
	}
	if ((data.flags & (GIZMO_SCALE | GIZMO_TRANSLATE)) != 0) 
	{
		DrawGizmoCenter(&data);
	}

	//------------------------------------------------------------------------

	rlDrawRenderBatchActive();
	rlSetLineWidth(prevLineWidth);
	rlEnableBackfaceCulling();
	rlEnableDepthTest();
	rlEnableDepthMask();

	//------------------------------------------------------------------------

	// If there's an active transformation, only the interested gizmo handles the input
	if (!IsGizmoTransforming() || data.curTransform == GIZMO.activeTransform)
	{
		GizmoHandleInput(&data);
	}

	//------------------------------------------------------------------------

	return IsThisGizmoTransforming(&data);
}

void SetGizmoSize(float size)
{
	GIZMO.gizmoSize = fmaxf(0, size);
}

void SetGizmoLineWidth(float width)
{
	GIZMO.lineWidth = fmaxf(0, width);
}

void SetGizmoColors(Color x, Color y, Color z, Color center)
{
	GIZMO.axisCfg[GZ_AXIS_X].color = x;
	GIZMO.axisCfg[GZ_AXIS_Y].color = y;
	GIZMO.axisCfg[GZ_AXIS_Z].color = z;
	GIZMO.trCircleColor = center;
}

void SetGizmoGlobalAxis(Vector3 right, Vector3 up, Vector3 forward)
{
	GIZMO.axisCfg[GZ_AXIS_X].normal = Vector3Normalize(right);
	GIZMO.axisCfg[GZ_AXIS_Y].normal = Vector3Normalize(up);
	GIZMO.axisCfg[GZ_AXIS_Z].normal = Vector3Normalize(forward);
}

Transform GizmoIdentity(void)
{
	return (Transform){
		.rotation = QuaternionIdentity(),
		.scale = {1.0f, 1.0f, 1.0f},
		.translation = {0}
	};
}

Matrix GizmoToMatrix(Transform transform)
{
	return MatrixMultiply(MatrixMultiply(MatrixScale(transform.scale.x, transform.scale.y, transform.scale.z),
	                                     QuaternionToMatrix(transform.rotation)),
	                      MatrixTranslate(transform.translation.x, transform.translation.y, transform.translation.z));
}


//---------------------------------------------------------------------------------------------------
// Functions Definitions - Helper functions
//---------------------------------------------------------------------------------------------------

static void ComputeAxisOrientation(GizmoData* gizmoData)
{
	int flags = gizmoData->flags;

	// Scaling is currently supported only in local mode
	if (flags & GIZMO_SCALE)
	{
		flags &= ~GIZMO_VIEW;
		flags |= GIZMO_LOCAL;
	}

	if (flags & GIZMO_VIEW)
	{
		gizmoData->axis[GZ_AXIS_X] = gizmoData->right;
		gizmoData->axis[GZ_AXIS_Y] = gizmoData->up;
		gizmoData->axis[GZ_AXIS_Z] = gizmoData->forward;
	}
	else
	{
		gizmoData->axis[GZ_AXIS_X] = GIZMO.axisCfg[GZ_AXIS_X].normal;
		gizmoData->axis[GZ_AXIS_Y] = GIZMO.axisCfg[GZ_AXIS_Y].normal;
		gizmoData->axis[GZ_AXIS_Z] = GIZMO.axisCfg[GZ_AXIS_Z].normal;

		if (flags & GIZMO_LOCAL)
		{
			for (int i = 0; i < 3; ++i)
			{
				gizmoData->axis[i] = Vector3Normalize(
					Vector3RotateByQuaternion(gizmoData->axis[i], gizmoData->curTransform->rotation));
			}
		}
	}
}

static bool IsGizmoAxisActive(int axis)
{
	return (axis == GZ_AXIS_X && (GIZMO.activeAxis & GZ_ACTIVE_X)) ||
		(axis == GZ_AXIS_Y && (GIZMO.activeAxis & GZ_ACTIVE_Y)) ||
		(axis == GZ_AXIS_Z && (GIZMO.activeAxis & GZ_ACTIVE_Z));
}

static bool CheckGizmoType(const GizmoData* data, int type)
{
	return (data->flags & type) == type;
}

static bool IsGizmoTransforming(void)
{
	return GIZMO.curAction != GZ_ACTION_NONE;
}

static bool IsThisGizmoTransforming(const GizmoData* data)
{
	return IsGizmoTransforming() && data->curTransform == GIZMO.activeTransform;
}

static bool IsGizmoScaling(void)
{
	return GIZMO.curAction == GZ_ACTION_SCALE;
}

static bool IsGizmoTranslating(void)
{
	return GIZMO.curAction == GZ_ACTION_TRANSLATE;
}

static bool IsGizmoRotating(void)
{
	return GIZMO.curAction == GZ_ACTION_ROTATE;
}

static Vector3 Vec3ScreenToWorld(Vector3 source, const Matrix* matViewProjInv)
{
	const Quaternion qt = QuaternionTransform((Quaternion){source.x, source.y, source.z, 1.0f}, *matViewProjInv);
	return (Vector3){
		qt.x / qt.w,
		qt.y / qt.w,
		qt.z / qt.w
	};
}

static Ray Vec3ScreenToWorldRay(Vector2 position, const Matrix* matViewProjInv)
{
	Ray ray = {0};

	const float width = (float)(GetScreenWidth());

	const float height = (float)(GetScreenHeight());

	const Vector2 deviceCoords = {(2.0f * position.x) / width - 1.0f, 1.0f - (2.0f * position.y) / height};

	const Vector3 nearPoint = Vec3ScreenToWorld((Vector3){deviceCoords.x, deviceCoords.y, 0.0f}, matViewProjInv);

	const Vector3 farPoint = Vec3ScreenToWorld((Vector3){deviceCoords.x, deviceCoords.y, 1.0f}, matViewProjInv);

	const Vector3 cameraPlanePointerPos = Vec3ScreenToWorld((Vector3){deviceCoords.x, deviceCoords.y, -1.0f},
	                                                        matViewProjInv);

	const Vector3 direction = Vector3Normalize(Vector3Subtract(farPoint, nearPoint));

	/*
	if (camera.projection == CAMERA_PERSPECTIVE) ray.position = camera.position;
	else if (camera.projection == CAMERA_ORTHOGRAPHIC) ray.position = cameraPlanePointerPos;
	*/
	ray.position = cameraPlanePointerPos;

	// Apply calculated vectors to ray
	ray.direction = direction;

	return ray;
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Drawing functions
//---------------------------------------------------------------------------------------------------

static void DrawGizmoCube(const GizmoData* data, int axis)
{
	if (IsThisGizmoTransforming(data) && (!IsGizmoAxisActive(axis) || !IsGizmoScaling()))
	{
		return;
	}

	const float gizmoSize = CheckGizmoType(data, GIZMO_SCALE | GIZMO_TRANSLATE)
		                        ? data->gizmoSize * 0.5f
		                        : data->gizmoSize;

	const Vector3 endPos = Vector3Add(data->curTransform->translation,
	                                  Vector3Scale(data->axis[axis], gizmoSize * (1.0f - GIZMO.trArrowWidthFactor)));

	DrawLine3D(data->curTransform->translation, endPos, GIZMO.axisCfg[axis].color);

	const float boxSize = data->gizmoSize * GIZMO.trArrowWidthFactor;

	const Vector3 dim1 = Vector3Scale(data->axis[(axis + 1) % 3], boxSize);
	const Vector3 dim2 = Vector3Scale(data->axis[(axis + 2) % 3], boxSize);
	const Vector3 n = data->axis[axis];
	const Color col = GIZMO.axisCfg[axis].color;

	const Vector3 depth = Vector3Scale(n, boxSize);

	const Vector3 a = Vector3Subtract(Vector3Subtract(endPos, Vector3Scale(dim1, 0.5f)), Vector3Scale(dim2, 0.5f));
	const Vector3 b = Vector3Add(a, dim1);
	const Vector3 c = Vector3Add(b, dim2);
	const Vector3 d = Vector3Add(a, dim2);

	const Vector3 e = Vector3Add(a, depth);
	const Vector3 f = Vector3Add(b, depth);
	const Vector3 g = Vector3Add(c, depth);
	const Vector3 h = Vector3Add(d, depth);

	rlBegin(RL_QUADS);

	rlColor4ub(col.r, col.g, col.b, col.a);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(d.x, d.y, d.z);

	rlVertex3f(e.x, e.y, e.z);
	rlVertex3f(f.x, f.y, f.z);
	rlVertex3f(g.x, g.y, g.z);
	rlVertex3f(h.x, h.y, h.z);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(e.x, e.y, e.z);
	rlVertex3f(f.x, f.y, f.z);
	rlVertex3f(d.x, d.y, d.z);

	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(f.x, f.y, f.z);
	rlVertex3f(g.x, g.y, g.z);
	rlVertex3f(c.x, c.y, c.z);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(f.x, f.y, f.z);
	rlVertex3f(e.x, e.y, e.z);

	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(g.x, g.y, g.z);
	rlVertex3f(h.x, h.y, h.z);
	rlVertex3f(d.x, d.y, d.z);

	rlEnd();
}


//---------------------------------------------------------------------------------------------------------

static void DrawGizmoPlane(const GizmoData* data, int index)
{
	if (IsThisGizmoTransforming(data))
	{
		return;
	}

	const Vector3 dir1 = data->axis[(index + 1) % 3];
	const Vector3 dir2 = data->axis[(index + 2) % 3];
	const Color col = GIZMO.axisCfg[index].color;

	const float offset = GIZMO.trPlaneOffsetFactor * data->gizmoSize;
	const float size = GIZMO.trPlaneSizeFactor * data->gizmoSize;

	const Vector3 a = Vector3Add(Vector3Add(data->curTransform->translation, Vector3Scale(dir1, offset)),
	                             Vector3Scale(dir2, offset));
	const Vector3 b = Vector3Add(a, Vector3Scale(dir1, size));
	const Vector3 c = Vector3Add(b, Vector3Scale(dir2, size));
	const Vector3 d = Vector3Add(a, Vector3Scale(dir2, size));

	rlBegin(RL_QUADS);

	rlColor4ub(col.r, col.g, col.b, (unsigned char)((float)col.a * 0.5f));

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(d.x, d.y, d.z);

	rlEnd();

	rlBegin(RL_LINES);

	rlColor4ub(col.r, col.g, col.b, col.a);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(b.x, b.y, b.z);

	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(c.x, c.y, c.z);

	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(d.x, d.y, d.z);

	rlVertex3f(d.x, d.y, d.z);
	rlVertex3f(a.x, a.y, a.z);

	rlEnd();
}

static void DrawGizmoArrow(const GizmoData* data, int axis)
{
	if (IsThisGizmoTransforming(data) && (!IsGizmoAxisActive(axis) || !IsGizmoTranslating()))
	{
		return;
	}

	const Vector3 endPos = Vector3Add(data->curTransform->translation,
	                                  Vector3Scale(data->axis[axis],
	                                               data->gizmoSize * (1.0f - GIZMO.trArrowLengthFactor)));

	if (!(data->flags & GIZMO_SCALE))
		DrawLine3D(data->curTransform->translation, endPos, GIZMO.axisCfg[axis].color);

	const float arrowLength = data->gizmoSize * GIZMO.trArrowLengthFactor;
	const float arrowWidth = data->gizmoSize * GIZMO.trArrowWidthFactor;

	const Vector3 dim1 = Vector3Scale(data->axis[(axis + 1) % 3], arrowWidth);
	const Vector3 dim2 = Vector3Scale(data->axis[(axis + 2) % 3], arrowWidth);
	const Vector3 n = data->axis[axis];
	const Color col = GIZMO.axisCfg[axis].color;

	const Vector3 v = Vector3Add(endPos, Vector3Scale(n, arrowLength));

	const Vector3 a = Vector3Subtract(Vector3Subtract(endPos, Vector3Scale(dim1, 0.5f)), Vector3Scale(dim2, 0.5f));
	const Vector3 b = Vector3Add(a, dim1);
	const Vector3 c = Vector3Add(b, dim2);
	const Vector3 d = Vector3Add(a, dim2);

	rlBegin(RL_TRIANGLES);

	rlColor4ub(col.r, col.g, col.b, col.a);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(c.x, c.y, c.z);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(d.x, d.y, d.z);

	rlVertex3f(a.x, a.y, a.z);
	rlVertex3f(v.x, v.y, v.z);
	rlVertex3f(b.x, b.y, b.z);

	rlVertex3f(b.x, b.y, b.z);
	rlVertex3f(v.x, v.y, v.z);
	rlVertex3f(c.x, c.y, c.z);

	rlVertex3f(c.x, c.y, c.z);
	rlVertex3f(v.x, v.y, v.z);
	rlVertex3f(d.x, d.y, d.z);

	rlVertex3f(d.x, d.y, d.z);
	rlVertex3f(v.x, v.y, v.z);
	rlVertex3f(a.x, a.y, a.z);

	rlEnd();
}

static void DrawGizmoCenter(const GizmoData* data)
{
	const Vector3 origin = data->curTransform->translation;

	const float radius = data->gizmoSize * GIZMO.trCircleRadiusFactor;
	const Color col = GIZMO.trCircleColor;
	const int angleStep = 15;

	rlPushMatrix();
	rlTranslatef(origin.x, origin.y, origin.z);

	rlBegin(RL_LINES);
	rlColor4ub(col.r, col.g, col.b, col.a);
	for (int i = 0; i < 360; i += angleStep)
	{
		float angle = (float)i * DEG2RAD;
		Vector3 p = Vector3Scale(data->right, sinf(angle) * radius);
		p = Vector3Add(p, Vector3Scale(data->up, cosf(angle) * radius));
		rlVertex3f(p.x, p.y, p.z);

		angle += (float)angleStep * DEG2RAD;
		p = Vector3Scale(data->right, sinf(angle) * radius);
		p = Vector3Add(p, Vector3Scale(data->up, cosf(angle) * radius));
		rlVertex3f(p.x, p.y, p.z);
	}
	rlEnd();
	rlPopMatrix();
}

static void DrawGizmoCircle(const GizmoData* data, int axis)
{
	if (IsThisGizmoTransforming(data) && (!IsGizmoAxisActive(axis) || !IsGizmoRotating()))
	{
		return;
	}
	
	const Vector3 origin = data->curTransform->translation;

	const Vector3 dir1 = data->axis[(axis + 1) % 3];
	const Vector3 dir2 = data->axis[(axis + 2) % 3];

	const Color col = GIZMO.axisCfg[axis].color;

	const float radius = data->gizmoSize;
	const int angleStep = 10;

	rlPushMatrix();
	rlTranslatef(origin.x, origin.y, origin.z);

	rlBegin(RL_LINES);
	rlColor4ub(col.r, col.g, col.b, col.a);
	for (int i = 0; i < 360; i += angleStep)
	{
		float angle = (float)i * DEG2RAD;
		Vector3 p = Vector3Scale(dir1, sinf(angle) * radius);
		p = Vector3Add(p, Vector3Scale(dir2, cosf(angle) * radius));
		rlVertex3f(p.x, p.y, p.z);

		angle += (float)angleStep * DEG2RAD;
		p = Vector3Scale(dir1, sinf(angle) * radius);
		p = Vector3Add(p, Vector3Scale(dir2, cosf(angle) * radius));
		rlVertex3f(p.x, p.y, p.z);
	}
	rlEnd();
	rlPopMatrix();
}


//---------------------------------------------------------------------------------------------------
// Functions Definition - Mouse ray to Gizmo intersections
//---------------------------------------------------------------------------------------------------

static bool CheckOrientedBoundingBox(const GizmoData* data, Ray ray, Vector3 obbCenter, Vector3 obbHalfSize)
{
	const Vector3 oLocal = Vector3Subtract(ray.position, obbCenter);

	Ray localRay;

	localRay.position.x = Vector3DotProduct(oLocal, data->axis[GZ_AXIS_X]);
	localRay.position.y = Vector3DotProduct(oLocal, data->axis[GZ_AXIS_Y]);
	localRay.position.z = Vector3DotProduct(oLocal, data->axis[GZ_AXIS_Z]);

	localRay.direction.x = Vector3DotProduct(ray.direction, data->axis[GZ_AXIS_X]);
	localRay.direction.y = Vector3DotProduct(ray.direction, data->axis[GZ_AXIS_Y]);
	localRay.direction.z = Vector3DotProduct(ray.direction, data->axis[GZ_AXIS_Z]);

	const BoundingBox aabbLocal = {
		.min = {-obbHalfSize.x, -obbHalfSize.y, -obbHalfSize.z},
		.max = {+obbHalfSize.x, +obbHalfSize.y, +obbHalfSize.z}
	};

	return GetRayCollisionBox(localRay, aabbLocal).hit;
}

static bool CheckGizmoAxis(const GizmoData* data, int axis, Ray ray, int type)
{
	float halfDim[3];

	halfDim[axis] = data->gizmoSize * 0.5f;
	halfDim[(axis + 1) % 3] = data->gizmoSize * GIZMO.trArrowWidthFactor * 0.5f;
	halfDim[(axis + 2) % 3] = halfDim[(axis + 1) % 3];

	if (type == GIZMO_SCALE && CheckGizmoType(data, GIZMO_TRANSLATE | GIZMO_SCALE))
	{
		halfDim[axis] *= 0.5f;
	}

	const Vector3 obbCenter = Vector3Add(data->curTransform->translation,
	                                     Vector3Scale(data->axis[axis], halfDim[axis]));

	return CheckOrientedBoundingBox(data, ray, obbCenter, (Vector3){halfDim[0], halfDim[1], halfDim[2]});
}

static bool CheckGizmoPlane(const GizmoData* data, int axis, Ray ray)
{
	const Vector3 dir1 = data->axis[(axis + 1) % 3];
	const Vector3 dir2 = data->axis[(axis + 2) % 3];


	const float offset = GIZMO.trPlaneOffsetFactor * data->gizmoSize;
	const float size = GIZMO.trPlaneSizeFactor * data->gizmoSize;

	const Vector3 a = Vector3Add(Vector3Add(data->curTransform->translation, Vector3Scale(dir1, offset)),
	                             Vector3Scale(dir2, offset));
	const Vector3 b = Vector3Add(a, Vector3Scale(dir1, size));
	const Vector3 c = Vector3Add(b, Vector3Scale(dir2, size));
	const Vector3 d = Vector3Add(a, Vector3Scale(dir2, size));

	return GetRayCollisionQuad(ray, a, b, c, d).hit;
}

static bool CheckGizmoCircle(const GizmoData* data, int index, Ray ray)
{
	const Vector3 origin = data->curTransform->translation;

	const Vector3 dir1 = data->axis[(index + 1) % 3];
	const Vector3 dir2 = data->axis[(index + 2) % 3];

	const float circleRadius = data->gizmoSize;
	const int angleStep = 10;

	const float sphereRadius = /*2.0f **/ circleRadius * sinf((float)angleStep * DEG2RAD / 2.0f);

	for (int i = 0; i < 360; i += angleStep)
	{
		const float angle = (float)i * DEG2RAD;
		Vector3 p = Vector3Add(origin, Vector3Scale(dir1, sinf(angle) * circleRadius));
		p = Vector3Add(p, Vector3Scale(dir2, cosf(angle) * circleRadius));

		if (GetRayCollisionSphere(ray, p, sphereRadius).hit)
		{
			return true;
		}
	}

	return false;
}

static bool CheckGizmoCenter(const GizmoData* data, Ray ray)
{
	return GetRayCollisionSphere(ray, data->curTransform->translation, data->gizmoSize * GIZMO.trCircleRadiusFactor).
		hit;
}


//---------------------------------------------------------------------------------------------------
// Functions Definitions - Input Handling
//---------------------------------------------------------------------------------------------------

static Vector3 GetWorldMouse(const GizmoData* data)
{
	const float dist = Vector3Distance(data->camPos, data->curTransform->translation);
	const Ray mouseRay = Vec3ScreenToWorldRay(GetMousePosition(), &data->invViewProj);
	return Vector3Add(mouseRay.position, Vector3Scale(mouseRay.direction, dist));
}

static void GizmoHandleInput(const GizmoData* data)
{
	int action = GIZMO.curAction;

	if (action != GZ_ACTION_NONE)
	{
		if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT))
		{
			//SetMouseCursor(MOUSE_CURSOR_DEFAULT);
			action = GZ_ACTION_NONE;
			GIZMO.activeAxis = 0;
		}
		else
		{
			const Vector3 endWorldMouse = GetWorldMouse(data);
			const Vector3 pVec = Vector3Subtract(endWorldMouse, GIZMO.startWorldMouse);

			switch (action)
			{
			case GZ_ACTION_TRANSLATE:
				{
					GIZMO.activeTransform->translation = GIZMO.startTransform.translation;
					if (GIZMO.activeAxis == GZ_ACTIVE_XYZ)
					{
						GIZMO.activeTransform->translation = Vector3Add(GIZMO.activeTransform->translation,
						                                                Vector3Project(pVec, data->right));
						GIZMO.activeTransform->translation = Vector3Add(GIZMO.activeTransform->translation,
						                                                Vector3Project(pVec, data->up));
					}
					else
					{
						if (GIZMO.activeAxis & GZ_ACTIVE_X)
						{
							const Vector3 prj = Vector3Project(pVec, data->axis[GZ_AXIS_X]);
							GIZMO.activeTransform->translation = Vector3Add(GIZMO.activeTransform->translation, prj);
						}
						if (GIZMO.activeAxis & GZ_ACTIVE_Y)
						{
							const Vector3 prj = Vector3Project(pVec, data->axis[GZ_AXIS_Y]);
							GIZMO.activeTransform->translation = Vector3Add(GIZMO.activeTransform->translation, prj);
						}
						if (GIZMO.activeAxis & GZ_ACTIVE_Z)
						{
							const Vector3 prj = Vector3Project(pVec, data->axis[GZ_AXIS_Z]);
							GIZMO.activeTransform->translation = Vector3Add(GIZMO.activeTransform->translation, prj);
						}
					}
				}
				break;
			case GZ_ACTION_SCALE:
				{
					GIZMO.activeTransform->scale = GIZMO.startTransform.scale;
					if (GIZMO.activeAxis == GZ_ACTIVE_XYZ)
					{
						const float delta = Vector3DotProduct(pVec, GIZMO.axisCfg[GZ_AXIS_X].normal);
						GIZMO.activeTransform->scale = Vector3AddValue(GIZMO.activeTransform->scale, delta);
					}
					else
					{
						if (GIZMO.activeAxis & GZ_ACTIVE_X)
						{
							const Vector3 prj = Vector3Project(pVec, GIZMO.axisCfg[GZ_AXIS_X].normal);
							// data->axis[GIZMO_AXIS_X]);
							GIZMO.activeTransform->scale = Vector3Add(GIZMO.activeTransform->scale, prj);
						}
						if (GIZMO.activeAxis & GZ_ACTIVE_Y)
						{
							const Vector3 prj = Vector3Project(pVec, GIZMO.axisCfg[GZ_AXIS_Y].normal);
							GIZMO.activeTransform->scale = Vector3Add(GIZMO.activeTransform->scale, prj);
						}
						if (GIZMO.activeAxis & GZ_ACTIVE_Z)
						{
							const Vector3 prj = Vector3Project(pVec, GIZMO.axisCfg[GZ_AXIS_Z].normal);
							GIZMO.activeTransform->scale = Vector3Add(GIZMO.activeTransform->scale, prj);
						}
					}
				}
				break;
			case GZ_ACTION_ROTATE:
				{
					GIZMO.activeTransform->rotation = GIZMO.startTransform.rotation;
					//SetMouseCursor(MOUSE_CURSOR_RESIZE_EW);
					const float delta = Clamp(Vector3DotProduct(pVec, Vector3Add(data->right, data->up)), -2 * PI,
					                          +2 * PI);
					if (GIZMO.activeAxis & GZ_ACTIVE_X)
					{
						const Quaternion q = QuaternionFromAxisAngle(data->axis[GZ_AXIS_X], delta);
						GIZMO.activeTransform->rotation = QuaternionMultiply(q, GIZMO.activeTransform->rotation);
					}
					if (GIZMO.activeAxis & GZ_ACTIVE_Y)
					{
						const Quaternion q = QuaternionFromAxisAngle(data->axis[GZ_AXIS_Y], delta);
						GIZMO.activeTransform->rotation = QuaternionMultiply(q, GIZMO.activeTransform->rotation);
					}
					if (GIZMO.activeAxis & GZ_ACTIVE_Z)
					{
						const Quaternion q = QuaternionFromAxisAngle(data->axis[GZ_AXIS_Z], delta);
						GIZMO.activeTransform->rotation = QuaternionMultiply(q, GIZMO.activeTransform->rotation);
					}
					//BUG FIXED: Updating the transform "starting point" prevents uncontrolled rotations in local mode
					GIZMO.startTransform = *GIZMO.activeTransform;
					GIZMO.startWorldMouse = endWorldMouse;
				}
				break;
			default:
				break;
			}
		}
	}
	else
	{
		if (IsMouseButtonPressed(MOUSE_BUTTON_LEFT))
		{
			const Ray mouseRay = Vec3ScreenToWorldRay(GetMousePosition(), &data->invViewProj);

			int hit = -1;
			action = GZ_ACTION_NONE;

			for (int k = 0; hit == -1 && k < 2; ++k)
			{
				const int gizmoFlag = k == 0 ? GIZMO_SCALE : GIZMO_TRANSLATE;
				const int gizmoAction = k == 0 ? GZ_ACTION_SCALE : GZ_ACTION_TRANSLATE;

				if (data->flags & gizmoFlag)
				{
					if (CheckGizmoCenter(data, mouseRay))
					{
						action = gizmoAction;
						hit = 6;
						break;
					}
					for (int i = 0; i < GIZMO_AXIS_COUNT; ++i)
					{
						if (CheckGizmoAxis(data, i, mouseRay, gizmoFlag))
						{
							action = gizmoAction;
							hit = i;
							break;
						}
						if (CheckGizmoPlane(data, i, mouseRay))
						{
							action = CheckGizmoType(data, GIZMO_SCALE | GIZMO_TRANSLATE)
								         ? GIZMO_TRANSLATE
								         : gizmoAction;
							hit = 3 + i;
							break;
						}
					}
				}
			}

			if (hit == -1 && data->flags & GIZMO_ROTATE)
			{
				for (int i = 0; i < GIZMO_AXIS_COUNT; ++i)
				{
					if (CheckGizmoCircle(data, i, mouseRay))
					{
						action = GZ_ACTION_ROTATE;
						hit = i;
						break;
					}
				}
			}

			GIZMO.activeAxis = 0;
			if (hit >= 0)
			{
				switch (hit)
				{
				case 0:
					GIZMO.activeAxis = GZ_ACTIVE_X;
					break;
				case 1:
					GIZMO.activeAxis = GZ_ACTIVE_Y;
					break;
				case 2:
					GIZMO.activeAxis = GZ_ACTIVE_Z;
					break;
				case 3:
					GIZMO.activeAxis = GZ_ACTIVE_Y | GZ_ACTIVE_Z;
					break;
				case 4:
					GIZMO.activeAxis = GZ_ACTIVE_X | GZ_ACTIVE_Z;
					break;
				case 5:
					GIZMO.activeAxis = GZ_ACTIVE_X | GZ_ACTIVE_Y;
					break;
				case 6:
					GIZMO.activeAxis = GZ_ACTIVE_XYZ;
					break;
				}
				GIZMO.activeTransform = data->curTransform;
				GIZMO.startTransform = *data->curTransform;
				GIZMO.startWorldMouse = GetWorldMouse(data);
			}
		}
	}

	GIZMO.curAction = action;
}