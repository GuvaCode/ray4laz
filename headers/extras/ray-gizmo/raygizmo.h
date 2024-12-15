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

#ifndef RAY_GIZMO_H
#define RAY_GIZMO_H

//--------------------------------------------------------------------------------------------------
// Includes
//--------------------------------------------------------------------------------------------------

#include <raylib.h>


/**
 * Bitwise flags for configuring DrawGizmo3D().
 * Use these flags to customize specific gizmo behaviors.
 * @note Mutually exclusive axis orientation flags (e.g., GIZMO_LOCAL, GIZMO_VIEW)
 * override the default global axis behavior.
 */
typedef enum
{
	GIZMO_DISABLED	= 0,		// 0: Disables gizmo drawing

	// Bitwise flags
	GIZMO_TRANSLATE = 1 << 0,	// Enables translation gizmo
	GIZMO_ROTATE	= 1 << 1,	// Enables rotation gizmo
	GIZMO_SCALE		= 1 << 2,	// Enables scaling gizmo (implicitly enables GIZMO_LOCAL)
	GIZMO_ALL		= GIZMO_TRANSLATE | GIZMO_ROTATE | GIZMO_SCALE,		// Enables all gizmos

	// Mutually exclusive axis orientation flags
	// Default: Global axis orientation
	GIZMO_LOCAL		= 1 << 3,	// Orients axes locally
	GIZMO_VIEW		= 1 << 4	// Orients axes based on screen view
} GizmoFlags;



//--------------------------------------------------------------------------------------------------
// GIZMO API
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------

#if defined(__cplusplus)
extern "C" {
#endif

//--------------------------------------------------------------------------------------------------

	/**
	 * Initialize a gizmo Transform with default values.
	 * @return A Transform initialized to default values.
	 */
	RLAPI Transform GizmoIdentity(void);

	/**
	 * Convert a gizmo Transform to the corresponding Matrix.
	 * @param transform The gizmo Transform to convert.
	 * @return A Matrix built from the Transform values.
	 */
	RLAPI Matrix GizmoToMatrix(Transform transform);

	/**
	 * Draw the gizmo on the screen in an immediate-mode style.
	 * @param flags A combination of GizmoFlags to configure gizmo behavior.
	 * @param transform A pointer to the Transform affected by the gizmo.
	 * @return true if the gizmo is active and affecting the transform; false otherwise.
	 */
	RLAPI bool DrawGizmo3D(int flags, Transform* transform);

	/**
	 * Set the size of the gizmo.
	 * @param size The new size of the gizmo.
	 * @note All internal gizmo metrics are expressed as a fraction of this measure.
	 * @default 1.5f
	 */
	RLAPI void SetGizmoSize(float size);

	/**
	 * Set the line width of the gizmo geometry.
	 * @param width The new line width.
	 * @default 2.5f
	 */
	RLAPI void SetGizmoLineWidth(float width);

	/**
	 * Set the colors used by the gizmo.
	 * @param x Color of the X-axis.
	 * @param y Color of the Y-axis.
	 * @param z Color of the Z-axis.
	 * @param center Color of the central circle.
	 * @default {229, 72, 91, 255}, {131, 205, 56, 255}, {69, 138, 242, 255}, {255, 255, 255, 200}
	 */
	RLAPI void SetGizmoColors(Color x, Color y, Color z, Color center);

	/**
	 * Change the global axis orientation.
	 * @param right Direction of the right vector.
	 * @param up Direction of the up vector.
	 * @param forward Direction of the forward vector.
	 * @note The vectors should be orthogonal to each other for consistent behavior.
	 * @default (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)
	 */
	RLAPI void SetGizmoGlobalAxis(Vector3 right, Vector3 up, Vector3 forward);


//--------------------------------------------------------------------------------------------------

#if defined(__cplusplus)
}
#endif

//--------------------------------------------------------------------------------------------------

#endif  // RAY_GIZMO_H