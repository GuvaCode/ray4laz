unit raygizmo;
(*
  Copyright (c) 2024 Claudio Z. (@cloudofoz)
  https://github.com/cloudofoz/raylib-gizmo
  pascal header translation 2024 gunko vadim

  *)

{$mode objfpc}{$H+}
{$packrecords c}
{$ALIGN 8}
{$MINENUMSIZE 4}
// Include configuration file
{$I ../raylib.inc}
interface

uses
  raylib;

type
  PGizmoFlags = ^TGizmoFlags;
  TGizmoFlags = Integer;
  const
    GIZMO_DISABLED  = TConfigFlags(0);                  // Disables gizmo drawing
    GIZMO_TRANSLATE = TConfigFlags(1 shl 0);            // Enables translation gizmo
    GIZMO_ROTATE    = TConfigFlags(1 shl 1);            // Enables rotation gizmo
    GIZMO_SCALE     = TConfigFlags(1 shl 2);            // Enables scaling gizmo (implicitly enables GIZMO_LOCAL)
    GIZMO_LOCAL     = TConfigFlags(1 shl 3);            // Orients axes locally
    GIZMO_VIEW      = TConfigFlags(1 shl 4);            // Orients axes based on screen view
    GIZMO_ALL       = TConfigFlags(GIZMO_TRANSLATE or GIZMO_ROTATE or GIZMO_SCALE); // Enables all gizmos


(* Initialize a gizmo Transform with default values.
   @return A Transform initialized to default values. *)
function GizmoIdentity(): TTransform; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GizmoIdentity';

(* Convert a gizmo Transform to the corresponding Matrix.
   @param transform The gizmo Transform to convert.
   @return A Matrix built from the Transform values. *)
function GizmoToMatrix(transform: TTransform): TMatrix; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'GizmoToMatrix';

(* Draw the gizmo on the screen in an immediate-mode style.
   @param flags A combination of GizmoFlags to configure gizmo behavior.
   @param transform A pointer to the Transform affected by the gizmo.
   @return true if the gizmo is active and affecting the transform; false otherwise. *)
function DrawGizmo3D(flags: integer; transform: PTransform): boolean; cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'DrawGizmo3D';

(* Set the size of the gizmo.
   @param size The new size of the gizmo.
   @note All internal gizmo metrics are expressed as a fraction of this measure.
   @default 1.5f *)
procedure SetGizmoSize(size: single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGizmoSize';


(* Set the line width of the gizmo geometry.
   @param width The new line width.
   @default 2.5f *)
procedure SetGizmoLineWidth(width: single); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGizmoLineWidth';

(* Set the colors used by the gizmo.
   @param x Color of the X-axis.
   @param y Color of the Y-axis.
   @param z Color of the Z-axis.
   @param center Color of the central circle.
   @default {229, 72, 91, 255}, {131, 205, 56, 255}, {69, 138, 242, 255}, {255, 255, 255, 200} *)
procedure SetGizmoColors(x, y, z, center: TColorB); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGizmoColors';


(* Change the global axis orientation.
   @param right Direction of the right vector.
   @param up Direction of the up vector.
   @param forward Direction of the forward vector.
   @note The vectors should be orthogonal to each other for consistent behavior.
   @default (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0) *)
procedure SetGizmoGlobalAxis(right, up, forward: TVector3); cdecl; external {$IFNDEF RAY_STATIC}cDllName{$ENDIF} name 'SetGizmoGlobalAxis';



implementation

end.

