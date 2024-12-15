unit raygizmo;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$I ../raylib.inc}
interface

uses
  raylib, raymath, rlgl, types;


// Битовые флаги для активно трансформируемой оси
type
  PGizmoActiveAxis = ^TGizmoActiveAxis;
  TGizmoActiveAxis = Integer;
  const
    GZ_ACTIVE_X = TGizmoActiveAxis(1 shl 0);   // Активная трансформация по оси X
    GZ_ACTIVE_Y = TGizmoActiveAxis(1 shl 1);   // Активная трансформация по оси Y
    GZ_ACTIVE_Z = TGizmoActiveAxis(1 shl 2);   // Активная трансформация по оси Z
    GZ_ACTIVE_XYZ = TGizmoActiveAxis(GZ_ACTIVE_X or GZ_ACTIVE_Y or GZ_ACTIVE_Z);  // Активная трансформация по всем осям

  // Типы активной трансформации для gizmo
type
  PGizmoAction = ^TGizmoAction;
  TGizmoAction = Integer;
  const
    GZ_ACTION_NONE       = TGizmoAction(0);   // Нет активной трансформации
    GZ_ACTION_TRANSLATE  = TGizmoAction(1);   // Трансформация перемещения
    GZ_ACTION_SCALE      = TGizmoAction(2);   // Трансформация масштабирования
    GZ_ACTION_ROTATE     = TGizmoAction(3);   // Трансформация вращения

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

  const
    // Индексы осей
    GZ_AXIS_X = 0;                        // Индекс оси X
    GZ_AXIS_Y = 1;                        // Индекс оси Y
    GZ_AXIS_Z = 2;                        // Индекс оси Z
    GIZMO_AXIS_COUNT = 3;                 // Общее количество осей



type
  // Структура для оси Gizmo
  TGizmoAxis = record
    Normal: TVector3; // Направление оси
    Color: TColorB;    // Цвет, используемый для представления оси
  end;

  (*
   * Global variables used by the gizmos.
   * These are shared across all gizmos to maintain consistent behavior and appearance.
   *)

   PGizmoGlobals = ^TGizmoGlobals;
   TGizmoGlobals = record
     axisCfg: array[0..GIZMO_AXIS_COUNT-1] of TGizmoAxis;  // Data related to the 3 axes, globally oriented.

     gizmoSize: single;                      // Size of the gizmos. All other metrics are expressed as fractions of this measure.
     lineWidth: single;                      // Width of the lines used to draw the gizmos.

     trArrowWidthFactor: single;             // Width of the arrows (and cubes) as a fraction of gizmoSize.
     trArrowLengthFactor: single;            // Length of the arrows as a fraction of gizmoSize.
     trPlaneOffsetFactor: single;            // Offset of the gizmo planes from the center as a fraction of gizmoSize.
     trPlaneSizeFactor: single;              // Size of the planes (quad representation) as a fraction of gizmoSize.
     trCircleRadiusFactor: single;           // Radius of the central circle as a fraction of gizmoSize.
     trCircleColor: TColorB;                 // Color of the central circle.

     curAction: Integer;                      // Currently active GizmoAction.
     activeAxis: Integer;                     // Active axis (a combination of GizmoActiveAxis flags) for the current action.
     startTransform: TTransform;              // Backup Transform saved before the transformation begins.
     activeTransform: PTransform;             // Pointer to the active Transform to update during transformation.
     startWorldMouse: TVector3;               // Position of the mouse in world space at the start of the transformation.
     public
     procedure SetDefault;
   end;



   (*
    * Temporary data associated with a gizmo.
    * This data is recalculated at each call to DrawGizmo3D(), in immediate-mode style.
    *)

   PGizmoData = ^TGizmoData;
   TGizmoData = record
     invViewProj: TMatrix;                  // Inverted View-Projection matrix.
     curTransform: PTransform;              // Pointer to the current Transform. Only one can be the "activeTransform" at a time.

     axis: array [0..GIZMO_AXIS_COUNT-1] of TVector3;  // Current axes used for transformations (may differ from global axes).
                                                       // Axes can be in global, view, or local mode depending on configuration.

     gizmoSize: single;                     // Actual gizmo size, adjusted to maintain camera-independent scaling.
     camPos: TVector3;                      // Position of the camera, extracted during rendering.
     right, up, forward: TVector3;          // Local orientation vectors: right, up, and forward.
     flags: TConfigFlags;                        // Configuration flags for the gizmo.
   end;

// Function Declarations - Helper Functions
{*
 * Compute the axis orientation for a specific gizmo.
 * Determines whether the axes are oriented globally, locally, or in view mode.
 * @param gizmoData Pointer to the data associated with the gizmo.
 *}
procedure ComputeAxisOrientation(gizmoData: PGizmoData);

(*
 * Check if a specific axis is actively used for the current transformation.
 * @param axis The axis to check (GZ_AXIS_X, GZ_AXIS_Y, or GZ_AXIS_Z).
 * @return true if the axis is actively used; false otherwise.
 *)
function IsGizmoAxisActive(axis: Integer): boolean;

(*
 * Check if a gizmo is compatible with a specific transformation type.
 * @param data Pointer to the data associated with the gizmo.
 * @param type A combination of GizmoFlags values specifying the transformation type.
 * @return true if the gizmo supports the requested transformation type; false otherwise.
 *)
function CheckGizmoType(const data: PGizmoData; type_: Integer): boolean;

(*
 * Check if any gizmo is currently transforming.
 * @return true if a gizmo is active and transforming; false otherwise.
 *)
function IsGizmoTransforming(): boolean;

(*
 * Check if the active transformation is of scaling type.
 * @return true if a gizmo is actively scaling; false otherwise.
 *)
function IsGizmoScaling(): boolean;

(*
 * Check if the active transformation is of translation type.
 * @return true if a gizmo is actively translating; false otherwise.
 *)
function IsGizmoTranslating(): boolean;

(*
 * Check if the active transformation is of rotation type.
 * @return true if a gizmo is actively rotating; false otherwise.
 *)
 function IsGizmoRotating(): boolean;

 (*
  * Convert a screen-space vector to world space.
  * @param source The screen-space Vector3 to convert.
  * @param matViewProjInv Pointer to the inverted view-projection matrix.
  * @return The input vector transformed to world space.
  * @note This function does not depend on a Camera. Refer to raylib's Vector3Unproject() for the original implementation.
  *)
 function Vec3ScreenToWorld(source: TVector3; const matViewProjInv: PMatrix): TVector3;




var
  Gizmo: TGizmoGlobals;

implementation

procedure ComputeAxisOrientation(gizmoData: PGizmoData);
var flags: TConfigFlags;
  i: integer;
begin
  flags := gizmoData^.flags;
  // Scaling is currently supported only in local mode
  //if (flags & GIZMO_SCALE)
  if (flags and GIZMO_SCALE) <> 0 then
  begin
    //flags &= ~GIZMO_VIEW;
    //flags |= GIZMO_LOCAL;
    flags := flags and not GIZMO_VIEW; // Clear the GIZMO_VIEW flag
    flags := flags or GIZMO_LOCAL;      // Set the GIZMO_LOCAL flag
  end;

  if (flags and GIZMO_VIEW) <> 0 then
  begin
    gizmoData^.axis[GZ_AXIS_X] := gizmoData^.right;
    gizmoData^.axis[GZ_AXIS_Y] := gizmoData^.up;
    gizmoData^.axis[GZ_AXIS_Z] := gizmoData^.forward;
  end
  else
  begin
    gizmoData^.axis[GZ_AXIS_X] := GIZMO.axisCfg[GZ_AXIS_X].normal;
    gizmoData^.axis[GZ_AXIS_Y] := GIZMO.axisCfg[GZ_AXIS_Y].normal;
    gizmoData^.axis[GZ_AXIS_Z] := GIZMO.axisCfg[GZ_AXIS_Z].normal;

    if (flags and GIZMO_LOCAL) <> 0 then
      begin
        for i := 0 to GIZMO_AXIS_COUNT - 1 do // (int i = 0; i < 3; ++i)
  	  begin
  	    gizmoData^.axis[i] := Vector3Normalize(
  	    Vector3RotateByQuaternion(gizmoData^.axis[i], gizmoData^.curTransform^.rotation));
  	  end;
      end;
  end;

end;

function IsGizmoAxisActive(axis: Integer): boolean;
begin
  Result := ((axis = GZ_AXIS_X) and ((GIZMO.activeAxis and GZ_ACTIVE_X) <> 0)) or
            ((axis = GZ_AXIS_Y) and ((GIZMO.activeAxis and GZ_ACTIVE_Y) <> 0)) or
            ((axis = GZ_AXIS_Z) and ((GIZMO.activeAxis and GZ_ACTIVE_Z) <> 0));
end;

function CheckGizmoType(const data: PGizmoData; type_: Integer): boolean;
begin
  Result := (data^.Flags and type_) = type_;
  //return (data->flags & type) == type;
end;

function IsGizmoTransforming(): boolean;
begin
  Result := GIZMO.curAction <> GZ_ACTION_NONE;
  //return GIZMO.curAction != GZ_ACTION_NONE;
end;

function IsGizmoScaling(): boolean;
begin
  Result := GIZMO.curAction = GZ_ACTION_SCALE;
end;

function IsGizmoTranslating(): boolean;
begin
  Result := GIZMO.curAction = GZ_ACTION_TRANSLATE;
end;

function IsGizmoRotating(): boolean;
begin
 Result := GIZMO.curAction = GZ_ACTION_ROTATE;
end;

function Vec3ScreenToWorld(source: TVector3; const matViewProjInv: PMatrix
  ): TVector3;
var qt: TQuaternion;
begin
  qt := QuaternionTransform(QuaternionCreate(source.x, source.y, source.z, 1.0), matViewProjInv^);
  Result := Vector3Create(qt.x / qt.w, qt.y / qt.w, qt.z / qt.w);
end;





{ TGizmoGlobals }
procedure TGizmoGlobals.SetDefault;
begin
  AxisCfg[0].Normal := Vector3Create(1,0,0);
  AxisCfg[0].Color := ColorCreate(229,72,91,255);

  AxisCfg[1].Normal := Vector3Create(0,1,0);
  AxisCfg[1].Color := ColorCreate(131,205,56,255);

  AxisCfg[1].Normal := Vector3Create(0,0,1);
  AxisCfg[1].Color := ColorCreate(69,138,242,255);

  GizmoSize:= 1.5;
  LineWidth:= 2.5;
  TrArrowLengthFactor:= 0.15;
  TrArrowWidthFactor:= 0.1;
  TrPlaneOffsetFactor:= 0.3;
  TrPlaneSizeFactor:= 0.15;
  TrCircleRadiusFactor:= 0.1;
  TrCircleColor:= ColorCreate (255,255,255,200);
  CurAction:= GZ_ACTION_NONE; // Assuming GZ_ACTION_NONE is defined elsewhere
  ActiveAxis:= 0;

end;

end.

