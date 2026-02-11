(*
{ TVector2 operators }

operator := (a: TVector2Data): TVector2;
begin
  Result := Vector2Create(a[0], a[1]);
end;

operator + (a, b: TVector2): TVector2;
begin
  Result := Vector2Add(a, b);
end;

operator + (a: TVector2; b: Single): TVector2;
begin
  Result := Vector2AddValue(a, b);
end;

operator - (a: TVector2): TVector2;
begin
  Result := Vector2Negate(a);
end;

operator - (a, b: TVector2): TVector2;
begin
  Result := Vector2Subtract(a, b);
end;

operator - (a: TVector2; b: Single): TVector2;
begin
  Result := Vector2SubtractValue(a, b);
end;

operator * (a, b: TVector2): TVector2;
begin
  Result := Vector2Multiply(a, b);
end;

operator * (a: TVector2; b: Single): TVector2;
begin
  Result := Vector2Scale(a, b);
end;

operator / (a, b: TVector2): TVector2;
begin
  Result := Vector2Divide(a, b);
end;

operator = (a, b: TVector2): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

{ TVector3 operators }

operator := (a: TVector3Data): TVector3;
begin
  Result := Vector3Create(a[0], a[1], a[2]);
end;

operator + (a, b: TVector3): TVector3;
begin
  Result := Vector3Add(a, b);
end;

operator + (a: TVector3; b: Single): TVector3;
begin
  Result := Vector3AddValue(a, b);
end;

operator - (a: TVector3): TVector3;
begin
  Result := Vector3Negate(a);
end;

operator - (a, b: TVector3): TVector3;
begin
  Result := Vector3Subtract(a, b);
end;

operator - (a: TVector3; b: Single): TVector3;
begin
  Result := Vector3SubtractValue(a, b);
end;

operator * (a, b: TVector3): TVector3;
begin
  Result := Vector3Multiply(a, b);
end;

operator * (a: TVector3; b: Single): TVector3;
begin
  Result := Vector3Scale(a, b);
end;

operator / (a, b: TVector3): TVector3;
begin
  Result := Vector3Divide(a, b);
end;

operator = (a, b: TVector3): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
end;

{ TVector4 operators }

operator := (a: TVector4Data): TVector4;
begin
  Result := Vector4Create(a[0], a[1], a[2], a[3]);
end;

operator + (a, b: TVector4): TVector4;
begin
  Result := Vector4Add(a, b);
end;

operator + (a: TVector4; b: Single): TVector4;
begin
  Result := Vector4AddValue(a, b);
end;

operator - (a: TVector4): TVector4;
begin
  Result := Vector4Negate(a);
end;

operator - (a, b: TVector4): TVector4;
begin
  Result := Vector4Subtract(a, b);
end;

operator - (a: TVector4; b: Single): TVector4;
begin
  Result := Vector4SubtractValue(a, b);
end;

operator * (a, b: TVector4): TVector4;
begin
  Result := Vector4Multiply(a, b);
end;

operator * (a: TVector4; b: Single): TVector4;
begin
  Result := Vector4Scale(a, b);
end;

operator / (a, b: TVector4): TVector4;
begin
  Result := Vector4Divide(a, b);
end;

operator = (a, b: TVector4): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z) and (a.w = b.w);
end;

{ TMatrix operators }

operator + (a, b: TMatrix): TMatrix;
begin
  Result := MatrixAdd(a, b);
end;

operator - (a, b: TMatrix): TMatrix;
begin
  Result := MatrixSubtract(a, b);
end;

operator * (a, b: TMatrix): TMatrix;
begin
  Result := MatrixMultiply(a, b);
end;

operator = (a, b: TMatrix): Boolean;
begin
  Result := (a.m0 = b.m0) and (a.m1 = b.m1) and (a.m2 = b.m2) and (a.m3 = b.m3) and
    (a.m4 = b.m4) and (a.m5 = b.m5) and (a.m6 = b.m6) and (a.m7 = b.m7) and
    (a.m8 = b.m8) and (a.m9 = b.m9) and (a.m10 = b.m10) and (a.m11 = b.m11) and
    (a.m12 = b.m12) and (a.m13 = b.m13) and (a.m14 = b.m14) and (a.m15 = b.m15);
end;

{ TColorB operators }

operator := (a: TColorBData): TColorB;
begin
  Result := ColorCreate(a[0], a[1], a[2], a[3]);
end;

operator = (a, b: TColorB): Boolean;
begin
  Result := (a.r = b.r) and (a.g = b.g) and (a.b = b.b) and (a.a = b.a);
end;      *)
