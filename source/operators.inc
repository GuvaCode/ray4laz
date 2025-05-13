{ TVector2 operators }

operator := (a: TFloat2Array): TVector2;
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

{ TVector3 operators }

operator := (a: TFloat3Array): TVector3;
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

{ TVector4 operators }

operator := (a: TFloat4Array): TVector4;
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
