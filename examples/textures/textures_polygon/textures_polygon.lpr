program textures_polygon;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib,
rlgl,
raymath;

const
  screenWidth = 800;
  screenHeight = 450;
var
  Texture: TTexture2D;
  Texcoords: array of TVector2;
  Points: array of TVector2;
  Positions: array of TVector2;
  I: Integer;
  Angle: Single;

procedure DrawTexturePoly(Texture: TTexture2D; Center: TVector2; Points: PVector2; Texcoords: PVector2; PointCount: Integer; Tint: TColor);
var
  I: Integer;
begin
  rlSetTexture(Texture.Id);

  // Texturing is only supported on RL_QUADS
  rlBegin(RL_QUADS);

    rlColor4ub(Tint.R, Tint.G, Tint.B, Tint.A);

    for I := 0 to PointCount - 2 do
    begin
      rlTexCoord2f(0.5, 0.5);
      rlVertex2f(Center.X, Center.Y);

      rlTexCoord2f(Texcoords[I].X, Texcoords[I].Y);
      rlVertex2f(Points[I].X + Center.X, Points[I].Y + Center.Y);

      rlTexCoord2f(Texcoords[I + 1].X, Texcoords[I + 1].Y);
      rlVertex2f(Points[I + 1].X + Center.X, Points[I + 1].Y + Center.Y);

      rlTexCoord2f(Texcoords[I + 1].X, Texcoords[I + 1].Y);
      rlVertex2f(Points[I + 1].X + Center.X, Points[I + 1].Y + Center.Y);
    end;

  rlEnd();

  rlSetTexture(0);
end;



begin
  // Initialization
  //---------------------------------------------------------------------------------------------
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [textures] example - textured polygon');

  // Define texture coordinates to map our texture to poly
  Texcoords := [
    Vector2Create(0.75, 0.0),
    Vector2Create(0.25, 0.0),
    Vector2Create(0.0, 0.5),
    Vector2Create(0.0, 0.75),
    Vector2Create(0.25, 1.0),
    Vector2Create(0.375, 0.875),
    Vector2Create(0.625, 0.875),
    Vector2Create(0.75, 1.0),
    Vector2Create(1.0, 0.75),
    Vector2Create(1.0, 0.5),
    Vector2Create(0.75, 0.0)  // Close the poly
  ];

  // Define the base poly vertices from the UV's
  // NOTE: They can be specified in any other way
  SetLength(Points, Length(Texcoords));
  for I := 0 to High(Points) do
  begin
    Points[i].X := (Texcoords[I].X - 0.5) * 256.0;
    Points[i].Y := (Texcoords[I].Y - 0.5) * 256.0;
  end;

  // Define the vertices drawing position
  // NOTE: Initially same as points but updated every frame
  SetLength(Positions, Length(Texcoords));
  for I := 0 to High(Points) do
    Positions[I] := Points[i];

  // Load texture to be mapped to poly
  Texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/cat.png'));

  Angle := 0.0;             // Rotation angle (in degrees)

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    // Update points rotation with an angle transform
    // NOTE: Base points position are not modified
    Angle := Angle + 1;
    for I := 0 to High(Points) do
      Positions[I] := Vector2Rotate(Points[I], Angle * DEG2RAD);

    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawText('textured polygon', 20, 20, 20, DARKGRAY);

      DrawTexturePoly(Texture, Vector2Create(GetScreenWidth() div 2, GetScreenHeight() div 2),
                      @Positions[0], @Texcoords[0], Length(Points), WHITE);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading

  CloseWindow(); // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

