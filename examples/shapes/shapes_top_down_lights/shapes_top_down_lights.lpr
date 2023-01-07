program shapes_top_down_lights;

{$mode objfpc}{$H+}

uses 
cmem, Math,raymath, rlgl, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  RLGL_SRC_ALPHA = $0302;
  RLGL_MIN = $8007;
  RLGL_MAX = $8008;

  MAX_BOXES = 20;
  MAX_SHADOWS = MAX_BOXES * 3; // MAX_BOXES *3. Each box can cast up to two shadow volumes for the edges it is away from, and one for the box itself
  MAX_LIGHTS = 16;
  // Shadow geometry type
  type TShadowGeometry = record
    Vertices: array [0..3] of TVector2;
  end;

  // Light info type
  type TLightInfo = record
    Active: Boolean;  // Is this light slot active?
    Dirty: Boolean;   // Does this light need to be updated?
    Valid: Boolean;   // Is this light in a valid position?

    Position: TVector2;   // Light position
    Mask: TRenderTexture; // Alpha mask for the light
    OuterRadius: Single;  // The distance the light touches
    Bounds: TRectangle;   // A cached rectangle of the light bounds to help with culling

    Shadows: array [0..MAX_SHADOWS-1] of TShadowGeometry;
    ShadowCount: Integer;
  end;

  var
    Lights: array [0..MAX_LIGHTS-1] of TLightInfo;

    // Move a light and mark it as dirty so that we update it's mask next frame
    procedure MoveLight(Slot: Integer; X, Y: Single);
    begin
      Lights[Slot].Dirty := True;
      Lights[Slot].Position.X := X;
      Lights[Slot].Position.Y := Y;

      // update the cached bounds
      Lights[Slot].Bounds.X := X - Lights[Slot].OuterRadius;
      Lights[Slot].Bounds.Y := Y - Lights[Slot].OuterRadius;
    end;

    // Compute a shadow volume for the edge
    // It takes the edge and projects it back by the light radius and turns it into a quad
    procedure ComputeShadowVolumeForEdge(Slot: Integer; Sp, Ep: TVector2);
    var
      Extension: Single;
      SpVector, SpProjection: TVector2;
      EpVector, EpProjection: TVector2;
    begin
      if Lights[Slot].ShadowCount >= MAX_SHADOWS then
        exit;

      Extension := Lights[Slot].OuterRadius * 2;

      SpVector := Vector2Normalize(Vector2Subtract(Sp, Lights[Slot].Position));
      SpProjection := Vector2Add(Sp, Vector2Scale(SpVector, Extension));

      EpVector := Vector2Normalize(Vector2Subtract(Ep, Lights[Slot].Position));
      EpProjection := Vector2Add(Ep, Vector2Scale(EpVector, Extension));

      Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[0] := Sp;
      Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[1] := Ep;
      Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[2] := EpProjection;
      Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[3] := SpProjection;

      Inc(Lights[Slot].ShadowCount);
    end;

    // Draw the light and shadows to the mask for a light
    procedure DrawLightMask(Slot: Integer);
    var
      I: Integer;
    begin
      // Use the light mask
      BeginTextureMode(Lights[Slot].Mask);

        ClearBackground(WHITE);

        // Force the blend mode to only set the alpha of the destination
        rlSetBlendFactors(RLGL_SRC_ALPHA, RLGL_SRC_ALPHA, RLGL_MIN);
        rlSetBlendMode(BLEND_CUSTOM);

        // If we are valid, then draw the light radius to the alpha mask
        if Lights[Slot].Valid then
          DrawCircleGradient(Trunc(Lights[Slot].Position.X), Trunc(Lights[Slot].Position.Y), Lights[Slot].OuterRadius, ColorAlpha(WHITE, 0), WHITE);

        rlDrawRenderBatchActive();

        // Cut out the shadows from the light radius by forcing the alpha to maximum
        rlSetBlendMode(BLEND_ALPHA);
        rlSetBlendFactors(RLGL_SRC_ALPHA, RLGL_SRC_ALPHA, RLGL_MAX);
        rlSetBlendMode(BLEND_CUSTOM);

        // Draw the shadows to the alpha mask
        for I := 0 to Lights[Slot].ShadowCount - 1 do
        begin
          DrawTriangleFan(@Lights[Slot].Shadows[I].Vertices[0], 4, WHITE);
        end;

        rlDrawRenderBatchActive();

        // Go back to normal blend mode
        rlSetBlendMode(BLEND_ALPHA);

      EndTextureMode();
    end;

    // Setup a light
    procedure SetupLight(Slot: Integer; X, Y, Radius: Single);
    begin
      Lights[Slot].Active := True;
      Lights[Slot].Valid := False; // The light must prove it is valid
      Lights[Slot].Mask := LoadRenderTexture(GetScreenWidth(), GetScreenHeight());
      Lights[Slot].OuterRadius := Radius;

      Lights[Slot].Bounds.Width := Radius * 2;
      Lights[Slot].Bounds.Height := Radius * 2;

      MoveLight(Slot, X, Y);

      // Force the render texture to have something in it
      DrawLightMask(Slot);
    end;

    // See if a light needs to update it's mask
    function UpdateLight(Slot: Integer; Boxes: PRectangle; Count: Integer): Boolean;
    var
      I: Integer;
      Sp: TVector2;
      Ep: TVector2;
    begin
      if not Lights[Slot].Active or not Lights[Slot].Dirty then
          Exit(False);

      Lights[Slot].Dirty := False;
      Lights[Slot].ShadowCount := 0;
      Lights[Slot].Valid := False;

      for I := 0 to Count - 1 do
      begin
        // Are we in a box? if so we are not valid
        if CheckCollisionPointRec(Lights[Slot].Position, Boxes[I]) then
          exit(False);

        // If this box is outside our bounds, we can skip it
        if not CheckCollisionRecs(Lights[Slot].Bounds, Boxes[I]) then
          continue;

        // Check the edges that are on the same side we are, and cast shadow volumes out from them

        // Top
        Sp := Vector2Create(Boxes[I].X, Boxes[I].Y);
        Ep := Vector2Create(Boxes[I].X + Boxes[I].Width, Boxes[I].Y);

        if Lights[Slot].Position.Y > EP.Y then
          ComputeShadowVolumeForEdge(Slot, Sp, Ep);

        // Right
        Sp := Ep;
        Ep.Y := Ep.Y + Boxes[I].Height;
        if Lights[Slot].Position.X < Ep.X then
          ComputeShadowVolumeForEdge(Slot, Sp, Ep);

        // Bottom
        Sp := Ep;
        Ep.X := Ep.X - Boxes[I].Width;
        if Lights[Slot].Position.Y < Ep.Y then
          ComputeShadowVolumeForEdge(Slot, Sp, Ep);

        // Left
        Sp := Ep;
        Ep.Y := Ep.Y - Boxes[I].Height;
        if Lights[Slot].Position.X > Ep.X then
          ComputeShadowVolumeForEdge(Slot, Sp, Ep);

        // The box itself
        Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[0] := Vector2Create(Boxes[I].X, Boxes[I].Y);
        Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[1] := Vector2Create(Boxes[I].X, Boxes[I].Y + Boxes[I].Height);
        Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[2] := Vector2Create(Boxes[I].X + Boxes[I].Width, Boxes[I].Y + Boxes[I].Height);
        Lights[Slot].Shadows[Lights[Slot].ShadowCount].Vertices[3] := Vector2Create(Boxes[I].X + Boxes[I].Width, Boxes[I].Y);
        Inc(Lights[Slot].ShadowCount);
      end;

      Lights[Slot].Valid := True;

      DrawLightMask(Slot);

      Result := True;
    end;

    // Set up some boxes
    procedure SetupBoxes(Boxes: PRectangle; Count: PInteger);
    var
      I: Integer;
    begin
      Boxes[0] := RectangleCreate(150,80, 40, 40);
      Boxes[1] := RectangleCreate(1200, 700, 40, 40);
      Boxes[2] := RectangleCreate(200, 600, 40, 40);
      Boxes[3] := RectangleCreate(1000, 50, 40, 40);
      Boxes[4] := RectangleCreate(500, 350, 40, 40);

      for I := 5 to MAX_BOXES - 1 do
      begin
        Boxes[I] := RectangleCreate(GetRandomValue(0, GetScreenWidth()),
        GetRandomValue(0, GetScreenHeight()), GetRandomValue(10, 100), GetRandomValue(10, 100));
      end;

      Count^ := MAX_BOXES;
    end;

var
  BoxCount: Integer;
  Boxes: array [0..MAX_BOXES-1] of TRectangle;
  Img: TImage;
  BackgroundTexture: TTexture2D;
  LightMask: TRenderTexture;
  NextLight: Integer;
  ShowLines: Boolean;
  DirtyLights: Boolean;
  I, B, S: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shapes] example - top down lights');  FillChar(Lights, SizeOf(Lights), 0);

  // Initialize our 'world' of boxes
  BoxCount := 0;
  FillChar(Boxes, SizeOf(Boxes), 0);
  SetupBoxes(@Boxes, @BoxCount);

  // Create a checkerboard ground texture
  Img := GenImageChecked(64, 64, 32, 32, DARKBROWN, DARKGRAY);
  BackgroundTexture := LoadTextureFromImage(Img);
  UnloadImage(Img);

  // Create a global light mask to hold all the blended lights
  LightMask := LoadRenderTexture(GetScreenWidth(), GetScreenHeight());

  // Setup initial light
  SetupLight(0, 600, 400, 300);
  NextLight := 1;

  ShowLines := False;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Drag light 0
      if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
        MoveLight(0, GetMousePosition().X, GetMousePosition().Y);

      // Make a new light
      if IsMouseButtonPressed(MOUSE_BUTTON_RIGHT) and (nextLight < MAX_LIGHTS) then
      begin
        SetupLight(NextLight, GetMousePosition().X, GetMousePosition().Y, 200);
        Inc(NextLight);
      end;

      // Toggle debug info
      if IsKeyPressed(KEY_F1) then
        ShowLines := not showLines;

      // Update the lights and keep track if any were dirty so we know if we need to update the master light mask
      DirtyLights := False;
      for I := 0 to MAX_LIGHTS - 1 do
      begin
        if UpdateLight(I, @Boxes[0], BoxCount) then
          DirtyLights := True;
      end;

      // Update the light mask
      if DirtyLights then
      begin
        // Build up the light mask
        BeginTextureMode(LightMask);

          ClearBackground(BLACK);

          // Force the blend mode to only set the alpha of the destination
          rlSetBlendFactors(RLGL_SRC_ALPHA, RLGL_SRC_ALPHA, RLGL_MIN);
          rlSetBlendMode(BLEND_CUSTOM);

          // Merge in all the light masks
          for I := 0 to MAX_LIGHTS - 1 do
          begin
              if Lights[I].Active then
                DrawTextureRec(Lights[I].Mask.Texture, RectangleCreate(0, 0, GetScreenWidth(), -GetScreenHeight()),
                Vector2Zero(), WHITE);
          end;

          rlDrawRenderBatchActive();

          // Go back to normal blend
          rlSetBlendMode(BLEND_ALPHA);
        EndTextureMode();
      end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(BLACK);

      // Draw the tile background
      DrawTextureRec(BackgroundTexture, RectangleCreate(0, 0, GetScreenWidth(),
      GetScreenHeight()), Vector2Zero(), WHITE);

      // Overlay the shadows from all the lights
      DrawTextureRec(LightMask.Texture, RectangleCreate(0, 0, GetScreenWidth(),
      -GetScreenHeight()), Vector2Zero(), ColorAlpha(WHITE, IfThen(ShowLines, 0.75, 1.0)));

      // Draw the lights
      for I := 0 to MAX_LIGHTS - 1 do
      begin
        if Lights[I].Active then
        begin
          if i = 0 then
            DrawCircle(Trunc(Lights[I].Position.X), Trunc(Lights[I].Position.Y), 10, YELLOW)
          else
            DrawCircle(Trunc(Lights[I].Position.X), Trunc(Lights[I].Position.Y), 10, WHITE);
        end;
      end;

      if ShowLines then
      begin
        for S := 0 to Lights[0].ShadowCount - 1 do
        begin
          DrawTriangleFan(@Lights[0].Shadows[S].Vertices, 4, DARKPURPLE);
        end;

        for B := 0 to BoxCount - 1 do
        begin
          if CheckCollisionRecs(Boxes[B], Lights[0].Bounds) then
            DrawRectangleRec(Boxes[B], PURPLE);

          DrawRectangleLines(Trunc(Boxes[B].X), Trunc(Boxes[B].Y), Trunc(Boxes[B].Width), Trunc(Boxes[B].Height), DARKBLUE);
        end;

          DrawText('(F1) Hide Shadow Volumes', 10, 50, 10, GREEN);
      end else
      begin
        DrawText('(F1) Show Shadow Volumes', 10, 50, 10, GREEN);
      end;

      DrawFPS(screenWidth - 80, 10);
      DrawText('Drag to move light #1', 10, 10, 10, DARKGREEN);
      DrawText('Right click to add new light', 10, 30, 10, DARKGREEN);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------

  UnloadTexture(BackgroundTexture);
   UnloadRenderTexture(LightMask);
   for I := 0 to MAX_LIGHTS - 1 do
     if Lights[I].Active then
       UnloadRenderTexture(Lights[I].Mask);

  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

