program models_mesh_picking;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;
  SINGLE_MAX = 340282346638528859811704183484516925440.0;
var
  Camera: TCamera;
  Ray: TRay;
  Tower: TModel;
  Texture: TTexture;
  TowerPos: TVector3;
  TowerBBox: TBoundingBox;
  G0, G1, G2, G3, Ta, Tb, TC, Bary, Sp: TVector3;
  Sr: Single;
  Collision, GroundHitInfo, TriHitInfo, SphereHitInfo, BoxHitInfo, MeshHitInfo: TRayCollision;
  HitObjectName: string;
  CursorColor: TColor;
  M: Integer;
  NormalEnd: TVector3;
  YPos: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - mesh picking');
  // Define the camera to look into our 3d world
   Camera := Default(TCamera);
   Camera.Position := Vector3Create(20.0, 20.0, 20.0); // Camera position
   Camera.Target := Vector3Create(0.0, 8.0, 0.0);      // Camera looking at point
   Camera.Up := Vector3Create(0.0, 1.6, 0.0);          // Camera up vector (rotation towards target)
   Camera.Fovy := 45.0;                                  // Camera field-of-view Y
   Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

   Ray := Default(TRay);

   Tower := LoadModel(PChar(GetApplicationDirectory + 'resources/models/obj/turret.obj'));             // Load OBJ model
   Texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/models/obj/turret_diffuse.png')); // Load model texture
   Tower.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture;                     // Set model diffuse texture

   TowerPos := Vector3Create(0.0, 0.0, 0.0);                        // Set model position
   TowerBBox := GetMeshBoundingBox(Tower.Meshes[0]);                  // Get mesh bounding box

   // Ground quad
   G0 := Vector3Create(-50.0, 0.0, -50.0);
   G1 := Vector3Create(-50.0, 0.0,  50.0);
   G2 := Vector3Create( 50.0, 0.0,  50.0);
   G3 := Vector3Create( 50.0, 0.0, -50.0);

   // Test triangle
   Ta := Vector3Create(-25.0, 0.5, 0.0);
   Tb := Vector3Create( -4.0, 2.5, 1.0);
   Tc := Vector3Create( -8.0, 6.5, 0.0);

   Bary := Vector3Create(0.0, 0.0, 0.0);

   // Test sphere
   Sp := Vector3Create(-30.0, 5.0, 5.0);
   Sr := 4.0;

   DisableCursor;

   SetTargetFPS(60); // Set our game to run at 60 frames-per-second

  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_FREE);

      // Display information about closest hit
      Collision := Default(TRayCollision);
      HitObjectName := 'None';
      Collision.Distance := SINGLE_MAX;
      Collision.Hit := False;
      CursorColor := WHITE;

      // Get ray and test against objects
      Ray := GetMouseRay(GetMousePosition(), Camera);

      // Check ray collision against ground quad
      GroundHitInfo := GetRayCollisionQuad(Ray, G0, G1, G2, g3);

      if (GroundHitInfo.Hit) and (GroundHitInfo.Distance < Collision.Distance) then
      begin
        Collision := GroundHitInfo;
        CursorColor := GREEN;
        HitObjectName := 'Ground';
      end;

      // Check ray collision against test triangle
      TriHitInfo := GetRayCollisionTriangle(Ray, Ta, Tb, Tc);

      if (TriHitInfo.Hit) and (TriHitInfo.Distance < Collision.Distance) then
      begin
        Collision := TriHitInfo;
        CursorColor := PURPLE;
        HitObjectName := 'Triangle';

        Bary := Vector3Barycenter(Collision.Point, Ta, Tb, Tc);
      end;

      // Check ray collision against test sphere
      SphereHitInfo := GetRayCollisionSphere(Ray, Sp, Sr);

      if (SphereHitInfo.Hit) and (SphereHitInfo.Distance < Collision.Distance) then
      begin
        Collision := SphereHitInfo;
        CursorColor := ORANGE;
        HitObjectName := 'Sphere';
      end;

      // Check ray collision against bounding box first, before trying the full ray-mesh test
      BoxHitInfo := GetRayCollisionBox(Ray, TowerBBox);

      if (BoxHitInfo.Hit) and (BoxHitInfo.Distance < Collision.Distance) then
      begin
        Collision := BoxHitInfo;
        CursorColor := ORANGE;
        HitObjectName := 'Box';

        // Check ray collision against model meshes
        MeshHitInfo := Default(TRayCollision);
        for M := 0 to Tower.MeshCount - 1 do
        begin
          // NOTE: We consider the model.transform for the collision check but
          // it can be checked against any transform Matrix, used when checking against same
          // model drawn multiple times with multiple transforms
          MeshHitInfo := GetRayCollisionMesh(Ray, Tower.Meshes[M], Tower.Transform);
          if MeshHitInfo.Hit then
          begin
            // Save the closest hit mesh
            if (not Collision.Hit) or (Collision.Distance > MeshHitInfo.Distance) then
              Collision := MeshHitInfo;

            break; // Stop once one mesh collision is detected, the colliding mesh is m
          end;
        end;

        if MeshHitInfo.Hit then
        begin
          Collision := MeshHitInfo;
          CursorColor := ORANGE;
          HitObjectName := 'Mesh';
        end;
      end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);
      // Draw the tower
      // WARNING: If scale is different than 1.0f,
      // not considered by GetRayCollisionModel()
      DrawModel(Tower, TowerPos, 1.0, WHITE);

      // Draw the test triangle
      DrawLine3D(Ta, Tb, PURPLE);
      DrawLine3D(tb, Tc, PURPLE);
      DrawLine3D(Tc, Ta, PURPLE);

      // Draw the test sphere
      DrawSphereWires(Sp, Sr, 8, 8, PURPLE);

      // Draw the mesh bbox if we hit it
      if BoxHitInfo.Hit then
        DrawBoundingBox(TowerBBox, LIME);

      // If we hit something, draw the cursor at the hit point
      if Collision.hit then
      begin
        DrawCube(Collision.Point, 0.3, 0.3, 0.3, CursorColor);
        DrawCubeWires(Collision.Point, 0.3, 0.3, 0.3, RED);

        NormalEnd.X := Collision.Point.X + Collision.Normal.X;
        NormalEnd.Y := Collision.Point.Y + Collision.Normal.Y;
        NormalEnd.Z := Collision.Point.Z + Collision.Normal.Z;

        DrawLine3D(Collision.Point, NormalEnd, RED);
      end;

      DrawRay(Ray, MAROON);

      DrawGrid(10, 10.0);

    EndMode3D();

    // Draw some debug GUI text
    DrawText(TextFormat('Hit Object: %s', PAnsiChar(HitObjectName)), 10, 50, 10, BLACK);

    if Collision.Hit then
    begin
      YPos := 70;

      DrawText(TextFormat('Distance: %3.2f', Collision.Distance), 10, YPos, 10, BLACK);

      DrawText(TextFormat('Hit Pos: %3.2f %3.2f %3.2f',
                          Collision.Point.X,
                          Collision.Point.Y,
                          Collision.Point.Z), 10, YPos + 15, 10, BLACK);

      DrawText(TextFormat('Hit Norm: %3.2f %3.2f %3.2f',
                          Collision.Normal.X,
                          Collision.Normal.Y,
                          Collision.Normal.Z), 10, YPos + 30, 10, BLACK);

      if TriHitInfo.Hit and TextIsEqual(PChar(HitObjectName), 'Triangle') then
          DrawText(TextFormat('Barycenter: %3.2f %3.2f %3.2f', Bary.X, Bary.Y, Bary.Z), 10, YPos + 45, 10, BLACK);
    end;

    DrawText('Use Mouse to Move Camera', 10, 430, 10, GRAY);

    DrawText('(c) Turret 3D model by Alberto Cano', ScreenWidth - 200, ScreenHeight - 20, 10, GRAY);

    DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadModel(Tower);         // Unload model
  UnloadTexture(Texture);     // Unload texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

