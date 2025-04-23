program models_mesh_generation;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  NUM_MODELS = 9; // Parametric 3d shapes to generate

var
  Camera: TCamera;
  Checked: TImage;
  Texture: TTexture2D;
  Models: array [0..NUM_MODELS-1] of TModel;
  I: Integer;
  Position: TVector3;
  CurrentModel: Integer;

  function GenMeshCustom(): TMesh; // Generate a simple triangle mesh from code
  begin
    Result := Default(TMesh);
    Result.TriangleCount := 1;
    Result.VertexCount := Result.TriangleCount * 3;
    Result.Vertices := MemAlloc(Result.VertexCount * 3 * SizeOf(Single));    // 3 vertices, 3 coordinates each (x, y, z)
    Result.Texcoords := MemAlloc(Result.VertexCount * 2 * SizeOf(Single));   // 3 vertices, 2 coordinates each (x, y)
    Result.Normals := MemAlloc(Result.VertexCount * 3 * SizeOf(Single));     // 3 vertices, 3 coordinates each (x, y, z)

    // Vertex at (0, 0, 0)
    Result.Vertices[0] := 0;
    Result.Vertices[1] := 0;
    Result.Vertices[2] := 0;
    Result.Normals[0] := 0;
    Result.Normals[1] := 1;
    Result.Normals[2] := 0;
    Result.Texcoords[0] := 0;
    Result.Texcoords[1] := 0;

    // Vertex at (1, 0, 2)
    Result.Vertices[3] := 1;
    Result.Vertices[4] := 0;
    Result.Vertices[5] := 2;
    Result.Normals[3] := 0;
    Result.Normals[4] := 1;
    Result.Normals[5] := 0;
    Result.Texcoords[2] := 0.5;
    Result.Texcoords[3] := 1.0;

    // Vertex at (2, 0, 0)
    Result.Vertices[6] := 2;
    Result.Vertices[7] := 0;
    Result.Vertices[8] := 0;
    Result.Normals[6] := 0;
    Result.Normals[7] := 1;
    Result.Normals[8] := 0;
    Result.Texcoords[4] := 1;
    Result.Texcoords[5] := 0;

    // Upload mesh data from CPU (RAM) to GPU (VRAM) memory
    UploadMesh(@Result, False);
  end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [models] example - mesh generation');

  // We generate a checked image for texturing
  Checked := GenImageChecked(2, 2, 1, 1, RED, GREEN);
  Texture := LoadTextureFromImage(Checked);
  UnloadImage(Checked);

  Models[0] := LoadModelFromMesh(GenMeshPlane(2, 2, 5, 5));
  Models[1] := LoadModelFromMesh(GenMeshCube(2.0, 1.0, 2.0));
  Models[2] := LoadModelFromMesh(GenMeshSphere(2, 32, 32));
  Models[3] := LoadModelFromMesh(GenMeshHemiSphere(2, 16, 16));
  Models[4] := LoadModelFromMesh(GenMeshCylinder(1, 2, 16));
  Models[5] := LoadModelFromMesh(GenMeshTorus(0.25, 4.0, 16, 32));
  Models[6] := LoadModelFromMesh(GenMeshKnot(1.0, 2.0, 16, 128));
  Models[7] := LoadModelFromMesh(GenMeshPoly(5, 2.0));
  Models[8] := LoadModelFromMesh(GenMeshCustom());

  // Set checked texture as default diffuse component for all models material
  for I := 0 to NUM_MODELS - 1 do
    Models[I].Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture;

  // Define the camera to look into our 3d world
  Camera := Camera3DCreate(
    Vector3Create(5.0, 5.0, 5.0),
    Vector3Create(0.0, 0.0, 0.0),
    Vector3Create(0.0, 1.0, 0.0),
    45.0,
    0);

  // Model drawing position
  Position := Vector3Create(0, 0, 0);

  CurrentModel := 0;



  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_ORBITAL);

      if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
        CurrentModel := (CurrentModel + 1) mod NUM_MODELS; // Cycle between the textures

      if IsKeyPressed(KEY_RIGHT) then
      begin
        Inc(CurrentModel);
        if CurrentModel >= NUM_MODELS then
          CurrentModel := 0;
      end
      else if IsKeyPressed(KEY_LEFT) then
      begin
        Dec(CurrentModel);
        if CurrentModel < 0 then
          CurrentModel := NUM_MODELS - 1;
      end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);

       DrawModel(Models[CurrentModel], Position, 1.0, WHITE);
       DrawGrid(10, 1.0);

      EndMode3D();

      DrawRectangle(30, 400, 310, 30, Fade(SKYBLUE, 0.5));
      DrawRectangleLines(30, 400, 310, 30, Fade(DARKBLUE, 0.5));
      DrawText('MOUSE LEFT BUTTON to CYCLE PROCEDURAL MODELS', 40, 410, 10, BLUE);

      case CurrentModel of
        0: DrawText('PLANE', 680, 10, 20, DARKBLUE);
        1: DrawText('CUBE', 680, 10, 20, DARKBLUE);
        2: DrawText('SPHERE', 680, 10, 20, DARKBLUE);
        3: DrawText('HEMISPHERE', 640, 10, 20, DARKBLUE);
        4: DrawText('CYLINDER', 680, 10, 20, DARKBLUE);
        5: DrawText('TORUS', 680, 10, 20, DARKBLUE);
        6: DrawText('KNOT', 680, 10, 20, DARKBLUE);
        7: DrawText('POLY', 680, 10, 20, DARKBLUE);
        8: DrawText('Custom (triangle)', 580, 10, 20, DARKBLUE);
      end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Unload texture

  // Unload models data (GPU VRAM)
  for I := 0 to NUM_MODELS - 1 do
    UnloadModel(Models[I]);

  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

