program models_cubicmap;

{$MODE objfpc}{$H+}

uses cmem, raylib, raymath, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  camera: TCamera;
  image: TImage;
  cubicmap, texture: TTexture2D;
  mesh: TMesh;
  model: TModel;
  mapPixels: PColorB;
  mapPosition: TVector3;

begin
      InitWindow(screenWidth, screenHeight, 'raylib [models] example - cubesmap loading and drawing');
  // Define the camera to look into our 3d world
 // camera = { { 16.0f, 14.0f, 16.0f }, { 0.0f, 0.0f, 0.0f }, { 0.0f, 1.0f, 0.0f }, 45.0f, 0 };
  camera:= Camera3DCreate(Vector3Create(16.0,14.0,16.0),
  Vector3Create(0.0,0.0,0.0),
  Vector3Create(0.0,1.0,0.0),45.0,0);

  image := LoadImage(PChar(GetApplicationDirectory + 'resources/cubicmap.png'));      // Load cubicmap image (RAM)

  cubicmap := LoadTextureFromImage(image);       // Convert image to texture to display (VRAM)

  mesh := GenMeshCubicmap(image, Vector3Create(1.0,1.0,1.0));
  model := LoadModelFromMesh(mesh);

  // NOTE: By default each cube is mapped to one part of texture atlas
  texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/cubicmap_atlas.png'));    // Load map texture
  model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;             // Set map diffuse texture

  Vector3Set(@mapPosition,-16.0, 0.0, -8.0);   // Set model position
  //mapPosition = { -16.0f, 0.0f, -8.0f };

     UnloadImage(image);     // Unload cubesmap image from RAM, already uploaded to VRAM



     SetTargetFPS(60);                       // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do   // Detect window close button or ESC key
  begin
      // update
      UpdateCamera(@camera,CAMERA_ORBITAL);              // Update camera

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();

         ClearBackground(RAYWHITE);

            BeginMode3D(camera);

            DrawModel(model, mapPosition, 1.0, WHITE);

            EndMode3D();

            DrawTextureEx(cubicmap,Vector2Create(screenWidth - cubicmap.width*4.0 - 20, 20.0),0.0, 4.0, WHITE);
            DrawRectangleLines(screenWidth - cubicmap.width*4 - 20, 20, cubicmap.width*4, cubicmap.height*4, GREEN);

            DrawText('cubicmap image used to', 658, 90, 10, GRAY);
            DrawText('generate map 3d model', 658, 104, 10, GRAY);

            DrawFPS(10, 10);


      EndDrawing();
      //----------------------------------------------------------------------------------
  end;

  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(cubicmap);    // Unload cubicmap texture
  UnloadTexture(texture);     // Unload map texture
  UnloadModel(model);         // Unload map model

  CloseWindow();              // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.
