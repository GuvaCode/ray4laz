program models_first_person_maze;

{$MODE objfpc}{$H+}

uses cmem, raylib, raymath, math;

const
  screenWidth = 800;
  screenHeight = 450;

var
  camera: TCamera;
  imMap: TImage;
  cubicmap, texture: TTexture2D;
  mesh: TMesh;
  model: TModel;
  mapPixels: PColor;
  mapPosition, oldCamPos: TVector3;
  playerPos: TVector2;
  playerRadius: single;
  playerCellX,playerCellY,y,x:integer;

begin
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - first person maze');

      // Define the camera to look into our 3d world
      Camera3DSet(@Camera,Vector3Create(0.2,0.4,0.2),
                  Vector3Create(0.0,0.0,0.0),
                  Vector3Create(0.0,1.0,0.0),45.0,0);


      imMap := LoadImage(PChar(GetApplicationDirectory + 'resources/cubicmap.png'));      // Load cubicmap image (RAM)
      cubicmap := LoadTextureFromImage(imMap);       // Convert image to texture to display (VRAM)
      mesh := GenMeshCubicmap(imMap,Vector3Create(1.0,1.0,1.0));
      model := LoadModelFromMesh(mesh);

      // NOTE: By default each cube is mapped to one part of texture atlas
      texture := LoadTexture(PChar(GetApplicationDirectory +'resources/cubicmap_atlas.png'));    // Load map texture
      model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture;             // Set map diffuse texture

      // Get map image data to be used for collision detection
      mapPixels := LoadImageColors(imMap);
      UnloadImage(imMap);             // Unload image from RAM

      Vector3Set(@mapPosition,-16.0,0.0,-8.0);  // Set model position


      SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
      DisableCursor;
      //--------------------------------------------------------------------------------------

  //--------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do   // Detect window close button or ESC key
  begin
      // update
      oldCamPos := camera.position;    // Store old camera position

        UpdateCamera(@camera,CAMERA_FIRST_PERSON);      // Update camera

        // Check player collision (we simplify to 2D collision detection)
        Vector2Set(@playerPos,camera.position.x,camera.position.z);

        playerRadius := 0.1;  // Collision radius (player is modelled as a cilinder for collision)

         playerCellX := round(playerPos.x - mapPosition.x + 0.5);
         playerCellY := round(playerPos.y - mapPosition.z + 0.5);

        // Out-of-limits security check
        if playerCellX < 0 then playerCellX := 0
        else
        if playerCellX >= cubicmap.width then playerCellX := cubicmap.width - 1;

        if playerCellY < 0 then playerCellY := 0
        else
        if playerCellY >= cubicmap.height then playerCellY := cubicmap.height - 1;

        // Check map collisions using image data and player position
        // TODO: Improvement: Just check player surrounding cells for collision
        for y:=0 to cubicmap.height do
        begin
            for x:=0 to  cubicmap.width do
            begin
                  if (mapPixels[y*cubicmap.width + x].r = 255) and  // Collision: white pixel, only check R channel
                   CheckCollisionCircleRec(playerPos, playerRadius,RectangleCreate(mapPosition.x - 0.5 + x*1.0, mapPosition.z - 0.5 + y*1.0, 1.0, 1.0))
                then
                begin
                    // Collision detected, reset camera position
                    camera.position := oldCamPos;
                end;
            end;
        end;
        //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();


                  ClearBackground(RAYWHITE);

                  BeginMode3D(camera);
                      DrawModel(model, mapPosition, 1.0, WHITE);                     // Draw maze map
                  EndMode3D();

                  DrawTextureEx(cubicmap,Vector2Create( GetScreenWidth() - cubicmap.width*4.0 - 20, 20.0),0.0,4.0,White);
                  //(Vector2){ GetScreenWidth() - cubicmap.width*4.0f - 20, 20.0f }, 0.0f, 4.0f, WHITE);
                  DrawRectangleLines(GetScreenWidth() - cubicmap.width*4 - 20, 20, cubicmap.width*4, cubicmap.height*4, GREEN);

                  // Draw player position radar
                  DrawRectangle(GetScreenWidth() - cubicmap.width*4 - 20 + playerCellX*4, 20 + playerCellY*4, 4, 4, RED);

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
