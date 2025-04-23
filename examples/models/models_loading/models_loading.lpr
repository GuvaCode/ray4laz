program models_loading;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;

var
  Camera: TCamera;
  Model: TModel;
  Texture: TTexture;
  Position: TVector3;
  Bounds: TBoundingBox;
  Selected: Boolean;
  DroppedFiles: TFilePathList;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - models loading');

  Camera := Default(TCamera);
  Camera.Position := Vector3Create(50.0, 50.0, 50.0); // Camera position
  Camera.Target := Vector3Create(0.0, 10.0, 0.0);     // Camera looking at point
  Camera.Up := Vector3Create(0.0, 1.0, 0.0);          // Camera up vector (rotation towards target)
  Camera.Fovy := 45.0;                                  // Camera field-of-view Y
  Camera.Projection := CAMERA_PERSPECTIVE;              // Camera mode type

  Model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/obj/castle.obj'));   // Load model
  Texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/models/obj/castle_diffuse.png')); // Load model texture
  Model.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture;            // Set map diffuse texture

  Position := Vector3Create(0.0, 0.0, 0.0);                    // Set model position
  Bounds := GetMeshBoundingBox(Model.Meshes[0]);   // Set model bounds

  // NOTE: bounds are calculated from the original size of the model,
  // if model is scaled on drawing, bounds must be also scaled

  Selected := False;          // Selected object flag
 // DisableCursor;
  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //-------------------------------------------------------------------------------
      UpdateCamera(@Camera,CAMERA_FREE);

       // Load new models/textures on drag&drop
       if IsFileDropped() then
       begin
         DroppedFiles := LoadDroppedFiles();

         if DroppedFiles.Count = 1 then // Only support one file dropped
         begin
           if IsFileExtension(DroppedFiles.Paths[0], '.obj') or
              IsFileExtension(DroppedFiles.Paths[0], '.gltf') or
              IsFileExtension(DroppedFiles.Paths[0], '.glb') or
              IsFileExtension(DroppedFiles.Paths[0], '.vox') or
              IsFileExtension(DroppedFiles.Paths[0], '.iqm') or
              IsFileExtension(DroppedFiles.Paths[0], '.m3d') then       // Model file formats supported
           begin
             UnloadModel(Model);                          // Unload previous model
             Model := LoadModel(DroppedFiles.Paths[0]);   // Load new model
             Model.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture; // Set current map diffuse texture

             Bounds := GetMeshBoundingBox(Model.Meshes[0]);

             // TODO: Move camera position from target enough distance to visualize model properly
           end
           else if IsFileExtension(DroppedFiles.Paths[0], '.png') then  // Texture file formats supported
           begin
             // Unload current model texture and load new one
             UnloadTexture(Texture);
             Texture := LoadTexture(DroppedFiles.Paths[0]);
             Model.Materials[0].Maps[MATERIAL_MAP_DIFFUSE].Texture := Texture;
           end;
         end;

         UnloadDroppedFiles(droppedFiles);    // Unload filepaths from memory
       end;

       // Select model on mouse click
       if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then
       begin
         // Check collision between ray and box
         if GetRayCollisionBox(GetMouseRay(GetMousePosition(), Camera), Bounds).Hit then
           Selected := not Selected
         else
           Selected := False;
       end;

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(Camera);
        DrawModel(Model, Position, 1.0, WHITE);        // Draw 3d model with texture
        DrawGrid(20, 10.0);         // Draw a grid

        if Selected then
          DrawBoundingBox(Bounds, GREEN);   // Draw selection box
      EndMode3D();

      DrawText('Drag & drop model to load mesh/texture.', 10, GetScreenHeight() - 20, 10, DARKGRAY);
      if Selected then
        DrawText('MODEL SELECTED', GetScreenWidth() - 110, 10, 10, GREEN);

      DrawText('(c) Castle 3D model by Alberto Cano', ScreenWidth - 200, ScreenHeight - 20, 10, GRAY);

      DrawFPS(10, 10);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

