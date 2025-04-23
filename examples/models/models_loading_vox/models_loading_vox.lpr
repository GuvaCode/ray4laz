program models_loading_vox;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
raymath,
//rlgl,
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_VOX_FILES = 3;

var
  voxFileNames: array [0..MAX_VOX_FILES] of String;
  models: array[0..MAX_VOX_FILES] of TModel;
  bb: TBoundingBox;
  camera: TCamera;
  i,currentModel: integer;
  t0,t1: double;
  center: TVector3;
  matTranslate:TMatrix;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - magicavoxel loading');

  voxFileNames[0]:='resources/models/vox/chr_knight.vox';
  voxFileNames[1]:='resources/models/vox/chr_sword.vox';
  voxFileNames[2]:='resources/models/vox/monu9.vox';

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 10.0, 10.0, 10.0 ); // Camera position
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera mode type

  // Load MagicaVoxel files

  for i:= 0 to MAX_VOX_FILES do
	begin
      // Load VOX file and measure time
	  t0 := GetTime()*1000.0;
	  models[i] := LoadModel(PChar(voxFileNames[i]));
	  t1 := GetTime()*1000.0;

                TraceLog(LOG_WARNING, TextFormat('[%s] File loaded in %.3f ms', PChar(voxFileNames[i]), t1 - t0));

	        // Compute model translation matrix to center model on draw position (0, 0 , 0)
		bb := GetModelBoundingBox(models[i]);
		center.x:= bb.min.x  + (((bb.max.x - bb.min.x)/2));
		center.z:= bb.min.z  + (((bb.max.z - bb.min.z)/2));
	        matTranslate := MatrixTranslate(-center.x, 0, -center.z);
		models[i].transform := matTranslate;
	end;

       currentModel := 0;
       DisableCursor;
       SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_ORBITAL);      // Update our camera to orbit
      // Cycle between models on mouse click
      if IsMouseButtonPressed(MOUSE_BUTTON_LEFT) then currentModel := (currentModel + 1) mod MAX_VOX_FILES;

        // Cycle between models on key pressed
		if IsKeyPressed(KEY_RIGHT) then
		begin
                        inc(currentModel);
			if currentModel >= MAX_VOX_FILES then currentModel := 0;
		end
		else if IsKeyPressed(KEY_LEFT) then
		begin
                        Dec(currentModel);
			if (currentModel < 0) then currentModel := MAX_VOX_FILES ;
		end;

      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

                // Draw 3D model
                BeginMode3D(camera);

                    DrawModel(models[currentModel],Vector3Create(0,0,0)
                    , 1.0, WHITE);
                    DrawGrid(10, 1.0);

                EndMode3D();

                // Display info
                DrawRectangle(10, 400, 310, 30, Fade(SKYBLUE, 0.5));
                DrawRectangleLines(10, 400, 310, 30, Fade(DARKBLUE, 0.5));
                DrawText('MOUSE LEFT BUTTON to CYCLE VOX MODELS', 40, 410, 10, BLUE);
                DrawText(TextFormat('File: %s', GetFileName(PChar(voxFileNames[currentModel]))), 10, 10, 20, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------

  // Unload models data (GPU VRAM)
  for i:=0 to MAX_VOX_FILES do UnloadModel(models[i]);//UnloadModel(models[i]);
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

