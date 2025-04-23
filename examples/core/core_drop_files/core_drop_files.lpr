program core_drop_files;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;

var
  DroppedFiles: TFilePathList;
  I: Integer;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - drop files');
  DroppedFiles := Default(TFilePathList);
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
       if IsFileDropped then
       begin
         // Is some files have been previously loaded, unload them
         //if (droppedFiles.count > 0) then
         //UnloadDroppedFiles(droppedFiles); ///я хз
         // Load new dropped files
         droppedFiles := LoadDroppedFiles();
       end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      if droppedFiles.count = 0 then
      DrawText('Drop your files to this window!', 100, 40, 20, DARKGRAY)
      else
      DrawText('Dropped files:', 100, 40, 20, DARKGRAY);

      for i:=0 to droppedFiles.count -1 do
      begin
          if (i mod 2 = 0) then DrawRectangle(0, 85 + 40 * i, screenWidth, 40, Fade(LIGHTGRAY, 0.5))
          else
          DrawRectangle(0, 85 + 40 * i, screenWidth, 40, Fade(LIGHTGRAY, 0.3));

          DrawText(droppedFiles.paths[i], 120, 100 +  40 * i, 10, GRAY);
        ///  writeln(droppedFiles.paths[i]);
        end;

      DrawText('Drop new files...', 100, 110 + 40 * droppedFiles.count, 20, DARKGRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

