program textures_logo_raylib;

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
var
  Texture: TTexture2D;
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'aylib [textures] example - texture loading and drawing');

    // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  Texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/raylib_logo.png')); // Texture loading

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawTexture(Texture, ScreenWidth div 2 - Texture.Width div 2, ScreenHeight div 2 - Texture.Height div 2, WHITE);
        DrawText('this IS a texture!', 360, 370, 10, GRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(Texture); // Texture unloading
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

