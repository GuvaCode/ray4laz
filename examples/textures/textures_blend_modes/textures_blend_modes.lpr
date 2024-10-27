program textures_blend_modes;

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
  blendCountMax = 6;

var
  BgImage: TImage;
  BgTexture: TTexture;
  FgImage: TImage;
  FgTexture: TTexture;
  BlendMode: TBlendMode;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - blend modess');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  BgImage := LoadImage(PChar(GetApplicationDirectory + 'resources/cyberpunk_street_background.png')); // Loaded in CPU memory (RAM)
  BgTexture := LoadTextureFromImage(BgImage); // Image converted to texture, GPU memory (VRAM)

  FgImage := LoadImage(PChar(GetApplicationDirectory + 'resources/cyberpunk_street_foreground.png')); // Loaded in CPU memory (RAM)
  FgTexture := LoadTextureFromImage(FgImage); // Image converted to texture, GPU memory (VRAM)

  // Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM
  UnloadImage(BgImage);
  UnloadImage(FgImage);

  BlendMode := 0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      while not WindowShouldClose() do // Detect window close button or ESC key
      begin
        // Update
        //-------------------------------------------------------------------------------------------
        if IsKeyPressed(KEY_SPACE) then
        begin
          if BlendMode >= (BlendCountMax - 1) then
            BlendMode := 0
          else
            Inc(BlendMode);
        end;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawTexture(BgTexture, ScreenWidth div 2 - BgTexture.Width div 2, ScreenHeight div 2 - BgTexture.Height div 2, WHITE);

      // Apply the blend mode and then draw the foreground texture
      BeginBlendMode(BlendMode);
        DrawTexture(FgTexture, ScreenWidth div 2 - FgTexture.Width div 2, ScreenHeight div 2 - FgTexture.Height div 2, WHITE);
      EndBlendMode();

      // Draw the texts
      DrawText('Press SPACE to change blend modes.', 310, 350, 10, GRAY);

      case BlendMode of
        BLEND_ALPHA: DrawText('Current: BLEND_ALPHA', (ScreenWidth div 2) - 60, 370, 10, GRAY);
        BLEND_ADDITIVE: DrawText('Current: BLEND_ADDITIVE', (ScreenWidth div 2) - 60, 370, 10, GRAY);
        BLEND_MULTIPLIED: DrawText('Current: BLEND_MULTIPLIED', (ScreenWidth div 2) - 60, 370, 10, GRAY);
        BLEND_ADD_COLORS: DrawText('Current: BLEND_ADD_COLORS', (ScreenWidth div 2) - 60, 370, 10, GRAY);
        BLEND_SUBTRACT_COLORS: DrawText('Current: BLEND_SUBTRACT_COLORS', (ScreenWidth div 2) - 60, 370, 10, GRAY);
        BLEND_ALPHA_PREMULTIPLY: DrawText('Current: BLEND_ALPHA_PREMULTIPLY', (ScreenWidth div 2) - 60, 370, 10, GRAY);
      end;

      DrawText('(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)', ScreenWidth - 330, ScreenHeight - 20, 10, GRAY);

      EndDrawing();


    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(FgTexture); // Unload foreground texture
  UnloadTexture(BgTexture); // Unload background texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
    end;
    end.

