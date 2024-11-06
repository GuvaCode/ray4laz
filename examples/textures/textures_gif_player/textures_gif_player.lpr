program textures_gif_player;

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
  MAX_FRAME_DELAY = 20;
  MIN_FRAME_DELAY = 1;
var
  AnimFrames: Integer;
  ImScarfyAnim: TImage;
  TexScarfyAnim: TTexture;
  NextFrameDataOffset: Cardinal;
  CurrentAnimFrame: Integer;
  FrameDelay: Integer;
  FrameCounter: Integer;
  I: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - gif playing');

  AnimFrames := 0;

  // Load all GIF animation frames into a single Image
  // NOTE: GIF data is always loaded as RGBA (32bit) by default
  // NOTE: Frames are just appended one after another in image.data memory
  ImScarfyAnim := LoadImageAnim(PChar(GetApplicationDirectory + 'resources/scarfy_run.gif'), @AnimFrames);

  // Load texture from image
  // NOTE: We will update this texture when required with next frame data
  // WARNING: It's not recommended to use this technique for sprites animation,
  // use spritesheets instead, like illustrated in textures_sprite_anim example
  TexScarfyAnim := LoadTextureFromImage(ImScarfyAnim);

  NextFrameDataOffset := 0;    // Current byte offset to next frame in image.data

  CurrentAnimFrame := 0;       // Current animation frame to load and draw
  FrameDelay := 8;             // Frame delay to switch between animation frames
  FrameCounter := 0;           // General frames counter

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      Inc(FrameCounter);
      if FrameCounter >= FrameDelay then
      begin
        // Move to next frame
        // NOTE: If final frame is reached we return to first frame
        Inc(CurrentAnimFrame);
        if CurrentAnimFrame >= AnimFrames then
          CurrentAnimFrame := 0;

        // Get memory offset position for next frame data in image.data
        NextFrameDataOffset := ImScarfyAnim.Width * ImScarfyAnim.Height * 4 * CurrentAnimFrame;

        // Update GPU texture data with next frame image data
        // WARNING: Data size (frame size) and pixel format must match already created texture
        UpdateTexture(TexScarfyAnim, PByte(imScarfyAnim.data) + NextFrameDataOffset);

        FrameCounter := 0;
        // Control frames delay


        if FrameDelay > MAX_FRAME_DELAY then FrameDelay := MAX_FRAME_DELAY
        else if FrameDelay < MIN_FRAME_DELAY then FrameDelay := MIN_FRAME_DELAY;
      //----------------------------------------------------------------------------------
      end;

       if IsKeyPressed(KEY_RIGHT) then Inc(FrameDelay)
       else if IsKeyPressed(KEY_LEFT) then Dec(FrameDelay);

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      DrawText(TextFormat('TOTAL GIF FRAMES:  %02i', AnimFrames), 50, 30, 20, LIGHTGRAY);
      DrawText(TextFormat('CURRENT FRAME: %02i', CurrentAnimFrame), 50, 60, 20, GRAY);
      DrawText(TextFormat('CURRENT FRAME IMAGE.DATA OFFSET: %02i', NextFrameDataOffset), 50, 90, 20, GRAY);

      DrawText('FRAMES DELAY: ', 100, 305, 10, DARKGRAY);
      DrawText(TextFormat('%02i frames', FrameDelay), 620, 305, 10, DARKGRAY);
      DrawText('PRESS RIGHT/LEFT KEYS to CHANGE SPEED!', 290, 350, 10, DARKGRAY);

      for I := 0 to MAX_FRAME_DELAY - 1 do
      begin
        if I < FrameDelay then DrawRectangle(190 + 21 * I, 300, 20, 20, RED);
        DrawRectangleLines(190 + 21 * I, 300, 20, 20, MAROON);
      end;

      DrawTexture(TexScarfyAnim, GetScreenWidth() div 2 - TexScarfyAnim.Width div 2, 140, WHITE);

      DrawText('(c) Scarfy sprite by Eiden Marsal', ScreenWidth - 200, ScreenHeight - 20, 10, GRAY);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(TexScarfyAnim);   // Unload texture
  UnloadImage(ImScarfyAnim);      // Unload image (contains all frames)
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

