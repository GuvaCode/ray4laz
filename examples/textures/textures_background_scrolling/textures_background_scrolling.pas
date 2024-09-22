program textures_background_scrolling;

{$MODE objfpc}{$H+}

uses  cmem, raylib, math;

const screenWidth = 800;
      screenHeight = 450;

var
     background:TTexture2D;
     midground:TTexture2D;
     foreground:TTexture2D;
     scrollingBack:float;
     scrollingMid:float;
     scrollingFore:float;
     music:TMusic;
begin
    InitWindow(screenWidth, screenHeight, 'raylib [textures] example - background scrolling');
    InitAudioDevice();
    // NOTE: Be careful, background width must be equal or bigger than screen width
    // if not, texture should be draw more than two times for scrolling effect
    background := LoadTexture(PChar(GetApplicationDirectory + 'resources/cyberpunk_street_background.png'));
    midground := LoadTexture(PChar(GetApplicationDirectory + 'resources/cyberpunk_street_midground.png'));
    foreground := LoadTexture(PChar(GetApplicationDirectory + 'resources/cyberpunk_street_foreground.png'));
    music := LoadMusicStream(PChar(GetApplicationDirectory + 'resources/mini1111.xm'));
    music.looping:=true;
    PlayMusicStream(music);

    scrollingBack := 0.0;
    scrollingMid := 0.0;
    scrollingFore := 0.0;

    SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

    // Main game loop
   while not WindowShouldClose() do
    begin
     UpdateMusicStream(music);      // Update music buffer with new stream data
     scrollingBack :=scrollingBack- 0.1;
     scrollingMid :=scrollingMid- 0.5;
     scrollingFore :=scrollingFore- 1.0;

        // NOTE: Texture is scaled twice its size, so it sould be considered on scrolling
        if (scrollingBack <= -background.width*2) then scrollingBack := 0;
        if (scrollingMid <= -midground.width*2) then scrollingMid := 0;
        if (scrollingFore <= -foreground.width*2) then scrollingFore := 0;

        BeginDrawing();
             ClearBackground(GetColor($052c46ff));
            // Draw background image twice
            // NOTE: Texture is scaled twice its size
            DrawTextureEx(background,Vector2Create(scrollingBack,20),0.0,2.0,WHITE);
            DrawTextureEx(background, Vector2Create(background.width*2 + scrollingBack, 20 ), 0.0, 2.0, WHITE);
            // Draw midground image twice
            DrawTextureEx(midground, Vector2Create( scrollingMid, 20 ), 0.0, 2.0, WHITE);
            DrawTextureEx(midground, Vector2Create( midground.width*2 + scrollingMid, 20 ), 0.0, 2.0, WHITE);
            // Draw foreground image twice
            DrawTextureEx(foreground, Vector2Create( scrollingFore, 70 ), 0.0, 2.0, WHITE);
            DrawTextureEx(foreground, Vector2Create( foreground.width*2 + scrollingFore, 70 ), 0.0, 2.0, WHITE);
            DrawText('BACKGROUND SCROLLING & PARALLAX', 10, 10, 20, RED);
            DrawText('(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)', screenWidth - 330, screenHeight - 20, 10, RAYWHITE);
        EndDrawing();
        //----------------------------------------------------------------------------------
    end;
    UnloadMusicStream(music);          // Unload music stream buffers from RAM
    UnloadTexture(background);  // Unload background texture
    UnloadTexture(midground);   // Unload midground texture
    UnloadTexture(foreground);  // Unload foreground texture
    CloseWindow();              // Close window and OpenGL context
end.

