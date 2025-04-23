program text_font_spritefont;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  msg1: string = 'THIS IS A custom SPRITE FONT...';
  msg2: string = '...and this is ANOTHER CUSTOM font...';
  msg3: string = '...and a THIRD one! GREAT! :D';

var
  Font1, Font2, Font3: TFont;
  FontPosition1, FontPosition2, FontPosition3: TVector2;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - sprite font loading');

  // NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)
  Font1 := LoadFont(PChar(GetApplicationDirectory + 'resources/custom_mecha.png'));          // Font loading
  Font2 := LoadFont(PChar(GetApplicationDirectory + 'resources/custom_alagard.png'));        // Font loading
  Font3 := LoadFont(PChar(GetApplicationDirectory + 'resources/custom_jupiter_crash.png'));  // Font loading

  FontPosition1 := Vector2Create(ScreenWidth / 2.0 - MeasureTextEx(Font1, PChar(Msg1), Font1.BaseSize, -3).X / 2,
                                 ScreenHeight / 2.0 - Font1.BaseSize / 2.0 - 80.0);

  FontPosition2 := Vector2Create(ScreenWidth / 2.0 - MeasureTextEx(Font2, PChar(Msg2), Font2.BaseSize, -2.0).X / 2,
                                 ScreenHeight / 2.0 - Font2.BaseSize / 2.0 - 10.0);

  FontPosition3 := Vector2Create(ScreenWidth / 2.0 - MeasureTextEx(Font3, PChar(Msg3), Font3.BaseSize, 2).X / 2,
                                 ScreenHeight / 2.0 - Font1.BaseSize / 2.0 + 50.0);

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
        DrawTextEx(Font1, PChar(Msg1), FontPosition1, Font1.BaseSize, -3, WHITE);
        DrawTextEx(Font2, PChar(Msg2), FontPosition2, Font2.BaseSize, -2, WHITE);
        DrawTextEx(Font3, PChar(Msg3), FontPosition3, Font3.BaseSize, 2, WHITE);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

