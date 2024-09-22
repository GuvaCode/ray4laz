program text_raylib_fonts;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_FONTS = 8;
  Messages: array [0..MAX_FONTS-1] of string = (
    'ALAGARD FONT designed by Hewett Tsoi',
    'PIXELPLAY FONT designed by Aleksander Shevchuk',
    'MECHA FONT designed by Captain Falcon',
    'SETBACK FONT designed by Brian Kent (AEnigma)',
    'ROMULUS FONT designed by Hewett Tsoi',
    'PIXANTIQUA FONT designed by Gerhard Grossmann',
    'ALPHA_BETA FONT designed by Brian Kent (AEnigma)',
    'JUPITER_CRASH FONT designed by Brian Kent (AEnigma)');
  Spacings: array [0..MAX_FONTS-1] of Integer = (2, 4, 8, 4, 3, 4, 4, 1);

var
  Fonts: array [0..MAX_FONTS-1] of TFont;
  Positions: array [0..MAX_FONTS-1] of TVector2;
  Colors: array of TColor;
  I: Integer;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [text] example - raylib fonts');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  Fonts[0] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/alagard.png'));
  Fonts[1] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/pixelplay.png'));
  Fonts[2] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/mecha.png'));
  Fonts[3] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/setback.png'));
  Fonts[4] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/romulus.png'));
  Fonts[5] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/pixantiqua.png'));
  Fonts[6] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/alpha_beta.png'));
  Fonts[7] := LoadFont(PChar(GetApplicationDirectory + 'resources/fonts/jupiter_crash.png'));

  for I := 0 to MAX_FONTS - 1 do
  begin
    Positions[I].X := ScreenWidth / 2.0 - MeasureTextEx(Fonts[I], PChar(Messages[I]), Fonts[I].BaseSize * 2.0, Spacings[I]).X / 2.0;
    Positions[I].Y := 60.0 + Fonts[I].BaseSize + 45.0 * I;
  end;

  // Small Y position corrections
  Positions[3].Y := Positions[3].Y + 8;
  Positions[4].Y := Positions[4].Y + 2;
  Positions[7].Y := Positions[7].Y - 8;

  Colors := [MAROON, ORANGE, DARKGREEN, DARKBLUE, DARKPURPLE, LIME, GOLD, RED];

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
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

       DrawText(UTF8String('free fonts included with raylib'), 250, 20, 20, DARKGRAY);
       DrawLine(220, 50, 590, 50, DARKGRAY);

       for I := 0 to MAX_FONTS - 1 do
         DrawTextEx(Fonts[I], PAnsiChar(UTF8String(Messages[I])), Positions[I], fonts[I].BaseSize * 2.0, Spacings[I], colors[I]);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
    // Fonts unloading
  for I := 0 to MAX_FONTS - 1 do
    UnloadFont(Fonts[I]);

  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

