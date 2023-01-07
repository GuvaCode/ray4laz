program shaders_palette_switch;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

  MAX_PALETTES = 3;
  COLORS_PER_PALETTE = 8;
  VALUES_PER_COLOR = 3;

  Palettes: array [0..MAX_PALETTES-1] of array [0..VALUES_PER_COLOR*COLORS_PER_PALETTE-1] of Integer = (
    ( // 3-BIT RGB
      0, 0, 0,
      255, 0, 0,
      0, 255, 0,
      0, 0, 255,
      0, 255, 255,
      255, 0, 255,
      255, 255, 0,
      255, 255, 255
    ),
    ( // AMMO-8 (GameBoy-like)
      4, 12, 6,
      17, 35, 24,
      30, 58, 41,
      48, 93, 66,
      77, 128, 97,
      137, 162, 87,
      190, 220, 127,
      238, 255, 204
    ),
    (  // RKBV (2-strip film)
      21, 25, 26,
      138, 76, 88,
      217, 98, 117,
      230, 184, 193,
      69, 107, 115,
      75, 151, 166,
      165, 189, 194,
      255, 245, 247
    )
  );

  PaletteText: array [0..MAX_PALETTES-1] of string = (
    '3-BIT RGB',
    'AMMO-8 (GameBoy-like)',
    'RKBV (2-strip film)'
  );

var
  Shader: TShader;
  I: Integer;
  PaletteLoc, CurrentPalette, LineHeight: Integer;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - color palette switch');
  // Load shader to be used on some parts drawing
  // NOTE 1: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
  // NOTE 2: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  Shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/palette_switch.fs', Integer(GLSL_VERSION)));

  // Get variable (uniform) location on the shader to connect with the program
  // NOTE: If uniform variable could not be found in the shader, function returns -1
  PaletteLoc := GetShaderLocation(Shader, 'palette');

  CurrentPalette := 0;
  LineHeight := ScreenHeight div COLORS_PER_PALETTE;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      if IsKeyPressed(KEY_RIGHT) then
        Inc(CurrentPalette)
      else if IsKeyPressed(KEY_LEFT) then
        Dec(CurrentPalette);

      if CurrentPalette >= MAX_PALETTES then
        CurrentPalette := 0
      else if CurrentPalette < 0 then
        CurrentPalette := MAX_PALETTES - 1;

      // Send new value to the shader to be used on drawing.
      // NOTE: We are sending RGB triplets w/o the alpha channel
      SetShaderValueV(Shader, PaletteLoc, @Palettes[CurrentPalette], SHADER_UNIFORM_IVEC3, COLORS_PER_PALETTE);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginShaderMode(Shader);

        for I := 0 to COLORS_PER_PALETTE - 1 do
        begin
          // Draw horizontal screen-wide rectangles with increasing "palette index"
          // The used palette index is encoded in the RGB components of the pixel
          DrawRectangle(0, LineHeight * I, GetScreenWidth(), LineHeight, ColorCreate(I, I, I, 255));
        end;

      EndShaderMode();

      DrawText('< >', 10, 10, 30, DARKBLUE);
      DrawText('CURRENT PALETTE:', 60, 15, 20, RAYWHITE);
      DrawText(PChar(PaletteText[CurrentPalette]), 300, 15, 20, RED);

      DrawFPS(700, 15);

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(Shader);       // Unload shader
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

