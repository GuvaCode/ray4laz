program shaders_julia_set;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath;

const
  screenWidth = 800;
  screenHeight = 450;

const
  GLSL_VERSION = 330;

  // A few good julia sets
  PointsOfInterest: array [0..5,0..1] of Single = (
    ( -0.348827, 0.607167 ),
    ( -0.786268, 0.169728 ),
    ( -0.8, 0.156 ),
    ( 0.285, 0.0 ),
    ( -0.835, -0.2321 ),
    ( -0.70176, -0.3842 )
  );

var
  Shader: TShader;
  Target: TRenderTexture2D;
  C, Offset, ScreenDims: array [0..1] of Single;
  Zoom: Single;
  OffsetSpeed: TVector2;
  CLoc, ZoomLoc, OffsetLoc, IncrementSpeed: Integer;
  ShowControls, Pause: Boolean;
  MousePos: TVector2;
  Amount: Single;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - julia sets');

  // Load julia set shader
  // NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
  Shader := LoadShader(nil, TextFormat('resources/shaders/glsl%i/julia_set.fs', Integer(GLSL_VERSION)));

  // Create a RenderTexture2D to be used for render to texture
  Target := LoadRenderTexture(GetScreenWidth(), GetScreenHeight());

  // c constant to use in z^2 + c
  C[0] := PointsOfInterest[0][0]; C[1] := PointsOfInterest[0][1];

  // Offset and zoom to draw the julia set at. (centered on screen and default size)
  Offset[0] := -GetScreenWidth() / 2; Offset[1] := -GetScreenHeight() / 2;
  Zoom := 1.0;

  OffsetSpeed := Vector2Create(0.0, 0.0);

  // Get variable (uniform) locations on the shader to connect with the program
  // NOTE: If uniform variable could not be found in the shader, function returns -1
  CLoc := GetShaderLocation(Shader, 'c');
  ZoomLoc := GetShaderLocation(Shader, 'zoom');
  OffsetLoc := GetShaderLocation(Shader, 'offset');

  // Tell the shader what the screen dimensions, zoom, offset and c are
  ScreenDims[0] := GetScreenWidth(); ScreenDims[1] := GetScreenHeight();
  SetShaderValue(Shader, GetShaderLocation(Shader, 'screenDims'), @ScreenDims, SHADER_UNIFORM_VEC2);

  SetShaderValue(Shader, CLoc, @C, SHADER_UNIFORM_VEC2);
  SetShaderValue(Shader, ZoomLoc, @Zoom, SHADER_UNIFORM_FLOAT);
  SetShaderValue(Shader, OffsetLoc, @Offset, SHADER_UNIFORM_VEC2);

  IncrementSpeed := 1;             // Multiplier of speed to change c value
  ShowControls := True;           // Show controls
  Pause := False;                 // Pause animation

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Press [1 - 6] to reset c to a point of interest
      if IsKeyPressed(KEY_ONE) or
         IsKeyPressed(KEY_TWO) or
         IsKeyPressed(KEY_THREE) or
         IsKeyPressed(KEY_FOUR) or
         IsKeyPressed(KEY_FIVE) or
         IsKeyPressed(KEY_SIX) then
      begin
          if IsKeyPressed(KEY_ONE) then
          begin
            C[0] := PointsOfInterest[0][0];
            C[1] := PointsOfInterest[0][1];
          end
          else if IsKeyPressed(KEY_TWO) then
          begin
             C[0] := PointsOfInterest[1][0];
             C[1] := PointsOfInterest[1][1];
          end
          else if IsKeyPressed(KEY_THREE) then
          begin
            C[0] := PointsOfInterest[2][0];
            C[1] := PointsOfInterest[2][1];
          end
          else if IsKeyPressed(KEY_FOUR) then
          begin
            C[0] := PointsOfInterest[3][0];
            C[1] := pointsOfInterest[3][1];
          end
          else if IsKeyPressed(KEY_FIVE) then
          begin
            C[0] := PointsOfInterest[4][0];
            C[1] := PointsOfInterest[4][1];
          end
          else if IsKeyPressed(KEY_SIX) then
          begin
            C[0] := pointsOfInterest[5][0];
            C[1] := pointsOfInterest[5][1];
          end;

          SetShaderValue(Shader, CLoc, @C, SHADER_UNIFORM_VEC2);
      end;

      if IsKeyPressed(KEY_SPACE) then
        Pause := not Pause;                 // Pause animation (c change)
      if IsKeyPressed(KEY_F1) then
        ShowControls := not ShowControls;  // Toggle whether or not to show controls

      if not Pause then
      begin
        if IsKeyPressed(KEY_RIGHT) then
          Inc(IncrementSpeed)
        else if IsKeyPressed(KEY_LEFT) then
          Dec(IncrementSpeed);

        // TODO: The idea is to zoom and move around with mouse
        // Probably offset movement should be proportional to zoom level
        if IsMouseButtonDown(MOUSE_BUTTON_LEFT) or IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then
        begin
          if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then Zoom := Zoom + Zoom * 0.003;
          if IsMouseButtonDown(MOUSE_BUTTON_RIGHT) then Zoom := Zoom - Zoom * 0.003;

          MousePos := GetMousePosition();

          OffsetSpeed.X := MousePos.X - ScreenWidth / 2;
          OffsetSpeed.Y := MousePos.Y - ScreenHeight / 2;

          // Slowly move camera to targetOffset
          Offset[0] := Offset[0] + GetFrameTime() * OffsetSpeed.X * 0.8;
          Offset[1] := Offset[1] + GetFrameTime() * OffsetSpeed.Y * 0.8;
        end else
          OffsetSpeed := Vector2Create(0.0, 0.0);

        SetShaderValue(Shader, ZoomLoc, @Zoom, SHADER_UNIFORM_FLOAT);
        SetShaderValue(Shader, OffsetLoc, @Offset, SHADER_UNIFORM_VEC2);

        // Increment c value with time
        Amount := GetFrameTime() * IncrementSpeed * 0.0005;
        C[0] := C[0] + Amount;
        C[1] := C[1] + Amount;

        SetShaderValue(Shader, CLoc, @C[0], SHADER_UNIFORM_VEC2);
      end;

      //----------------------------------------------------------------------------------


      // Draw
      //----------------------------------------------------------------------------------
      // Using a render texture to draw Julia set
      BeginTextureMode(Target);       // Enable drawing to texture
        ClearBackground(BLACK);     // Clear the render texture

        // Draw a rectangle in shader mode to be used as shader canvas
        // NOTE: Rectangle uses font white character texture coordinates,
        // so shader can not be applied here directly because input vertexTexCoord
        // do not represent full screen coordinates (space where want to apply shader)
        DrawRectangle(0, 0, GetScreenWidth(), GetScreenHeight(), BLACK);
      EndTextureMode();

      BeginDrawing();
        ClearBackground(BLACK);     // Clear screen background

        // Draw the saved texture and rendered julia set with shader
        // NOTE: We do not invert texture on Y, already considered inside shader
        BeginShaderMode(Shader);
          // WARNING: If FLAG_WINDOW_HIGHDPI is enabled, HighDPI monitor scaling should be considered
          // when rendering the RenderTexture2D to fit in the HighDPI scaled Window
          DrawTextureEx(Target.Texture, Vector2Create(0.0, 0.0), 0.0, 1.0, WHITE);
        EndShaderMode();

        if ShowControls then
        begin
          DrawText('Press Mouse buttons right/left to zoom in/out and move', 10, 15, 10, RAYWHITE);
          DrawText('Press KEY_F1 to toggle these controls', 10, 30, 10, RAYWHITE);
          DrawText('Press KEYS [1 - 6] to change point of interest', 10, 45, 10, RAYWHITE);
          DrawText('Press KEY_LEFT | KEY_RIGHT to change speed', 10, 60, 10, RAYWHITE);
          DrawText('Press KEY_SPACE to pause movement animation', 10, 75, 10, RAYWHITE);
        end;

      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(Shader);               // Unload shader
  UnloadRenderTexture(Target);        // Unload render texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

