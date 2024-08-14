program shaders_julia_set;

{$mode objfpc}{$H+}

uses 
cmem, raylib, raymath, math;

const
  screenWidth = 800;
  screenHeight = 450;

const
  GLSL_VERSION = 330;
  startingZoom = 0.75;
  zoomSpeed = 1.01;
  offsetSpeedMul = 2.0;

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
  CLoc, ZoomLoc, OffsetLoc, IncrementSpeed: Integer;
  ShowControls: Boolean;
  MousePos, offsetVelocity: TVector2;

  dc: Single;
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
  Offset[0] := 0.0;
  Offset[1] := 0.0;
  Zoom := 1.0;

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

  IncrementSpeed := 3;             // Multiplier of speed to change c value
  ShowControls := True;           // Show controls


  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
    // Update
    // Press [1 - 6] to reset c to a point of interest
    if (IsKeyPressed(KEY_ONE) or
        IsKeyPressed(KEY_TWO) or
        IsKeyPressed(KEY_THREE) or
        IsKeyPressed(KEY_FOUR) or
        IsKeyPressed(KEY_FIVE) or
        IsKeyPressed(KEY_SIX)) then
    begin
      if (IsKeyPressed(KEY_ONE)) then
      begin
      c[0] := pointsOfInterest[0][0];
      c[1] := pointsOfInterest[0][1];
      end else
      if (IsKeyPressed(KEY_TWO)) then
      begin
      c[0] := pointsOfInterest[1][0];
      c[1] := pointsOfInterest[1][1];
      end else
      if (IsKeyPressed(KEY_THREE)) then
      begin
      c[0] := pointsOfInterest[2][0];
      c[1] := pointsOfInterest[2][1];
      end else
      if (IsKeyPressed(KEY_FOUR)) then
      begin
      c[0] := pointsOfInterest[3][0];
      c[1] := pointsOfInterest[3][1];
      end else
      if (IsKeyPressed(KEY_FIVE)) then
      begin
      c[0] := pointsOfInterest[4][0];
      c[1] := pointsOfInterest[4][1];

      end else
      if (IsKeyPressed(KEY_SIX)) then
      begin
      c[0] := pointsOfInterest[5][0];
      c[1] := pointsOfInterest[5][1];
      end;
      SetShaderValue(shader, cLoc, @c, SHADER_UNIFORM_VEC2);
    end;

    // If "R" is pressed, reset zoom and offset.
    if (IsKeyPressed(KEY_R)) then
    begin
      zoom := startingZoom;
      offset[0] := 0.0;
      offset[1] := 0.0;
      SetShaderValue(shader, zoomLoc, @zoom, SHADER_UNIFORM_FLOAT);
      SetShaderValue(shader, offsetLoc, @offset, SHADER_UNIFORM_VEC2);
    end;

    if (IsKeyPressed(KEY_SPACE)) then incrementSpeed := 0;         // Pause animation (c change)
    if (IsKeyPressed(KEY_F1)) then showControls := not showControls;  // Toggle whether or not to show controls

    if (IsKeyPressed(KEY_RIGHT)) then Inc(incrementSpeed)
    else if (IsKeyPressed(KEY_LEFT)) then Dec(incrementSpeed);        // If either left or right button is pressed, zoom in/out.

    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT) or IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) then
    begin
      // Change zoom. If Mouse left -> zoom in. Mouse right -> zoom out.
    zoom:= zoom * ifthen(IsMouseButtonDown(MOUSE_BUTTON_LEFT), zoomSpeed, 1.0/zoomSpeed);
    mousePos := GetMousePosition();

    // Find the velocity at which to change the camera. Take the distance of the mouse
    // from the center of the screen as the direction, and adjust magnitude based on
    // the current zoom.
    offsetVelocity.x := (mousePos.x/screenWidth - 0.5)*offsetSpeedMul/zoom;
    offsetVelocity.y := (mousePos.y/screenHeight - 0.5)*offsetSpeedMul/zoom;

    // Apply move velocity to camera
    offset[0] += GetFrameTime()*offsetVelocity.x;
    offset[1] += GetFrameTime()*offsetVelocity.y;

    // Update the shader uniform values!
    SetShaderValue(shader, zoomLoc, @zoom, SHADER_UNIFORM_FLOAT);
    SetShaderValue(shader, offsetLoc, @offset, SHADER_UNIFORM_VEC2);
    end;


    // Increment c value with time
    dc := GetFrameTime()*incrementSpeed*0.0005;
    c[0] += dc;
    c[1] += dc;
    SetShaderValue(shader, cLoc, @c, SHADER_UNIFORM_VEC2);





    //----------------------------------------------------------------------------------





      // Draw
      //----------------------------------------------------------------------------------
      // Using a render texture to draw Julia set
      BeginTextureMode(target);       // Enable drawing to texture
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
          BeginShaderMode(shader);
              // WARNING: If FLAG_WINDOW_HIGHDPI is enabled, HighDPI monitor scaling should be considered
              // when rendering the RenderTexture2D to fit in the HighDPI scaled Window
              DrawTextureEx(target.texture, Vector2Create( 0.0, 0.0 ), 0.0, 1.0, WHITE);
          EndShaderMode();

          if (showControls) then
          begin
              DrawText('Press Mouse buttons right/left to zoom in/out and move', 10, 15, 10, RAYWHITE);
              DrawText('Press KEY_F1 to toggle these controls', 10, 30, 10, RAYWHITE);
              DrawText('Press KEYS [1 - 6] to change point of interest', 10, 45, 10, RAYWHITE);
              DrawText('Press KEY_LEFT | KEY_RIGHT to change speed', 10, 60, 10, RAYWHITE);
              DrawText('Press KEY_SPACE to stop movement animation', 10, 75, 10, RAYWHITE);
              DrawText('Press KEY_R to recenter the camera', 10, 90, 10, RAYWHITE);
          end;
      EndDrawing();    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadShader(Shader);               // Unload shader
  UnloadRenderTexture(Target);        // Unload render texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

