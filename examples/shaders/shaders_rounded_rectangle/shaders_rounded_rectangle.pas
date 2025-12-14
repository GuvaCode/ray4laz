program shaders_rounded_rectangle;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, raylib, raymath;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 450;

{$IF Defined(PLATFORM_DESKTOP)}
  GLSL_VERSION = 330;
{$ELSE}
  GLSL_VERSION = 100;
{$ENDIF}

type
  TRoundedRectangle = record
    cornerRadius: TVector4;  // Individual corner radius (top-left, top-right, bottom-left, bottom-right)

    // Shadow variables
    shadowRadius: Single;
    shadowOffset: TVector2;
    shadowScale: Single;

    // Border variables
    borderThickness: Single;  // Inner-border thickness

    // Shader locations
    rectangleLoc: Integer;
    radiusLoc: Integer;
    colorLoc: Integer;
    shadowRadiusLoc: Integer;
    shadowOffsetLoc: Integer;
    shadowScaleLoc: Integer;
    shadowColorLoc: Integer;
    borderThicknessLoc: Integer;
    borderColorLoc: Integer;
  end;

// Forward declarations

var
  shader: TShader;
  roundedRectangle: TRoundedRectangle;
  rectangleColor, shadowColor, borderColor: TColor;
  rec: TRectangle;
  rectValues: array[0..3] of Single;
  colorValues: array[0..3] of Single;
  shadowColorValues: array[0..3] of Single;
  borderColorValues: array[0..3] of Single;
  radiusValues: array[0..3] of Single;
  shadowOffsetValues: array[0..1] of Single;



procedure UpdateRoundedRectangle(rec: TRoundedRectangle; shader: TShader);
begin
  radiusValues[0] := rec.cornerRadius.x;
  radiusValues[1] := rec.cornerRadius.y;
  radiusValues[2] := rec.cornerRadius.z;
  radiusValues[3] := rec.cornerRadius.w;
  SetShaderValue(shader, rec.radiusLoc, @radiusValues[0], SHADER_UNIFORM_VEC4);

  SetShaderValue(shader, rec.shadowRadiusLoc, @rec.shadowRadius, SHADER_UNIFORM_FLOAT);

  shadowOffsetValues[0] := rec.shadowOffset.x;
  shadowOffsetValues[1] := rec.shadowOffset.y;
  SetShaderValue(shader, rec.shadowOffsetLoc, @shadowOffsetValues[0], SHADER_UNIFORM_VEC2);

  SetShaderValue(shader, rec.shadowScaleLoc, @rec.shadowScale, SHADER_UNIFORM_FLOAT);
  SetShaderValue(shader, rec.borderThicknessLoc, @rec.borderThickness, SHADER_UNIFORM_FLOAT);
end;

function CreateRoundedRectangle(cornerRadius: TVector4; shadowRadius: Single;
  shadowOffset: TVector2; shadowScale: Single; borderThickness: Single;
  shader: TShader): TRoundedRectangle;
begin
  Result.cornerRadius := cornerRadius;
  Result.shadowRadius := shadowRadius;
  Result.shadowOffset := shadowOffset;
  Result.shadowScale := shadowScale;
  Result.borderThickness := borderThickness;

  // Get shader uniform locations
  Result.rectangleLoc := GetShaderLocation(shader, 'rectangle');
  Result.radiusLoc := GetShaderLocation(shader, 'radius');
  Result.colorLoc := GetShaderLocation(shader, 'color');
  Result.shadowRadiusLoc := GetShaderLocation(shader, 'shadowRadius');
  Result.shadowOffsetLoc := GetShaderLocation(shader, 'shadowOffset');
  Result.shadowScaleLoc := GetShaderLocation(shader, 'shadowScale');
  Result.shadowColorLoc := GetShaderLocation(shader, 'shadowColor');
  Result.borderThicknessLoc := GetShaderLocation(shader, 'borderThickness');
  Result.borderColorLoc := GetShaderLocation(shader, 'borderColor');

  UpdateRoundedRectangle(Result, shader);
end;

begin
  // Initialization
  InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, 'raylib [shaders] example - rounded rectangle');

  // Load the shader
  Shader := LoadShader(TextFormat('resources/shaders/glsl%i/base.vs', GLSL_VERSION),
                       TextFormat('resources/shaders/glsl%i/rounded_rectangle.fs', GLSL_VERSION));

  // Create a rounded rectangle
  roundedRectangle := CreateRoundedRectangle(
    Vector4Create(5.0, 10.0, 15.0, 20.0),  // Corner radius
    20.0,                                   // Shadow radius
    Vector2Create(0.0, -5.0),               // Shadow offset
    0.95,                                   // Shadow scale
    5.0,                                    // Border thickness
    shader                                  // Shader
  );



  // Update shader uniforms
  UpdateRoundedRectangle(roundedRectangle, shader);

  rectangleColor := BLUE;
  shadowColor := DARKBLUE;
  borderColor := SKYBLUE;

  SetTargetFPS(60);

  // Main game loop
  while not WindowShouldClose() do
  begin
    // Draw
    BeginDrawing();
      ClearBackground(RAYWHITE);

      // Draw rectangle box with rounded corners using shader
      rec := RectangleCreate(50, 70, 110, 60);
      DrawRectangleLines(Round(rec.x) - 20, Round(rec.y) - 20,
        Round(rec.width) + 40, Round(rec.height) + 40, DARKGRAY);
      DrawText('Rounded rectangle', Round(rec.x) - 20, Round(rec.y) - 35, 10, DARKGRAY);

      // Flip Y axis to match shader coordinate system
      rec.y := SCREEN_HEIGHT - rec.y - rec.height;
      rectValues[0] := rec.x;
      rectValues[1] := rec.y;
      rectValues[2] := rec.width;
      rectValues[3] := rec.height;
      SetShaderValue(shader, roundedRectangle.rectangleLoc, @rectValues[0], SHADER_UNIFORM_VEC4);

      // Only rectangle color
      colorValues[0] := rectangleColor.r / 255.0;
      colorValues[1] := rectangleColor.g / 255.0;
      colorValues[2] := rectangleColor.b / 255.0;
      colorValues[3] := rectangleColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.colorLoc, @colorValues[0], SHADER_UNIFORM_VEC4);

      shadowColorValues[0] := 0.0;
      shadowColorValues[1] := 0.0;
      shadowColorValues[2] := 0.0;
      shadowColorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.shadowColorLoc, @shadowColorValues[0], SHADER_UNIFORM_VEC4);

      borderColorValues[0] := 0.0;
      borderColorValues[1] := 0.0;
      borderColorValues[2] := 0.0;
      borderColorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.borderColorLoc, @borderColorValues[0], SHADER_UNIFORM_VEC4);

      BeginShaderMode(shader);
        DrawRectangle(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, WHITE);
      EndShaderMode();

      // Draw rectangle shadow using shader
      rec := RectangleCreate(50, 200, 110, 60);
      DrawRectangleLines(Round(rec.x) - 20, Round(rec.y) - 20,
        Round(rec.width) + 40, Round(rec.height) + 40, DARKGRAY);
      DrawText('Rounded rectangle shadow', Round(rec.x) - 20, Round(rec.y) - 35, 10, DARKGRAY);

      rec.y := SCREEN_HEIGHT - rec.y - rec.height;
      rectValues[0] := rec.x;
      rectValues[1] := rec.y;
      rectValues[2] := rec.width;
      rectValues[3] := rec.height;
      SetShaderValue(shader, roundedRectangle.rectangleLoc, @rectValues[0], SHADER_UNIFORM_VEC4);

      // Only shadow color
      colorValues[0] := 0.0;
      colorValues[1] := 0.0;
      colorValues[2] := 0.0;
      colorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.colorLoc, @colorValues[0], SHADER_UNIFORM_VEC4);

      shadowColorValues[0] := shadowColor.r / 255.0;
      shadowColorValues[1] := shadowColor.g / 255.0;
      shadowColorValues[2] := shadowColor.b / 255.0;
      shadowColorValues[3] := shadowColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.shadowColorLoc, @shadowColorValues[0], SHADER_UNIFORM_VEC4);

      borderColorValues[0] := 0.0;
      borderColorValues[1] := 0.0;
      borderColorValues[2] := 0.0;
      borderColorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.borderColorLoc, @borderColorValues[0], SHADER_UNIFORM_VEC4);

      BeginShaderMode(shader);
        DrawRectangle(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, WHITE);
      EndShaderMode();

      // Draw rectangle's border using shader
      rec := RectangleCreate(50, 330, 110, 60);
      DrawRectangleLines(Round(rec.x) - 20, Round(rec.y) - 20,
        Round(rec.width) + 40, Round(rec.height) + 40, DARKGRAY);
      DrawText('Rounded rectangle border', Round(rec.x) - 20, Round(rec.y) - 35, 10, DARKGRAY);

      rec.y := SCREEN_HEIGHT - rec.y - rec.height;
      rectValues[0] := rec.x;
      rectValues[1] := rec.y;
      rectValues[2] := rec.width;
      rectValues[3] := rec.height;
      SetShaderValue(shader, roundedRectangle.rectangleLoc, @rectValues[0], SHADER_UNIFORM_VEC4);

      // Only border color
      colorValues[0] := 0.0;
      colorValues[1] := 0.0;
      colorValues[2] := 0.0;
      colorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.colorLoc, @colorValues[0], SHADER_UNIFORM_VEC4);

      shadowColorValues[0] := 0.0;
      shadowColorValues[1] := 0.0;
      shadowColorValues[2] := 0.0;
      shadowColorValues[3] := 0.0;
      SetShaderValue(shader, roundedRectangle.shadowColorLoc, @shadowColorValues[0], SHADER_UNIFORM_VEC4);

      borderColorValues[0] := borderColor.r / 255.0;
      borderColorValues[1] := borderColor.g / 255.0;
      borderColorValues[2] := borderColor.b / 255.0;
      borderColorValues[3] := borderColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.borderColorLoc, @borderColorValues[0], SHADER_UNIFORM_VEC4);

      BeginShaderMode(shader);
        DrawRectangle(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, WHITE);
      EndShaderMode();

      // Draw one more rectangle with all three colors
      rec := RectangleCreate(240, 80, 500, 300);
      DrawRectangleLines(Round(rec.x) - 30, Round(rec.y) - 30,
        Round(rec.width) + 60, Round(rec.height) + 60, DARKGRAY);
      DrawText('Rectangle with all three combined', Round(rec.x) - 30, Round(rec.y) - 45, 10, DARKGRAY);

      rec.y := SCREEN_HEIGHT - rec.y - rec.height;
      rectValues[0] := rec.x;
      rectValues[1] := rec.y;
      rectValues[2] := rec.width;
      rectValues[3] := rec.height;
      SetShaderValue(shader, roundedRectangle.rectangleLoc, @rectValues[0], SHADER_UNIFORM_VEC4);

      // All three colors
      colorValues[0] := rectangleColor.r / 255.0;
      colorValues[1] := rectangleColor.g / 255.0;
      colorValues[2] := rectangleColor.b / 255.0;
      colorValues[3] := rectangleColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.colorLoc, @colorValues[0], SHADER_UNIFORM_VEC4);

      shadowColorValues[0] := shadowColor.r / 255.0;
      shadowColorValues[1] := shadowColor.g / 255.0;
      shadowColorValues[2] := shadowColor.b / 255.0;
      shadowColorValues[3] := shadowColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.shadowColorLoc, @shadowColorValues[0], SHADER_UNIFORM_VEC4);

      borderColorValues[0] := borderColor.r / 255.0;
      borderColorValues[1] := borderColor.g / 255.0;
      borderColorValues[2] := borderColor.b / 255.0;
      borderColorValues[3] := borderColor.a / 255.0;
      SetShaderValue(shader, roundedRectangle.borderColorLoc, @borderColorValues[0], SHADER_UNIFORM_VEC4);

      BeginShaderMode(shader);
        DrawRectangle(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, WHITE);
      EndShaderMode();

      DrawText('(c) Rounded rectangle SDF by Iñigo Quilez. MIT License.',
        SCREEN_WIDTH - 300, SCREEN_HEIGHT - 20, 10, BLACK);

    EndDrawing();
  end;

  // De-Initialization
  UnloadShader(shader); // Unload shader
  CloseWindow();        // Close window and OpenGL context
end.
