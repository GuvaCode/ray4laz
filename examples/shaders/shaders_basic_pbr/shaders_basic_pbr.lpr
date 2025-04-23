{*******************************************************************************************
*
*   raylib [core] example - Model Defuse Normal Shader (adapted for HTML5 platform)
*
*   This example is prepared to compile for PLATFORM_WEB and PLATFORM_DESKTOP
*   As you will notice, code structure is slightly different to the other examples...
*   To compile it for PLATFORM_WEB just uncomment #define PLATFORM_WEB at beginning
*
*   This example has been created using raylib 5.0 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2023-2024 Afan OLOVCIC (@_DevDad)  2015 Ramon Santamaria (@raysan5)
*   Model: "Old Rusty Car" (https://skfb.ly/LxRy) by Renafox
*
*   Pascal conversion 2023 Gunko Vadim (@GuvaCode)
*
********************************************************************************************}
program shaders_basic_pbr;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  GLSL_VERSION = 330;
  MAX_LIGHTS = 4;           // Max dynamic lights supported by shader
  screenWidth = 800;
  screenHeight = 450;
  LIGHT_DIRECTIONAL =0;
  LIGHT_POINT = 1;
  LIGHT_SPOT = 2;



type
  PPBRLight = ^TPBRLight;
  TPBRLight = record
    enabled: Integer;
    type_: Integer;
    position: TVector3;
    target: TVector3;
    color: array[0..3] of Single;
    intensity: Single;

    enabledLoc: Integer;
    typeLoc: Integer;
    positionLoc: Integer;
    targetLoc: Integer;
    colorLoc: Integer;
    intensityLoc: Integer;
  end;



var lightsCount: integer; // Current number of dynamic lights that have been created

// Send light properties to shader
// NOTE: Light shader locations should be available
procedure PBRLightUpdate(shader: TShader; light: TPBRLight);
var position, target: array[0..2] of Single;
begin
  SetShaderValue(shader, light.enabledLoc, @light.enabled, SHADER_UNIFORM_INT);
  SetShaderValue(shader, light.typeLoc, @light.type_, SHADER_UNIFORM_INT);
  // Send to shader light position values
  position[0] := light.position.x;
  position[1] := light.position.y;
  position[2] := light.position.z;
  SetShaderValue(shader, light.positionLoc, @position, SHADER_UNIFORM_VEC3);
  // Send to shader light target position values
  target[0] := light.target.x;
  target[1] := light.target.y;
  target[2] := light.target.z;
  SetShaderValue(shader, light.targetLoc, @target, SHADER_UNIFORM_VEC3);
  SetShaderValue(shader, light.colorLoc, @light.color, SHADER_UNIFORM_VEC4);
  SetShaderValue(shader, light.intensityLoc, @light.intensity, SHADER_UNIFORM_FLOAT);
end;

// Create a light and get shader locations
function PBRLightCreate(type_: Integer; position, target: TVector3; Color: TColorB; intensity: Single; shader: TShader): TPBRLight;
var light: TPBRLight;
begin
  if (lightsCount < MAX_LIGHTS) then
  begin
    light.enabled := 1;
    light.type_ := type_;
    light.position := position;
    light.target := target;
    light.color[0] := color.r / 255;
    light.color[1] := color.g / 255;
    light.color[2] := color.b / 255;
    light.color[3] := color.a / 255;
    light.intensity := intensity;
    // NOTE: Lighting shader naming must be the provided ones
    light.enabledLoc := GetShaderLocation(shader, TextFormat('lights[%i].enabled', lightsCount));
    light.typeLoc := GetShaderLocation(shader, TextFormat('lights[%i].type_', lightsCount));
    light.positionLoc := GetShaderLocation(shader, TextFormat('lights[%i].position', lightsCount));
    light.targetLoc := GetShaderLocation(shader, TextFormat('lights[%i].target', lightsCount));
    light.colorLoc := GetShaderLocation(shader, TextFormat('lights[%i].color', lightsCount));
    light.intensityLoc := GetShaderLocation(shader, TextFormat('lights[%i].intensity', lightsCount));
    PBRLightUpdate(shader, light);
    Inc(lightsCount);
  end;
  result := light;
end;


var camera: TCamera;
    shader: TShader;
    numOfLightsLoc: Integer;
    numOfLights: Integer;
    ambCol, col: TColorB;
    ambColNormalized: TVector3;
    ambIntens: Single;
    albedoLoc: Integer;
    ambColLoc: Integer;
    ambLoc: Integer;
    emissiveIntensityLoc: Integer;
    emissiveColorLoc: Integer;
    textureTilingLoc: Integer;
    model, floor: TModel;
    modelTiling, floorTiling: TVector2;
    lights: array [0..MAX_LIGHTS] of TPBRLight;
    one, emissiveCnt, i: Integer;
    cameraPos: array [0..2] of Single;
    floorEmission, modelEmission: TVector4;
    intensity: Single;

begin
  // Initialization
  SetConfigFlags(FLAG_MSAA_4X_HINT);
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - basic pbr');

  // Define the camera to look into our 3d world
  camera := Default(TCamera);
  camera.position := Vector3Create( 2.0, 2.0, 6.0 );    // Camera position
  camera.target := Vector3Create( 0.0, 0.5, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;             // Camera projection type

  shader := LoadShader(TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/pbr.vs'),GLSL_VERSION),
                       TextFormat(PChar(GetApplicationDirectory + 'resources/shaders/glsl%i/pbr.fs'),GLSL_VERSION));
  shader.locs[SHADER_LOC_MAP_ALBEDO] := GetShaderLocation(shader, 'albedoMap');
  // In reality, metalness, roughness, and ambient occlusion are all packed into the MRA texture
  // We'll pass it in as the metalness map
  shader.locs[SHADER_LOC_MAP_METALNESS] := GetShaderLocation(shader, 'mraMap');
  shader.locs[SHADER_LOC_MAP_NORMAL] := GetShaderLocation(shader, 'normalMap');
  // Similarly to the MRA map, the emissive map packs different information into a single texture
  // This map stores both height and emission in reality
  shader.locs[SHADER_LOC_MAP_EMISSION] := GetShaderLocation(shader, 'emissiveMap');
  shader.locs[SHADER_LOC_COLOR_DIFFUSE] := GetShaderLocation(shader, 'albedoColor');

  shader.locs[SHADER_LOC_VECTOR_VIEW] := GetShaderLocation(shader, 'viewPos');
  numOfLightsLoc := GetShaderLocation(shader, 'numOfLights');
  numOfLights := 4;
  SetShaderValue(shader, numOfLightsLoc, @numOfLights, SHADER_UNIFORM_INT);

  ambCol := ColorCreate( 26,32,135,255 );
  ambColNormalized := Vector3Create( ambCol.r / 255.0, ambCol.g / 255.0, ambCol.b / 255.0 );
  ambIntens := 0.02;

  albedoLoc := GetShaderLocation(shader, 'albedo');
  ambColLoc := GetShaderLocation(shader, 'ambientColor');
  ambLoc := GetShaderLocation(shader, 'ambient');
  SetShaderValue(shader, ambColLoc, @ambColNormalized, SHADER_UNIFORM_VEC3);
  SetShaderValue(shader, ambLoc, @ambIntens, SHADER_UNIFORM_FLOAT);

  emissiveIntensityLoc := GetShaderLocation(shader, 'emissivePower');
  emissiveColorLoc := GetShaderLocation(shader, 'emissiveColor');
  textureTilingLoc := GetShaderLocation(shader, 'tiling');

  model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/old_car_new.glb'));
  // If the OBJ file format is used, we will have to generate tangents manually:
  // GenMeshTangents(&model.meshes[0]);

  model.materials[0].shader := shader;

  model.materials[0].maps[MATERIAL_MAP_ALBEDO].color := WHITE;
  model.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0.0;
  model.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 0.0;
  model.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
  model.materials[0].maps[MATERIAL_MAP_EMISSION].color := ColorCreate( 255, 162, 0, 255 );

  model.materials[0].maps[MATERIAL_MAP_ALBEDO].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/old_car_d.png'));
  model.materials[0].maps[MATERIAL_MAP_METALNESS].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/old_car_mra.png'));
  model.materials[0].maps[MATERIAL_MAP_NORMAL].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/old_car_n.png'));
  model.materials[0].maps[MATERIAL_MAP_EMISSION].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/old_car_e.png'));
  // We store tiling parameters in the generic parameter slots in the Material class
  modelTiling := Vector2Create( 0.5, 0.5 );

  floor := LoadModel(PChar(GetApplicationDirectory + 'resources/models/plane.glb'));

  floor.materials[0].shader := shader;

  floor.materials[0].maps[MATERIAL_MAP_ALBEDO].color := WHITE;
  floor.materials[0].maps[MATERIAL_MAP_METALNESS].value := 0.0;
  floor.materials[0].maps[MATERIAL_MAP_ROUGHNESS].value := 0.0;
  floor.materials[0].maps[MATERIAL_MAP_OCCLUSION].value := 1.0;
  floor.materials[0].maps[MATERIAL_MAP_EMISSION].color := BLACK;

  floor.materials[0].maps[MATERIAL_MAP_ALBEDO].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/road_a.png'));
  floor.materials[0].maps[MATERIAL_MAP_METALNESS].texture := LoadTexture(PChar(GetApplicationDirectory +'resources/road_mra.png'));
  floor.materials[0].maps[MATERIAL_MAP_NORMAL].texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/road_n.png'));

  floorTiling := Vector2Create( 0.5, 0.5 );

  // Create lights
  lights[0] := PBRLightCreate(LIGHT_POINT, Vector3Create( -1, 1, -2 ), Vector3Create(0,0,0), YELLOW,4, shader);
  lights[1] := PBRLightCreate(LIGHT_POINT, Vector3Create( 2,  1, 1 ), Vector3Create(0,0,0), GREEN,3.3, shader);
  lights[2] := PBRLightCreate(LIGHT_POINT, Vector3Create( -2, 1, 1 ), Vector3Create(0,0,0), RED,8.3, shader);
  lights[3] := PBRLightCreate(LIGHT_POINT, Vector3Create( 1,  1, -2 ), Vector3Create(0,0,0), BLUE,2, shader);

  // The textures are always used
  one := 1;
  SetShaderValue(shader, GetShaderLocation(shader, 'useTexAlbedo'), @one, SHADER_UNIFORM_INT);
  SetShaderValue(shader, GetShaderLocation(shader, 'useTexNormal'), @one, SHADER_UNIFORM_INT);
  SetShaderValue(shader, GetShaderLocation(shader, 'useTexMRA'), @one, SHADER_UNIFORM_INT);
  SetShaderValue(shader, GetShaderLocation(shader, 'useTexEmissive'), @one, SHADER_UNIFORM_INT);
  SetTargetFPS(60);               // Set our game to run at 60 frames-per-second-------------------------------------------------------------
  emissiveCnt := 0;

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_ORBITAL);
      // Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
      cameraPos[0] := camera.position.x;
      cameraPos[1] := camera.position.y;
      cameraPos[2] := camera.position.z;
      SetShaderValue(shader, shader.locs[SHADER_LOC_VECTOR_VIEW], @cameraPos, SHADER_UNIFORM_VEC3);
      // Check key inputs to enable/disable lights
      if (IsKeyPressed(KEY_Y)) then  lights[0].enabled := not lights[0].enabled;
      if (IsKeyPressed(KEY_G)) then  lights[1].enabled := not lights[1].enabled;
      if (IsKeyPressed(KEY_R)) then  lights[2].enabled := not lights[2].enabled;
      if (IsKeyPressed(KEY_B)) then  lights[3].enabled := not lights[3].enabled;

      // Update light values (actually, only enable/disable them)
      for  i := 0 to MAX_LIGHTS -1 do PBRLightUpdate(shader, lights[i]);

      // Draw
      BeginDrawing();
        ClearBackground(BLACK);
        BeginMode3D(camera);

          SetShaderValue(shader, textureTilingLoc, @floorTiling, SHADER_UNIFORM_VEC2);
          floorEmission := ColorNormalize(floor.materials[0].maps[MATERIAL_MAP_EMISSION].color);
          SetShaderValue(shader, emissiveColorLoc, @floorEmission, SHADER_UNIFORM_VEC4);
          DrawModel(floor, Vector3Create(0,0,0), 5.0, WHITE);

          Dec(emissiveCnt);
          if (emissiveCnt <= 0) then
          begin
            emissiveCnt := GetRandomValue(0, 20);
            intensity := GetRandomValue(0, 100) / 100;
            SetShaderValue(shader, emissiveIntensityLoc, @intensity, SHADER_UNIFORM_FLOAT);
          end;

          SetShaderValue(shader, textureTilingLoc, @modelTiling, SHADER_UNIFORM_VEC2);
          modelEmission := ColorNormalize(model.materials[0].maps[MATERIAL_MAP_EMISSION].color);
          SetShaderValue(shader, emissiveColorLoc, @modelEmission, SHADER_UNIFORM_VEC4);
          DrawModel(model, Vector3Create(0, 0.0, 0), 0.25, WHITE);

          // Draw spheres to show where the lights are
          for i := 0 to MAX_LIGHTS -1 do
          begin
          col := ColorCreate(Round(lights[i].color[0]* 255),
                             Round(lights[i].color[1]* 255),
                             Round(lights[i].color[2]* 255),
                             Round(lights[i].color[3]* 255));

         if (lights[i].enabled) = 1 then
            DrawSphereEx(lights[i].position, 0.2, 8, 8, col)
          else
            DrawSphereWires(lights[i].position, 0.2, 8, 8, ColorAlpha(col, 0.3));
          end;

        EndMode3D();
        DrawText('Use keys [Y][R][G][B] to toggle lights', 10, 40, 20, DARKGRAY);
        DrawText('(c) Old Rusty Car model by Renafox (https://skfb.ly/LxRy)', screenWidth - 320, screenHeight - 20, 10, LIGHTGRAY);
        DrawFPS(10, 10);
      EndDrawing();
    end;

  // De-Initialization
  UnloadModel(floor);                 // Unload model
  UnloadModel(model);                 // Unload model
  UnloadShader(shader);               // Unload Shader

  CloseWindow();        // Close window and OpenGL context
end.

