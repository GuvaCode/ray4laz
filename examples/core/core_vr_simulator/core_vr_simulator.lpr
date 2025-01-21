{*******************************************************************************************
*
*   raylib [core] example - VR Simulator (Oculus Rift CV1 parameters)
*
*   Example originally created with raylib 2.5, last time updated with raylib 4.0
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2017-2023 Ramon Santamaria (@raysan5)
*   Pascal translation 2023 Gunko Vadim (@GuvaCode)
*
********************************************************************************************}
program core_vr_simulator;

{$mode objfpc}{$H+}

uses 
cmem, raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  GLSL_VERSION = 330;

var
  device: TVrDeviceInfo;
  config: TVrStereoConfig;
  distortion: TShader;
  target: TRenderTexture2D;
  sourceRec, destRec: TRectangle;
  camera: TCamera;
  cubePosition: TVector3;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - vr simulator');

  // VR device parameters definition

  // Oculus Rift CV1 parameters for simulator
  device.hResolution := 2160;                 // Horizontal resolution in pixels
  device.vResolution := 1200;                 // Vertical resolution in pixels
  device.hScreenSize := 0.133793;            // Horizontal size in meters
  device.vScreenSize := 0.0669;              // Vertical size in meters
  //device.vScreenCenter := 0.04678;           // Screen center in meters
  device.eyeToScreenDistance := 0.041;       // Distance between eye and display in meters
  device.lensSeparationDistance := 0.07;     // Lens separation distance in meters
  device.interpupillaryDistance := 0.07;     // IPD (distance between pupils) in meters

  // NOTE: CV1 uses fresnel-hybrid-asymmetric lenses with specific compute shaders
  // Following parameters are just an approximation to CV1 distortion stereo rendering
  device.lensDistortionValues[0] := 1.0;     // Lens distortion constant parameter 0
  device.lensDistortionValues[1] := 0.22;    // Lens distortion constant parameter 1
  device.lensDistortionValues[2] := 0.24;    // Lens distortion constant parameter 2
  device.lensDistortionValues[3] := 0.0;     // Lens distortion constant parameter 3
  device.chromaAbCorrection[0] := 0.996;     // Chromatic aberration correction parameter 0
  device.chromaAbCorrection[1] := -0.004;    // Chromatic aberration correction parameter 1
  device.chromaAbCorrection[2] := 1.014;     // Chromatic aberration correction parameter 2
  device.chromaAbCorrection[3] := 0.0;       // Chromatic aberration correction parameter 3

  // Load VR stereo config for VR device parameteres (Oculus Rift CV1 parameters)
  config := LoadVrStereoConfig(device);

  // Distortion shader (uses device lens distortion and chroma)
  distortion := LoadShader(nil, TextFormat(PChar(GetApplicationDirectory + 'resources/distortion%i.fs'), GLSL_VERSION));
  // Update distortion shader with lens and distortion-scale parameters
  SetShaderValue(distortion, GetShaderLocation(distortion, 'leftLensCenter'),
                  @config.leftLensCenter, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'rightLensCenter'),
                  @config.rightLensCenter, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'leftScreenCenter'),
                  @config.leftScreenCenter, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'rightScreenCenter'),
                  @config.rightScreenCenter, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'scale'),
                  @config.scale, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'scaleIn'),
                  @config.scaleIn, SHADER_UNIFORM_VEC2);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'deviceWarpParam'),
                  @device.lensDistortionValues, SHADER_UNIFORM_VEC4);

  SetShaderValue(distortion, GetShaderLocation(distortion, 'chromaAbParam'),
                  @device.chromaAbCorrection, SHADER_UNIFORM_VEC4);

  // Initialize framebuffer for stereo rendering
  // NOTE: Screen size should match HMD aspect ratio
  target := LoadRenderTexture(device.hResolution, device.vResolution);

  // The target's height is flipped (in the source Rectangle), due to OpenGL reasons
  sourceRec := RectangleCreate(0.0, 0.0, target.texture.width, -target.texture.height);
  destRec := RectangleCreate( 0.0, 0.0, GetScreenWidth(), GetScreenHeight() );

  // Define the camera to look into our 3d world
  camera := Default(TCamera);
  camera.position := Vector3Create( 5.0, 2.0, 5.0 );    // Camera position
  camera.target := Vector3Create( 0.0, 2.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector
  camera.fovy := 60.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera type

  cubePosition := Vector3Create( 0.0, 0.0, 0.0 );


  SetTargetFPS(90); // Set our game to run at 90 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      UpdateCamera(@camera,CAMERA_FIRST_PERSON);
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginTextureMode(target);
          ClearBackground(RAYWHITE);
          BeginVrStereoMode(config);
              BeginMode3D(camera);

                  DrawCube(cubePosition, 2.0, 2.0, 2.0, RED);
                  DrawCubeWires(cubePosition, 2.0, 2.0, 2.0, MAROON);
                  DrawGrid(40, 1.0);

              EndMode3D();
          EndVrStereoMode();
      EndTextureMode();

      BeginDrawing();
          ClearBackground(RAYWHITE);
          BeginShaderMode(distortion);
              DrawTexturePro(target.texture, sourceRec, destRec, Vector2Create( 0.0, 0.0 ), 0.0, WHITE);
          EndShaderMode();
          DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadVrStereoConfig(config);   // Unload stereo config
  UnloadRenderTexture(target);    // Unload stereo render fbo
  UnloadShader(distortion);       // Unload distortion shader
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

