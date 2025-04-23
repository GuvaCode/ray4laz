(*******************************************************************************************
*
*   raylib [models] example - Plane rotations (yaw, pitch, roll)
*
*   This example has been created using raylib 1.8 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by Berni (@Berni8k) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2017 Berni (@Berni8k) and Ramon Santamaria (@raysan5)
*   Pascal translate Gunko Vadim (@GuvaCode)
********************************************************************************************)
program models_yaw_pitch_roll;

{$mode objfpc}{$H+}

uses cmem, raylib, raymath;

const
 screenWidth = 800;
 screenHeight = 450;

 var
     pitch : single = 0.0;
     roll  : single = 0.0;
     yaw   : single = 0.0;
     model : TModel;
     texture: TTexture2d;
     camera: TCamera;
begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}
    //SetConfigFlags(FLAG_MSAA_4X_HINT);
    InitWindow(screenWidth, screenHeight, 'raylib [models] example - plane rotations (yaw, pitch, roll)');
    camera.position := Vector3Create( 0.0, 50.0, -120.0);// Camera position perspective
    camera.target   := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
    camera.up := Vector3Create (0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
    camera.fovy := 30.0;                                // Camera field-of-view Y
    camera.projection := CAMERA_PERSPECTIVE;             // Camera type
    // Model loading
    // NOTE: Diffuse map loaded automatically
    model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/obj/plane.obj'));
    texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/models/obj/plane_diffuse.png'));
    model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture:= texture;            // Set map diffuse texture
    SetTargetFPS(60);


 // Main game loop
 while not WindowShouldClose() do
 begin

  // Update
        //----------------------------------------------------------------------------------
        // Plane pitch (x-axis) controls
        if IsKeyDown(KEY_DOWN) then pitch += 0.6
        else if IsKeyDown(KEY_UP) then pitch -= 0.6
        else
        begin
            if (pitch > 0.3) then pitch -= 0.3
            else if (pitch < -0.3) then pitch += 0.3;
        end;

        // Plane yaw (y-axis) controls
        if IsKeyDown(KEY_S) then yaw += 1.0
        else if IsKeyDown(KEY_A) then yaw -= 1.0
        else
        begin
            if (yaw > 0.0) then yaw -= 0.5
            else if (yaw < 0.0) then yaw += 0.5;
        end;

        // Plane roll (z-axis) controls
        if IsKeyDown(KEY_LEFT) then roll += 1.0
        else if IsKeyDown(KEY_RIGHT) then roll -= 1.0
        else
        begin
            if (roll > 0.0) then roll -= 0.5
            else if (roll < 0.0) then roll += 0.5;
        end;

        // Tranformation matrix for rotations
        model.transform := MatrixRotateXYZ( Vector3Create( DEG2RAD*pitch, DEG2RAD*yaw, DEG2RAD*roll) );
        //----------------------------------------------------------------------------------

  BeginDrawing();
   ClearBackground(RAYWHITE);
               // Draw 3D model (recomended to draw 3D always before 2D)
            BeginMode3D(camera);
                DrawModel(model, Vector3Create( 0.0, -8.0, 0.0 ), 1.0, WHITE);   // Draw 3d model with texture
                //DrawModel(model, (Vector3){ 0.0f, 0.0f, 15.0f }, 0.25f, WHITE);   // Draw 3d model with texture
                //                DrawModel(model, (Vector3){ 0.0f, -8.0f, 0.0f }, 1.0f, WHITE);   // Draw 3d model with texture
                DrawGrid(10, 10.0);
            EndMode3D();

            // Draw controls info
            DrawRectangle(30, 370, 260, 70, Fade(GREEN, 0.5));
            DrawRectangleLines(30, 370, 260, 70, Fade(DARKGREEN, 0.5));
            DrawText('Pitch controlled with: KEY_UP / KEY_DOWN', 40, 380, 10, DARKGRAY);
            DrawText('Roll controlled with: KEY_LEFT / KEY_RIGHT', 40, 400, 10, DARKGRAY);
            DrawText('Yaw controlled with: KEY_A / KEY_S', 40, 420, 10, DARKGRAY);

            DrawText('(c) WWI Plane Model created by GiaHanLam', screenWidth - 240, screenHeight - 20, 10, DARKGRAY);
  EndDrawing();
 end;

  // De-Initialization
    //--------------------------------------------------------------------------------------
    UnloadModel(model);     // Unload model data
    CloseWindow();

end.

