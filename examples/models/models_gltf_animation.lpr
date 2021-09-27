program models_gltf_animation;

{$mode objfpc}{$H+}

uses cmem, ray_header;

const
 screenWidth = 800;
 screenHeight = 450;

var i,
    animsCount:longint;
    animFrameCounter,
    animationDirection:integer;
    anims:PModelAnimation;
    camera:TCamera;
    model:TModel;
    position:TVector3;
begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [models] example - model animation');


  // Define the camera to look into our 3d world
    camera.position := Vector3Create( 10.0, 10.0, 10.0); // Camera position
    camera.target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
    camera.up := Vector3Create( 0.0, 1.0, 0.0 );       // Camera up vector (rotation towards target)
    camera.fovy := 45.0;                              // Camera field-of-view Y
    camera.projection := CAMERA_PERSPECTIVE;           // Camera mode type

    model := LoadModel('resources/gltf/rigged_figure.glb');               // Load the animated model mesh and

    position:=Vector3Create(0.0,0.0,0.0);     // Set model position


     // Load animation data
    animsCount:= 0;
    //ModelAnimation
    anims := LoadModelAnimations('resources/gltf/rigged_figure.glb', @animsCount);
    animFrameCounter := 0;
    animationDirection := 1;

    SetCameraMode(camera, CAMERA_FREE); // Set free camera mode
    SetTargetFPS(30);


 while not WindowShouldClose() do 
 begin

    // Update
    //----------------------------------------------------------------------------------
        UpdateCamera(@camera);
        // Play animation when spacebar is held down
        if IsKeyDown(KEY_SPACE) then
        begin
            animFrameCounter += animationDirection;
            if (animFrameCounter >= anims[0].frameCount) or (animFrameCounter <= 0)  then
            begin
                animationDirection *= -1;
                animFrameCounter += animationDirection;
            end;
            UpdateModelAnimation(model, anims[0], animFrameCounter);
        end;
        //----------------------------------------------------------------------------------
        BeginDrawing();
           ClearBackground(RAYWHITE);
            BeginMode3D(camera);
                DrawModelEx(model, position, Vector3Create(1.0, 0.0, 0.0), -90.0, Vector3Create(1.0,1.0,1.0), WHITE);

             for i:=0 to model.boneCount -1 do
                    DrawSphere(anims[0].framePoses[animFrameCounter][i].translation, 0.01, RED);

             DrawGrid(10, 1.0);         // Draw a grid
            EndMode3D();
            DrawText('PRESS SPACE to PLAY MODEL ANIMATION', 10, 10, 20, MAROON);
            DrawText('(cc4) Rigged Figure by @Cesium', screenWidth - 200, screenHeight - 20, 10, GRAY);
           EndDrawing();
 end;

    // De-Initialization
    for i:=0 to animsCount -1  do  UnloadModelAnimation(anims[i]);
    free(anims);
         UnloadModel(model);         // Unload model


CloseWindow(); 

end.

