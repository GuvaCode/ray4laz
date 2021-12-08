program models_gltf;

{$mode objfpc}{$H+}

uses cmem, raylib;

const
 screenWidth = 800;
 screenHeight = 450;
 MAX_MODELS = 6;

var i,
    currentModel: integer;
  //  animFrameCounter,
  //  animationDirection:integer;
  //  anims:PModelAnimation;
    camera:TCamera;
    //model:TModel;
    model: array[0..MAX_MODELS] of TModel;
    position:TVector3;
begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [models] example');


  // Define the camera to look into our 3d world

    camera.position := Vector3Create( 10.0, 10.0, 10.0); // Camera position
    camera.target := Vector3Create(0.0, 0.0, 0.0);      // Camera looking at point
    camera.up := Vector3Create( 0.0, 1.0, 0.0 );       // Camera up vector (rotation towards target)
    camera.fovy := 45.0;                              // Camera field-of-view Y
    camera.projection := CAMERA_PERSPECTIVE;           // Camera mode type

    model[0] := LoadModel('resources/models/gltf/raylib_32x32.glb');
    model[1] := LoadModel('resources/models/gltf/rigged_figure.glb');
    model[2] := LoadModel('resources/models/gltf/GearboxAssy.glb');
    model[3] := LoadModel('resources/models/gltf/BoxAnimated.glb');
    model[4] := LoadModel('resources/models/gltf/girl.glb');
    model[5] := LoadModel('resources/models/gltf/AnimatedMorphCube.glb');

     position:= Vector3Create( 0.0, 0.0, 0.0 );

    SetCameraMode(camera, CAMERA_FREE); // Set free camera mode
    SetTargetFPS(60);
    currentModel:=0;

 while not WindowShouldClose() do 
 begin

    // Update
    //----------------------------------------------------------------------------------
        UpdateCamera(@camera);
      if IsKeyReleased(KEY_RIGHT) then
        begin
            inc(currentModel);//++;
            if (currentModel = MAX_MODELS ) then currentModel := 0;
        end;

        if IsKeyReleased(KEY_LEFT) then
        begin
            dec(currentModel);//--;
            if (currentModel < 0) then currentModel := MAX_MODELS - 1;
        end;

        //----------------------------------------------------------------------------------
        BeginDrawing();
        ClearBackground(SKYBLUE);
            BeginMode3D(camera);
                DrawModelEx(model[currentModel], position, Vector3Create( 0.0, 1.0, 0.0 ), 180.0,
                Vector3Create( 2.0, 2.0, 2.0 ), WHITE);
              DrawGrid(10, 1.0);         // Draw a grid
            EndMode3D();
           EndDrawing();
 end;


 // De-Initialization
  for i:=0 to MAX_MODELS-1 do UnloadModel(model[i]);

CloseWindow(); 

end.

