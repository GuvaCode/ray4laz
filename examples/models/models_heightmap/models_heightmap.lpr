program models_heightmap;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

const
 screenWidth = 800;
 screenHeight = 450;

var camera:TCamera3D;
    image:TImage;
    texture:TTexture2D;
    mesh:Tmesh;
    model:TModel;
    mapPosition:TVector3;

begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [models] example - heightmap loading and drawing');
 SetTargetFPS(60);
 // Define our custom camera to look into our 3d world
 camera:=Camera3DCreate(Vector3Create(18.0,18.0,18.0),
                         Vector3Create(0.0,0.0,0.0),
                         Vector3Create(0.0,1.0,0.0),45.0,0);

 image := LoadImage(PChar(GetApplicationDirectory + 'resources/heightmap.png'));           // Load heightmap image (RAM)
 texture := LoadTextureFromImage(image);                  // Convert image to texture (VRAM)
 mesh := GenMeshHeightmap(image, Vector3Create(16,8,16)); // Generate heightmap mesh (RAM and VRAM)
 model := LoadModelFromMesh(mesh);                        // Load model from generated mesh
 model.materials[0].maps[MATERIAL_MAP_DIFFUSE].texture := texture; // Set map diffuse texture
 Vector3Set(@mapPosition,-8.0,0.0,-8.0);                  // Define model position


 UnloadImage(image);                     // Unload heightmap image from RAM, already uploaded to VRAM





 while not WindowShouldClose() do
 begin
   // Update
 //----------------------------------------------------------------------------------
 UpdateCamera(@camera,CAMERA_ORBITAL);              // Update camera
 //----------------------------------------------------------------------------------



  BeginDrawing();
  ClearBackground(RAYWHITE);

   BeginMode3D(camera);

     DrawModel(model, mapPosition, 1.0, RED);

     DrawGrid(20, 1.0);

     EndMode3D();

     DrawTexture(texture, screenWidth - texture.width - 20, 20, WHITE);
     DrawRectangleLines(screenWidth - texture.width - 20, 20, texture.width, texture.height, GREEN);

     DrawFPS(10, 10);
     DrawText('raylib in lazarus !!!', 10, 30, 20, SKYBLUE);

  EndDrawing(); 
 end;

    UnloadTexture(texture);     // Unload texture
    UnloadModel(model);         // Unload model
    CloseWindow();

end.

