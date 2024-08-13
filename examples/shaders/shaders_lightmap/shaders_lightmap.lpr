{*******************************************************************************************
*
*   raylib [shaders] example - lightmap
*
*   NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
*         OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version.
*
*   NOTE: Shaders used in this example are #version 330 (OpenGL 3.3).
*
*   Example contributed by Jussi Viitala (@nullstare) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2019-2023 Jussi Viitala (@nullstare) and Ramon Santamaria (@raysan5)
*   Pascal translation 2023 Vadim Gunko (@guvacode)
*
********************************************************************************************}
program shaders_lightmap;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
raymath,
rlgl,
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  MAP_SIZE = 10;
  GLSL_VERSION = 330;

var
  camera: TCamera;
  mesh: TMesh;
  shader: TShader;
  texture, light: TTexture;
  lightmap: TRenderTexture;
  material: TMaterial;

begin
  // Initialization
  SetConfigFlags(FLAG_MSAA_4X_HINT);  // Enable Multi Sampling Anti Aliasing 4x (if available)

  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - lightmap');

  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 4.0, 6.0, 8.0 );    // Camera position
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );      // Camera looking at point
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );          // Camera up vector (rotation towards target)
  camera.fovy := 45.0;                                  // Camera field-of-view Y
  camera.projection := CAMERA_PERSPECTIVE;              // Camera projection type

  mesh := GenMeshPlane(MAP_SIZE, MAP_SIZE, 1, 1);

  // GenMeshPlane doesn't generate texcoords2 so we will upload them separately
  mesh.texcoords2 := malloc(mesh.vertexCount * 2 * sizeof(single)) ;

  // X                          // Y
  mesh.texcoords2[0] := 0.0;    mesh.texcoords2[1] := 0.0;
  mesh.texcoords2[2] := 1.0;    mesh.texcoords2[3] := 0.0;
  mesh.texcoords2[4] := 0.0;    mesh.texcoords2[5] := 1.0;
  mesh.texcoords2[6] := 1.0;    mesh.texcoords2[7] := 1.0;

  // Load a new texcoords2 attributes buffer
  mesh.vboId[SHADER_LOC_VERTEX_TEXCOORD02] := rlLoadVertexBuffer(mesh.texcoords2, mesh.vertexCount*2*sizeof(single),false);
  rlEnableVertexArray(mesh.vaoId);

  // Index 5 is for texcoords2
  rlSetVertexAttribute(5, 2, RL_FLOAT, false, 0, 0);
  rlEnableVertexAttribute(5);
  rlDisableVertexArray();

  // Load lightmap shader
  shader := LoadShader(TextFormat('resources/shaders/glsl%i/lightmap.vs', GLSL_VERSION),
                       TextFormat('resources/shaders/glsl%i/lightmap.fs', GLSL_VERSION));

  texture := LoadTexture('resources/cubicmap_atlas.png');
  light := LoadTexture('resources/spark_flame.png');

  GenTextureMipmaps(@texture);
  SetTextureFilter(texture, TEXTURE_FILTER_TRILINEAR);

  lightmap := LoadRenderTexture(MAP_SIZE, MAP_SIZE);

  SetTextureFilter(lightmap.texture, TEXTURE_FILTER_TRILINEAR);

  material := LoadMaterialDefault();
  material.shader := shader;
  material.maps[MATERIAL_MAP_ALBEDO].texture := texture;
  material.maps[MATERIAL_MAP_METALNESS].texture := lightmap.texture;

  // Drawing to lightmap
  BeginTextureMode(lightmap);
      ClearBackground(BLACK);

      BeginBlendMode(BLEND_ADDITIVE);
          DrawTexturePro(
              light,
              RectangleCreate( 0, 0, light.width, light.height ),
              RectangleCreate( 0, 0, 20, 20 ),
              Vector2Create( 10.0, 10.0 ),
              0.0,
              RED
          );
          DrawTexturePro(
              light,
              RectangleCreate( 0, 0, light.width, light.height ),
              RectangleCreate( 8, 4, 20, 20 ),
              Vector2Create( 10.0, 10.0 ),
              0.0,
              BLUE
          );
          DrawTexturePro(
              light,
              RectangleCreate( 0, 0, light.width, light.height ),
              RectangleCreate( 8, 8, 10, 10 ),
              Vector2Create( 5.0, 5.0 ),
              0.0,
              GREEN
          );
      BeginBlendMode(BLEND_ALPHA);
  EndTextureMode();

  SetTargetFPS(60);                   // Set our game to run at 60 frames-per-second


  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_ORBITAL);

      // Draw
      BeginDrawing();
        ClearBackground(RAYWHITE);

        BeginMode3D(camera);
          DrawMesh(mesh, material, MatrixIdentity());
        EndMode3D();

        DrawFPS(10, 10);

        DrawTexturePro(lightmap.texture,
                       RectangleCreate( 0, 0, -MAP_SIZE, -MAP_SIZE ),
                       RectangleCreate( GetRenderWidth() - MAP_SIZE*8 - 10, 10, MAP_SIZE*8, MAP_SIZE*8 ),
                       Vector2Create( 0.0, 0.0 ),
                       0.0,
                       WHITE);

        DrawText('lightmap', GetRenderWidth() - 66, 16 + MAP_SIZE*8, 10, GRAY);
        DrawText('10x10 pixels', GetRenderWidth() - 76, 30 + MAP_SIZE*8, 10, GRAY);


      EndDrawing();
    end;

  // De-Initialization
  UnloadMesh(mesh);       // Unload the mesh
  UnloadShader(shader);   // Unload shader

  CloseWindow();        // Close window and OpenGL context
end.

