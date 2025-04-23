program models_draw_cube_texture;

{$mode objfpc}{$H+}

uses 
cmem, rlgl,raylib;

const
  screenWidth = 800;
  screenHeight = 450;

// Draw cube textured
// NOTE: Cube position is the center position
procedure DrawCubeTexture(texture: TTexture2D; position: TVector3; width, height, length: Single; color: TColor);
var x,y,z: single;
begin
  x := position.x;
  y := position.y;
  z := position.z;
  // Set desired texture to be enabled while drawing following vertex data
  rlSetTexture(texture.id);
  // Vertex data transformation can be defined with the commented lines,
  // but in this example we calculate the transformed vertex data directly when calling rlVertex3f()
  rlBegin(RL_QUADS);
    rlColor4ub(color.r, color.g, color.b, color.a);
    // Front Face
    rlNormal3f  (0.0, 0.0, 1.0);       // Normal Pointing Towards Viewer
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x - width/2, y - height/2, z + length/2);  // Bottom Left Of The Texture and Quad
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x + width/2, y - height/2, z + length/2);  // Bottom Right Of The Texture and Quad
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x + width/2, y + height/2, z + length/2);  // Top Right Of The Texture and Quad
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x - width/2, y + height/2, z + length/2);  // Top Left Of The Texture and Quad
    // Back Face
    rlNormal3f  (0.0, 0.0, - 1.0);     // Normal Pointing Away From Viewer
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x - width/2, y - height/2, z - length/2);  // Bottom Right Of The Texture and Quad
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x - width/2, y + height/2, z - length/2);  // Top Right Of The Texture and Quad
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x + width/2, y + height/2, z - length/2);  // Top Left Of The Texture and Quad
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x + width/2, y - height/2, z - length/2);  // Bottom Left Of The Texture and Quad
    // Top Face
    rlNormal3f  (0.0, 1.0, 0.0);       // Normal Pointing Up
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x - width/2, y + height/2, z - length/2);  // Top Left Of The Texture and Quad
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x - width/2, y + height/2, z + length/2);  // Bottom Left Of The Texture and Quad
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x + width/2, y + height/2, z + length/2);  // Bottom Right Of The Texture and Quad
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x + width/2, y + height/2, z - length/2);  // Top Right Of The Texture and Quad
    // Bottom Face
    rlNormal3f  (0.0, - 1.0, 0.0);     // Normal Pointing Down
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x - width/2, y - height/2, z - length/2);  // Top Right Of The Texture and Quad
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x + width/2, y - height/2, z - length/2);  // Top Left Of The Texture and Quad
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x + width/2, y - height/2, z + length/2);  // Bottom Left Of The Texture and Quad
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x - width/2, y - height/2, z + length/2);  // Bottom Right Of The Texture and Quad
    // Right face
    rlNormal3f  (1.0, 0.0, 0.0);       // Normal Pointing Right
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x + width/2, y - height/2, z - length/2);  // Bottom Right Of The Texture and Quad
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x + width/2, y + height/2, z - length/2);  // Top Right Of The Texture and Quad
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x + width/2, y + height/2, z + length/2);  // Top Left Of The Texture and Quad
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x + width/2, y - height/2, z + length/2);  // Bottom Left Of The Texture and Quad
    // Left Face
    rlNormal3f( -1.0, 0.0, 0.0);    // Normal Pointing Left
    rlTexCoord2f(0.0, 0.0); rlVertex3f(x - width/2, y - height/2, z - length/2);  // Bottom Left Of The Texture and Quad
    rlTexCoord2f(1.0, 0.0); rlVertex3f(x - width/2, y - height/2, z + length/2);  // Bottom Right Of The Texture and Quad
    rlTexCoord2f(1.0, 1.0); rlVertex3f(x - width/2, y + height/2, z + length/2);  // Top Right Of The Texture and Quad
    rlTexCoord2f(0.0, 1.0); rlVertex3f(x - width/2, y + height/2, z - length/2);  // Top Left Of The Texture and Quad
  rlEnd();
  rlSetTexture(0);
end;

// Draw cube with texture piece applied to all faces
procedure DrawCubeTextureRec(texture: TTexture2D; source: TRectangle; position: TVector3; width, height, length: Single; color: TColor);
var x,y,z,texWidth,texHeight: Single;
begin
  x := position.x;
  y := position.y;
  z := position.z;
  texWidth := texture.width;
  texHeight := texture.height;

  // Set desired texture to be enabled while drawing following vertex data
  rlSetTexture(texture.id);

  // We calculate the normalized texture coordinates for the desired texture-source-rectangle
  // It means converting from (tex.width, tex.height) coordinates to [0.0f, 1.0f] equivalent
  rlBegin(RL_QUADS);
    rlColor4ub(color.r, color.g, color.b, color.a);
    // Front face
    rlNormal3f(0.0, 0.0, 1.0);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y - height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y - height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y + height/2, z + length/2);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y + height/2, z + length/2);

    // Back face
    rlNormal3f(0.0, 0.0, - 1.0);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y - height/2, z - length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y + height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y + height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y - height/2, z - length/2);

    // Top face
    rlNormal3f(0.0, 1.0, 0.0);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y + height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y + height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y + height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y + height/2, z - length/2);

    // Bottom face
    rlNormal3f(0.0, - 1.0, 0.0);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y - height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y - height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y - height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y - height/2, z + length/2);

    // Right face
    rlNormal3f(1.0, 0.0, 0.0);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y - height/2, z - length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y + height/2, z - length/2);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x + width/2, y + height/2, z + length/2);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x + width/2, y - height/2, z + length/2);

    // Left face
    rlNormal3f( - 1.0, 0.0, 0.0);
    rlTexCoord2f(source.x/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y - height/2, z - length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, (source.y + source.height)/texHeight);
    rlVertex3f(x - width/2, y - height/2, z + length/2);
    rlTexCoord2f((source.x + source.width)/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y + height/2, z + length/2);
    rlTexCoord2f(source.x/texWidth, source.y/texHeight);
    rlVertex3f(x - width/2, y + height/2, z - length/2);
  rlEnd();
  rlSetTexture(0);
end;

var texture: TTexture2d;
    camera: TCamera;


begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - draw cube texture');
  // Define the camera to look into our 3d world
  camera.position := Vector3Create( 0.0, 10.0, 10.0 );
  camera.target := Vector3Create( 0.0, 0.0, 0.0 );
  camera.up := Vector3Create( 0.0, 1.0, 0.0 );
  camera.fovy := 45.0;
  camera.projection := CAMERA_PERSPECTIVE;

  // Load texture to be applied to the cubes sides
  texture := LoadTexture(PChar(GetApplicationDirectory + 'resources/cubicmap_atlas.png'));
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        BeginMode3D(camera);

          // Draw cube with an applied texture
          DrawCubeTexture(texture, Vector3Create( -2.0, 2.0, 0.0 ), 2.0, 4.0, 2.0, WHITE);

          // Draw cube with an applied texture, but only a defined rectangle piece of the texture
          DrawCubeTextureRec(texture, RectangleCreate( 0, texture.height/2, texture.width/2, texture.height/2 ),
          Vector3Create( 2.0, 1.0, 0.0 ), 2.0, 2.0, 2.0, WHITE);

          DrawGrid(10, 1.0);        // Draw a grid
        EndMode3D();
        DrawFPS(10, 10);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  UnloadTexture(texture); // Unload texture
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

