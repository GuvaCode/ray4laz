unit mUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, OpenGLContext,
  gl, glu, glext, raylib;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
  private
       textureId: GLuint;                    // OpenGL texture ID for raylib rendering
    raylibInitialized: Boolean;           // Flag to track raylib initialization
    Camera: TCamera3D;                    // 3D camera for raylib scene
    cubePosition: TVector3;               // Position of the 3D cube
    procedure RenderTextureToScreen;      // Renders texture to OpenGL control
    procedure InitializeRaylib;           // Initializes raylib window
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean); // Idle event handler for continuous rendering
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize OpenGL context
   OpenGLControl1.MakeCurrent();

   // Set up camera parameters
   Camera.position := Vector3Create(10.0, 10.0, 10.0);  // Camera position in 3D space
   Camera.target := Vector3Create(0.0, 0.0, 0.0);       // Camera look-at target
   Camera.up := Vector3Create(0.0, 1.0, 0.0);           // Camera up vector
   Camera.fovy := 45.0;                                 // Camera field of view
   Camera.projection := CAMERA_PERSPECTIVE;             // Perspective projection

   cubePosition := Vector3Create(0.0, 0.0, 0.0);        // Cube position at origin

   // Generate OpenGL texture for raylib rendering
   glGenTextures(1, @textureId);

   raylibInitialized := False;             // Raylib not initialized yet

   // Raylib will be initialized on first Paint event

   // Enable idle event handling for continuous rendering
   Application.OnIdle := @ApplicationIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   // Clean up resources
   glDeleteTextures(1, @textureId);        // Delete OpenGL texture

   // Close raylib window if it was initialized
   if raylibInitialized then
     CloseWindow();
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  Image: TImage;
begin
  // Ensure OpenGL context is current
  OpenGLControl1.MakeCurrent();

  // Initialize raylib on first paint call
  InitializeRaylib;

  if not raylibInitialized then
    Exit;

  // Update camera (orbital camera mode)
  UpdateCamera(@camera, CAMERA_ORBITAL);

  // Render scene using raylib to offscreen buffer
  BeginDrawing();
    ClearBackground(RAYWHITE);            // Clear background to white

    // 3D rendering section
    BeginMode3D(camera);
      DrawCube(cubePosition, 2.0, 2.0, 2.0, RED);        // Draw red cube
      DrawCubeWires(cubePosition, 2.0, 2.0, 2.0, MAROON); // Draw cube wireframe
      DrawGrid(10, 1.0);                  // Draw grid
    EndMode3D();

    // 2D overlay rendering
    DrawFps(10, 10);                      // Draw FPS counter
    DrawText('WOW! raylib on MEMORY BUFFER!!!', 40, 40, 20, MAROON); // Draw text
  EndDrawing();

  // Capture rendered image from raylib's framebuffer
  Image := LoadImageFromScreen();

  // Bind texture for updating
  glBindTexture(GL_TEXTURE_2D, textureId);

  // Set texture parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  // Load image data into OpenGL texture
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Image.width, Image.height, 0,
               GL_RGBA, GL_UNSIGNED_BYTE, Image.data);

  // Clear OpenGL buffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glClearColor(0.0, 0.0, 0.0, 1.0);      // Clear to black

  // Render the texture to OpenGL control
  RenderTextureToScreen;

  // Clean up image resources
  UnloadImage(Image);

  OpenGLControl1.SwapBuffers;             // Swap front and back buffers

end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
    if raylibInitialized then
  begin
    // Update raylib window size when control is resized
    SetWindowSize(OpenGLControl1.Width, OpenGLControl1.Height);
  end;

  // Update OpenGL viewport to match control size
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
  OpenGLControl1.Invalidate;
end;

procedure TForm1.RenderTextureToScreen;
begin
  // Set up orthographic projection for 2D rendering
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.Height, 0, -1, 1);

  // Set up modelview matrix
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  // Enable and bind texture
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, textureId);

  // Render texture as a quad covering the entire control
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex2f(0, 0);                    // Bottom-left
    glTexCoord2f(1, 1); glVertex2f(OpenGLControl1.Width, 0); // Bottom-right
    glTexCoord2f(1, 0); glVertex2f(OpenGLControl1.Width, OpenGLControl1.Height); // Top-right
    glTexCoord2f(0, 0); glVertex2f(0, OpenGLControl1.Height); // Top-left
  glEnd();

  glDisable(GL_TEXTURE_2D);               // Disable texture after rendering
end;

procedure TForm1.InitializeRaylib;
begin
  if not raylibInitialized then
  begin
    // Initialize raylib with current control dimensions for offscreen rendering
    InitWindow(OpenGLControl1.Width, OpenGLControl1.Height, 'raylib offscreen');
    raylibInitialized := True;            // Mark raylib as initialized
  end;
end;

procedure TForm1.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  Done := False; // Continue receiving idle events for continuous rendering
  OpenGLControl1.Invalidate;              // Force repaint
end;

end.

