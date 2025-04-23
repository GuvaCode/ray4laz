program textures_sprite_button;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib; 

const
  screenWidth = 800;
  screenHeight = 450;
  NUM_FRAMES = 3; // Number of frames (rectangles) for the button sprite texture

var
  FxButton: TSound;
  Button: TTexture2D;
  FrameHeight: Single;
  SourceRec, BtnBounds: TRectangle;
  BtnState: Integer;
  BtnAction: Boolean;
  MousePoint: TVector2;


begin
  // Initialization
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [textures] example - sprite button');

  InitAudioDevice(); // Initialize audio device

  FxButton := LoadSound(PChar(GetApplicationDirectory + 'resources/buttonfx.wav')); // Load button sound
  Button := LoadTexture(PChar(GetApplicationDirectory + 'resources/button.png')); // Load button texture

  // Define frame rectangle for drawing
  FrameHeight := Button.Height / NUM_FRAMES;
  SourceRec := RectangleCreate(0, 0, Button.Width, FrameHeight);

  // Define button bounds on screen
  BtnBounds := RectangleCreate(ScreenWidth / 2.0 - Button.Width / 2.0, ScreenHeight / 2.0 - Button.Height div NUM_FRAMES / 2.0, Button.Width, FrameHeight);

  //BtnState := 0; // Button state: 0-NORMAL, 1-MOUSE_HOVER, 2-PRESSED
  //BtnAction := False; // Button action should be activated

  MousePoint := Vector2Create(0.0, 0.0);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    MousePoint := GetMousePosition();
    BtnAction := False;

    // Check button state
    if CheckCollisionPointRec(MousePoint, BtnBounds) then
    begin
      if IsMouseButtonDown(MOUSE_BUTTON_LEFT) then
        BtnState := 2
      else
        BtnState := 1;

      if IsMouseButtonReleased(MOUSE_BUTTON_LEFT) then
        BtnAction := True;
    end else
      BtnState := 0;

    if BtnAction then
    begin
        PlaySound(FxButton);

        // TODO: Any desired action
    end;

    // Calculate button frame rectangle to draw depending on button state
    SourceRec.Y := BtnState * FrameHeight;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      DrawTextureRec(Button, SourceRec, Vector2Create(BtnBounds.X, BtnBounds.Y), WHITE); // Draw button frame

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(Button);  // Unload button texture
  UnloadSound(FxButton);  // Unload sound

  CloseAudioDevice();     // Close audio device

  CloseWindow(); // Close window and OpenGL context
end.

