program textures_npatch_drawing;

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
var
  NPatchTexture: TTexture2D;
  MousePosition, Origin: TVector2;
  DstRec1, DstRec2, DstRecH, DstRecV: TRectangle;
  NinePatchInfo1, NinePatchInfo2, H3PatchInfo, V3PatchInfo: TNPatchInfo;

begin
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [textures] example - N-patch drawing');

  // NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
  NPatchTexture := LoadTexture(PChar(GetApplicationDirectory + 'resources/ninepatch_button.png'));

  MousePosition := Default(TVector2);
  Origin := Vector2Create(0, 0);

  // Position and size of the n-patches
  DstRec1 := RectangleCreate(480.0, 160.0, 32.0, 32.0);
  DstRec2 := RectangleCreate(160.0, 160.0, 32.0, 32.0);
  DstRecH := RectangleCreate(160.0, 93.0, 32.0, 32.0);
  DstRecV := RectangleCreate(92.0, 160.0, 32.0, 32.0);

  // A 9-patch (NPATCH_NINE_PATCH) changes its sizes in both axis
  NinePatchInfo1.source:=RectangleCreate(0.0,0.0,64.0,64.0);
  NinePatchInfo1.left:=12;
  NinePatchInfo1.top:=40;
  NinePatchInfo1.right:=12;
  NinePatchInfo1.bottom:=12;
  NinePatchInfo1.layout:=NPATCH_NINE_PATCH;

  NinePatchInfo2.source:=RectangleCreate(0.0,128.0,64.0,64.0);
  NinePatchInfo2.left:=16;
  NinePatchInfo2.top:=16;
  NinePatchInfo2.right:=16;
  NinePatchInfo2.bottom:=16;
  NinePatchInfo2.layout:=NPATCH_NINE_PATCH;


  // A horizontal 3-patch (NPATCH_THREE_PATCH_HORIZONTAL) changes its sizes along the x axis only
  H3PatchInfo.source:=RectangleCreate(0.0,64.0,64.0,64.0);
  H3PatchInfo.left:=8;
  H3PatchInfo.top:=8;
  H3PatchInfo.right:=8;
  H3PatchInfo.bottom:=8;
  H3PatchInfo.layout:=NPATCH_THREE_PATCH_HORIZONTAL;

  // A vertical 3-patch (NPATCH_THREE_PATCH_VERTICAL) changes its sizes along the y axis only
  V3PatchInfo.source:=RectangleCreate(0.0,192.0,64.0,64.0);
  V3PatchInfo.left:=6;
  V3PatchInfo.top:=6;
  V3PatchInfo.right:=6;
  V3PatchInfo.bottom:=6;
  V3PatchInfo.layout:=NPATCH_THREE_PATCH_VERTICAL;

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    MousePosition := GetMousePosition();

    // Resize the n-patches based on mouse position
    DstRec1.Width := MousePosition.X - DstRec1.X;
    DstRec1.Height := MousePosition.Y - DstRec1.Y;
    DstRec2.Width := MousePosition.X - DstRec2.X;
    DstRec2.Height := MousePosition.Y - DstRec2.Y;
    DstRecH.Width := MousePosition.X - DstRecH.X;
    DstRecV.Height := MousePosition.Y - DstRecV.Y;

    // Set a minimum width and/or height
    if DstRec1.Width < 1.0 then DstRec1.Width := 1.0;
    if DstRec1.Width > 300.0 then DstRec1.Width := 300.0;
    if DstRec1.Height < 1.0 then DstRec1.Height := 1.0;
    if DstRec2.Width < 1.0 then DstRec2.Width := 1.0;
    if DstRec2.Width > 300.0 then DstRec2.Width := 300.0;
    if DstRec2.Height < 1.0 then DstRec2.Height := 1.0;
    if DstRecH.Width < 1.0 then DstRecH.Width := 1.0;
    if DstRecV.Height < 1.0 then DstRecV.Height := 1.0;
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(RAYWHITE);

      // Draw the n-patches
      DrawTextureNPatch(NPatchTexture, NinePatchInfo2, DstRec2, Origin, 0.0, WHITE);
      DrawTextureNPatch(NPatchTexture, NinePatchInfo1, DstRec1, Origin, 0.0, WHITE);
      DrawTextureNPatch(NPatchTexture, H3PatchInfo, DstRecH, Origin, 0.0, WHITE);
      DrawTextureNPatch(NPatchTexture, V3PatchInfo, DstRecV, Origin, 0.0, WHITE);

      // Draw the source texture
      DrawRectangleLines(5, 88, 74, 266, BLUE);
      DrawTexture(nPatchTexture, 10, 93, WHITE);
      DrawText('TEXTURE', 15, 360, 10, DARKGRAY);

      DrawText('Move the mouse to stretch or shrink the n-patches', 10, 20, 20, DARKGRAY);

    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  UnloadTexture(NPatchTexture);

  CloseWindow(); // Close window and OpenGL context
end.

