program core_2d_camera_platformer;

{$mode Delphi}{$H+}
uses
  SysUtils, Math, raylib, raymath;

const
  G = 400;
  PLAYER_JUMP_SPD = 350.0;
  PLAYER_HOR_SPD = 200.0;

type
  TPlayer = record
    Position: TVector2;
    Speed: Single;
    CanJump: Boolean;
  end;

  { TEnvItem }

  TEnvItem = record
    Rect: TRectangle;
    Blocking: Integer;
    Color: TColor;
    constructor Create(Rect: TRectangle; Blocking: Integer; Color: TColor);
  end;

//----------------------------------------------------------------------------------
// Module functions declaration
//----------------------------------------------------------------------------------
procedure UpdatePlayer(var Player: TPlayer; var EnvItems: array of TEnvItem; Delta: Single); forward;
procedure UpdateCameraCenter(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer); forward;
procedure UpdateCameraCenterInsideMap(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer); forward;
procedure UpdateCameraCenterSmoothFollow(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer); forward;
procedure UpdateCameraEvenOutOnLanding(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer); forward;
procedure UpdateCameraPlayerBoundsPush(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer); forward;

type
  TCameraUpdate = procedure(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);


  procedure UpdateCameraCenter(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);
  begin
    Camera.Offset := Vector2Create(Width / 2.0, Height / 2.0);
    Camera.Target := Player.Position;
  end;

  procedure UpdateCameraCenterInsideMap(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);
  var
    I: Integer;
    Ei: ^TEnvItem;
    MinX, MinY, MaxX, MaxY: Single;
    MinV, MaxV: TVector2;
  begin
    Camera.Target := Player.Position;
    Camera.Offset := Vector2Create(Width / 2.0, Height / 2.0);
    MinX := 1000; MinY := 1000; MaxX := -1000; MaxY := -1000;

      for I := 0 to High(EnvItems) do
      begin
        Ei := @EnvItems[I];
        MinX := Min(Ei.Rect.X, MinX);
        MaxX := Max(Ei.Rect.X + Ei.Rect.Width, MaxX);
        MinY := Min(Ei.Rect.Y, MinY);
        MaxY := Max(Ei.Rect.Y + Ei.Rect.Height, MaxY);
      end;

      MaxV := GetWorldToScreen2D(Vector2Create(MaxX, MaxY), Camera);
      MinV := GetWorldToScreen2D(Vector2Create(MinX, MinY), Camera);

      if MaxV.X < Width then
        Camera.Offset.X := Width - (MaxV.X - Width / 2);
      if MaxV.Y < Height then
        Camera.Offset.Y := Height - (MaxV.Y - Height / 2);
      if MinV.X > 0 then
        Camera.Offset.X := Width / 2 - MinV.X;
      if MinV.Y > 0 then
        Camera.Offset.Y := Height / 2 - MinV.Y;
  end;

  var
    MinSpeed: Single = 30;
    MinEffectLength: Single = 10;
    FractionSpeed: Single = 0.8;

  procedure UpdateCameraCenterSmoothFollow(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);
  var
    Diff: TVector2;
    LengthV: Single;
    Speed: Single;
  begin
    Camera.Offset := Vector2Create(Width / 2.0, Height / 2.0);
    Diff := Vector2Subtract(Player.Position, Camera.Target);
    LengthV := Vector2Length(Diff);

    if LengthV > MinEffectLength then
    begin
      Speed := Max(FractionSpeed * LengthV, MinSpeed);
      Camera.Target := Vector2Add(Camera.Target, Vector2Scale(Diff, Speed * Delta / LengthV));
    end;
  end;

  var
    EvenOutSpeed: Single = 700;
    EveningOut: Boolean = False;
    EvenOutTarget: Single = 0;

  procedure UpdateCameraEvenOutOnLanding(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);
  begin
    Camera.Offset := Vector2Create(Width / 2.0, Height / 2.0);
    Camera.Target.X := Player.Position.X;

    if EveningOut then
    begin
      if evenOutTarget > Camera.Target.Y then
      begin
        Camera.Target.Y := Camera.Target.Y + EvenOutSpeed * Delta;

        if Camera.Target.Y > EvenOutTarget then
        begin
          Camera.Target.Y := EvenOutTarget;
          EveningOut := False;
        end;
      end else
      begin
        Camera.Target.Y := Camera.Target.Y - EvenOutSpeed * Delta;

        if Camera.Target.Y < EvenOutTarget then
        begin
          Camera.Target.Y := EvenOutTarget;
          EveningOut := False;
        end;
      end;
    end else
    begin
      if Player.CanJump and (Player.Speed = 0) and (Player.Position.Y <> Camera.Target.Y) then
      begin
        EveningOut := True;
        EvenOutTarget := Player.Position.Y;
      end;
    end;
  end;

  var
    BBox: TVector2 = (X: 0.2; Y: 0.2);

  procedure UpdateCameraPlayerBoundsPush(var Camera: TCamera2D; var Player: TPlayer; const EnvItems: array of TEnvItem; Delta: Single; Width, Height: Integer);
  var
    BBoxWorldMin, BBoxWorldMax: TVector2;
  begin
    BBoxWorldMin := GetScreenToWorld2D(Vector2Create((1 - BBox.X) * 0.5 * Width, (1 - BBox.Y) * 0.5 * Height), Camera);
    BBoxWorldMax := GetScreenToWorld2D(Vector2Create((1 - BBox.X) * 0.5 * Width, (1 - BBox.Y) * 0.5 * Height), Camera);
    Camera.Offset := Vector2Create((1 - BBox.X) * 0.5 * Width, (1 - BBox.Y) * 0.5 * Height);

    if Player.Position.X < BBoxWorldMin.X then
      Camera.Target.X := Player.Position.X;
    if Player.Position.Y < BBoxWorldMin.Y then
      Camera.Target.Y := Player.Position.Y;
    if Player.Position.X > BBoxWorldMax.X then
      Camera.Target.X := BBoxWorldMin.X + (Player.Position.X - BBoxWorldMax.X);
    if Player.Position.Y > BBoxWorldMin.Y then
      Camera.Target.Y := BBoxWorldMin.Y + (Player.Position.Y - BBoxWorldMax.Y);
  end;

  procedure UpdatePlayer(var Player: TPlayer; var EnvItems: array of TEnvItem; Delta: Single);
  var
    I: Integer;
    HitObstacle: Integer;
    Ei: ^TEnvItem;
    P: ^TVector2;
  begin
    if IsKeyDown(KEY_LEFT) then
      Player.Position.X := Player.Position.X - PLAYER_HOR_SPD * Delta;
    if IsKeyDown(KEY_RIGHT) then
      Player.Position.X := Player.Position.X + PLAYER_HOR_SPD * Delta;
    if IsKeyDown(KEY_SPACE) and Player.canJump then
    begin
      Player.Speed := -PLAYER_JUMP_SPD;
      Player.CanJump := False;
    end;

    HitObstacle := 0;
    for I := 0 to High(EnvItems) do
    begin
      Ei := @EnvItems[i];
      P := @(Player.Position);
      if (Ei.Blocking <> 0) and
        (Ei.Rect.X <= P.X) and
        (Ei.Rect.X + Ei.Rect.Width >= P.X) and
        (Ei.Rect.Y >= P.Y) and
        (Ei.Rect.Y <= P.Y + Player.Speed * Delta) then
      begin
        HitObstacle := 1;
        Player.Speed := 0.0;
        P.Y := Ei.Rect.Y;
      end;
    end;

    if hitObstacle = 0 then
    begin
      Player.Position.Y := Player.Position.Y + Player.Speed * Delta;
      Player.Speed := Player.Speed + G * Delta;
      Player.CanJump := False;
    end else
      Player.CanJump := True;
  end;


//------------------------------------------------------------------------------------
// Program main entry point
//------------------------------------------------------------------------------------

const
  ScreenWidth = 800;
  ScreenHeight = 450;
var
  Player: TPlayer;
  Camera: TCamera2D;
  EnvItems: array of TEnvItem;
  CameraUpdaters: array of TCameraUpdate;
  CameraDescriptions: array of string;
  CameraOption: Integer;
  DeltaTime: Single;
  I: Integer;
  PlayerRect: TRectangle;

{ TEnvItem }

constructor TEnvItem.Create(Rect: TRectangle; Blocking: Integer; Color: TColor);
begin
  Self.Rect := Rect;
  Self.Blocking := Blocking;
  Self.Color := Color;
end;

begin
  // Initialization
  //---------------------------------------------------------------------------------------------
  SetConfigFlags(FLAG_WINDOW_HIGHDPI or FLAG_MSAA_4X_HINT);
  InitWindow(ScreenWidth, ScreenHeight, 'raylib [core] example - 2d camera');

  Player := Default(TPlayer);
  Player.Position := Vector2Create(400, 280);
  Player.Speed := 0;
  Player.CanJump := False;

  EnvItems := [
    TEnvItem.Create(RectangleCreate(  0,   0, 1000, 400), 0, LIGHTGRAY),
    TEnvItem.Create(RectangleCreate(  0, 400, 1000, 200), 1, GRAY),
    TEnvItem.Create(RectangleCreate(300, 200,  400,  10), 1, GRAY),
    TEnvItem.Create(RectangleCreate(250, 300,  100,  10), 1, GRAY),
    TEnvItem.Create(RectangleCreate(650, 300,  100,  10), 1, GRAY),
    TEnvItem.Create(RectangleCreate(450, 50,   100,  10), 1, BEIGE)
  ];

  Camera := Default(TCamera2D);
  Camera.Target := Player.Position;
  Camera.Offset := Vector2Create(ScreenWidth / 2.0, ScreenHeight / 2.0);
  Camera.Rotation := 0.0;
  Camera.Zoom := 1.0;

  CameraUpdaters := [
    UpdateCameraCenter,
    UpdateCameraCenterInsideMap,
    UpdateCameraCenterSmoothFollow,
    UpdateCameraEvenOutOnLanding,
    UpdateCameraPlayerBoundsPush
  ];

  CameraOption := 0;

  CameraDescriptions := [
    'Follow player center',
    'Follow player center, but clamp to map edges',
    'Follow player center; smoothed',
    'Follow player center horizontally; updateplayer center vertically after landing',
    'Player push camera on getting too close to screen edge'
  ];

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //---------------------------------------------------------------------------------------------

  // Main game loop
  while not WindowShouldClose() do // Detect window close button or ESC key
  begin
    // Update
    //-------------------------------------------------------------------------------------------
    DeltaTime := GetFrameTime();

    UpdatePlayer(Player, EnvItems, DeltaTime);

    Camera.Zoom := Camera.Zoom + GetMouseWheelMove() * 0.05;

    if Camera.Zoom > 3.0 then
      Camera.Zoom := 3.0
    else if Camera.Zoom < 0.25 then
      Camera.Zoom := 0.25;

    if IsKeyPressed(KEY_R) then
    begin
      Camera.Zoom := 1.0;
      Player.Position := Vector2Create(400, 280);
    end;

    if IsKeyPressed(KEY_C) then
      CameraOption := (CameraOption + 1) mod Length(CameraUpdaters);

    // Call update camera function by its pointer
    CameraUpdaters[CameraOption](Camera, Player, EnvItems, DeltaTime, ScreenWidth, ScreenHeight);
    //-------------------------------------------------------------------------------------------

    // Draw
    //-------------------------------------------------------------------------------------------
    BeginDrawing();

      ClearBackground(LIGHTGRAY);

      BeginMode2D(Camera);

        for I := 0 to High(EnvItems) do
          DrawRectangleRec(EnvItems[I].Rect, EnvItems[I].Color);

        PlayerRect := RectangleCreate(Player.Position.X - 20, Player.Position.Y - 40, 40, 40);
        DrawRectangleRec(PlayerRect, RED);

      EndMode2D();

      DrawText(UTF8String('Controls:'), 20, 20, 10, BLACK);
      DrawText(UTF8String('- Right/Left to move'), 40, 40, 10, DARKGRAY);
      DrawText(UTF8String('- Space to jump'), 40, 60, 10, DARKGRAY);
      DrawText(UTF8String('- Mouse Wheel to Zoom in-out, R to reset zoom'), 40, 80, 10, DARKGRAY);
      DrawText(UTF8String('- C to change camera mode'), 40, 100, 10, DARKGRAY);
      DrawText(UTF8String('Current camera mode:'), 20, 120, 10, BLACK);
      DrawText(PUTF8Char(UTF8String(CameraDescriptions[CameraOption])), 40, 140, 10, DARKGRAY);
    EndDrawing();
    //-------------------------------------------------------------------------------------------
  end;

  // De-Initialization
  //---------------------------------------------------------------------------------------------
  CloseWindow(); // Close window and OpenGL context
  //---------------------------------------------------------------------------------------------
end.


{ TEnvItem }

{constructor TEnvItem.Create(Rect: TRectangle; Blocking: Integer; Color: TColor);
begin

end;}


end.

