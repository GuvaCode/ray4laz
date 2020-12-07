program project1;
 {$mode objfpc}{$H+}

uses cmem, ray.engine2D, ray.headers,  sysutils, Types;

type

{ TBullet }

TBullet = class(TSprite)
 private
 public
   procedure DoMove(MoveCount: Double); override;
   procedure DoCollision(const Sprite: TSprite); override;
 end;



const
screenWidth = 800;
screenHeight = 600;

var camera: TCamera2D;
    Texture: TSpriteTexture;
    SpriteEngine: TSpriteEngine;
    TestSprite:TSprite;
    rest:TRectangle;
    bull:TBullet;
    player:TAnimatedSprite;
{ TBullet }

procedure TBullet.DoMove(MoveCount: Double);
begin
  inherited DoMove(MoveCount);
  CollidePos:=Point(Round(X), Round(y));
end;

procedure TBullet.DoCollision(const Sprite: TSprite);
begin
  inherited DoCollision(Sprite);
  angle:=angle-1;
end;


begin
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - 2d camera');
  SetTargetFPS(60);
  Camera.zoom:=1;

  SpriteEngine:=TSpriteEngine.Create;
  SpriteEngine.VisibleHeight:=1024;
  SpriteEngine.VisibleWidth:=1024;
  SpriteEngine.Camera := Camera;

  Texture := TSpriteTexture.Create;
  Texture.LoadFromFile('planet4.png', 256, 256);
  Texture.LoadFromFile('planet3.png', 256, 256);
  Texture.LoadFromFile('Guest.png', 32, 48);

  TestSprite := TSprite.Create(SpriteEngine, Texture);
  TestSprite.TextureIndex := 0;
  TestSprite.Z := 4;
  TestSprite.X:=256;
  TestSprite.y:=256;
  TestSprite.Visible:=true;
  TestSprite.Collisioned:=true;
  TestSprite.CollideRadius:=40;

  bull:=TBullet.Create(SpriteEngine, Texture);
  bull.TextureIndex:=0;
  bull.Z := 4;
  bull.X:=256;
  bull.y:=256;
  bull.Visible:=true;
  bull.Collisioned:=true;
  bull.CollideRadius:=40;


  Player := TAnimatedSprite.Create(SpriteEngine, Texture);
  Player.TextureIndex := 2;

  Player.PatternHeight := 48;
  Player.PatternWidth := 32;

  Player.AnimSpeed:=5;
  Player.X := 100;
  Player.Y := 100;
  Player.Z := 0;
  Player.AnimPos := 1;



  while not WindowShouldClose do
   begin
      BeginDrawing();
      ClearBackground(BLACK);
      BeginMode2D(Camera);
      TestSprite.CollidePos:=Point(Round(TestSprite.X), Round(TestSprite.y));
      bull.Collision;
      SpriteEngine.Draw;

      //GREEN
     SpriteEngine.Move(1);
      EndMode2D;
      EndDrawing;
      TestSprite.Angle:=TestSprite.Angle+0.5;
   Player.DoAnim(True, 9, 12, 3);
   SpriteEngine.WorldX := (Player.X - 400);
  SpriteEngine.worldY := (Player.Y - 250);
   end;

end.

