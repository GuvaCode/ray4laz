program game;

{$mode objfpc}{$H+}

uses cmem, ray_headers,ray_sprite, math, Classes;

const
 screenWidth = 800;
 screenHeight = 450;
 MapSize = 6;
 Delta = 0.200;
  AnimSpeed = 0.280;

 var
 Camera: TCamera2D;
 a, b: Integer;
  Engine: TRay2DEngine;

  Texture: TRayTexture;

  Copas: TRaySprite;
  Moeda: TRaySprite;

  Ground: array of array of TRaySprite;

  Tree: array of TRaySprite;

  Player: TRayAnimatedSprite;



begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib pascal - basic window');
 SetTargetFPS(60);

 Engine:= TRay2DEngine.Create;
 Engine.VisibleHeight:=screenHeight;
 Engine.VisibleWidth:=screenWidth;
// Engine.Camera:=camera;
 Texture := TRayTexture.Create;
 Texture.LoadFromFile('0.png', 38, 38);
 Texture.LoadFromFile('3.png', 38, 38);
 Texture.LoadFromFile('bg.png', 256, 256);
 Texture.LoadFromFile('Guest.png', 128, 192);
Texture.LoadFromFile('tree.png', 256, 512);
 Texture.LoadFromFile('snowtree.png', 128, 256);

 Copas := TRaySprite.Create(Engine, Texture);
 Copas.TextureIndex := 0;
 Copas.Z := 4;
 Copas.Scale:=1.0;
 Moeda := TRaySprite.Create(Engine, Texture);
 Moeda.TextureIndex := 1;
 Moeda.Z := 5;

SetLength(Ground, MapSize + 1, MapSize + 1);

 for a := 0 to MapSize do
  begin

    for b := 0 to MapSize do
    begin

      Ground[a, b] := TRaySprite.Create(Engine, Texture);
      Ground[a, b].TextureName := 'bg.png';
      Ground[a, b].X := a * 256;
      Ground[a, b].Y := b * 256;
      Ground[a, b].Z := 1;
     // Ground[a, b].
      //:= Rect(-300, -300, 800, 600);
     Ground[a, b].Scale:=1.0;
    end;

  end;

  SetLength(Tree, 4);

  for a := 0 to 3 do
  begin
    Tree[a] := TRaySprite.Create(Engine, Texture);
    Tree[a].TextureName := 'tree.png';
    Tree[a].X := 350;
    Tree[a].Y := a * 450;
    Tree[a].Z := Tree[a].Y + 220;
    Tree[a].Scale:=1.0;
  end;

  Tree[3].TextureName := 'snowtree.png';
  Tree[3].X := 10;
  Tree[3].Y := 80;
  Tree[3].Z := Tree[3].Y + 115;
  Tree[3].Scale:=1.0;

  Player := TRayAnimatedSprite.Create(Engine, Texture);
  Player.TextureName := 'Guest.png';

  Player.PatternHeight := 48;
  Player.PatternWidth := 32;
  Player.Scale:=1.0;
  Player.X := 0;
  Player.Y := 0;
  Player.Z := 999;
  Player.AnimPos := 1;



  camera.target := Vector2Create(player.x + 20, player.y + 20);
  camera.offset := Vector2Create(screenWidth / 2.0, screenHeight / 2.0);
  camera.rotation := 0.0;
  camera.zoom := 1.0;





 while not WindowShouldClose() do
 begin
  //updae ----------------------------------------------
    Engine.Move(GetFPS);

  if IsKeyDown(KEY_RIGHT) then
    begin
      player.x := player.x + 1 ;
      Player.DoAnim(True, 9, 12, AnimSpeed);
    end;
    if IsKeyDown(KEY_LEFT) then
      begin
          Player.X := Player.X-1;
      Player.DoAnim(True, 5, 8, AnimSpeed);
      end;
  if IsKeyDown(KEY_UP) then
    begin
      player.Y := player.Y-1 ;
      Player.DoAnim(True, 13, 16, AnimSpeed);
    end;
    if IsKeyDown(KEY_DOWN) then
      begin
          Player.Y := Player.Y+1;
      Player.DoAnim(True, 1, 4, AnimSpeed);
      end;

    camera.target := Vector2Create(player.x , player.y );
      if (Moeda.Y >= 0) and (Moeda.Y < 300) then
  begin
    Moeda.Y := Moeda.Y + cos(25) * 2;
    Moeda.X := Moeda.X * 2;
  end;
  //
  BeginDrawing();
  ClearBackground(BLACK);
   DrawText('raylib in lazarus !!!', 20, 20, 20, SKYBLUE);

   BeginMode2D(camera);
   Engine.Draw();
   EndMode2D;
  EndDrawing(); 
 end;
CloseWindow(); 

end.

