{*******************************************************************************************
*
*   raylib [core] example - 2d camera platformer
*
*   This example has been created using raylib 2.5 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Example contributed by arvyy (@arvyy) and reviewed by Ramon Santamaria (@raysan5)
*
*   Copyright (c) 2019 arvyy (@arvyy)
*   Pascal translation 2020 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program core_2d_camera_platformer;

{$mode objfpc}{$H+}

uses 
{uncomment if necessary}
ray_math,
//ray_rlgl, 
ray_header; 

const
  screenWidth = 800;
  screenHeight = 450;
  G = 400;
  PLAYER_JUMP_SPD = 350.0;
  PLAYER_HOR_SPD = 200.0;

  type
  PPlayer =^TPlayer;
  TPlayer = record
       position: TVector2;
       speed: single;
       canJump: boolean;
  end;

   PEnvItem = ^TEnvItem;
   TEnvItem = record
       rect: TRectangle;
       blocking: integer;
       color: TColor;
   end;

procedure UpdatePlayer(Player:TPlayer; EnvItems:TEnvItem; EnvItemsLength:integer; delta:Single);
var hitObstacle:integer;
    i: integer;
    ei: TEnvItem;
    p:TVector2;
begin

    if IsKeyDown(KEY_LEFT) then  player.position.x -= PLAYER_HOR_SPD*delta;
    if IsKeyDown(KEY_RIGHT) then player.position.x += PLAYER_HOR_SPD*delta;

    if IsKeyDown(KEY_SPACE) and player.canJump then
    begin
        player.speed := -PLAYER_JUMP_SPD;
        player.canJump := false;
    end;

    hitObstacle := 0;

    for i:=0 to  envItemsLength do
    begin
        ei := envItems;// + i;
        Vector2Set(@P,player.position.x,player.position.y);

      if ((ei.blocking and trunc(ei.rect.x)) <= trunc(p.x)) and
      (trunc(ei.rect.x + ei.rect.width) >= trunc(p.x)) and
      (trunc(ei.rect.y) >= trunc(p.y)) and
      (trunc(ei.rect.y) < trunc(p.y + player.speed*delta)) then

        begin
            hitObstacle := 1;
            player.speed := 0.0;
            p.y := ei.rect.y;
        end;
    end;

    if  hitObstacle<1 then
    begin
        player.position.y += player.speed*delta;
        player.speed += G*delta;
        player.canJump := false;
    end
    else player.canJump := true;
end;

procedure UpdateCameraCenter(camera: TCamera2d; player: TPlayer; envItems: TEnvItem; envItemsLength:integer; delta:single; width,height: integer);
begin
  camera.offset := Vector2Create( width/2.0, height/2.0 );
  camera.target := player.position;
end;

procedure UpdateCameraCenterInsideMap(camera: TCamera2d; player: TPlayer; envItems: TEnvItem; envItemsLength: integer; delta: single; width,height: integer);
var minX,minY,MaxX,MaxY:single;
begin
    camera.target := player.position;
    camera.offset := Vector2Create(width/2.0, height/2.0);
    minX := 1000; minY := 1000; maxX := -1000; maxY := -1000;

    for (int i = 0; i < envItemsLength; i++)
    begin
        EnvItem *ei = envItems + i;
        minX = fminf(ei->rect.x, minX);
        maxX = fmaxf(ei->rect.x + ei->rect.width, maxX);
        minY = fminf(ei->rect.y, minY);
        maxY = fmaxf(ei->rect.y + ei->rect.height, maxY);
    end;

    Vector2 max = GetWorldToScreen2D((Vector2){ maxX, maxY }, *camera);
    Vector2 min = GetWorldToScreen2D((Vector2){ minX, minY }, *camera);

    if (max.x < width) camera->offset.x = width - (max.x - width/2);
    if (max.y < height) camera->offset.y = height - (max.y - height/2);
    if (min.x > 0) camera->offset.x = width/2 - min.x;
    if (min.y > 0) camera->offset.y = height/2 - min.y;
end;


{void UpdateCameraCenterSmoothFollow(Camera2D *camera, Player *player, EnvItem *envItems, int envItemsLength, float delta, int width, int height);
void UpdateCameraEvenOutOnLanding(Camera2D *camera, Player *player, EnvItem *envItems, int envItemsLength, float delta, int width, int height);
void UpdateCameraPlayerBoundsPush(Camera2D *camera, Player *player, EnvItem *envItems, int envItemsLength, float delta, int width, int height);
}
begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib - simple project');
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
        DrawText('raylib in lazarus !!!', 20, 20, 10, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

