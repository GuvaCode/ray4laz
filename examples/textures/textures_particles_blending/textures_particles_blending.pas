program textures_particles_blending;

{$MODE objfpc}{$H+}

uses cmem, raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

const
  MAX_PARTICLES = 200;

type
  Particle = record
    position: TVector2;
    color: TColorB;
    alpha: Single;
    size: Single;
    rotation: Single;
    active: Boolean; // NOTE: Use it to activate/deactive particle
  end;

var
  mouseTail: array [0 .. (MAX_PARTICLES) - 1] of Particle;
  i: integer;
  gravity: Single;
  smoke: TTexture2D;
  blending: integer;

begin
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - particles blending');

  for i := 0 to MAX_PARTICLES - 1 do
  begin
    mouseTail[i].position := Vector2Create(0, 0);
    mouseTail[i].color := ColorCreate(GetRandomValue(0, 255), GetRandomValue(0, 255),
      GetRandomValue(0, 255), 255);
    mouseTail[i].alpha := 1.0;
    mouseTail[i].size := Single(GetRandomValue(1, 30)) / 20.0;
    mouseTail[i].rotation := GetRandomValue(0, 360);
    mouseTail[i].active := false;
  end;

  gravity := 3.0;
  smoke := LoadTexture(PChar(GetApplicationDirectory + 'resources/spark_flame.png'));
  blending := BLEND_ALPHA;
  SetTargetFPS(60);

  while not WindowShouldClose do
  begin
    for i := 0 to MAX_PARTICLES - 1 do
    begin
      if not mouseTail[i].active then
      begin
        mouseTail[i].active := true;
        mouseTail[i].alpha := 1.0;
        mouseTail[i].position := GetMousePosition;
        break;
      end;
    end;

    for i := 0 to MAX_PARTICLES - 1 do
    begin
      if mouseTail[i].active then
      begin
        mouseTail[i].position.y := mouseTail[i].position.y + gravity;
        mouseTail[i].alpha := mouseTail[i].alpha - 0.01;
        if mouseTail[i].alpha <= 0.0 then
          mouseTail[i].active := false;
        mouseTail[i].rotation := mouseTail[i].rotation + 5.0;
      end;
    end;

    if IsKeyPressed(KEY_SPACE) then
    begin
      if blending = BLEND_ALPHA then
        blending := BLEND_ADDITIVE
      else
        blending := BLEND_ALPHA;
    end;

    BeginDrawing;
      ClearBackground(DARKGRAY);
      BeginBlendMode(blending);
        for i := 0 to MAX_PARTICLES - 1 do
        begin
          if mouseTail[i].active then
            DrawTexturePro(smoke, RectangleCreate(0, 0, smoke.width, smoke.height),
              RectangleCreate(Trunc(mouseTail[i].position.x),
              Trunc(mouseTail[i].position.y), Trunc(smoke.width * mouseTail[i].size),
              Trunc(smoke.height * mouseTail[i].size)),
              Vector2Create(smoke.width * mouseTail[i].size / 2,
              smoke.height * mouseTail[i].size / 2), mouseTail[i].rotation,
              Fade(mouseTail[i].color, mouseTail[i].alpha));
        end;
      EndBlendMode();
      DrawText('PRESS SPACE to CHANGE BLENDING MODE', 180, 20, 20, BLACK);
      if blending = BLEND_ALPHA then
        DrawText('ALPHA BLENDING', 290, screenHeight - 40, 20, BLACK)
      else
        DrawText('ADDITIVE BLENDING', 280, screenHeight - 40, 20, RAYWHITE);
    EndDrawing();
  end;

  UnloadTexture(smoke);
  CloseWindow();
end.
