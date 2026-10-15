{*******************************************************************************************
*
*   raylib [text] example - animations
*
*   Example complexity rating: [★☆☆☆] 1/4
*
*   Example originally created with raylib 6.0, last time updated with raylib 6.0
*
*   Example contributed by Gabriel Piangers (@gabriel-piangers) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2026 Gabriel Piangers (@gabriel-piangers)
*   Pascal conversion 2026 Vadim Gunko
*
********************************************************************************************}

program text_animations;

{$MODE OBJFPC}{$H+}

uses
  raylib, reasings, raymath, SysUtils;

const
  MAX_TEXT_LENGTH = 64;
  TEXT_SPACING    = 4;

type
  // Animated text type
  TAnimatedText = record
    str: array[0..MAX_TEXT_LENGTH - 1] of Char; // Text to animate
    position: TVector2;                          // Text position on screen
    delta: Single;                               // Intensity of the effect
    offset: Single;                              // Time offset (in frames) between each char
    duration: Single;                            // Time duration (in frames) of the animation
  end;

var
  screenWidth, screenHeight: Integer;
  animations: array[0..2] of PAnsiChar = ('Waving', 'Flashing', 'Shaking');
  animationsSize: Integer;
  currentAnimation: Integer;
  text: TAnimatedText;
  frameCount: Integer;
  glyphPosition: array[0..MAX_TEXT_LENGTH - 1] of TVector2;
  glyphAlpha: array[0..MAX_TEXT_LENGTH - 1] of Byte;
  i: Integer;
  offsetX: Single;
  time: Single;
  deltaX, deltaY: Single;
  c: TColor;
begin
  // Initialization
  screenWidth  := 800;
  screenHeight := 450;

  InitWindow(screenWidth, screenHeight, 'raylib [text] example - animations');

  animationsSize   := 3;
  currentAnimation := 0;

  // text = { "This is an animated text!", { 200, 250 }, 24.0f, 6, 60 };
  StrPLCopy(text.str, 'This is an animated text!', MAX_TEXT_LENGTH - 1);
  text.position := Vector2Create(200, 250);
  text.delta    := 24.0;
  text.offset   := 6.0;
  text.duration := 60.0;

  frameCount := 0;

  // Обнуление массивов
  for i := 0 to MAX_TEXT_LENGTH - 1 do
  begin
    glyphPosition[i] := Vector2Create(0, 0);
    glyphAlpha[i]    := 0;
  end;

  SetTargetFPS(60);

  // Main game loop
  while not WindowShouldClose() do
  begin
    // Update
    Inc(frameCount);

    if IsKeyPressed(KEY_S) then
      text.delta := Clamp(text.delta - 2.0, 10.0, 60.0)
    else if IsKeyPressed(KEY_W) then
      text.delta := Clamp(text.delta + 2.0, 10.0, 60.0);

    if IsKeyPressed(KEY_A) then
      text.duration := Trunc(Clamp(text.duration - 5.0, 20.0, 120.0))
    else if IsKeyPressed(KEY_D) then
      text.duration := Trunc(Clamp(text.duration + 5.0, 20.0, 120.0));

    if IsKeyPressed(KEY_Z) then
      text.offset := Clamp(text.offset - 1.0, 0.0, 15.0)
    else if IsKeyPressed(KEY_X) then
      text.offset := Clamp(text.offset + 1.0, 0.0, 15.0);

    if IsKeyPressed(KEY_Q) then
    begin
      if currentAnimation > 0 then
        Dec(currentAnimation)
      else
        currentAnimation := animationsSize - 1;
    end
    else if IsKeyPressed(KEY_E) then
      currentAnimation := (currentAnimation + 1) mod animationsSize;

    // Расчёт позиции и альфы каждой глифы
    offsetX := 0;
    for i := 0 to TextLength(PAnsiChar(@text.str)) - 1 do
    begin
      glyphPosition[i] := Vector2Create(text.position.x + offsetX, text.position.y);
      glyphAlpha[i]    := 255;

      offsetX := offsetX + MeasureText(TextSubtext(PAnsiChar(@text.str), i, 1), 30) + TEXT_SPACING;
    end;

    case currentAnimation of
      0: // Waving animation
      begin
        for i := 0 to TextLength(PAnsiChar(@text.str)) - 1 do
        begin
          time := Trunc(frameCount + i * text.offset) mod Trunc(text.duration);

          if time < text.duration / 2 then
            glyphPosition[i].y := EaseSineIn(time, text.position.y, text.delta, text.duration)
          else
            glyphPosition[i].y := EaseSineOut(time, text.position.y + text.delta, -text.delta, text.duration);
        end;
      end;

      1: // Flashing animation
      begin
        for i := 0 to TextLength(PAnsiChar(@text.str)) - 1 do
        begin
          time := Trunc(frameCount + i * text.offset) mod Trunc(text.duration);
          glyphAlpha[i] := Byte(Trunc(EaseSineInOut(time, 255.0, -255.0, text.duration)));
        end;
      end;

      2: // Shaking animation
      begin
        for i := 0 to TextLength(PAnsiChar(@text.str)) - 1 do
        begin
          time   := Trunc(frameCount + i * text.offset) mod Trunc(text.duration);
          deltaX := GetRandomValue(-Trunc(text.delta / 10), Trunc(text.delta / 10));
          deltaY := GetRandomValue(-Trunc(text.delta / 10), Trunc(text.delta / 10));

          if time < (text.duration / 2) then
          begin
            glyphPosition[i].x := EaseLinearNone(time, glyphPosition[i].x, deltaX, text.duration / 2);
            glyphPosition[i].y := EaseLinearNone(time, text.position.y, deltaY, text.duration / 2);
          end
          else
          begin
            glyphPosition[i].x := EaseLinearNone(time - text.duration / 2, glyphPosition[i].x + deltaX, -deltaX, text.duration / 2);
            glyphPosition[i].y := EaseLinearNone(time - text.duration / 2, text.position.y + deltaY, -deltaY, text.duration / 2);
          end;
        end;
      end;
    end;

    // Draw
    BeginDrawing();

    ClearBackground(RAYWHITE);

    DrawText(TextFormat('Duration (A | D): %d', Trunc(text.duration)), 20, 20, 20, GRAY);
    DrawText(TextFormat('Delta (W | S): %.1f', text.delta), 20, 50, 20, GRAY);
    DrawText(TextFormat('Offset (Z | X): %.1f', text.offset), 20, 80, 20, GRAY);
    DrawText(TextFormat('Curent Animation (Q | E): %s', animations[currentAnimation]), 20, 110, 20, GRAY);

    for i := 0 to TextLength(PAnsiChar(@text.str)) - 1 do
    begin
      c := ColorCreate(200, 122, 255, glyphAlpha[i]);
      DrawTextEx(GetFontDefault(), TextSubtext(PAnsiChar(@text.str), i, 1),
        glyphPosition[i], GetFontDefault().baseSize * 3.0, 0, c);
    end;

    EndDrawing();
  end;

  // De-Initialization
  CloseWindow();
end.
