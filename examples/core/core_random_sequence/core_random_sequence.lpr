{*******************************************************************************************
*
*   raylib [core] example - Generates a random sequence
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example contributed by Dalton Overmyer (@REDl3east) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2023 Dalton Overmyer (@REDl3east)
*   pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}

// is not completed example

program core_random_sequence;

{$mode objfpc}{$H+}

uses 
cmem, 
{uncomment if necessary}
//raymath, 
//rlgl, 
raylib, math;

const
  screenWidth = 800;
  screenHeight = 450;

type
   PColorRect = ^TColorRect;
   TColorRect = record
     c: TColorB;
     r: TRectangle;
   end;

function GenerateRandomColor(): TColorB;
begin
  result := ColorCreate(GetRandomValue(0, 255),GetRandomValue(0, 255),GetRandomValue(0, 255),255);
end;

function GenerateRandomColorRectSequence(rectCount: integer; rectWidth, screenWidth, screenHeight: Single): PColorRect;
var seq: Pinteger;
    rectangles: PColorRect;
    rectSeqWidth, startX: Single;
    x, rectHeight: integer;
begin
  seq := LoadRandomSequence(rectCount, 0, rectCount-1);
  rectangles := malloc(rectCount*sizeof(TColorRect));
  rectSeqWidth := rectCount * rectWidth;

  startX := (screenWidth - rectSeqWidth) * 0.5;
  for x :=0 to rectCount do
  begin
    rectHeight := Round(seq[x] * (screenHeight / (rectCount - 1)));
    rectangles[x].c := GenerateRandomColor();
    rectangles[x].r := RectangleCreate(startX + x * rectWidth,
    screenHeight - rectHeight, rectWidth, rectHeight);
  end;
  UnloadRandomSequence(seq);
  result := @rectangles;
end;

procedure ShuffleColorRectSequence(rectangles: PColorRect; rectCount: integer);
var seq: PInteger;
    i1: integer;
    r1, r2: PColorRect;
    tmp: TColorRect;
begin
  seq := LoadRandomSequence(rectCount, 0, rectCount-1);
  for i1 :=0 to rectCount  do // (int i1=0;i1<rectCount;i1++)
  begin
    r1 := @rectangles[i1];
    r2 := @rectangles[seq[i1]];
    // swap only the color and height
    {
    tmp := r1^;
    r1^.c := r2^.c;
    r1^.r.height := r2^.r.height;
    r1^.r.y := r2^.r.y;
    r2^.c := tmp.c;
    r2^.r.height := tmp.r.height;
    r2^.r.y := tmp.r.y;
    }
  end;
  UnloadRandomSequence(seq);
end;

procedure DrawTextCenterKeyHelp(const key: PChar; const text: PChar; posX, posY, fontSize: integer; color: TColorB);
var spaceSize, pressSize, keySize, textSize, totalSize, textSizeCurrent: integer;
begin
  spaceSize := MeasureText(' ', fontSize);
  pressSize := MeasureText('Press', fontSize);
  keySize := MeasureText(key, fontSize);
  textSize := MeasureText(text, fontSize);
  totalSize := pressSize + 2 * spaceSize + keySize + 2 * spaceSize + textSize;
  textSizeCurrent := 0;

  DrawText('Press', posX, posY, fontSize, color);
  textSizeCurrent += pressSize + 2 * spaceSize;
  DrawText(key, posX + textSizeCurrent, posY, fontSize, RED);
  DrawRectangle(posX + textSizeCurrent, posY + fontSize, keySize, 3, RED);
  textSizeCurrent += keySize + 2 * spaceSize;
  DrawText(text, posX + textSizeCurrent, posY, fontSize, color);
end;

var
  rectCount, fontSize, x, rectCountTextSize: integer;
  rectSize: single;
  rectangles: PColorRect;
  rectCountText: PChar;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - Generates a random sequence');

  rectCount := 20;
  rectSize := screenWidth/rectCount;
  rectangles := GenerateRandomColorRectSequence(rectCount, rectSize, screenWidth, 0.75 * screenHeight);

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      if(IsKeyPressed(KEY_SPACE)) then
      begin
        ShuffleColorRectSequence(rectangles, rectCount);
      end;

      if(IsKeyPressed(KEY_UP)) then
      begin
        Inc(rectCount);
        rectSize := screenWidth/rectCount;
           //   free(rectangles);
        rectangles := GenerateRandomColorRectSequence(rectCount, rectSize, screenWidth, 0.75 * screenHeight);
      end;

      if(IsKeyPressed(KEY_DOWN)) then
      begin
        if(rectCount >= 4) then
        begin
          Dec(rectCount);
          rectSize := screenWidth/rectCount;
             //   free(rectangles);
          rectangles := GenerateRandomColorRectSequence(rectCount, rectSize, screenWidth, 0.75 * screenHeight);
        end;
      end;


      // Draw
      BeginDrawing();
      ClearBackground(RAYWHITE);

      fontSize := 20;
      for x:= 0 to 19 do
      begin
        DrawRectangleRec(rectangles[x].r, rectangles[x].c);
        DrawTextCenterKeyHelp('SPACE', 'to shuffle the sequence.', 10, screenHeight - 96, fontSize, BLACK);
        DrawTextCenterKeyHelp('UP', 'to add a rectangle and generate a new sequence.', 10, screenHeight - 64, fontSize, BLACK);
        DrawTextCenterKeyHelp('DOWN', 'to remove a rectangle and generate a new sequence.', 10, screenHeight - 32, fontSize, BLACK);
      end;

      rectCountText := TextFormat('%d rectangles', rectCount);
      rectCountTextSize := MeasureText(rectCountText, fontSize);
      DrawText(rectCountText, screenWidth - rectCountTextSize - 10, 10, fontSize, BLACK);

      DrawFPS(10, 10);

      EndDrawing();
    end;

  // De-Initialization

  CloseWindow();        // Close window and OpenGL context
end.

