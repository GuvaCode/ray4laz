{*******************************************************************************************
*
*   raylib [shaders] example - Simple shader mask
*
*   Example originally created with raylib 2.5, last time updated with raylib 3.7
*
*   Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2019-2024 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
*   pascal conversion 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************
*
*   The shader makes alpha holes in the forground to give the appearance of a top
*   down look at a spotlight casting a pool of light...
*
*   The right hand side of the screen there is just enough light to see whats
*   going on without the spot light, great for a stealth type game where you
*   have to avoid the spotlights.
*
*   The left hand side of the screen is in pitch dark except for where the spotlights are.
*
*   Although this example doesn't scale like the letterbox example, you could integrate
*   the two techniques, but by scaling the actual colour of the render texture rather
*   than using alpha as a mask.
*
********************************************************************************************}
program shaders_spotlight;

{$mode objfpc}{$H+}

uses 
cmem,
{uncomment if necessary}
raymath,sysutils,
//rlgl, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAX_SPOTS = 3;        // NOTE: It must be the same as define in shader
  MAX_STARS = 400;
  GLSL_VERSION = 330;

type
  // Spot data
  TSpot = record
    position: TVector2;
    speed: TVector2;
    inner: single;
    radius: single;
    positionLoc: LongWord;
    innerLoc: LongWord;
    radiusLoc: LongWord;
  end;

  // Stars in the star field have a position and velocity
  PStar = ^TStar;
  TStar = record
    position: TVector2;
    speed: TVector2;
  end;

procedure ResetStar(s: PStar);
begin
  s^.position := Vector2Create( GetScreenWidth()/2.0, GetScreenHeight()/2.0 );
  repeat
     s^.speed.x := GetRandomValue(-1000, 1000) / 100;
     s^.speed.y := GetRandomValue(-1000, 1000) / 100
   until abs(s^.speed.x) + abs(s^.speed.y) <= 1 ;
 s^.position := Vector2Add(s^.position, Vector2Multiply(s^.speed, Vector2Create( 8.0, 8.0 )));
end;

procedure UpdateStar(s: PStar);
begin
  s^.position := Vector2Add(s^.position, s^.speed);
  if ((s^.position.x < 0) or (s^.position.x > GetScreenWidth()) or (s^.position.y < 0) or
  (s^.position.y > GetScreenHeight())) then
  begin
    ResetStar(s);
  end;

end;

var
  texRay: TTexture;
  stars: array[0..MAX_STARS] of TStar;
  spots: array[0..MAX_SPOTS] of TSpot;
  n, m, i, py ,px , frameCounter: integer;
  shdrSpot: TShader;
  posName, innerName, radiusName: PChar;
  wLoc: LongWord;
  sw: single;
  mp: TVector2;
begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [shaders] example - shader spotlight');
  HideCursor();
  texRay := LoadTexture('resources/raysan.png');

  for n := 0 to MAX_STARS do ResetStar(@stars[n]);

  // Progress all the stars on, so they don't all start in the centre
  for m :=0 to screenWidth div 2 do
  begin
    for n := 0 to MAX_STARS do UpdateStar(@stars[n]);
  end;

  frameCounter := 0;
  // Use default vert shader
  shdrSpot := LoadShader(nil, TextFormat('resources/shaders/glsl%i/spotlight.fs', GLSL_VERSION));

  // Get the locations of spots in the shader
  for i :=0 to MAX_SPOTS do
  begin
    posName := PChar('spots[' + IntToStr(i) + '].pos');
    innerName := PChar('spots[' + IntToStr(i) + '].inner');
    radiusName:= PChar('spots[' + IntToStr(i) + '].radius');

    spots[i].positionLoc := GetShaderLocation(shdrSpot, posName);
    spots[i].innerLoc := GetShaderLocation(shdrSpot, innerName);
    spots[i].radiusLoc := GetShaderLocation(shdrSpot, radiusName);
  end;

  // Tell the shader how wide the screen is so we can have
  // a pitch black half and a dimly lit half.
  wLoc := GetShaderLocation(shdrSpot, 'screenWidth');
  sw := GetScreenWidth();
  SetShaderValue(shdrSpot, wLoc, @sw, SHADER_UNIFORM_FLOAT);

  // Randomize the locations and velocities of the spotlights
  // and initialize the shader locations
  for i := 0 to MAX_SPOTS do
  begin
    spots[i].position.x := GetRandomValue(64, screenWidth - 64);
    spots[i].position.y := GetRandomValue(64, screenHeight - 64);
    spots[i].speed := Vector2Create( 0, 0 );


      while ((abs(spots[i].speed.x) + abs(spots[i].speed.y)) < 2) do
      begin
        spots[i].speed.x := GetRandomValue(-400, 40) / 100.0;
        spots[i].speed.y := GetRandomValue(-400, 40) / 100.0;
      end;

      spots[i].inner := 28.0 * (i + 1);
      spots[i].radius := 48.0 * (i + 1);

      SetShaderValue(shdrSpot, spots[i].positionLoc, @spots[i].position.x, SHADER_UNIFORM_VEC2);
      SetShaderValue(shdrSpot, spots[i].innerLoc, @spots[i].inner, SHADER_UNIFORM_FLOAT);
      SetShaderValue(shdrSpot, spots[i].radiusLoc, @spots[i].radius, SHADER_UNIFORM_FLOAT);
  end;

  SetTargetFPS(60);// Set our game to run at 60 frames-per-second

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      Inc(frameCounter);

      // Move the stars, resetting them if the go offscreen
      for n := 0 to MAX_STARS do UpdateStar(@stars[n]);



      // Update the spots, send them to the shader
      for i := 0 to MAX_SPOTS do
      begin
          if (i = 0) then
          begin
              mp := GetMousePosition();
              spots[i].position.x := mp.x;
              spots[i].position.y := screenHeight - mp.y;
          end
          else
          begin
              spots[i].position.x += spots[i].speed.x;
              spots[i].position.y += spots[i].speed.y;
              if (spots[i].position.x < 64) then spots[i].speed.x := -spots[i].speed.x;
              if (spots[i].position.x > (screenWidth - 64)) then spots[i].speed.x := -spots[i].speed.x;
              if (spots[i].position.y < 64) then spots[i].speed.y := -spots[i].speed.y;
              if (spots[i].position.y > (screenHeight - 64)) then spots[i].speed.y := -spots[i].speed.y;
          end;
          SetShaderValue(shdrSpot, spots[i].positionLoc, @spots[i].position.x, SHADER_UNIFORM_VEC2);
      end;



      // Draw
      BeginDrawing();
      ClearBackground(DARKBLUE);

      // Draw stars and bobs
      for n := 0 to MAX_STARS do
      begin
          // Single pixel is just too small these days!
          DrawRectangle(Trunc(stars[n].position.x), Trunc(stars[n].position.y), 2, 2, WHITE);
      end;

      for i :=0 to 10 do ///(int i = 0; i < 16; i++)
      begin
       py := Trunc(screenWidth/2.0  + cos((frameCounter + i*8)/51.45)*(screenWidth/2.2) - 32);
       px := Trunc(screenHeight/2.0 + sin((frameCounter + i*8)/17.87)*(screenHeight/4.2));
       DrawTexture(texRay,py,px, WHITE);
      end;

      // Draw spot lights
      BeginShaderMode(shdrSpot);
          // Instead of a blank rectangle you could render here
          // a render texture of the full screen used to do screen
          // scaling (slight adjustment to shader would be required
          // to actually pay attention to the colour!)
          DrawRectangle(0, 0, screenWidth, screenHeight, WHITE);
      EndShaderMode();

      DrawFPS(10, 10);

      DrawText('Move the mouse!', 10, 30, 20, GREEN);
      DrawText('Pitch Black', Trunc(screenWidth*0.2), screenHeight div 2 , 20, GREEN);
      DrawText('Dark', Trunc(screenWidth*0.66), screenHeight div 2, 20, GREEN);
      EndDrawing();
    end;

  // De-Initialization
  UnloadTexture(texRay);
  UnloadShader(shdrSpot);
  CloseWindow();        // Close window and OpenGL context
end.

