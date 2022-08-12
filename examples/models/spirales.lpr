program spirales;

{$mode objfpc}{$H+}

uses cmem,
Math,
raylib;

const
 screenWidth = 800;
 screenHeight = 450;

type

  TXYZPoint = record
    x, y, z: Single;
    colors:TColorB;
  end;

  TCircle = array[1..36] of TXYZPoint;

var CircleArray: array[1..16] of TCircle;
    time,cameraTime : double;
    i,j,x,y: Integer;
    xf, yf, zc, scale, blockScale, cubeSize: single;
    cam: TCamera;
    tex:TTexture2D;

procedure Povorot(xi, yi: Single; var x, y: Single; fi: Single);
begin
  x := xi * Cos(DegToRad(fi)) - yi * Sin(DegToRad(fi));
  y := xi * Sin(DegToRad(fi)) + yi * Cos(DegToRad(fi));
end;

procedure Proekcia(Point: TXYZPoint; var x, y: integer);
begin
  x := Round(Point.x + Point.z / 255);
  y := Round(Point.y + Point.z / 255);
end;

begin
 InitWindow(screenWidth, screenHeight, 'raylib pascal - spiral');
 cam  :=  Camera3DCreate(Vector3Create( 30.0, 200.0, 30.0),
                   Vector3Create( 0.0, 0.0, 0.0 ),
                   Vector3Create( 0.0, 1.0, 0.0),
                   70.0,CAMERA_PERSPECTIVE);

 //////////////////////////////////////
   zc := - 144;
  for i := 1 to 16 do
  begin
    for j := 1 to 36 do
    begin
      CircleArray[i][j].x := cos(DegToRad(j * 10)) * 50;
      CircleArray[i][j].y := sin(DegToRad(j * 10)) * 50;
      CircleArray[i][j].z := zc;
      CircleArray[i][j].colors:= ColorFromHSV((x + y + zC)*zc,0.75, 0.9);
      zc := zc + 0.5;
    end;
  end;
///////////////////////////////////////

tex:=LoadTexture('resources/raylib_logo.png');


 SetTargetFPS(60);

 while not WindowShouldClose() do 
 begin
         time := GetTime();
        // Calculate time scale for cube position and size
         scale := (2.0 + sin(time))*0.5;
        // Move camera around the scene
        cameraTime := time*0.3;
        cam.position.x := cos(cameraTime)*80.0;
        cam.position.z := sin(cameraTime)*80.0;
        //----------------------------------------------------------------------------------
  BeginDrawing();
  ClearBackground(BLACK);
  for i := 1 to 16 do
    for j := 1 to 36 do
    begin
      Povorot(CircleArray[i][j].x, CircleArray[i][j].z, xf, yf, 0.5);
      CircleArray[i][j].x := xf;
      CircleArray[i][j].z := yf;
      Povorot(CircleArray[i][j].y, CircleArray[i][j].z, xf, yf, 0.5);
      CircleArray[i][j].y := xf;
      CircleArray[i][j].z := yf;
      Povorot(CircleArray[i][j].x, CircleArray[i][j].y, xf, yf, 0.5);
      CircleArray[i][j].x := xf;
      CircleArray[i][j].y := yf;
      Proekcia(CircleArray[i][j], x, y);
      blockScale := (x + y + I) / 60.0;
      cubeSize := (2.0 - scale)*blockScale;

      BeginMode3d(cam);

      DrawCube(Vector3Create(CircleArray[i][j].x,CircleArray[i][j].y,CircleArray[i][j].z),
      cubeSize, cubeSize, cubeSize, CircleArray[i][j].colors);

      DrawCubeTexture(tex,Vector3Create( 0,0, 0), cubeSize*10 ,cubeSize*10,cubeSize*10,ColorFromHSV((cam.position.x + cam.position.Y )*i, 0.75, 0.9));
      EndMode3d();
    end;

  EndDrawing();
 end;
 UnloadTexture(tex); // Unload texture
CloseWindow(); 

end.

