{*******************************************************************************************
*
*   raylib example - point rendering
*
*   Example originally created with raylib 5.0, last time updated with raylib 5.0
*
*   Example contributed by Reese Gallagher (@satchelfrost) and reviewed by Ramon Santamaria (@raysan5)
*
*   Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
*   BSD-like license that allows static linking with closed source software
*
*   Copyright (c) 2024 Reese Gallagher (@satchelfrost)
*   pascal translation 2024 Gunko Vadim (@guvacode)
*
********************************************************************************************}
program models_point_rendering;

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

  MAX_POINTS = 10000000;     // 10 million
  MIN_POINTS = 1000;         // 1 thousand
  // RAND_MAX = 32767;


function GenMeshPoints(numPoints: integer): TMesh;  // определение функции GenMeshPoints
var
    mesh: TMesh;   // создание структуры Mesh
    i: integer;
    theta, phi, r: single;  // объявление переменных
    color: TColorB;  // объявление переменной для хранения цвета
  begin
    // инициализация структуры Mesh
    mesh := Default(TMesh);
    mesh.triangleCount := 1;
    mesh.vertexCount := numPoints;

    GetMem(mesh.vertices, numPoints*3*sizeof(single));  // выделение памяти под массив вершин
    GetMem(mesh.colors, numPoints*4*sizeof(byte));  // выделение памяти под массив цветов

    for i := 0 to numPoints-1 do  // цикл по всем вершинам
    begin
        theta := Pi * random;  // генерация угла в радианах
        phi := 2 * Pi * random; // генерация другого угла в радианах
        r := 10 * random;      // генерация расстояния от начала координат

        mesh.vertices[i*3 + 0] := r * sin(theta) * cos(phi);
        mesh.vertices[i*3 + 1] := r * sin(theta) * sin(phi);
        mesh.vertices[i*3 + 2] := r * cos(theta);

        color := ColorFromHSV(r * 360, 1, 1);  // получение цвета по заданным параметрам

        mesh.colors[i*4 + 0] := color.r;
        mesh.colors[i*4 + 1] := color.g;
        mesh.colors[i*4 + 2] := color.b;
        mesh.colors[i*4 + 3] := color.a;
    end;


    // Upload mesh data from CPU (RAM) to GPU (VRAM) memory
    UploadMesh(@mesh, true);
    result := Mesh;
end;


var
  camera: TCamera;
  position, pos: TVector3;
  useDrawModelPoints, numPointsChanged: Boolean;
  numPoints, i: Integer;
  mesh: TMesh;
  model: TModel;
  color: TColorB;

begin
  // Initialization
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - point rendering');

  camera := Camera3DCreate(Vector3Create( 3.0, 3.0, 3.0),
                           Vector3Create( 0.0, 0.0, 0.0),
                           Vector3Create( 0.0, 1.0, 0.0), 45.0, CAMERA_PERSPECTIVE);

  position := Vector3Create( 0.0, 0.0, 0.0);
  useDrawModelPoints := true;
  numPointsChanged := false;
  numPoints := 1000;

  mesh := GenMeshPoints(numPoints);


  model := LoadModelFromMesh(mesh);

  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      UpdateCamera(@camera, CAMERA_ORBITAL);

      if (IsKeyPressed(KEY_SPACE)) then useDrawModelPoints :=  not useDrawModelPoints;

      if (IsKeyPressed(KEY_UP)) then
      begin
        numPoints := ifthen(numPoints*2 > MAX_POINTS, MAX_POINTS, numPoints*2);
        numPointsChanged := true;
      end;

      if (IsKeyPressed(KEY_DOWN)) then
      begin
        numPoints := ifthen(numPoints/2 < MIN_POINTS, MIN_POINTS , numPoints div 2);
        numPointsChanged := true;
      end;

      // Upload a different point cloud size
      if (numPointsChanged) then
      begin
        Model := Default(TModel);
        UnloadModel(model);
        mesh  := GenMeshPoints(numPoints);
        model := LoadModelFromMesh(mesh);
        numPointsChanged := false;
      end;
      
      // Draw
      BeginDrawing();
        ClearBackground(BLACK);

        BeginMode3D(camera);

        // The new method only uploads the points once to the GPU
        if (useDrawModelPoints) then
        begin
          DrawModelPoints(model, position, 1.0, WHITE);
        end
          else
        begin
        // The old method must continually draw the "points" (lines)
          for i := 0 to numPoints - 1 do
          begin
            pos := Vector3Create(mesh.vertices[i*3 + 0],
                                 mesh.vertices[i*3 + 1],
                                 mesh.vertices[i*3 + 2]);

            color := ColorCreate(mesh.colors[i*4 + 0],
                                 mesh.colors[i*4 + 1],
                                 mesh.colors[i*4 + 2],
                                 mesh.colors[i*4 + 3]);

            DrawPoint3D(pos, color);
          end;
        end;

        // Draw a unit sphere for reference
        DrawSphereWires(position, 1.0, 10, 10, YELLOW);

        EndMode3D();

        // Draw UI text
        DrawText(TextFormat('Point Count: %d', numPoints), 20, screenHeight - 50, 40, WHITE);
        DrawText('Up - increase points', 20, 70, 20, WHITE);
        DrawText('Down - decrease points', 20, 100, 20, WHITE);
        DrawText('Space - drawing function', 20, 130, 20, WHITE);

        if (useDrawModelPoints) then DrawText('Using: DrawModelPoints()', 20, 160, 20, GREEN)
        else DrawText('Using: DrawPoint3D()', 20, 160, 20, RED);

        DrawFPS(10, 10);

      EndDrawing();
    end;

  // De-Initialization
  CloseWindow();        // Close window and OpenGL context
end.

