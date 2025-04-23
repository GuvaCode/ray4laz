program textures_bunnymark;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

type
 TBunny = record
    position:TVector2;
    speed:TVector2;
    color:TColorB;
  end;

const
 screenWidth = 800;
 screenHeight = 450;
 MAX_BUNNIES = 50000;    // 50K bunnies limit
 MAX_BATCH_ELEMENTS=8192;
var
  bunnies: array [0 .. (MAX_BUNNIES) ] of TBunny; // Bunnies array
  bunniesCount:integer=0; // Bunnies counter
  texBunny: TTexture2d;
  i:integer;

begin
{$IFDEF DARWIN}
SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
{$IFEND}

 InitWindow(screenWidth, screenHeight, 'raylib [textures] example - bunnymark');
// SetTargetFPS(144);
 // Load bunny texture
 texBunny := LoadTexture(PChar(GetApplicationDirectory + 'resources/wabbit_alpha.png'));


 while not WindowShouldClose() do 
 begin
  // update
  if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) then
        begin
            // Create more bunnies
            for i:=0 to 100 do
            begin
                if (bunniesCount < MAX_BUNNIES) then
                begin
                    bunnies[bunniesCount].position := GetMousePosition();
                    bunnies[bunniesCount].speed.x := Single(GetRandomValue(-250, 250)/60.0);
                    bunnies[bunniesCount].speed.y := Single(GetRandomValue(-250, 250)/60.0);
                    //bunnies[bunniesCount].color :=
                    ColorSet(@bunnies[bunniesCount].color,
                    GetRandomValue(50, 240),
                    GetRandomValue(80, 240),
                    GetRandomValue(100, 240), 255) ;
                    if bunniesCount < MAX_BUNNIES  then inc(bunniesCount);
                end;
          end;
        end;
  //
  // Update bunnies
        for i:=0 to  bunniesCount do
        begin
            bunnies[i].position.x:=bunnies[i].position.x+ bunnies[i].speed.x;
            bunnies[i].position.y:=bunnies[i].position.y+ bunnies[i].speed.y;

            if (((bunnies[i].position.x + texBunny.width/2) > GetScreenWidth()) or
                ((bunnies[i].position.x + texBunny.width/2) < 0)) then bunnies[i].speed.x *= -1;
            if (((bunnies[i].position.y + texBunny.height/2) > GetScreenHeight()) or
                ((bunnies[i].position.y + texBunny.height/2 - 40) < 0)) then bunnies[i].speed.y *= -1;
        end;

  BeginDrawing();
  ClearBackground(RAYWHITE);

  for i:= 0 to bunniesCount do
                // NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
                // a draw call is launched and buffer starts being filled again;
                // before issuing a draw call, updated vertex data from internal CPU buffer is send to GPU...
                // Process of sending data is costly and it could happen that GPU data has not been completely
                // processed for drawing while new data is tried to be sent (updating current in-use buffers)
                // it could generates a stall and consequently a frame drop, limiting the number of drawn bunnies
                DrawTexture(texBunny,round(bunnies[i].position.x),round(bunnies[i].position.y), bunnies[i].color);

            DrawRectangle(0, 0, screenWidth, 40, BLACK);
            DrawText(TextFormat('bunnies: %i', bunniesCount), 120, 10, 20, GREEN);
            DrawText(TextFormat('batched draw calls: %i', 1 + bunniesCount div MAX_BATCH_ELEMENTS), 320, 10, 20, MAROON);
            DrawFPS(10, 10);
  EndDrawing();
 end;

    UnloadTexture(texBunny);    // Unload bunny texture
    CloseWindow();              // Close window and OpenGL context
end.

