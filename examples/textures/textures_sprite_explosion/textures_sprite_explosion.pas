program textures_sprite_explosion;

{$mode objfpc}{$H+}

uses cmem, raylib, math;

const
screenWidth = 800;
screenHeight = 450;

NUM_FRAMES_PER_LINE = 5;
NUM_LINES = 5;

var
 fxBoom:TSound;
 explosion:TTexture2D;
 position:TVector2;
 frameRec: TRectangle;
 frameWidth,frameHeight,currentFrame,currentLine : integer;
 framesCounter:integer;
 active:boolean;
 framesSpeed:integer;
begin
 InitWindow(screenWidth, screenHeight, 'raylib [textures] example - Sprite explosion');
 InitAudioDevice();
 fxBoom := LoadSound(PChar(GetApplicationDirectory + 'resources/boom.wav'));
 explosion := LoadTexture(PChar(GetApplicationDirectory + 'resources/explosion.png'));
 frameWidth := explosion.width div NUM_FRAMES_PER_LINE; // Sprite one frame rectangle width
 frameHeight := explosion.height div NUM_LINES; // Sprite one frame rectangle height
 currentFrame := 0;
 currentLine := 0;
 frameRec.x:=0;
 frameRec.y:=0;
 frameRec.height:=frameHeight;
 frameRec.width:=frameWidth;
 framesSpeed:=50;
 position.y:=0.0;
 position.x:=0.0;
 active := false;
 framesCounter := 0;
 SetTargetFPS(120);

// Main game loop
 while not WindowShouldClose() do
  begin
   if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) and (not active) then
   begin
    position := GetMousePosition();
    active := true;
    position.x := position.x- frameWidth/2;
    position.y := position.y - frameHeight/2;
   PlaySound(fxBoom);
end;
// Compute explosion animation frames
if (active) then
begin
 inc(framesCounter);// framesCounter++;
 if (framesCounter > (60/framesSpeed)) then
 begin
   inc(currentFrame);// currentFrame++;
   if (currentFrame >= NUM_FRAMES_PER_LINE) then
    begin
     currentFrame := 0;
     inc(currentLine);//++;
     if (currentLine >= NUM_LINES) then
     begin
      currentLine := 0;
      active := false;
     end;
end;
framesCounter := 0;
end;

end;

frameRec.x := frameWidth*currentFrame;
frameRec.y := frameHeight*currentLine;

BeginDrawing();
ClearBackground(RAYWHITE);

if (active) then DrawTextureRec(explosion, frameRec, position, WHITE);
EndDrawing();

end;
UnloadTexture(explosion); // Unload texture
UnloadSound(fxBoom); // Unload sound
CloseAudioDevice();
CloseWindow(); // Close window and OpenGL context
end.
