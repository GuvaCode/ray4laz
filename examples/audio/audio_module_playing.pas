program audio_module_playing;

{$MODE objfpc}

uses cmem, ray_header, math;

const MAX_CIRCLES=64;
      screenWidth = 800;
      screenHeight = 450;

type
 CircleWave = record
  position:TVector2;
  radius:float;
  alpha:float;
  speed:float;
  color:TColor;
   end;

var
    circles: array [0..64] of CircleWave;
    colors:  array [0..13] of Tcolor;
    i:integer;
    music:TMusic;
    timePlayed:single;
    pause:boolean;

begin
   colors[0]:= ORANGE;
   colors[1]:= RED;
   colors[2]:= GOLD;
   colors[3]:= LIME;
   colors[4]:= BLUE;
   colors[5]:= VIOLET;
   colors[6]:= BROWN;
   colors[7]:= LIGHTGRAY;
   colors[8]:= PINK;
   colors[9]:= YELLOW;
   colors[10]:= GREEN;
   colors[11]:= SKYBLUE;
   colors[12]:= PURPLE;
   colors[13]:= BEIGE;


   SetConfigFlags(FLAG_MSAA_4X_HINT);  // NOTE: Try to enable MSAA 4X
   InitWindow(screenWidth, screenHeight, 'raylib [audio] example - module playing (streaming)');
   InitAudioDevice();

   for i:=0 to MAX_CIRCLES-1 do
   begin
        circles[i].alpha := 0.0;
        circles[i].radius := GetRandomValue(10, 40);
        circles[i].position.x := GetRandomValue(round(circles[i].radius), screenWidth - round(circles[i].radius));
        circles[i].position.y  := GetRandomValue(round(circles[i].radius), screenHeight - round(circles[i].radius));
        circles[i].speed := GetRandomValue(1, 100)/2000.0;
        circles[i].color:=colors[GetRandomValue(0, 13)];
   end;

   music := LoadMusicStream('resources/music/algar_-_happy_holidays.xm');
   //    music.looping = false; //  3.0 ???
   PlayMusicStream(music);
   timePlayed := 0.0;
   pause := false;
   SetTargetFPS(60);               // Set our game to run at 60 frames-per-second

   while not WindowShouldClose() do
    begin
      UpdateMusicStream(music);      // Update music buffer with new stream data
      // Restart music playing (stop and play)
      if IsKeyPressed(KEY_SPACE) then
      begin
       StopMusicStream(music);
       PlayMusicStream(music);
      end;
      // Pause/Resume music playing
       if IsKeyPressed(KEY_P) then
        begin
            pause := not pause;
            if pause then PauseMusicStream(music)
            else ResumeMusicStream(music);
        end;
       // Get timePlayed scaled to bar dimensions
        timePlayed := GetMusicTimePlayed(music) / GetMusicTimeLength(music) * screenWidth - 40;
      // Color circles animation
        for i:=0 to MAX_CIRCLES-1 do if (i>=0 ) and (not pause) then
         begin
            circles[i].alpha := circles[i].alpha + circles[i].speed;
            circles[i].radius := circles[i].radius + circles[i].speed*10.0;
            if (circles[i].alpha > 1.0) then  circles[i].speed :=circles[i].speed * circles[i].speed - 0.2;
            if (circles[i].alpha <= 0.0) then
            begin
                circles[i].alpha := 0.0;
                circles[i].radius := GetRandomValue(10, 40);
                circles[i].position.x := GetRandomValue(round(circles[i].radius), screenWidth - round(circles[i].radius));
                circles[i].position.y := GetRandomValue(round(circles[i].radius), screenHeight - round(circles[i].radius));
                circles[i].color := colors[GetRandomValue(0, 13)];
                circles[i].speed := GetRandomValue(1, 100)/2000.0;
            end;
        end;
        //----------------------------------------------------------------------------------
        BeginDrawing();
            ClearBackground(RAYWHITE);
            for i:=0 to  MAX_CIRCLES - 1 do if  i >= 0 then
                DrawCircleV(circles[i].position, circles[i].radius, Fade(circles[i].color, circles[i].alpha));
            // Draw time bar
            DrawRectangle(20, screenHeight - 20 - 12, screenWidth - 40, 12, LIGHTGRAY);
            DrawRectangle(20, screenHeight - 20 - 12, round(timePlayed), 12, MAROON);
            DrawRectangleLines(20, screenHeight - 20 - 12, screenWidth - 40, 12, GRAY);
        EndDrawing();


    end;

    UnloadMusicStream(music);          // Unload music stream buffers from RAM
    CloseAudioDevice();     // Close audio device (music streaming is automatically stopped)
    CloseWindow();          // Close window and OpenGL context
end.

