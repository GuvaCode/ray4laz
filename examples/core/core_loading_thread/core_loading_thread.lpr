program core_loading_thread;

{$mode objfpc}{$H+}

uses 
  {$IFDEF UNIX}
   cthreads,
   cmem,
   {$ENDIF} raylib, SysUtils, Classes;

const
  screenWidth = 800;
  screenHeight = 450;

var
  FramesCounter: Integer;
  State: (STATE_WAITING, STATE_LOADING, STATE_FINISHED);
  DataLoaded: Boolean = False;
  DataProgress: Integer = 0;

  // Loading data thread function definition
  procedure LoadDataThread;
  var
    TimeCounter: Integer;
    PrevTime: UInt64;
  begin
    TimeCounter := 0; // Time counted in ms
    PrevTime := TThread.GetTickCount64(); // Previous time
    // We simulate data loading with a time counter for 5 seconds
    while TimeCounter < 5000 do
    begin
      TimeCounter := TThread.GetTickCount64() - PrevTime;
      // We accumulate time over a global variable to be used in
      // main thread as a progress bar
      DataProgress := TimeCounter div 10;
    end;
    // When data has finished loading, we set global variable
    DataLoaded := True;
  end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [core] example - loading thread');
  State := STATE_WAITING;
  FramesCounter := 0;
  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
          case State of
      STATE_WAITING:
      begin
        if IsKeyPressed(KEY_ENTER) then
        begin
          TThread.CreateAnonymousThread(@LoadDataThread).Start;
          State := STATE_LOADING;
        end;
      end;
      STATE_LOADING:
      begin
        Inc(FramesCounter);
        if DataLoaded then
        begin
          FramesCounter := 0;
          State := STATE_FINISHED;
        end;
      end;
      STATE_FINISHED:
      begin
        if IsKeyPressed(KEY_ENTER) then
        begin
          // Reset everything to launch again
          DataLoaded := False;
          DataProgress := 0;
          State := STATE_WAITING;
        end;
      end;
    end;
      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        case State of
          STATE_WAITING:
          begin
            DrawText('PRESS ENTER to START LOADING DATA', 150, 170, 20, DARKGRAY);
          end;
          STATE_LOADING:
          begin
            DrawRectangle(150, 200, DataProgress, 60, SKYBLUE);
            if ((FramesCounter div 15) mod 2) <> 0 then
              DrawText('LOADING DATA...', 240, 210, 40, DARKBLUE);
          end;
          STATE_FINISHED:
          begin
            DrawRectangle(150, 200, 500, 60, LIME);
            DrawText('DATA LOADED!', 250, 210, 40, GREEN);
          end;
        end;

        DrawRectangleLines(150, 200, 500, 60, DARKGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

