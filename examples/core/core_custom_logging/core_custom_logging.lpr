program core_custom_logging;

{$mode Delphi}{$H+}

uses 
cmem, raylib, SysUtils, Classes;

const
  screenWidth = 800;
  screenHeight = 450;

  // Custom logging function
  procedure CustomLog(MsgType: TTraceLogLevel; const Text: PChar; AArg:Pointer); cdecl;
  var t, l, s: String;
  begin
    t := '[' + DateTimetoStr(Now) + ']';
    case MsgType of
     LOG_INFO: L := ('[INFO] : ');
     LOG_ERROR: L := ('[ERROR] : ');
     LOG_WARNING: L := ('[WARN] : ');
     LOG_DEBUG: L := ('[DEBUG] : ');
    end;
   s:=t+l+Text;
   writeln(s);
  end;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  // Set custom logger
  SetTraceLogCallback(CustomLog);

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - custom logging');


  SetTargetFPS(60);// Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // TODO: Update your variables here
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText('Check out the console output to see the custom logger in action!', 60, 200, 20, LIGHTGRAY);
      EndDrawing();
    end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

