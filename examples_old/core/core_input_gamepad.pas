program core_basic_gamepad;

{$MODE objfpc}

uses cmem, ray_header, math, sysutils;

const
  screenWidth = 800;
  screenHeight = 450;

(*
#if defined(PLATFORM_RPI)
    #define XBOX360_NAME_ID     "Microsoft X-Box 360 pad"
    #define PS3_NAME_ID         "PLAYSTATION(R)3 Controller"
#else
*)	
  XBOX360_NAME_ID = 'Xbox 360 Controller';
  XBOXONE_NAME_ID = 'Microsoft X-Box One pad';
  PS3_NAME_ID =	'PLAYSTATION(R)3 Controller';

var
  i: integer;
  gp1Text: AnsiString;
  texPs3Pad, texXboxPad: TTexture2D;

begin
  {$IFDEF DARWIN}
  SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
  {$IFEND}

  SetConfigFlags(FLAG_MSAA_4X_HINT);  // Set MSAA 4X hint before windows creation

  InitWindow(screenWidth, screenHeight, 'raylib [core] example - gamepad input');

  texPs3Pad := LoadTexture('resources/images/ps3.png');
  texXboxPad := LoadTexture('resources/images/xbox.png');

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
         // Draw
        //----------------------------------------------------------------------------------
        BeginDrawing();

            ClearBackground(RAYWHITE);

            if (IsGamepadAvailable(GAMEPAD_PLAYER1)) then 
            begin
		gp1Text := Format('GP1: %s', [GetGamepadName(GAMEPAD_PLAYER1)]);
                DrawText(PChar(gp1Text), 10, 10, 10, BLACK);

                if (IsGamepadName(GAMEPAD_PLAYER1, XBOX360_NAME_ID) or IsGamepadName(GAMEPAD_PLAYER1, XBOXONE_NAME_ID)) then
                begin
                    DrawTexture(texXboxPad, 0, 0, DARKGRAY);

                    // Draw buttons: xbox home
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE)) then  DrawCircle(394, 89, 19, RED);

                    // Draw buttons: basic
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE_RIGHT)) then  DrawCircle(436, 150, 9, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE_LEFT)) then  DrawCircle(352, 150, 9, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_LEFT)) then  DrawCircle(501, 151, 15, BLUE);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_DOWN)) then  DrawCircle(536, 187, 15, LIME);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_RIGHT)) then  DrawCircle(572, 151, 15, MAROON);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_UP)) then  DrawCircle(536, 115, 15, GOLD);

                    // Draw buttons: d-pad
                    DrawRectangle(317, 202, 19, 71, BLACK);
                    DrawRectangle(293, 228, 69, 19, BLACK);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_UP)) then  DrawRectangle(317, 202, 19, 26, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_DOWN)) then  DrawRectangle(317, 202 + 45, 19, 26, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_LEFT)) then  DrawRectangle(292, 228, 25, 19, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_RIGHT)) then  DrawRectangle(292 + 44, 228, 26, 19, RED);

                    // Draw buttons: left-right back
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_TRIGGER_1)) then  DrawCircle(259, 61, 20, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_TRIGGER_1)) then  DrawCircle(536, 61, 20, RED);

                    // Draw axis: left joystick
                    DrawCircle(259, 152, 39, BLACK);
                    DrawCircle(259, 152, 34, LIGHTGRAY);
                    DrawCircle(Trunc(259 + (GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_X)*20)),
                               Trunc(152 - (GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_Y)*20)), 25, BLACK);

                    // Draw axis: right joystick
                    DrawCircle(461, 237, 38, BLACK);
                    DrawCircle(461, 237, 33, LIGHTGRAY);
                    DrawCircle(Trunc(461 + (GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_X)*20)),
                               Trunc(237 - (GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_Y)*20)), 25, BLACK);

                    // Draw axis: left-right triggers
                    DrawRectangle(170, 30, 15, 70, GRAY);
                    DrawRectangle(604, 30, 15, 70, GRAY);
                    DrawRectangle(170, 30, 15, Trunc(((1.0 + GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_TRIGGER)) /2.0)*70), RED);
                    DrawRectangle(604, 30, 15, Trunc(((1.0 + GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_TRIGGER)) /2.0)*70), RED);

                    //DrawText(Format('Xbox axis LT: %02.02f', [ GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_TRIGGER) ]) , 10, 40, 10, BLACK);
                    //DrawText(Format('Xbox axis RT: %02.02f', [ GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_TRIGGER) ]) , 10, 60, 10, BLACK);
                end
                else if not (IsGamepadName(GAMEPAD_PLAYER1, PS3_NAME_ID)) then
                begin
                    DrawTexture(texPs3Pad, 0, 0, DARKGRAY);

                    // Draw buttons: ps
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE)) then  DrawCircle(396, 222, 13, RED);

                    // Draw buttons: basic
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE_LEFT)) then  DrawRectangle(328, 170, 32, 13, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_MIDDLE_RIGHT)) then  DrawTriangle(Vector2Create( 436, 168 ), Vector2Create( 436, 185 ), Vector2Create( 464, 177 ), RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_UP)) then  DrawCircle(557, 144, 13, LIME);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_RIGHT)) then  DrawCircle(586, 173, 13, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_DOWN)) then  DrawCircle(557, 203, 13, VIOLET);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_FACE_LEFT)) then  DrawCircle(527, 173, 13, PINK);

                    // Draw buttons: d-pad
                    DrawRectangle(225, 132, 24, 84, BLACK);
                    DrawRectangle(195, 161, 84, 25, BLACK);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_UP)) then  DrawRectangle(225, 132, 24, 29, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_DOWN)) then  DrawRectangle(225, 132 + 54, 24, 30, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_LEFT)) then  DrawRectangle(195, 161, 30, 25, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_FACE_RIGHT)) then  DrawRectangle(195 + 54, 161, 30, 25, RED);

                    // Draw buttons: left-right back buttons
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_LEFT_TRIGGER_1)) then  DrawCircle(239, 82, 20, RED);
                    if (IsGamepadButtonDown(GAMEPAD_PLAYER1, GAMEPAD_BUTTON_RIGHT_TRIGGER_1)) then  DrawCircle(557, 82, 20, RED);

                    // Draw axis: left joystick
                    DrawCircle(319, 255, 35, BLACK);
                    DrawCircle(319, 255, 31, LIGHTGRAY);
                    DrawCircle(319 + Trunc(GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_X)*20),
                               255 + Trunc(GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_Y)*20), 25, BLACK);

                    // Draw axis: right joystick
                    DrawCircle(475, 255, 35, BLACK);
                    DrawCircle(475, 255, 31, LIGHTGRAY);
                    DrawCircle(475 + Trunc(GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_X)*20),
                               255 + Trunc(GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_Y)*20), 25, BLACK);

                    // Draw axis: left-right triggers
                    DrawRectangle(169, 48, 15, 70, GRAY);
                    DrawRectangle(611, 48, 15, 70, GRAY);
                    DrawRectangle(169, 48, 15, Trunc(((1.0 - GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_LEFT_TRIGGER)) /2.0)*70), RED);
                    DrawRectangle(611, 48, 15, Trunc(((1.0 - GetGamepadAxisMovement(GAMEPAD_PLAYER1, GAMEPAD_AXIS_RIGHT_TRIGGER)) /2.0)*70), RED);
                end
                else
                begin
                    DrawText('- GENERIC GAMEPAD -', 280, 180, 20, GRAY);

                    // TODO: Draw generic gamepad
   	    end;

                DrawText(PChar(Format('DETECTED AXIS [%d]:', [GetGamepadAxisCount(GAMEPAD_PLAYER1)] )), 10, 50, 10, MAROON);

                for i := 0 to GetGamepadAxisCount(GAMEPAD_PLAYER1) -1 do
                begin
                    DrawText(PChar(Format('AXIS %d: %.02f', [ i, GetGamepadAxisMovement(GAMEPAD_PLAYER1, i)])), 20, 70 + 20*i, 10, DARKGRAY);
                end;

                if (GetGamepadButtonPressed() <> -1) then DrawText(PChar(Format('DETECTED BUTTON: %d', [ GetGamepadButtonPressed()] )), 10, 430, 10, RED)
                else DrawText('DETECTED BUTTON: NONE', 10, 430, 10, GRAY);
            end
            else
            begin
                DrawText('GP1: NOT DETECTED', 10, 10, 10, GRAY);

                DrawTexture(texXboxPad, 0, 0, LIGHTGRAY);
     	end;

        EndDrawing();
        //----------------------------------------------------------------------------------
    end;

    // De-Initialization
    //--------------------------------------------------------------------------------------
    UnloadTexture(texPs3Pad);
    UnloadTexture(texXboxPad);

    CloseWindow();        // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

end.
