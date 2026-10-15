program models_animation_blending;

{$mode objfpc}{$H+}

uses
  cmem, sysutils, raylib, raymath, raygui;

const
  screenWidth = 800;
  screenHeight = 450;

var
  camera: TCamera3D;
  model: TModel;
  position: TVector3;
  anims: PModelAnimation;
  animCount: integer;
  currentAnimPlaying: integer;
  nextAnimToPlay: integer;
  animTransition: boolean;
  animIndex0: integer;
  animCurrentFrame0: single;
  animFrameSpeed0: single;
  animIndex1: integer;
  animCurrentFrame1: single;
  animFrameSpeed1: single;
  animBlendFactor: single;
  animBlendTime: single;
  animBlendTimeCounter: single;
  animPause: boolean;
  animFrameProgress0: single;
  animFrameProgress1: single;
  animBlendProgress: single;
  dropdownEditMode0: boolean;
  dropdownEditMode1: boolean;
  i: integer;
  animNames: array[0..63] of PChar;
  animNamesStr: string;
begin
  InitWindow(screenWidth, screenHeight, 'raylib [models] example - animation blending');

  camera.position := Vector3Create(6.0, 6.0, 6.0);
  camera.target := Vector3Create(0.0, 2.0, 0.0);
  camera.up := Vector3Create(0.0, 1.0, 0.0);
  camera.fovy := 45.0;
  camera.projection := CAMERA_PERSPECTIVE;

  model := LoadModel(PChar(GetApplicationDirectory + 'resources/models/gltf/robot.glb'));
  position := Vector3Create(0.0, 0.0, 0.0);

  animCount := 0;
  anims := LoadModelAnimations(PChar(GetApplicationDirectory + 'resources/models/gltf/robot.glb'), @animCount);

  // Initialize animation names
  for i := 0 to animCount - 1 do
    animNames[i] := anims[i].name;
  animNames[animCount] := nil;

  currentAnimPlaying := 0;
  nextAnimToPlay := 1;
  animTransition := false;

  animIndex0 := 10;
  animCurrentFrame0 := 0.0;
  animFrameSpeed0 := 0.5;
  animIndex1 := 6;
  animCurrentFrame1 := 0.0;
  animFrameSpeed1 := 0.5;

  animBlendFactor := 0.0;
  animBlendTime := 2.0;
  animBlendTimeCounter := 0.0;

  animPause := false;

  animFrameProgress0 := 0.0;
  animFrameProgress1 := 0.0;
  animBlendProgress := 0.0;

  dropdownEditMode0 := false;
  dropdownEditMode1 := false;

  // Create animation names string for dropdown
  animNamesStr := '';
  for i := 0 to animCount - 1 do
  begin
    if i > 0 then animNamesStr := animNamesStr + ';';
    animNamesStr := animNamesStr + animNames[i];
  end;

  SetTargetFPS(60);

  while not WindowShouldClose() do
  begin
    UpdateCamera(@camera, CAMERA_ORBITAL);

    if IsKeyPressed(KEY_P) then animPause := not animPause;

    if not animPause then
    begin
      if IsKeyPressed(KEY_SPACE) and (not animTransition) then
      begin
        if currentAnimPlaying = 0 then
        begin
          nextAnimToPlay := 1;
          animCurrentFrame1 := 0.0;
        end
        else
        begin
          nextAnimToPlay := 0;
          animCurrentFrame0 := 0.0;
        end;

        animTransition := true;
        animBlendTimeCounter := 0.0;
        animBlendFactor := 0.0;
      end;

      if animTransition then
      begin
        animCurrentFrame0 := animCurrentFrame0 + animFrameSpeed0;
        if animCurrentFrame0 >= anims[animIndex0].keyframeCount then animCurrentFrame0 := 0.0;
        animCurrentFrame1 := animCurrentFrame1 + animFrameSpeed1;
        if animCurrentFrame1 >= anims[animIndex1].keyframeCount then animCurrentFrame1 := 0.0;

        animBlendFactor := animBlendTimeCounter / animBlendTime;
        if animBlendFactor > 1.0 then animBlendFactor := 1.0;
        animBlendTimeCounter := animBlendTimeCounter + GetFrameTime();
        animBlendProgress := animBlendFactor;

        if nextAnimToPlay = 1 then
          UpdateModelAnimationEx(model, anims[animIndex0], Trunc(animCurrentFrame0),
            anims[animIndex1], Trunc(animCurrentFrame1), animBlendFactor)
        else
          UpdateModelAnimationEx(model, anims[animIndex1], Trunc(animCurrentFrame1),
            anims[animIndex0], Trunc(animCurrentFrame0), animBlendFactor);

        if animBlendFactor >= 1.0 then
        begin
          if currentAnimPlaying = 0 then animCurrentFrame0 := 0.0
          else if currentAnimPlaying = 1 then animCurrentFrame1 := 0.0;
          currentAnimPlaying := nextAnimToPlay;

          animBlendFactor := 0.0;
          animTransition := false;
          animBlendTimeCounter := 0.0;
        end;
      end
      else
      begin
        if currentAnimPlaying = 0 then
        begin
          animCurrentFrame0 := animCurrentFrame0 + animFrameSpeed0;
          if animCurrentFrame0 >= anims[animIndex0].keyframeCount then animCurrentFrame0 := 0.0;
          UpdateModelAnimation(model, anims[animIndex0], Trunc(animCurrentFrame0));
        end
        else if currentAnimPlaying = 1 then
        begin
          animCurrentFrame1 := animCurrentFrame1 + animFrameSpeed1;
          if animCurrentFrame1 >= anims[animIndex1].keyframeCount then animCurrentFrame1 := 0.0;
          UpdateModelAnimation(model, anims[animIndex1], Trunc(animCurrentFrame1));
        end;
      end;
    end;

    animFrameProgress0 := animCurrentFrame0;
    animFrameProgress1 := animCurrentFrame1;

    BeginDrawing();
      ClearBackground(RAYWHITE);

      BeginMode3D(camera);
        DrawModel(model, position, 1.0, WHITE);
        DrawGrid(10, 1.0);
      EndMode3D();

      if animTransition then DrawText('ANIM TRANSITION BLENDING!', 170, 50, 30, BLUE);

      // Speed sliders
      if dropdownEditMode0 then GuiDisable();
      GuiSlider(RectangleCreate(10, 38, 160, 12),
        nil, PChar(Format('x%.1f', [animFrameSpeed0])), @animFrameSpeed0, 0.1, 2.0);
      GuiEnable();

      if dropdownEditMode1 then GuiDisable();
      GuiSlider(RectangleCreate(GetScreenWidth() - 170.0, 38, 160, 12),
        PChar(Format('%.1fx', [animFrameSpeed1])), nil, @animFrameSpeed1, 0.1, 2.0);
      GuiEnable();

      // Dropdown boxes for animation selection
      GuiSetStyle(DROPDOWNBOX, DROPDOWN_ITEMS_SPACING, 1);
      if GuiDropdownBox(RectangleCreate(10, 10, 160, 24),
        PChar(animNamesStr), @animIndex0, dropdownEditMode0) <> 0 then
        dropdownEditMode0 := not dropdownEditMode0;

      // Blend progress bar
      if nextAnimToPlay = 1 then
        GuiSetStyle(PROGRESSBAR, PROGRESS_SIDE, 0) // Left-->Right
      else
        GuiSetStyle(PROGRESSBAR, PROGRESS_SIDE, 1); // Right-->Left

      GuiProgressBar(RectangleCreate(180, 14, 440, 16), nil, nil, @animBlendProgress, 0.0, 1.0);
      GuiSetStyle(PROGRESSBAR, PROGRESS_SIDE, 0); // Reset to Left-->Right

      if GuiDropdownBox(RectangleCreate(GetScreenWidth() - 170.0, 10, 160, 24),
        PChar(animNamesStr), @animIndex1, dropdownEditMode1) <> 0 then
        dropdownEditMode1 := not dropdownEditMode1;

      // Label
      GuiSetStyle(LABEL_, TEXT_ALIGNMENT, TEXT_ALIGN_CENTER);
      GuiSetStyle(DEFAULT, TEXT_SIZE, GuiGetFont().baseSize * 2);
      GuiLabel(RectangleCreate(0, GetScreenHeight() - 100.0, GetScreenWidth(), 40),
        'PRESS SPACE to START BLENDING');
      GuiSetStyle(DEFAULT, TEXT_SIZE, GuiGetFont().baseSize);
      GuiSetStyle(LABEL_, TEXT_ALIGNMENT, TEXT_ALIGN_LEFT);

      // Timeline for anim0
      GuiProgressBar(RectangleCreate(60, GetScreenHeight() - 60.0, GetScreenWidth() - 180.0, 20), 'ANIM 0',
        PChar(Format('FRAME: %.2f / %d', [animFrameProgress0, anims[animIndex0].keyframeCount])),
        @animFrameProgress0, 0.0, anims[animIndex0].keyframeCount);

      for i := 0 to anims[animIndex0].keyframeCount - 1 do
        DrawRectangle(
          60 + Trunc(((GetScreenWidth() - 180) / anims[animIndex0].keyframeCount) * i),
          GetScreenHeight() - 60, 1, 20, BLUE);

      // Timeline for anim1
      GuiProgressBar(RectangleCreate(60, GetScreenHeight() - 30.0, GetScreenWidth() - 180.0, 20), 'ANIM 1',
        PChar(Format('FRAME: %.2f / %d', [animFrameProgress1, anims[animIndex1].keyframeCount])),
        @animFrameProgress1, 0.0, anims[animIndex1].keyframeCount);

      for i := 0 to anims[animIndex1].keyframeCount - 1 do
        DrawRectangle(
          60 + Trunc(((GetScreenWidth() - 180) / anims[animIndex1].keyframeCount) * i),
          GetScreenHeight() - 30, 1, 20, BLUE);

    EndDrawing();
  end;

  UnloadModelAnimations(anims, animCount);
  UnloadModel(model);
  CloseWindow();
end.
