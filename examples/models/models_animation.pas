program animation_test;

{$MODE objfpc}

uses cmem, raylib, rlgl, raymath;

const
	screenWidth = 800;
	screenHeight = 450;

var
  cam: TCamera;
  model : TModel;
  texture : TTexture2D;
  position : TVector3;
  animsCount, animFrameCounter : Integer;
  anims : PModelAnimation;
  i : Integer;
  bb:TBoundingBox;
  mat: TMatrix;
  vv: TVector3;
  rot:single;

procedure DrawModelFx(AModel:TModel; APosition:TVector3; AAxis:TVector3; AAngle:Single; AScale:Single; ATint:TColor);
var
  Scale:TVector3;
begin
  Scale:=Vector3Create(AScale,AScale,AScale);
  DrawModelEx(AModel, APosition, AAxis, AAngle, Scale, ATint);
end;

begin

	InitWindow(screenWidth, screenHeight, 'Animation Test');	

	cam.position := Vector3Create(15.0, 15.0, 15.0);
	cam.target := Vector3Create(0.0,0.0,0.0);
	cam.up := Vector3Create(0.0, 1.0, 0.0);
	cam.fovy := 45.0;
	cam.projection := CAMERA_PERSPECTIVE;

	model := LoadModel('resources/models/iqm/guy.iqm');
	texture := LoadTexture('resources/models/iqm/guytex.png');
	SetMaterialTexture(@model.materials[0], MATERIAL_MAP_DIFFUSE, texture);

	position := Vector3Create(0.0,0.0,0.0);

	// Load Animation Data
	animsCount := 0;
	anims := LoadModelAnimations('resources/models/iqm/guy.iqm', @animsCount);
	animFrameCounter := 0;

	WriteLn('animsCount: ', animsCount);

	SetCameraMode(cam, CAMERA_THIRD_PERSON);

	SetTargetFPS(60);

	while not WindowShouldClose() do
	begin
		// Test that we are receiving the Struct data back correctly (YES)
		//mousePos := GetMousePosition();
		//WriteLn('Mouse Pos: ', FloatToStr(mousePos.x), ', ', FloatToStr(mousePos.y));

		UpdateCamera(@cam);

		if IsKeyDown(KEY_SPACE) then
		begin
			inc(animFrameCounter);
			UpdateModelAnimation(model, anims[0], animFrameCounter);
			if animFrameCounter >= anims[0].frameCount then
				animFrameCounter := 0;
                        rot:=rot+1;
		end;

		BeginDrawing();
			ClearBackground(RAYWHITE);
			BeginMode3d(cam);

                            //    DrawModelFx(model, position, Vector3Create(1.0, 0.0, 0.0), -60, 1, WHITE); //fix for wondows tnx realmworksxyz
                                DRawModelEx(model, position, Vector3Create(1.0, 0.0, 0.0), rot, Vector3Create(1,1,1), WHITE);

                                for i := 0 to model.boneCount - 1 do
				begin

                                bb:= GetMeshBoundingBox(model.meshes[0]);
                                bb.min:=Vector3Scale(bb.min,1);
                                bb.max:=Vector3Scale(bb.max,1);

                               // mat:= model.transform;
                                mat:= MatrixRotate(

                                Vector3Create(-1.0, 0.0, 0.0),

                                rot * DEG2RAD);

                                bb.min:=Vector3Transform(bb.min,mat);
                                bb.max:=Vector3Transform(bb.max,mat);

                                DrawBoundingBox(bb,blue);


                                vv:=Vector3Transform(anims[0].framePoses[animFrameCounter][i].translation,mat);


                                DrawCube(vv, 0.2, 0.2, 0.2, RED);

                                 model.transform:=MatrixRotate( Vector3Create(-1.0, 0.0, 0.0),rot * DEG2RAD);



                                end;
				DrawGrid(10, 1.0);
                       EndMode3D();
			DrawFPS(10, 10);
		EndDrawing();
	end;
	UnloadTexture(texture);
	for i := 0 to animsCount - 1 do
		UnloadModelAnimation(anims[i]);
	
        Free(anims);
	UnloadModel(model);
	CloseWindow();
end.
