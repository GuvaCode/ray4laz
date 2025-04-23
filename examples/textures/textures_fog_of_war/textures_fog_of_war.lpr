program textures_fog_of_war;

{$mode objfpc}{$H+}

uses 
cmem, 
raylib;

const
  screenWidth = 800;
  screenHeight = 450;
  MAP_TILE_SIZE =  32;          // Tiles size 32x32 pixels
  PLAYER_SIZE =    16;          // Player size
  PLAYER_TILE_VISIBILITY = 2;   // Player can see 2 tiles around its position

  // Map data type
  type TMap = record
    TilesX: Cardinal;            // Number of tiles in X axis
    TilesY: Cardinal;            // Number of tiles in Y axis
    TileIds: PByte;         // Tile ids (tilesX*tilesY), defines type of tile to draw
    TileFog: PByte;         // Tile fog state (tilesX*tilesY), defines if a tile has fog or half-fog
  end;

var
  Map: TMap;
  I: Integer;
  PlayerPosition: TVector2;
  PlayerTileX, PlayerTileY: Integer;
  FogOfWar: TRenderTexture2D;
  X, Y: Integer;
  Color: TColor;

begin
  // Initialization
  //--------------------------------------------------------------------------------------
  InitWindow(screenWidth, screenHeight, 'raylib [textures] example - fog of war');

  Map := Default(TMap);
  Map.TilesX := 25;
  Map.TilesY := 15;

  // NOTE: We can have up to 256 values for tile ids and for tile fog state,
  // probably we don't need that many values for fog state, it can be optimized
  // to use only 2 bits per fog state (reducing size by 4) but logic will be a bit more complex
  Map.tileIds := AllocMem(Map.TilesX * Map.TilesY * SizeOf(Byte));
  Map.TileFog := AllocMem(Map.TilesX * Map.TilesY * sizeof(Byte));

  // Load map tiles (generating 2 random tile ids for testing)
  // NOTE: Map tile ids should be probably loaded from an external map file
  for I := 0 to Map.TilesY * Map.TilesX - 1 do
    Map.TileIds[I] := GetRandomValue(0, 1);

  // Player position on the screen (pixel coordinates, not tile coordinates)
  PlayerPosition := Vector2Create(180, 130);

  // Render texture to render fog of war
  // NOTE: To get an automatic smooth-fog effect we use a render texture to render fog
  // at a smaller size (one pixel per tile) and scale it on drawing with bilinear filtering
  FogOfWar := LoadRenderTexture(Map.TilesX, Map.TilesY);
  SetTextureFilter(FogOfWar.Texture, TEXTURE_FILTER_BILINEAR);

  SetTargetFPS(60); // Set our game to run at 60 frames-per-second
  //--------------------------------------------------------------------------------------
  // Main game loop
  while not WindowShouldClose() do
    begin
      // Update
      //----------------------------------------------------------------------------------
      // Move player around
      if IsKeyDown(KEY_RIGHT) then PlayerPosition.X := PlayerPosition.X + 5;
      if IsKeyDown(KEY_LEFT) then PlayerPosition.X := PlayerPosition.X - 5;
      if IsKeyDown(KEY_DOWN) then PlayerPosition.Y := PlayerPosition.Y + 5;
      if IsKeyDown(KEY_UP) then PlayerPosition.Y := PlayerPosition.Y - 5;

      // Check player position to avoid moving outside tilemap limits
      if PlayerPosition.X < 0 then
        PlayerPosition.X := 0
      else if (PlayerPosition.X + PLAYER_SIZE) > (Map.TilesX * MAP_TILE_SIZE) then
        PlayerPosition.X := Map.TilesX * MAP_TILE_SIZE - PLAYER_SIZE;
      if PlayerPosition.Y < 0 then
        PlayerPosition.Y := 0
      else if (PlayerPosition.Y + PLAYER_SIZE) > (Map.TilesY * MAP_TILE_SIZE) then
        PlayerPosition.Y := Map.TilesY * MAP_TILE_SIZE - PLAYER_SIZE;

      // Previous visited tiles are set to partial fog
      for I := 0 to Map.TilesY * Map.TilesX - 1 do
        if Map.TileFog[I] = 1 then
          Map.TileFog[I] := 2;

      // Get current tile position from player pixel position
      PlayerTileX := Trunc(PlayerPosition.X + MAP_TILE_SIZE div 2) div MAP_TILE_SIZE;
      PlayerTileY := Trunc(PlayerPosition.Y + MAP_TILE_SIZE div 2) div MAP_TILE_SIZE;

      // Check visibility and update fog
      // NOTE: We check tilemap limits to avoid processing tiles out-of-array-bounds (it could crash program)
      for Y := PlayerTileY - PLAYER_TILE_VISIBILITY to PlayerTileY + PLAYER_TILE_VISIBILITY - 1 do
        for X := PlayerTileX - PLAYER_TILE_VISIBILITY to PlayerTileX + PLAYER_TILE_VISIBILITY - 1 do
          if (X >= 0) and (X < Integer(Map.TilesX)) and (Y >= 0) and (Y < Integer(Map.TilesY)) then
            Map.TileFog[Y * Integer(Map.TilesX) + X] := 1;
      //----------------------------------------------------------------------------------

      // Draw
      //----------------------------------------------------------------------------------
      // Draw fog of war to a small render texture for automatic smoothing on scaling
      BeginTextureMode(FogOfWar);
        ClearBackground(BLANK);
        for Y := 0 to Map.TilesY - 1 do
          for X := 0 to Map.TilesX - 1 do
            if Map.TileFog[Y * Integer(Map.TilesX) + X] = 0 then
              DrawRectangle(X, Y, 1, 1, BLACK)
            else if Map.TileFog[Y * Integer(Map.TilesX) + X] = 2 then
              DrawRectangle(X, Y, 1, 1, Fade(BLACK, 0.8));
      EndTextureMode();

      BeginDrawing();

        ClearBackground(RAYWHITE);

        for Y := 0 to Map.TilesY - 1 do
        begin
          for X := 0 to Map.TilesX - 1 do
          begin
            // Draw tiles from id (and tile borders)
            if Map.TileIds[Y * Integer(Map.TilesX) + X] = 0 then Color := BLUE else Color := Fade(BLUE, 0.9);
            DrawRectangle(X * MAP_TILE_SIZE, Y * MAP_TILE_SIZE, MAP_TILE_SIZE, MAP_TILE_SIZE, Color);

            DrawRectangleLines(X * MAP_TILE_SIZE, Y * MAP_TILE_SIZE, MAP_TILE_SIZE, MAP_TILE_SIZE, Fade(DARKBLUE, 0.5));
          end;
        end;

        // Draw player
        DrawRectangleV(PlayerPosition, Vector2Create(PLAYER_SIZE, PLAYER_SIZE), RED);


        // Draw fog of war (scaled to full map, bilinear filtering)
        DrawTexturePro(FogOfWar.Texture, RectangleCreate(0, 0, FogOfWar.Texture.Width, -FogOfWar.Texture.Height),
                       RectangleCreate(0, 0, Map.TilesX * MAP_TILE_SIZE, Map.TilesY * MAP_TILE_SIZE),
                       Vector2Create(0, 0), 0.0, WHITE);

        // Draw player current tile
        DrawText(TextFormat('Current tile: [%i,%i]', PlayerTileX, PlayerTileY), 10, 10, 20, LIME);

      EndDrawing();
       end;
  // De-Initialization
  //--------------------------------------------------------------------------------------
  FreeMem(Map.TileIds);      // Free allocated map tile ids
  FreeMem(Map.TileFog);      // Free allocated map tile fog state

  UnloadRenderTexture(FogOfWar);  // Unload render texture

  CloseWindow();        // Close window and OpenGL context
  //--------------------------------------------------------------------------------------
end.

