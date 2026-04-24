// Breakout - Faithful LearnOpenGL Recreation in Flix
// Constants, level parsing, game initialization, and entry point

// ============================================================================
// Constants (from LearnOpenGL tutorial)
// ============================================================================

def screenWidth(): Int32 = 800
def screenHeight(): Int32 = 600
def playerWidth(): Float32 = 100.0f32
def playerHeight(): Float32 = 20.0f32
def playerVelocity(): Float32 = 500.0f32
def ballRadius(): Float32 = 12.5f32
def initialBallVelocityX(): Float32 = 100.0f32
def initialBallVelocityY(): Float32 = -350.0f32

// ============================================================================
// Level Data (from LearnOpenGL .lvl files)
// ============================================================================

/// .lvlファイルを読み込み、レベルデータをパースして GameLevel を返す
def loadLevel(path: String, levelWidth: Int32, levelHeight: Int32): Game.GameLevel \ Fs.FileRead =
    match Fs.FileRead.read(path) {
        case Ok(data) => parseLevel(data, levelWidth, levelHeight)
        case Err(_) => bug!("Failed to load level: ${path}")
    }

/// レベルデータ文字列をパースし、ブリックのGameObjectリストに変換
def parseLevel(data: String, levelWidth: Int32, levelHeight: Int32): Game.GameLevel =
    let lines = String.split({regex = "\n"}, data);
    let rows = List.map(parseLevelLine, lines);
    let validRows = List.filter(row -> not List.isEmpty(row), rows);
    let numRows = List.length(validRows);
    let numCols = List.head(validRows) |> Option.map(List.length) |> Option.getWithDefault(0);

    let unitWidth = if (numCols > 0) Int32.toFloat32(levelWidth) / Int32.toFloat32(numCols) else 0.0f32;
    let unitHeight = if (numRows > 0) Int32.toFloat32(levelHeight) / Int32.toFloat32(numRows) else 0.0f32;

    let bricks = buildBricks(validRows, unitWidth, unitHeight);
    {bricks = bricks}

/// スペース区切りの1行をInt32のリストにパース
def parseLevelLine(line: String): List[Int32] =
    let trimmed = String.trim(line);
    if (String.isEmpty(trimmed)) Nil
    else
        trimmed
            |> String.split({regex = " "})
            |> List.filterMap(s -> {
                let t = String.trim(s);
                if (String.isEmpty(t)) None else Int32.fromString(t)
            })

/// パース済み全行からブリックのGameObjectリストを構築
def buildBricks(rows: List[List[Int32]], unitW: Float32, unitH: Float32): List[Game.GameObject] =
    rows |> (List.zipWithIndex >> List.flatMap(match (rowIdx, row) ->
        buildBrickRow(row, rowIdx, unitW, unitH)
    ))

/// 1行分のタイルタイプ列からブリックを生成。タイプ0はスキップ、1はsolid
def buildBrickRow(row: List[Int32], rowIdx: Int32, unitW: Float32, unitH: Float32): List[Game.GameObject] =
    row |> (List.zipWithIndex >> List.filterMap(match (colIdx, tileType) ->
        if (tileType == 0) None
        else Some({
            position = {x = Int32.toFloat32(colIdx) * unitW, y = Int32.toFloat32(rowIdx) * unitH},
            size = {x = unitW, y = unitH},
            color = tileColor(tileType),
            isSolid = (tileType == 1),
            destroyed = false,
            textureKind = if (tileType == 1) Game.TextureKind.BlockSolid else Game.TextureKind.Block
        })
    ))

/// タイルタイプ(1-5)をLearnOpenGLの配色に対応するRGB色に変換
def tileColor(tileType: Int32): Game.Vec3 =
    match tileType {
        case 1 => {r = 0.8f32, g = 0.8f32, b = 0.7f32}  // Solid - tan/gray
        case 2 => {r = 0.2f32, g = 0.6f32, b = 1.0f32}  // Blue
        case 3 => {r = 0.0f32, g = 0.7f32, b = 0.0f32}  // Green
        case 4 => {r = 0.8f32, g = 0.8f32, b = 0.4f32}  // Yellow
        case 5 => {r = 1.0f32, g = 0.5f32, b = 0.0f32}  // Orange
        case _ => {r = 1.0f32, g = 1.0f32, b = 1.0f32}  // White
    }

// ============================================================================
// Game Initialization
// ============================================================================

/// 4レベル・プレイヤー・ボールの初期状態を構築して GameState を返す
def initGame(width: Int32, height: Int32): Game.GameState \ Fs.FileRead =
    let level1 = loadLevel("levels/one.lvl", width, height / 2);
    let level2 = loadLevel("levels/two.lvl", width, height / 2);
    let level3 = loadLevel("levels/three.lvl", width, height / 2);
    let level4 = loadLevel("levels/four.lvl", width, height / 2);

    let playerX = Int32.toFloat32(width) / 2.0f32 - playerWidth() / 2.0f32;
    let playerY = Int32.toFloat32(height) - playerHeight();

    let player = {
        position = {x = playerX, y = playerY},
        size = {x = playerWidth(), y = playerHeight()},
        color = {r = 1.0f32, g = 1.0f32, b = 1.0f32},
        isSolid = true,
        destroyed = false,
        textureKind = Game.TextureKind.Paddle
    };

    let ballX = playerX + playerWidth() / 2.0f32 - ballRadius();
    let ballY = playerY - ballRadius() * 2.0f32;

    {
        levels = level1 :: level2 :: level3 :: level4 :: Nil,
        currentLevel = 0,
        player = player,
        ballPos = {x = ballX, y = ballY},
        ballRadius = ballRadius()
    }

// ============================================================================
// Main Entry Point
// ============================================================================

def main(): Unit \ IO =
    println("Breakout - LearnOpenGL Recreation");
    discard run {
        Game.startGame()
    } with LwjglLayer.withLwjgl;
    println("Game ended.")
