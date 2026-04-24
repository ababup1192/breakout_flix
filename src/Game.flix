// Game module — types, effects, constants, level parsing, and game logic

mod Game {
    pub enum Direction with Eq, ToString {
        case Up
        case Right
        case Down
        case Left
    }

    pub enum GameEvent with Eq, ToString {
        case BrickDestroyed(Int32)
        case PaddleHit
        case BallReset
        case BallLaunched
    }

    pub type alias Vec2 = {x = Float32, y = Float32}
    pub type alias Vec3 = {r = Float32, g = Float32, b = Float32}

    pub type alias GameObject = {
        position = Vec2,
        size = Vec2,
        color = Vec3,
        isSolid = Bool,
        destroyed = Bool,
        texture = String
    }

    pub type alias GameLevel = {
        bricks = List[GameObject]
    }

    pub type alias GameState = {
        levels = List[GameLevel],
        currentLevel = Int32,
        player = GameObject,
        ballPos = Vec2,
        ballVelocity = Vec2,
        ballRadius = Float32,
        ballStuck = Bool
    }

    type alias BrickFoldAcc = (List[GameObject], Vec2, Vec2, List[GameEvent], Int32)

    pub type alias TextureRegistry = Map[String, Int32]

    pub eff Game {
        def getInitialState(): GameState
        def render(scene: SceneTree.Node): Unit
        def shouldClose(): Bool
        def getDeltaTime(): Float32
        def isKeyPressed(keyCode: Int32): Bool
    }

    // ========================================================================
    // Constants (from LearnOpenGL tutorial)
    // ========================================================================

    pub def screenWidth(): Int32 = 800
    pub def screenHeight(): Int32 = 600
    pub def playerWidth(): Float32 = 100.0f32
    pub def playerHeight(): Float32 = 20.0f32
    pub def playerVelocity(): Float32 = 500.0f32
    pub def ballRadiusConst(): Float32 = 12.5f32
    pub def initialBallVelocityX(): Float32 = 100.0f32
    pub def initialBallVelocityY(): Float32 = -350.0f32

    pub def keyA(): Int32 = 65
    pub def keyD(): Int32 = 68
    pub def keySpace(): Int32 = 32

    // ========================================================================
    // Texture Manifest (data-driven texture registration)
    // ========================================================================

    /// (name, path, hasAlpha) — add a line here for new textures
    pub def textureManifest(): List[(String, String, Bool)] =
        ("background", "textures/background.jpg", false) ::
        ("block",      "textures/block.png",      true) ::
        ("block_solid", "textures/block_solid.png", true) ::
        ("paddle",     "textures/paddle.png",      true) ::
        ("ball",       "textures/awesomeface.png", true) ::
        Nil

    // ========================================================================
    // Level Parsing
    // ========================================================================

    /// .lvlファイルを読み込み、レベルデータをパースして GameLevel を返す
    def loadLevel(path: String, levelWidth: Int32, levelHeight: Int32): GameLevel \ Fs.FileRead =
        match Fs.FileRead.read(path) {
            case Ok(data) => parseLevel(data, levelWidth, levelHeight)
            case Err(_) => bug!("Failed to load level: ${path}")
        }

    /// レベルデータ文字列をパースし、ブリックのGameObjectリストに変換
    pub def parseLevel(data: String, levelWidth: Int32, levelHeight: Int32): GameLevel =
        let validRows = data
            |> String.split({regex = "\n"})
            |> List.map(parseLevelLine)
            |> List.filter(row -> not List.isEmpty(row));
        let numRows = List.length(validRows);
        let numCols = List.head(validRows) |> Option.map(List.length) |> Option.getWithDefault(0);

        let unitWidth = if (numCols > 0) Int32.toFloat32(levelWidth) / Int32.toFloat32(numCols) else 0.0f32;
        let unitHeight = if (numRows > 0) Int32.toFloat32(levelHeight) / Int32.toFloat32(numRows) else 0.0f32;

        let bricks = buildBricks(validRows, unitWidth, unitHeight);
        {bricks = bricks}

    /// スペース区切りの1行をInt32のリストにパース
    pub def parseLevelLine(line: String): List[Int32] =
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
    def buildBricks(rows: List[List[Int32]], unitW: Float32, unitH: Float32): List[GameObject] =
        rows |> (List.zipWithIndex >> List.flatMap(match (rowIdx, row) ->
            buildBrickRow(row, rowIdx, unitW, unitH)
        ))

    /// 1行分のタイルタイプ列からブリックを生成。タイプ0はスキップ、1はsolid
    def buildBrickRow(row: List[Int32], rowIdx: Int32, unitW: Float32, unitH: Float32): List[GameObject] =
        row |> (List.zipWithIndex >> List.filterMap(match (colIdx, tileType) ->
            if (tileType == 0) None
            else Some({
                position = {x = Int32.toFloat32(colIdx) * unitW, y = Int32.toFloat32(rowIdx) * unitH},
                size = {x = unitW, y = unitH},
                color = tileColor(tileType),
                isSolid = (tileType == 1),
                destroyed = false,
                texture = if (tileType == 1) "block_solid" else "block"
            })
        ))

    /// タイルタイプ(1-5)をLearnOpenGLの配色に対応するRGB色に変換
    def tileColor(tileType: Int32): Vec3 =
        match tileType {
            case 1 => {r = 0.8f32, g = 0.8f32, b = 0.7f32}  // Solid - tan/gray
            case 2 => {r = 0.2f32, g = 0.6f32, b = 1.0f32}  // Blue
            case 3 => {r = 0.0f32, g = 0.7f32, b = 0.0f32}  // Green
            case 4 => {r = 0.8f32, g = 0.8f32, b = 0.4f32}  // Yellow
            case 5 => {r = 1.0f32, g = 0.5f32, b = 0.0f32}  // Orange
            case _ => {r = 1.0f32, g = 1.0f32, b = 1.0f32}  // White
        }

    // ========================================================================
    // Game Initialization
    // ========================================================================

    /// 4レベル・プレイヤー・ボールの初期状態を構築して GameState を返す
    pub def initGame(width: Int32, height: Int32): GameState \ Fs.FileRead =
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
            texture = "paddle"
        };

        let ballX = playerX + playerWidth() / 2.0f32 - ballRadiusConst();
        let ballY = playerY - ballRadiusConst() * 2.0f32;

        {
            levels = level1 :: level2 :: level3 :: level4 :: Nil,
            currentLevel = 0,
            player = player,
            ballPos = {x = ballX, y = ballY},
            ballVelocity = {x = initialBallVelocityX(), y = initialBallVelocityY()},
            ballRadius = ballRadiusConst(),
            ballStuck = true
        }

    // ========================================================================
    // Game Loop
    // ========================================================================

    pub def startGame(): GameState \ Game =
        let state = Game.getInitialState();
        runLoop(state)

    @Tailrec
    def runLoop(state: GameState): GameState \ Game =
        if (Game.shouldClose()) state
        else {
            let newState = updateGame(state);
            Game.render(BreakoutScene.toScene(newState));
            runLoop(newState)
        }

    // ========================================================================
    // Game Logic — per-frame update
    // ========================================================================

    /// 1フレームの更新を統合: 入力処理 → ボール移動 → 衝突処理 → リセット判定
    def updateGame(state: GameState): GameState \ Game =
        let dt = Game.getDeltaTime();
        let (finalState, allEvents) = GameAction.unpack(forM (
            s1 <- processInput(state, dt);
            s2 <- doCollisions(moveBall(s1, dt));
            s3 <- checkBallReset(s2)
        ) yield s3);
        handleEvents(finalState, allEvents)

    /// イベントハンドラ（将来、スコア・サウンド・エフェクト等を追加）
    def handleEvents(state: GameState, _events: List[GameEvent]): GameState =
        state

    /// パドルを dx だけ移動（ボールが貼り付いていればボールも追従）
    def movePlayer(state: GameState, dx: Float32): GameState =
        let sw = Int32.toFloat32(screenWidth());
        let rawX = state#player#position#x + dx;
        let newX = Float32.max(0.0f32, Float32.min(sw - playerWidth(), rawX));
        let newPlayer = {position = {x = newX, y = state#player#position#y} | state#player};
        let s = {player = newPlayer | state};
        if (s#ballStuck) {
            let bx = newX + playerWidth() / 2.0f32 - ballRadiusConst();
            {ballPos = {x = bx, y = s#ballPos#y} | s}
        } else s

    /// パドル移動 + ボール発射
    pub def processInput(state: GameState, dt: Float32): GameAction.GameAction[GameState] \ Game =
        let s1 = if (Game.isKeyPressed(keyA()))
                     movePlayer(state, -1.0f32 * playerVelocity() * dt)
                 else state;
        let s2 = if (Game.isKeyPressed(keyD()))
                     movePlayer(s1, playerVelocity() * dt)
                 else s1;
        if (Game.isKeyPressed(keySpace())) {
            let events = if (s2#ballStuck) GameEvent.BallLaunched :: Nil else Nil;
            GameAction.of({ballStuck = false | s2}, events)
        } else GameAction.pure(s2)

    /// ボール物理: 移動 + 壁反射（純粋関数）
    pub def moveBall(state: GameState, dt: Float32): GameState =
        if (state#ballStuck) state
        else {
            let sw = Int32.toFloat32(screenWidth());
            let ballSize = state#ballRadius * 2.0f32;
            let newX = state#ballPos#x + state#ballVelocity#x * dt;
            let newY = state#ballPos#y + state#ballVelocity#y * dt;
            let vx0 = state#ballVelocity#x;
            let vy0 = state#ballVelocity#y;
            // 左壁
            let (nx1, vx1) = if (newX <= 0.0f32) (0.0f32, Float32.abs(vx0)) else (newX, vx0);
            // 右壁
            let (nx2, vx2) = if (nx1 + ballSize >= sw) (sw - ballSize, -1.0f32 * Float32.abs(vx1)) else (nx1, vx1);
            // 上壁
            let (ny1, vy1) = if (newY <= 0.0f32) (0.0f32, Float32.abs(vy0)) else (newY, vy0);
            {ballPos = {x = nx2, y = ny1}, ballVelocity = {x = vx2, y = vy1} | state}
        }

    /// 画面下に落ちたらリセット（BallResetイベントを発火）
    pub def checkBallReset(state: GameState): GameAction.GameAction[GameState] =
        let sh = Int32.toFloat32(screenHeight());
        if (state#ballPos#y >= sh) {
            let px = state#player#position#x + playerWidth() / 2.0f32 - state#ballRadius;
            let py = state#player#position#y - state#ballRadius * 2.0f32;
            GameAction.of(
                {ballPos = {x = px, y = py},
                 ballVelocity = {x = initialBallVelocityX(), y = initialBallVelocityY()},
                 ballStuck = true | state},
                GameEvent.BallReset :: Nil)
        } else GameAction.pure(state)

    // ========================================================================
    // Collision Detection (LearnOpenGL Circle-AABB)
    // ========================================================================

    /// Newton法によるFloat32平方根
    def sqrt32(v: Float32): Float32 =
        if (v <= 0.0f32) 0.0f32
        else newtonSqrt(v, v, 15)

    @Tailrec
    def newtonSqrt(v: Float32, x: Float32, n: Int32): Float32 =
        if (n <= 0) x
        else newtonSqrt(v, (x + v / x) / 2.0f32, n - 1)

    /// 差分ベクトルが最も近いコンパス方向を返す
    pub def vectorDirection(target: Vec2): Direction =
        let dotUp    = target#y;
        let dotRight = target#x;
        let dotDown  = -1.0f32 * target#y;
        let dotLeft  = -1.0f32 * target#x;
        let (d0, v0) = (Direction.Up, dotUp);
        let (d1, v1) = if (dotRight > v0) (Direction.Right, dotRight) else (d0, v0);
        let (d2, v2) = if (dotDown > v1) (Direction.Down, dotDown) else (d1, v1);
        let (d3, _)  = if (dotLeft > v2) (Direction.Left, dotLeft) else (d2, v2);
        d3

    /// Circle-AABB衝突判定: (衝突有無, 方向, 差分ベクトル) を返す
    pub def checkCollisionCircleAABB(ballPos: Vec2, radius: Float32, box: GameObject): (Bool, Direction, Vec2) =
        let ballCenter = {x = ballPos#x + radius, y = ballPos#y + radius};
        let halfExtents = {x = box#size#x / 2.0f32, y = box#size#y / 2.0f32};
        let aabbCenter = {x = box#position#x + halfExtents#x, y = box#position#y + halfExtents#y};
        let diff = {x = ballCenter#x - aabbCenter#x, y = ballCenter#y - aabbCenter#y};
        let clamped = {
            x = Float32.max(-1.0f32 * halfExtents#x, Float32.min(halfExtents#x, diff#x)),
            y = Float32.max(-1.0f32 * halfExtents#y, Float32.min(halfExtents#y, diff#y))
        };
        let closest = {x = aabbCenter#x + clamped#x, y = aabbCenter#y + clamped#y};
        let difference = {x = closest#x - ballCenter#x, y = closest#y - ballCenter#y};
        let distSq = difference#x * difference#x + difference#y * difference#y;
        if (distSq < radius * radius)
            (true, vectorDirection(difference), difference)
        else
            (false, Direction.Up, {x = 0.0f32, y = 0.0f32})

    /// ボールの反射処理: 衝突方向に応じて位置と速度を補正
    def reflectBall(bPos: Vec2, bVel: Vec2, radius: Float32, dir: Direction, diff: Vec2): (Vec2, Vec2) =
        let horizontal = (dir == Direction.Left) or (dir == Direction.Right);
        if (horizontal) {
            let penetration = radius - Float32.abs(diff#x);
            let newX = if (dir == Direction.Left) bPos#x + penetration else bPos#x - penetration;
            ({x = newX, y = bPos#y}, {x = -1.0f32 * bVel#x, y = bVel#y})
        } else {
            let penetration = radius - Float32.abs(diff#y);
            let newY = if (dir == Direction.Up) bPos#y - penetration else bPos#y + penetration;
            ({x = bPos#x, y = newY}, {x = bVel#x, y = -1.0f32 * bVel#y})
        }

    /// 1ブリックとの衝突判定・反射を fold のステップとして実行
    def brickCollisionStep(radius: Float32, acc: BrickFoldAcc, brick: GameObject): BrickFoldAcc =
        let (bricks, bPos, bVel, events, idx) = acc;
        if (brick#destroyed)
            (brick :: bricks, bPos, bVel, events, idx + 1)
        else {
            let (collision, dir, diff) = checkCollisionCircleAABB(bPos, radius, brick);
            if (not collision)
                (brick :: bricks, bPos, bVel, events, idx + 1)
            else {
                let newBrick = if (brick#isSolid) brick else {destroyed = true | brick};
                let newEvents = if (brick#isSolid) events else GameEvent.BrickDestroyed(idx) :: events;
                let (newPos, newVel) = reflectBall(bPos, bVel, radius, dir, diff);
                (newBrick :: bricks, newPos, newVel, newEvents, idx + 1)
            }
        }

    /// レベルリスト中の指定インデックスのブリックを差し替え
    def updateLevelBricks(levels: List[GameLevel], levelIdx: Int32, newBricks: List[GameObject]): List[GameLevel] =
        let newLevel = {bricks = newBricks};
        levels |> List.zipWithIndex |> List.map(match (i, lvl) ->
            if (i == levelIdx) newLevel else lvl
        )

    /// 全衝突処理: ブリック衝突 → パドル衝突（イベント付き）
    def doCollisions(state: GameState): GameAction.GameAction[GameState] =
        if (state#ballStuck) GameAction.pure(state)
        else {
            forM (
                s1 <- doBrickCollisions(state);
                s2 <- doPaddleCollision(s1)
            ) yield s2
        }

    /// ブリック衝突処理: 各ブリックとの衝突を検出し、破壊+ボール反射
    def doBrickCollisions(state: GameState): GameAction.GameAction[GameState] =
        let processLevel = level -> {
            let init: BrickFoldAcc = (Nil, state#ballPos, state#ballVelocity, Nil, 0);
            let (newBricks, newBallPos, newBallVel, events, _) =
                List.foldLeft(brickCollisionStep(state#ballRadius), init, level#bricks);
            let newLevels = updateLevelBricks(state#levels, state#currentLevel, List.reverse(newBricks));
            GameAction.of(
                {levels = newLevels, ballPos = newBallPos, ballVelocity = newBallVel | state},
                List.reverse(events))
        };
        List.nth(state#currentLevel, state#levels)
            |> Option.map(processLevel)
            |> Option.getWithDefault(GameAction.pure(state))

    /// パドル衝突処理: 打ち返し角度をヒット位置に基づき調整
    def doPaddleCollision(state: GameState): GameAction.GameAction[GameState] =
        let (collision, _, _) = checkCollisionCircleAABB(state#ballPos, state#ballRadius, state#player);
        if (not collision) GameAction.pure(state)
        else {
            let centerBoard = state#player#position#x + state#player#size#x / 2.0f32;
            let distance = (state#ballPos#x + state#ballRadius) - centerBoard;
            let percentage = distance / (state#player#size#x / 2.0f32);
            let strength = 2.0f32;
            let oldVx = state#ballVelocity#x;
            let oldVy = state#ballVelocity#y;
            let speed = sqrt32(oldVx * oldVx + oldVy * oldVy);
            let newVx = initialBallVelocityX() * percentage * strength;
            let newVy = -1.0f32 * Float32.abs(state#ballVelocity#y);
            let len = sqrt32(newVx * newVx + newVy * newVy);
            let finalVx = if (len > 0.0f32) newVx / len * speed else newVx;
            let finalVy = if (len > 0.0f32) newVy / len * speed else newVy;
            GameAction.of(
                {ballVelocity = {x = finalVx, y = finalVy} | state},
                GameEvent.PaddleHit :: Nil)
        }
}
