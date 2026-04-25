// Game module — types, effects, constants, level parsing, and game logic

mod Game {
    pub type alias Vec3 = {r = Float32, g = Float32, b = Float32}

    pub type alias BrickTemplate = {
        position = Vec2D.Vec2,
        size = Vec2D.Vec2,
        color = Vec3,
        isSolid = Bool,
        texture = String
    }

    pub type alias LevelData = {
        bricks = List[BrickTemplate]
    }

    pub type alias GameState = {
        scene = SceneTree.Node,
        levels = List[LevelData],
        currentLevel = Int32,
        shakeTime = Float32
    }

    pub type alias TextureRegistry = Map[String, Int32]

    pub eff Game {
        def getInitialState(): GameState
        def render(scene: SceneTree.Node): Unit
        def shouldClose(): Bool
        def getDeltaTime(): Float32
        def isKeyPressed(keyCode: Int32): Bool
    }

    // ========================================================================
    // Constants
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
    // Texture Manifest
    // ========================================================================

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

    def loadLevel(path: String, levelWidth: Int32, levelHeight: Int32): LevelData \ Fs.FileRead =
        match Fs.FileRead.read(path) {
            case Ok(data) => parseLevel(data, levelWidth, levelHeight)
            case Err(_) => bug!("Failed to load level: ${path}")
        }

    pub def parseLevel(data: String, levelWidth: Int32, levelHeight: Int32): LevelData =
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

    def buildBricks(rows: List[List[Int32]], unitW: Float32, unitH: Float32): List[BrickTemplate] =
        rows |> (List.zipWithIndex >> List.flatMap(match (rowIdx, row) ->
            buildBrickRow(row, rowIdx, unitW, unitH)
        ))

    def buildBrickRow(row: List[Int32], rowIdx: Int32, unitW: Float32, unitH: Float32): List[BrickTemplate] =
        row |> (List.zipWithIndex >> List.filterMap(match (colIdx, tileType) ->
            if (tileType == 0) None
            else Some({
                position = {x = Int32.toFloat32(colIdx) * unitW, y = Int32.toFloat32(rowIdx) * unitH},
                size = {x = unitW, y = unitH},
                color = tileColor(tileType),
                isSolid = (tileType == 1),
                texture = if (tileType == 1) "block_solid" else "block"
            })
        ))

    def tileColor(tileType: Int32): Vec3 =
        match tileType {
            case 1 => {r = 0.8f32, g = 0.8f32, b = 0.7f32}
            case 2 => {r = 0.2f32, g = 0.6f32, b = 1.0f32}
            case 3 => {r = 0.0f32, g = 0.7f32, b = 0.0f32}
            case 4 => {r = 0.8f32, g = 0.8f32, b = 0.4f32}
            case 5 => {r = 1.0f32, g = 0.5f32, b = 0.0f32}
            case _ => {r = 1.0f32, g = 1.0f32, b = 1.0f32}
        }

    // ========================================================================
    // Game Initialization
    // ========================================================================

    pub def initGame(width: Int32, height: Int32): GameState \ Fs.FileRead =
        let level1 = loadLevel("levels/one.lvl", width, height / 2);
        let level2 = loadLevel("levels/two.lvl", width, height / 2);
        let level3 = loadLevel("levels/three.lvl", width, height / 2);
        let level4 = loadLevel("levels/four.lvl", width, height / 2);
        let levels = level1 :: level2 :: level3 :: level4 :: Nil;
        let scene = BreakoutScene.buildInitialScene(width, height, level1);
        {
            scene = scene,
            levels = levels,
            currentLevel = 0,
            shakeTime = 0.0f32
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
            let renderScene = BreakoutScene.applyShake(newState#shakeTime, newState#scene);
            Game.render(renderScene);
            runLoop(newState)
        }

    // ========================================================================
    // Game Logic — per-frame update
    // ========================================================================

    def updateGame(state: GameState): GameState \ Game =
        let dt = Game.getDeltaTime();
        let screenSize = {x = Int32.toFloat32(screenWidth()), y = Int32.toFloat32(screenHeight())};
        state
            |> processInput
            |> applyCollisions(dt, screenSize)
            |> tickShake(dt)

    // ========================================================================
    // Input Processing
    // ========================================================================

    pub def processInput(state: GameState): GameState \ Game =
        let velLeft = if (Game.isKeyPressed(keyA())) -1.0f32 * playerVelocity() else 0.0f32;
        let velRight = if (Game.isKeyPressed(keyD())) playerVelocity() else 0.0f32;
        let paddleVelX = velLeft + velRight;
        let s1 = {scene = BreakoutScene.setPaddleVelocity({x = paddleVelX, y = 0.0f32}, state#scene) | state};
        if (Game.isKeyPressed(keySpace())) {
            {scene = BreakoutScene.updateBall(bd -> {stuck = false | bd}, s1#scene) | s1}
        } else s1


    pub def applyCollisions(dt: Float32, screenSize: Vec2D.Vec2, state: GameState): GameState =
        run {
            let newScene = BreakoutScene.stepAndCollide(state#scene, dt, screenSize);
            {scene = newScene | state}
        } with handler SceneTree.OnCollision {
            def bodyEntered(nodeName, selfPos, body, k) =
                let s = k(());
                if (nodeName == BreakoutScene.ballName()) 
                    onBallBodyEntered(selfPos, body, s)
                else s
            def areaEntered(nodeName, _selfPos, body, k) =
                let s = k(());
                if (nodeName == BreakoutScene.ballName())
                    onBallAreaEntered(body, s)
                else s
        }
    // ========================================================================
    // Collision Handlers (Godot _on_*_entered style)
    // ========================================================================

    def onBallBodyEntered(selfPos: Vec2D.Vec2, body: SceneTree.BodyContact,
                          state: GameState): GameState =
        if(body#name == BreakoutScene.paddleName()) {
            let ball = BreakoutScene.ballSnapshot(state#scene);
            let direction = Vec2D.normalized(Vec2D.sub(selfPos, body#position));
            let speed = Vec2D.length(ball#vel);
            let newVel = Vec2D.mul(direction, speed);
            let scene1 = BreakoutScene.updateBall(bd -> {velocity = newVel | bd}, state#scene);
            {scene = scene1 | state}
        } else if(body#group == BreakoutScene.bricksName()) {
            if (body#isSolid)
                {shakeTime = 0.15f32 | state}
            else 
                let scene = BreakoutScene.destroyBrick(body, state#scene);
                {scene = scene | state}
         } else state

    def onBallAreaEntered(body: SceneTree.BodyContact,
                          state: GameState): GameState =
        if(body#name == BreakoutScene.deathZoneName())
            let scene1 = BreakoutScene.updateBall(
                _ -> {velocity = {x = initialBallVelocityX(), y = initialBallVelocityY()},
                        stuck = true}, state#scene);
            {scene = scene1 | state}
        else state

    pub def tickShake(dt: Float32, state: GameState): GameState =
        if (state#shakeTime > 0.0f32)
            {shakeTime = Float32.max(0.0f32, state#shakeTime - dt) | state}
        else state
}
