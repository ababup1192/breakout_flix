// Game module — types, effects, constants, level parsing, and game logic

mod Game {
    pub enum GameEvent with Eq, ToString {
        case BrickDestroyed(Int32)
        case PaddleHit
        case BallReset
        case BallLaunched
        case SolidBrickHit
    }

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

    type alias BrickFoldAcc = {
        bricks = List[SceneTree.Node],
        ballPos = Vec2D.Vec2,
        ballVel = Vec2D.Vec2,
        events = List[GameEvent],
        index = Int32
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
            let renderScene = BreakoutScene.applyShake(newState);
            Game.render(renderScene);
            runLoop(newState)
        }

    // ========================================================================
    // Game Logic — per-frame update
    // ========================================================================

    def updateGame(state: GameState): GameState \ Game =
        let dt = Game.getDeltaTime();
        let screenSize = {x = Int32.toFloat32(screenWidth()), y = Int32.toFloat32(screenHeight())};
        let s1 = processInput(state);
        let s2 = {scene = SceneTree.physicsStep(s1#scene, dt, screenSize) | s1};
        let (s3, allEvents) = GameAction.unpack(forM (
            sa <- doBrickCollisions(s2);
            sb <- doPaddleCollision(sa);
            sc <- checkDeathZone(sb)
        ) yield sc);
        tickShake(handleEvents(s3, allEvents), dt)

    pub def handleEvents(state: GameState, events: List[GameEvent]): GameState =
        List.foldLeft((s, event) -> match event {
            case GameEvent.SolidBrickHit => {shakeTime = 0.15f32 | s}
            case _ => s
        }, state, events)

    pub def tickShake(state: GameState, dt: Float32): GameState =
        if (state#shakeTime > 0.0f32)
            {shakeTime = Float32.max(0.0f32, state#shakeTime - dt) | state}
        else state

    // ========================================================================
    // Input Processing
    // ========================================================================

    pub def processInput(state: GameState): GameState \ Game =
        let velLeft = if (Game.isKeyPressed(keyA())) -1.0f32 * playerVelocity() else 0.0f32;
        let velRight = if (Game.isKeyPressed(keyD())) playerVelocity() else 0.0f32;
        let paddleVelX = velLeft + velRight;
        let s1 = {scene = SceneTree.updateKinematicBody("Paddle",
            _ -> {velocity = {x = paddleVelX, y = 0.0f32}},
            state#scene) | state};
        if (Game.isKeyPressed(keySpace())) {
            {scene = SceneTree.updateRigidBody("Ball",
                bd -> {stuck = false | bd},
                s1#scene) | s1}
        } else s1

    // ========================================================================
    // Helpers to extract ball state
    // ========================================================================

    pub def getBallPos(state: GameState): Vec2D.Vec2 =
        SceneTree.findNode("Ball", state#scene)
            |> Option.map(n -> (SceneTree.nodeTransform(n))#position)
            |> Option.getWithDefault(Vec2D.zero())

    pub def getBallVel(state: GameState): Vec2D.Vec2 =
        SceneTree.findNode("Ball", state#scene)
            |> Option.flatMap(SceneTree.rigidBodyData)
            |> Option.map(bd -> bd#velocity)
            |> Option.getWithDefault(Vec2D.zero())

    pub def getBallRadius(state: GameState): Float32 =
        SceneTree.findNode("Ball", state#scene)
            |> Option.flatMap(n -> SceneTree.findCircleShape(SceneTree.nodeChildren(n)))
            |> Option.getWithDefault(ballRadiusConst())

    pub def isBallStuck(state: GameState): Bool =
        SceneTree.findNode("Ball", state#scene)
            |> Option.flatMap(SceneTree.rigidBodyData)
            |> Option.map(bd -> bd#stuck)
            |> Option.getWithDefault(true)

    pub def getPaddlePos(state: GameState): Vec2D.Vec2 =
        SceneTree.findNode("Paddle", state#scene)
            |> Option.map(n -> (SceneTree.nodeTransform(n))#position)
            |> Option.getWithDefault(Vec2D.zero())

    def getPaddleSize(state: GameState): Vec2D.Vec2 =
        SceneTree.findNode("Paddle", state#scene)
            |> Option.flatMap(n -> SceneTree.findRectShape(SceneTree.nodeChildren(n)))
            |> Option.getWithDefault({x = playerWidth(), y = playerHeight()})

    // ========================================================================
    // Collision: Bricks
    // ========================================================================

    def doBrickCollisions(state: GameState): GameAction.GameAction[GameState] =
        if (isBallStuck(state)) GameAction.pure(state)
        else {
            let ballPos = getBallPos(state);
            let ballVel = getBallVel(state);
            let radius = getBallRadius(state);
            let brickNodes = SceneTree.findNode("Bricks", state#scene)
                |> Option.map(SceneTree.nodeChildren)
                |> Option.getWithDefault(Nil);
            let init: BrickFoldAcc = {bricks = Nil, ballPos = ballPos, ballVel = ballVel, events = Nil, index = 0};
            let result = List.foldLeft((acc, brick) -> brickCollisionStep(radius, acc, brick), init, brickNodes);
            let newBricksNode = SceneTree.Node.Node2D("Bricks", SceneTree.identityTransform(), List.reverse(result#bricks));
            let scene1 = SceneTree.mapNode("Bricks", _ -> newBricksNode, state#scene);
            let scene2 = SceneTree.updateRigidBody("Ball", bd -> {velocity = result#ballVel | bd},
                SceneTree.setNodePosition("Ball", result#ballPos, scene1));
            GameAction.of({scene = scene2 | state}, List.reverse(result#events))
        }

    def brickCollisionStep(radius: Float32, acc: BrickFoldAcc, brickNode: SceneTree.Node): BrickFoldAcc =
        let pass = {bricks = brickNode :: acc#bricks, ballPos = acc#ballPos, ballVel = acc#ballVel, events = acc#events, index = acc#index + 1};
        let brickPos = (SceneTree.nodeTransform(brickNode))#position;
        let brickSize = SceneTree.findRectShape(SceneTree.nodeChildren(brickNode))
            |> Option.getWithDefault(Vec2D.zero());
        let collision = forM (
            bodyData <- SceneTree.staticBodyData(brickNode)
                            |> Option.filter(bd -> not bd#destroyed);
            (dir, diff) <- SceneTree.checkCircleRect(acc#ballPos, radius, brickPos, brickSize)
        ) yield (bodyData, dir, diff);
        match collision {
            case None => pass
            case Some((bodyData, dir, diff)) =>
                let updatedBrick = if (bodyData#isSolid) brickNode
                    else SceneTree.modifyStaticBody(_ -> {destroyed = true | bodyData}, brickNode);
                let event = if (bodyData#isSolid) GameEvent.SolidBrickHit
                    else GameEvent.BrickDestroyed(acc#index);
                let (newPos, newVel) = SceneTree.resolveCollision(acc#ballPos, acc#ballVel, radius, dir, diff);
                {bricks = updatedBrick :: acc#bricks, ballPos = newPos, ballVel = newVel,
                 events = event :: acc#events, index = acc#index + 1}
        }

    // ========================================================================
    // Collision: Paddle
    // ========================================================================

    def doPaddleCollision(state: GameState): GameAction.GameAction[GameState] =
        if (isBallStuck(state)) GameAction.pure(state)
        else {
            let ballPos = getBallPos(state);
            let ballVel = getBallVel(state);
            let radius = getBallRadius(state);
            let paddlePos = getPaddlePos(state);
            let paddleSize = getPaddleSize(state);
            match SceneTree.checkCircleRect(ballPos, radius, paddlePos, paddleSize) {
                case None => GameAction.pure(state)
                case Some(_) =>
                    let centerBoard = paddlePos#x + paddleSize#x / 2.0f32;
                    let distance = (ballPos#x + radius) - centerBoard;
                    let percentage = distance / (paddleSize#x / 2.0f32);
                    let strength = 2.0f32;
                    let speed = Vec2D.length(ballVel);
                    let newVx = initialBallVelocityX() * percentage * strength;
                    let newVy = -1.0f32 * Float32.abs(ballVel#y);
                    let newVel = Vec2D.mul(Vec2D.normalized({x = newVx, y = newVy}), speed);
                    let scene1 = SceneTree.updateRigidBody("Ball",
                        bd -> {velocity = newVel | bd}, state#scene);
                    GameAction.of({scene = scene1 | state}, GameEvent.PaddleHit :: Nil)
            }
        }

    // ========================================================================
    // Death Zone Check
    // ========================================================================

    def checkDeathZone(state: GameState): GameAction.GameAction[GameState] =
        let ballPos = getBallPos(state);
        let deathY = SceneTree.findNode("DeathZone", state#scene)
            |> Option.map(n -> (SceneTree.nodeTransform(n))#position#y)
            |> Option.getWithDefault(Int32.toFloat32(screenHeight()));
        if (ballPos#y >= deathY) {
            let scene1 = SceneTree.updateRigidBody("Ball",
                _ -> {velocity = {x = initialBallVelocityX(), y = initialBallVelocityY()}, stuck = true},
                state#scene);
            GameAction.of({scene = scene1 | state}, GameEvent.BallReset :: Nil)
        } else GameAction.pure(state)
}
