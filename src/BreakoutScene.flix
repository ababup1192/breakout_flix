// BreakoutScene — scene construction, accessor APIs, and rendering effects

mod BreakoutScene {
    // ========================================================================
    // Node Name Constants (module-private)
    // ========================================================================

    def ballName(): String = "Ball"
    def paddleName(): String = "Paddle"
    def bricksName(): String = "Bricks"
    def deathZoneName(): String = "DeathZone"
    def rootName(): String = "Root"

    // ========================================================================
    // Scene Building
    // ========================================================================

    pub def buildBrickSubtree(level: Game.LevelData): SceneTree.Node =
        let brickNodes = level#bricks |> List.zipWithIndex |> List.map(match (idx, brick) ->
            let idxStr = Int32.toString(idx);
            SceneTree.staticBody2D(
                "Brick_${idxStr}",
                {position = brick#position, scale = Vec2D.one()},
                {isSolid = brick#isSolid, destroyed = false},
                SceneTree.sprite2D(
                    "BrickSprite_${idxStr}",
                    SceneTree.identityTransform(),
                    {size = brick#size, color = brick#color, texture = brick#texture, visible = true}
                ) ::
                SceneTree.collisionShape2D(
                    "BrickCollider_${idxStr}",
                    SceneTree.CollisionShapeData.RectShape(brick#size)
                ) :: Nil
            )
        );
        SceneTree.node2D(bricksName(), SceneTree.identityTransform(), brickNodes)

    pub def buildInitialScene(width: Int32, height: Int32, level: Game.LevelData): SceneTree.Node =
        let sw = Int32.toFloat32(width);
        let sh = Int32.toFloat32(height);
        let playerX = sw / 2.0f32 - Game.playerWidth() / 2.0f32;
        let playerY = sh - Game.playerHeight();
        let ballX = playerX + Game.playerWidth() / 2.0f32 - Game.ballRadiusConst();
        let ballY = playerY - Game.ballRadiusConst() * 2.0f32;
        let ballDiam = Game.ballRadiusConst() * 2.0f32;

        SceneTree.node2D(rootName(), SceneTree.identityTransform(),
            SceneTree.sprite2D("Background",
                SceneTree.identityTransform(),
                {size = {x = sw, y = sh}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "background", visible = true}
            ) ::
            buildBrickSubtree(level)
            ::
            SceneTree.kinematicBody2D(paddleName(),
                {position = {x = playerX, y = playerY}, scale = Vec2D.one()},
                {velocity = Vec2D.zero()},
                SceneTree.sprite2D("PaddleSprite",
                    SceneTree.identityTransform(),
                    {size = {x = Game.playerWidth(), y = Game.playerHeight()}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "paddle", visible = true}
                ) ::
                SceneTree.collisionShape2D("PaddleCollider",
                    SceneTree.CollisionShapeData.RectShape({x = Game.playerWidth(), y = Game.playerHeight()})
                ) :: Nil
            )
            ::
            SceneTree.rigidBody2D(ballName(),
                {position = {x = ballX, y = ballY}, scale = Vec2D.one()},
                {velocity = {x = Game.initialBallVelocityX(), y = Game.initialBallVelocityY()}, stuck = true},
                SceneTree.sprite2D("BallSprite",
                    SceneTree.identityTransform(),
                    {size = {x = ballDiam, y = ballDiam}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "ball", visible = true}
                ) ::
                SceneTree.collisionShape2D("BallCollider",
                    SceneTree.CollisionShapeData.CircleShape(Game.ballRadiusConst())
                ) :: Nil
            )
            ::
            SceneTree.area2D(deathZoneName(),
                {position = {x = 0.0f32, y = sh}, scale = Vec2D.one()},
                SceneTree.collisionShape2D("DeathZoneShape",
                    SceneTree.CollisionShapeData.RectShape({x = sw, y = 20.0f32})
                ) :: Nil
            )
            :: Nil
        )

    // ========================================================================
    // Snapshot Types
    // ========================================================================

    pub type alias BallSnapshot = {
        pos = Vec2D.Vec2,
        vel = Vec2D.Vec2,
        radius = Float32,
        stuck = Bool
    }

    pub type alias PaddleSnapshot = {
        pos = Vec2D.Vec2,
        size = Vec2D.Vec2
    }

    // ========================================================================
    // Snapshot APIs (one findNode per snapshot)
    // ========================================================================

    pub def ballSnapshot(scene: SceneTree.Node): BallSnapshot =
        match SceneTree.findNode(ballName(), scene) {
            case Some(n) =>
                let bd = SceneTree.rigidBodyData(n);
                {
                    pos = (SceneTree.nodeTransform(n))#position,
                    vel = bd |> Option.map(b -> b#velocity) |> Option.getWithDefault(Vec2D.zero()),
                    radius = SceneTree.findCircleShape(SceneTree.nodeChildren(n)) |> Option.getWithDefault(Game.ballRadiusConst()),
                    stuck = bd |> Option.map(b -> b#stuck) |> Option.getWithDefault(true)
                }
            case None => {pos = Vec2D.zero(), vel = Vec2D.zero(), radius = Game.ballRadiusConst(), stuck = true}
        }

    pub def paddleSnapshot(scene: SceneTree.Node): PaddleSnapshot =
        match SceneTree.findNode(paddleName(), scene) {
            case Some(n) => {
                pos = (SceneTree.nodeTransform(n))#position,
                size = SceneTree.findRectShape(SceneTree.nodeChildren(n))
                    |> Option.getWithDefault({x = Game.playerWidth(), y = Game.playerHeight()})
            }
            case None => {pos = Vec2D.zero(), size = {x = Game.playerWidth(), y = Game.playerHeight()}}
        }

    // ========================================================================
    // Read APIs (single-field convenience)
    // ========================================================================

    pub def ballPos(scene: SceneTree.Node): Vec2D.Vec2 =
        (ballSnapshot(scene))#pos

    pub def isBallStuck(scene: SceneTree.Node): Bool =
        (ballSnapshot(scene))#stuck

    pub def paddlePos(scene: SceneTree.Node): Vec2D.Vec2 =
        (paddleSnapshot(scene))#pos

    pub def paddleSize(scene: SceneTree.Node): Vec2D.Vec2 =
        (paddleSnapshot(scene))#size

    pub def brickNodes(scene: SceneTree.Node): List[SceneTree.Node] =
        SceneTree.findNode(bricksName(), scene)
            |> Option.map(SceneTree.nodeChildren)
            |> Option.getWithDefault(Nil)

    pub def deathZoneY(scene: SceneTree.Node): Float32 =
        SceneTree.findNode(deathZoneName(), scene)
            |> Option.map(n -> (SceneTree.nodeTransform(n))#position#y)
            |> Option.getWithDefault(Int32.toFloat32(Game.screenHeight()))

    // ========================================================================
    // Write APIs
    // ========================================================================

    pub def setPaddleVelocity(vel: Vec2D.Vec2, scene: SceneTree.Node): SceneTree.Node =
        SceneTree.updateKinematicBody(paddleName(),
            _ -> {velocity = vel},
            scene)

    pub def setBallVelocity(vel: Vec2D.Vec2, scene: SceneTree.Node): SceneTree.Node =
        SceneTree.updateRigidBody(ballName(),
            bd -> {velocity = vel | bd},
            scene)

    pub def setBallPosition(pos: Vec2D.Vec2, scene: SceneTree.Node): SceneTree.Node =
        SceneTree.setNodePosition(ballName(), pos, scene)

    pub def updateBall(f: SceneTree.RigidBodyData -> SceneTree.RigidBodyData, scene: SceneTree.Node): SceneTree.Node =
        SceneTree.updateRigidBody(ballName(), f, scene)

    pub def replaceBricks(bricks: List[SceneTree.Node], scene: SceneTree.Node): SceneTree.Node =
        let newBricksNode = SceneTree.node2D(bricksName(), SceneTree.identityTransform(), bricks);
        SceneTree.mapNode(bricksName(), _ -> newBricksNode, scene)

    // ========================================================================
    // Brick Inspection
    // ========================================================================

    pub type alias BrickRect = { pos = Vec2D.Vec2, size = Vec2D.Vec2 }

    pub def brickRect(brickNode: SceneTree.Node): BrickRect =
        {
            pos = (SceneTree.nodeTransform(brickNode))#position,
            size = SceneTree.findRectShape(SceneTree.nodeChildren(brickNode))
                |> Option.getWithDefault(Vec2D.zero())
        }

    pub def brickBodyData(brickNode: SceneTree.Node): Option[SceneTree.StaticBodyData] =
        SceneTree.staticBodyData(brickNode)

    pub def destroyBrick(bodyData: SceneTree.StaticBodyData, brickNode: SceneTree.Node): SceneTree.Node =
        SceneTree.modifyStaticBody(_ -> {destroyed = true | bodyData}, brickNode)

    // ========================================================================
    // Physics Step
    // ========================================================================

    pub def physicsStep(scene: SceneTree.Node, dt: Float32, screenSize: Vec2D.Vec2): SceneTree.PhysicsResult =
        let paddle = paddleSnapshot(scene);
        let config = {
            screenSize = screenSize,
            anchorPos = paddle#pos,
            anchorWidth = paddle#size#x
        };
        SceneTree.physicsStep(scene, dt, config)

    pub def destroyBrickByName(name: String, scene: SceneTree.Node): SceneTree.Node =
        SceneTree.updateStaticBody(name, bd -> {destroyed = true | bd}, scene)

    // ========================================================================
    // Rendering: apply shake offset
    // ========================================================================

    pub def applyShake(state: Game.GameState): SceneTree.Node =
        let shake = shakeOffset(state#shakeTime);
        SceneTree.setNodePosition(rootName(), shake, state#scene)

    // ========================================================================
    // Shake calculation
    // ========================================================================

    /// shakeTime > 0 のとき sin/cos でオフセットを生成
    pub def shakeOffset(shakeTime: Float32): Vec2D.Vec2 =
        if (shakeTime <= 0.0f32) Vec2D.zero()
        else {
            let ox = Vec2D.sin32(shakeTime * 100.0f32) * shakeTime * 80.0f32;
            let oy = Vec2D.cos32(shakeTime * 80.0f32) * shakeTime * 60.0f32;
            {x = ox, y = oy}
        }
}
