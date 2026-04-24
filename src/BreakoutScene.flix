// BreakoutScene — scene construction and rendering effects

mod BreakoutScene {
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
        SceneTree.node2D("Bricks", SceneTree.identityTransform(), brickNodes)

    pub def buildInitialScene(width: Int32, height: Int32, level: Game.LevelData): SceneTree.Node =
        let sw = Int32.toFloat32(width);
        let sh = Int32.toFloat32(height);
        let playerX = sw / 2.0f32 - Game.playerWidth() / 2.0f32;
        let playerY = sh - Game.playerHeight();
        let ballX = playerX + Game.playerWidth() / 2.0f32 - Game.ballRadiusConst();
        let ballY = playerY - Game.ballRadiusConst() * 2.0f32;
        let ballDiam = Game.ballRadiusConst() * 2.0f32;

        SceneTree.node2D("Root", SceneTree.identityTransform(),
            SceneTree.sprite2D("Background",
                SceneTree.identityTransform(),
                {size = {x = sw, y = sh}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "background", visible = true}
            ) ::
            buildBrickSubtree(level)
            ::
            SceneTree.kinematicBody2D("Paddle",
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
            SceneTree.rigidBody2D("Ball",
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
            SceneTree.area2D("DeathZone",
                {position = {x = 0.0f32, y = sh}, scale = Vec2D.one()},
                SceneTree.collisionShape2D("DeathZoneShape",
                    SceneTree.CollisionShapeData.RectShape({x = sw, y = 20.0f32})
                ) :: Nil
            )
            :: Nil
        )

    // ========================================================================
    // Rendering: apply shake offset
    // ========================================================================

    pub def applyShake(state: Game.GameState): SceneTree.Node =
        let shake = shakeOffset(state#shakeTime);
        SceneTree.setNodePosition("Root", shake, state#scene)

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
