// Scene builder: converts GameState to SceneTree for rendering

mod BreakoutScene {
    def white(): Game.Vec3 = {r = 1.0f32, g = 1.0f32, b = 1.0f32}

    /// shakeTime > 0 のとき sin/cos でオフセットを生成
    def shakeOffset(shakeTime: Float32): Vec2D.Vec2 =
        if (shakeTime <= 0.0f32) Vec2D.zero()
        else {
            let ox = Vec2D.sin32(shakeTime * 100.0f32) * shakeTime * 80.0f32;
            let oy = Vec2D.cos32(shakeTime * 80.0f32) * shakeTime * 60.0f32;
            {x = ox, y = oy}
        }

    /// GameState → rendering scene tree
    pub def toScene(state: Game.GameState): SceneTree.Node =
        let sw = Int32.toFloat32(Game.screenWidth());
        let sh = Int32.toFloat32(Game.screenHeight());
        let levelOpt = List.nth(state#currentLevel, state#levels);

        let shake = shakeOffset(state#shakeTime);
        SceneTree.Node.Group("Root", {position = shake, scale = {x = 1.0f32, y = 1.0f32}},
            SceneTree.Node.Sprite("Background",
                SceneTree.identityTransform(),
                {size = {x = sw, y = sh}, color = white(), texture = "background", visible = true},
                Nil)
            ::
            buildBrickSprites(levelOpt)
            ::
            SceneTree.Node.Sprite("Paddle",
                {position = state#player#position, scale = {x = 1.0f32, y = 1.0f32}},
                {size = state#player#size, color = state#player#color,
                 texture = state#player#texture, visible = true},
                Nil)
            ::
            SceneTree.Node.Sprite("Ball",
                {position = state#ballPos, scale = {x = 1.0f32, y = 1.0f32}},
                {size = {x = state#ballRadius * 2.0f32, y = state#ballRadius * 2.0f32},
                 color = white(), texture = "ball", visible = true},
                Nil)
            :: Nil
        )

    /// Convert level bricks to a Group node containing Sprite nodes
    def buildBrickSprites(levelOpt: Option[Game.GameLevel]): SceneTree.Node =
        match levelOpt {
            case None => SceneTree.Node.Group("Bricks", SceneTree.identityTransform(), Nil)
            case Some(level) =>
                let sprites = List.filterMap(brick ->
                    if (brick#destroyed) None
                    else Some(
                        SceneTree.Node.Sprite("Brick",
                            {position = brick#position, scale = {x = 1.0f32, y = 1.0f32}},
                            {size = brick#size, color = brick#color,
                             texture = brick#texture, visible = true},
                            Nil)
                    ),
                    level#bricks
                );
                SceneTree.Node.Group("Bricks", SceneTree.identityTransform(), sprites)
        }
}
