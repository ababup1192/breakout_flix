// Scene builder: converts GameState to SceneTree for rendering

mod BreakoutScene {
    def white(): Game.Vec3 = {r = 1.0f32, g = 1.0f32, b = 1.0f32}

    def pi32(): Float32 = 3.14159265f32
    def twoPi32(): Float32 = 6.28318530f32

    /// 角度を [-π, π] に正規化
    def reduceAngle(x: Float32): Float32 =
        let n = Float32.truncateToInt32(x / twoPi32());
        let reduced = x - Int32.toFloat32(n) * twoPi32();
        if (reduced > pi32()) reduced - twoPi32()
        else if (reduced < -1.0f32 * pi32()) reduced + twoPi32()
        else reduced

    /// Taylor展開による sin 近似 (x - x³/6 + x⁵/120 - x⁷/5040)
    def sin32(x0: Float32): Float32 =
        let x = reduceAngle(x0);
        let x2 = x * x;
        let x3 = x2 * x;
        let x5 = x3 * x2;
        let x7 = x5 * x2;
        x - x3 / 6.0f32 + x5 / 120.0f32 - x7 / 5040.0f32

    /// cos(x) = sin(x + π/2)
    def cos32(x: Float32): Float32 =
        sin32(x + pi32() / 2.0f32)

    /// shakeTime > 0 のとき sin/cos でオフセットを生成
    def shakeOffset(shakeTime: Float32): Game.Vec2 =
        if (shakeTime <= 0.0f32) {x = 0.0f32, y = 0.0f32}
        else {
            let ox = sin32(shakeTime * 100.0f32) * shakeTime * 80.0f32;
            let oy = cos32(shakeTime * 80.0f32) * shakeTime * 60.0f32;
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
