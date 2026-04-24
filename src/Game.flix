// Game effect and core type definitions
// Decouples game logic from rendering backend

mod Game {
    pub enum TextureKind with Eq, ToString {
        case Background
        case Block
        case BlockSolid
        case Paddle
        case Ball
    }

    pub type alias Vec2 = {x = Float32, y = Float32}
    pub type alias Vec3 = {r = Float32, g = Float32, b = Float32}

    pub type alias GameObject = {
        position = Vec2,
        size = Vec2,
        color = Vec3,
        isSolid = Bool,
        destroyed = Bool,
        textureKind = TextureKind
    }

    pub type alias GameLevel = {
        bricks = List[GameObject]
    }

    pub type alias GameState = {
        levels = List[GameLevel],
        currentLevel = Int32,
        player = GameObject,
        ballPos = Vec2,
        ballRadius = Float32
    }

    pub eff Game {
        def getInitialState(): GameState
        def render(state: GameState): Unit
        def shouldClose(): Bool
    }

    pub def startGame(): GameState \ Game =
        let state = Game.getInitialState();
        runLoop(state)

    @Tailrec
    def runLoop(state: GameState): GameState \ Game =
        if (Game.shouldClose()) state
        else {
            Game.render(state);
            runLoop(state)
        }
}
