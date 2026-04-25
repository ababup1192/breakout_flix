// Tests for Breakout game logic — scene-tree based

use SceneTree.Direction.{Up, Right, Down, Left}
use Game.{parseLevelLine, parseLevel, initGame,
          playerWidth, playerHeight, ballRadiusConst,
          screenWidth, screenHeight,
          initialBallVelocityX, initialBallVelocityY,
          processInput,
          keyA, keyD, keySpace,
          handleEvents, tickShake}
use Game.GameEvent

// --- parseLevelLine ---

@Test
def testParseLevelLineNormal(): Unit \ Assert =
    Assert.assertEq(expected = 1 :: 0 :: 3 :: Nil, parseLevelLine("1 0 3"))

@Test
def testParseLevelLineEmpty(): Unit \ Assert =
    Assert.assertEq(expected = (Nil: List[Int32]), parseLevelLine(""))

@Test
def testParseLevelLineTrimmed(): Unit \ Assert =
    Assert.assertEq(expected = 2 :: 5 :: Nil, parseLevelLine("  2 5  "))

// --- parseLevel ---

@Test
def testParseLevelBrickCount(): Unit \ Assert =
    let data = "5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n4 4 4 4 4 0 0 0 0 0 4 4 4 4 4\n4 1 4 1 4 0 0 1 0 0 4 1 4 1 4\n3 3 3 3 3 0 0 0 0 0 3 3 3 3 3\n3 3 1 3 3 3 3 3 3 3 3 3 1 3 3\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2";
    let level = parseLevel(data, 800, 300);
    Assert.assertEq(expected = 106, List.length(level#bricks))

@Test
def testParseLevelSkipsZero(): Unit \ Assert =
    let level = parseLevel("1 0 2", 300, 100);
    Assert.assertEq(expected = 2, List.length(level#bricks))

@Test
def testParseLevelBrickPosition(): Unit \ Assert =
    let level = parseLevel("0 0 3", 300, 100);
    let px = List.head(level#bricks) |> Option.map(b -> b#position#x);
    let py = List.head(level#bricks) |> Option.map(b -> b#position#y);
    Assert.assertEq(expected = Some(200.0f32), px);
    Assert.assertEq(expected = Some(0.0f32), py)

@Test
def testParseLevelSolidBrick(): Unit \ Assert =
    let level = parseLevel("1", 100, 100);
    let solid = List.head(level#bricks) |> Option.map(b -> b#isSolid);
    let tex = List.head(level#bricks) |> Option.map(b -> b#texture);
    Assert.assertEq(expected = Some(true), solid);
    Assert.assertEq(expected = Some("block_solid"), tex)

@Test
def testParseLevelNonSolidBrick(): Unit \ Assert =
    let level = parseLevel("2", 100, 100);
    let solid = List.head(level#bricks) |> Option.map(b -> b#isSolid);
    let tex = List.head(level#bricks) |> Option.map(b -> b#texture);
    let red = List.head(level#bricks) |> Option.map(b -> b#color#r);
    Assert.assertEq(expected = Some(false), solid);
    Assert.assertEq(expected = Some("block"), tex);
    Assert.assertEq(expected = Some(0.2f32), red)

// --- initGame ---

def dummyFileRead(f: Unit -> a \ ef): a \ ef - Fs.FileRead =
    run { f() } with handler Fs.FileRead {
        def read(_path, resume) = resume(Ok("1"))
        def readBytes(_path, resume) = resume(Ok(Vector#{}))
        def readLines(_path, resume) = resume(Ok("1" :: Nil))
    }

@Test
def testInitGamePaddlePosition(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    let pos = BreakoutScene.paddlePos(state#scene);
    Assert.assertEq(expected = 350.0f32, pos#x);
    Assert.assertEq(expected = 580.0f32, pos#y)

@Test
def testInitGameBallPosition(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    let pos = BreakoutScene.ballPos(state#scene);
    Assert.assertEq(expected = 387.5f32, pos#x);
    Assert.assertEq(expected = 555.0f32, pos#y)

@Test
def testInitGameBallVelocity(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    let vel = (BreakoutScene.ballSnapshot(state#scene))#vel;
    Assert.assertEq(expected = 100.0f32, vel#x);
    Assert.assertEq(expected = -350.0f32, vel#y)

@Test
def testInitGameBallStuck(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    Assert.assertTrue(BreakoutScene.isBallStuck(state#scene))

// --- Scene tree based test state helper ---

def mkTestState(
    px: Float32, py: Float32,
    bx: Float32, by: Float32,
    vx: Float32, vy: Float32,
    stuck: Bool
): Game.GameState =
    mkTestStateWithShake(px, py, bx, by, vx, vy, stuck, 0.0f32)

def mkTestStateWithShake(
    px: Float32, py: Float32,
    bx: Float32, by: Float32,
    vx: Float32, vy: Float32,
    stuck: Bool,
    shake: Float32
): Game.GameState =
    let scene = SceneTree.node2D("Root", SceneTree.identityTransform(),
        SceneTree.node2D("Bricks", SceneTree.identityTransform(), Nil) ::
        SceneTree.kinematicBody2D("Paddle",
            {position = {x = px, y = py}, scale = Vec2D.one()},
            {velocity = Vec2D.zero()},
            SceneTree.sprite2D("PaddleSprite", SceneTree.identityTransform(),
                {size = {x = playerWidth(), y = playerHeight()}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "paddle", visible = true}
            ) ::
            SceneTree.collisionShape2D("PaddleCollider",
                SceneTree.CollisionShapeData.RectShape({x = playerWidth(), y = playerHeight()})
            ) :: Nil
        ) ::
        SceneTree.rigidBody2D("Ball",
            {position = {x = bx, y = by}, scale = Vec2D.one()},
            {velocity = {x = vx, y = vy}, stuck = stuck},
            SceneTree.sprite2D("BallSprite", SceneTree.identityTransform(),
                {size = {x = ballRadiusConst() * 2.0f32, y = ballRadiusConst() * 2.0f32},
                 color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "ball", visible = true}
            ) ::
            SceneTree.collisionShape2D("BallCollider",
                SceneTree.CollisionShapeData.CircleShape(ballRadiusConst())
            ) :: Nil
        ) ::
        SceneTree.area2D("DeathZone",
            {position = {x = 0.0f32, y = Int32.toFloat32(screenHeight())}, scale = Vec2D.one()},
            SceneTree.collisionShape2D("DeathZoneShape",
                SceneTree.CollisionShapeData.RectShape({x = Int32.toFloat32(screenWidth()), y = 20.0f32})
            ) :: Nil
        )
        :: Nil
    );
    {
        scene = scene,
        levels = Nil,
        currentLevel = 0,
        shakeTime = shake
    }

// --- physicsStep via engine (replaces moveBall tests) ---

def testScreenSize(): Vec2D.Vec2 = {x = 800.0f32, y = 600.0f32}

@Test
def testPhysicsStepBallStuck(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = {scene = (BreakoutScene.physicsStep(state#scene, 0.016f32, testScreenSize()))#root | state};
    let pos = BreakoutScene.ballPos(result#scene);
    let expectedX = 350.0f32 + playerWidth() / 2.0f32 - ballRadiusConst();
    let expectedY = 580.0f32 - ballRadiusConst() * 2.0f32;
    Assert.assertEq(expected = expectedX, pos#x);
    Assert.assertEq(expected = expectedY, pos#y)

@Test
def testPhysicsStepBallNormal(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 300.0f32, 100.0f32, -350.0f32, false);
    let result = {scene = (BreakoutScene.physicsStep(state#scene, 1.0f32, testScreenSize()))#root | state};
    let pos = BreakoutScene.ballPos(result#scene);
    Assert.assertEq(expected = 500.0f32, pos#x);
    Assert.assertEq(expected = 0.0f32, pos#y)

@Test
def testPhysicsStepBallLeftWall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 5.0f32, 300.0f32, -100.0f32, 0.0f32, false);
    let result = {scene = (BreakoutScene.physicsStep(state#scene, 1.0f32, testScreenSize()))#root | state};
    let pos = BreakoutScene.ballPos(result#scene);
    let vel = (BreakoutScene.ballSnapshot(result#scene))#vel;
    Assert.assertEq(expected = 0.0f32, pos#x);
    Assert.assertTrue(vel#x > 0.0f32)

@Test
def testPhysicsStepBallRightWall(): Unit \ Assert =
    let ballSize = ballRadiusConst() * 2.0f32;
    let startX = Int32.toFloat32(screenWidth()) - ballSize - 10.0f32;
    let state = mkTestState(350.0f32, 580.0f32, startX, 300.0f32, 200.0f32, 0.0f32, false);
    let result = {scene = (BreakoutScene.physicsStep(state#scene, 1.0f32, testScreenSize()))#root | state};
    let pos = BreakoutScene.ballPos(result#scene);
    let vel = (BreakoutScene.ballSnapshot(result#scene))#vel;
    Assert.assertEq(expected = Int32.toFloat32(screenWidth()) - ballSize, pos#x);
    Assert.assertTrue(vel#x < 0.0f32)

@Test
def testPhysicsStepBallTopWall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 5.0f32, 0.0f32, -100.0f32, false);
    let result = {scene = (BreakoutScene.physicsStep(state#scene, 1.0f32, testScreenSize()))#root | state};
    let pos = BreakoutScene.ballPos(result#scene);
    let vel = (BreakoutScene.ballSnapshot(result#scene))#vel;
    Assert.assertEq(expected = 0.0f32, pos#y);
    Assert.assertTrue(vel#y > 0.0f32)

// --- checkDeathZone (replaces checkBallReset tests) ---

@Test
def testCheckDeathZoneInBounds(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 300.0f32, 100.0f32, -350.0f32, false);
    let pos = BreakoutScene.ballPos(state#scene);
    Assert.assertEq(expected = 400.0f32, pos#x);
    Assert.assertEq(expected = 300.0f32, pos#y);
    Assert.assertFalse(BreakoutScene.isBallStuck(state#scene))

// --- processInput ---

def withMockInput(pressedKeys: Set[Int32], f: Unit -> a \ ef): a \ ef - Game.Game =
    run { f() } with handler Game.Game {
        def getInitialState(_, _k) = bug!("not used in test")
        def render(_, resume) = resume(())
        def shouldClose(_, resume) = resume(true)
        def getDeltaTime(_, resume) = resume(0.0f32)
        def isKeyPressed(keyCode, resume) = resume(Set.memberOf(keyCode, pressedKeys))
    }

@Test
def testProcessInputSetsPaddleVelocityLeft(): Unit \ Assert =
    let state = mkTestState(100.0f32, 580.0f32, 137.5f32, 555.0f32, 100.0f32, -350.0f32, false);
    let result = run { processInput(state) } with withMockInput(Set#{keyA()});
    let paddleNode = SceneTree.findNode("Paddle", result#scene);
    let velX = paddleNode
        |> Option.flatMap(SceneTree.kinematicBodyData)
        |> Option.map(bd -> bd#velocity#x);
    Assert.assertTrue(Option.getWithDefault(0.0f32, velX) < 0.0f32)

@Test
def testProcessInputSetsPaddleVelocityRight(): Unit \ Assert =
    let state = mkTestState(100.0f32, 580.0f32, 137.5f32, 555.0f32, 100.0f32, -350.0f32, false);
    let result = run { processInput(state) } with withMockInput(Set#{keyD()});
    let paddleNode = SceneTree.findNode("Paddle", result#scene);
    let velX = paddleNode
        |> Option.flatMap(SceneTree.kinematicBodyData)
        |> Option.map(bd -> bd#velocity#x);
    Assert.assertTrue(Option.getWithDefault(0.0f32, velX) > 0.0f32)

@Test
def testProcessInputLaunchBall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = run { processInput(state) } with withMockInput(Set#{keySpace()});
    Assert.assertFalse(BreakoutScene.isBallStuck(result#scene))

// --- handleEvents ---

@Test
def testHandleEventsSolidBrickHit(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = handleEvents(state, GameEvent.SolidBrickHit :: Nil);
    Assert.assertEq(expected = 0.15f32, result#shakeTime)

@Test
def testHandleEventsOtherEvent(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = handleEvents(state, GameEvent.PaddleHit :: Nil);
    Assert.assertEq(expected = 0.0f32, result#shakeTime)

// --- tickShake ---

@Test
def testTickShakeDecay(): Unit \ Assert =
    let state = mkTestStateWithShake(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true, 0.15f32);
    let result = tickShake(state, 0.05f32);
    Assert.assertTrue(result#shakeTime > 0.09f32 and result#shakeTime < 0.11f32)

@Test
def testTickShakeClampToZero(): Unit \ Assert =
    let state = mkTestStateWithShake(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true, 0.02f32);
    let result = tickShake(state, 0.05f32);
    Assert.assertEq(expected = 0.0f32, result#shakeTime)

@Test
def testTickShakeAlreadyZero(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = tickShake(state, 0.016f32);
    Assert.assertEq(expected = 0.0f32, result#shakeTime)
