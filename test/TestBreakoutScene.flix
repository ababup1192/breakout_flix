// Tests for BreakoutScene module APIs

use Game.{screenWidth, screenHeight, playerWidth, playerHeight, ballRadiusConst,
          initialBallVelocityX, initialBallVelocityY}

// --- Helper ---

def mkSimpleLevel(): Game.LevelData =
    {bricks =
        {position = {x = 0.0f32, y = 0.0f32}, size = {x = 80.0f32, y = 20.0f32},
         color = {r = 0.2f32, g = 0.6f32, b = 1.0f32}, isSolid = false, texture = "block"} ::
        {position = {x = 100.0f32, y = 0.0f32}, size = {x = 80.0f32, y = 20.0f32},
         color = {r = 0.8f32, g = 0.8f32, b = 0.7f32}, isSolid = true, texture = "block_solid"} ::
        Nil
    }

def mkEmptyLevel(): Game.LevelData =
    {bricks = Nil}

def mkSceneWith800x600(): SceneTree.Node =
    BreakoutScene.buildInitialScene(800, 600, mkSimpleLevel())

// ========================================================================
// buildInitialScene
// ========================================================================

@Test
def testBuildInitialSceneRootName(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    Assert.assertEq(expected = "Root", SceneTree.nodeName(scene))

@Test
def testBuildInitialSceneChildCount(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    Assert.assertEq(expected = 5, List.length(SceneTree.nodeChildren(scene)))

@Test
def testBuildInitialSceneHasBackground(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let found = SceneTree.findNode("Background", scene) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Background"), found)

@Test
def testBuildInitialSceneHasBricks(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let found = SceneTree.findNode("Bricks", scene) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Bricks"), found)

@Test
def testBuildInitialSceneHasPaddle(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let found = SceneTree.findNode("Paddle", scene) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Paddle"), found)

@Test
def testBuildInitialSceneHasBall(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let found = SceneTree.findNode("Ball", scene) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Ball"), found)

@Test
def testBuildInitialSceneHasDeathZone(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let found = SceneTree.findNode("DeathZone", scene) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("DeathZone"), found)

// ========================================================================
// buildBrickSubtree
// ========================================================================

@Test
def testBuildBrickSubtreeName(): Unit \ Assert =
    let node = BreakoutScene.buildBrickSubtree(mkSimpleLevel());
    Assert.assertEq(expected = "Bricks", SceneTree.nodeName(node))

@Test
def testBuildBrickSubtreeChildCount(): Unit \ Assert =
    let node = BreakoutScene.buildBrickSubtree(mkSimpleLevel());
    Assert.assertEq(expected = 2, List.length(SceneTree.nodeChildren(node)))

@Test
def testBuildBrickSubtreeBrickNames(): Unit \ Assert =
    let node = BreakoutScene.buildBrickSubtree(mkSimpleLevel());
    let names = List.map(SceneTree.nodeName, SceneTree.nodeChildren(node));
    Assert.assertEq(expected = "Brick_0" :: "Brick_1" :: Nil, names)

@Test
def testBuildBrickSubtreeEmpty(): Unit \ Assert =
    let node = BreakoutScene.buildBrickSubtree(mkEmptyLevel());
    Assert.assertEq(expected = 0, List.length(SceneTree.nodeChildren(node)))

// ========================================================================
// Snapshot APIs
// ========================================================================

@Test
def testBallSnapshotPos(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.ballSnapshot(scene);
    let expectedX = 400.0f32 - ballRadiusConst();
    let expectedY = 580.0f32 - ballRadiusConst() * 2.0f32;
    Assert.assertEq(expected = expectedX, snap#pos#x);
    Assert.assertEq(expected = expectedY, snap#pos#y)

@Test
def testBallSnapshotVel(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.ballSnapshot(scene);
    Assert.assertEq(expected = initialBallVelocityX(), snap#vel#x);
    Assert.assertEq(expected = initialBallVelocityY(), snap#vel#y)

@Test
def testBallSnapshotRadius(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.ballSnapshot(scene);
    Assert.assertEq(expected = ballRadiusConst(), snap#radius)

@Test
def testBallSnapshotStuck(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.ballSnapshot(scene);
    Assert.assertTrue(snap#stuck)

@Test
def testPaddleSnapshotPos(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.paddleSnapshot(scene);
    Assert.assertEq(expected = 350.0f32, snap#pos#x);
    Assert.assertEq(expected = 580.0f32, snap#pos#y)

@Test
def testPaddleSnapshotSize(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let snap = BreakoutScene.paddleSnapshot(scene);
    Assert.assertEq(expected = playerWidth(), snap#size#x);
    Assert.assertEq(expected = playerHeight(), snap#size#y)

// ========================================================================
// Write APIs
// ========================================================================

@Test
def testSetPaddleVelocity(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = BreakoutScene.setPaddleVelocity({x = 500.0f32, y = 0.0f32}, scene);
    let paddleNode = SceneTree.findNode("Paddle", scene1);
    let vel = paddleNode |> Option.flatMap(SceneTree.kinematicBodyData) |> Option.map(bd -> bd#velocity#x);
    Assert.assertEq(expected = Some(500.0f32), vel)

@Test
def testUpdateBallSetVelocity(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = BreakoutScene.updateBall(bd -> {velocity = {x = 200.0f32, y = -400.0f32} | bd}, scene);
    let snap = BreakoutScene.ballSnapshot(scene1);
    Assert.assertEq(expected = 200.0f32, snap#vel#x);
    Assert.assertEq(expected = -400.0f32, snap#vel#y)

@Test
def testSetBallPosition(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = SceneTree.setNodePosition(BreakoutScene.ballName(), {x = 100.0f32, y = 200.0f32}, scene);
    let pos = (BreakoutScene.ballSnapshot(scene1))#pos;
    Assert.assertEq(expected = 100.0f32, pos#x);
    Assert.assertEq(expected = 200.0f32, pos#y)

@Test
def testUpdateBallUnstuck(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = BreakoutScene.updateBall(bd -> {stuck = false | bd}, scene);
    Assert.assertFalse((BreakoutScene.ballSnapshot(scene1))#stuck)

@Test
def testUpdateBallVelocity(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = BreakoutScene.updateBall(bd -> {velocity = {x = 999.0f32, y = -999.0f32} | bd}, scene);
    let snap = BreakoutScene.ballSnapshot(scene1);
    Assert.assertEq(expected = 999.0f32, snap#vel#x)

// ========================================================================
// destroyBrick
// ========================================================================

@Test
def testDestroyBrickSetsDestroyed(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let body: SceneTree.BodyContact = {
        name = "Brick_0",
        position = {x = 0.0f32, y = 0.0f32},
        kind = SceneTree.NodeKind.Fixed({isSolid = false, destroyed = false}),
        isSolid = false,
        group = "Bricks"
    };
    let scene1 = BreakoutScene.destroyBrick(body, scene);
    let brickNode = SceneTree.findNode("Brick_0", scene1);
    let destroyed = brickNode |> Option.flatMap(SceneTree.staticBodyData) |> Option.map(bd -> bd#destroyed);
    Assert.assertEq(expected = Some(true), destroyed)

@Test
def testDestroyBrickOtherUnaffected(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let body: SceneTree.BodyContact = {
        name = "Brick_0",
        position = {x = 0.0f32, y = 0.0f32},
        kind = SceneTree.NodeKind.Fixed({isSolid = false, destroyed = false}),
        isSolid = false,
        group = "Bricks"
    };
    let scene1 = BreakoutScene.destroyBrick(body, scene);
    let otherBrick = SceneTree.findNode("Brick_1", scene1);
    let destroyed = otherBrick |> Option.flatMap(SceneTree.staticBodyData) |> Option.map(bd -> bd#destroyed);
    Assert.assertEq(expected = Some(false), destroyed)

// ========================================================================
// Brick inspection (via SceneTree directly)
// ========================================================================

@Test
def testBrickRectSize(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let firstBrick = SceneTree.findNode("Brick_0", scene);
    let w = firstBrick |> Option.flatMap(n -> SceneTree.findRectShape(SceneTree.nodeChildren(n))) |> Option.map(s -> s#x);
    Assert.assertEq(expected = Some(80.0f32), w)

@Test
def testBrickBodyData(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let firstBrick = SceneTree.findNode("Brick_0", scene);
    let bd = firstBrick |> Option.flatMap(SceneTree.staticBodyData);
    let isSolid = bd |> Option.map(b -> b#isSolid);
    Assert.assertEq(expected = Some(false), isSolid)

// ========================================================================
// shakeOffset
// ========================================================================

@Test
def testShakeOffsetZeroTime(): Unit \ Assert =
    let offset = BreakoutScene.shakeOffset(0.0f32);
    Assert.assertEq(expected = 0.0f32, offset#x);
    Assert.assertEq(expected = 0.0f32, offset#y)

@Test
def testShakeOffsetNegativeTime(): Unit \ Assert =
    let offset = BreakoutScene.shakeOffset(-1.0f32);
    Assert.assertEq(expected = 0.0f32, offset#x);
    Assert.assertEq(expected = 0.0f32, offset#y)

@Test
def testShakeOffsetPositiveTime(): Unit \ Assert =
    let offset = BreakoutScene.shakeOffset(0.1f32);
    let nonZero = not (offset#x == 0.0f32 and offset#y == 0.0f32);
    Assert.assertTrue(nonZero)

// ========================================================================
// applyShake
// ========================================================================

@Test
def testApplyShakeZero(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let rendered = BreakoutScene.applyShake(0.0f32, scene);
    let pos = SceneTree.nodePosition(rendered);
    Assert.assertEq(expected = 0.0f32, pos#x);
    Assert.assertEq(expected = 0.0f32, pos#y)

@Test
def testApplyShakeNonZero(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let rendered = BreakoutScene.applyShake(0.1f32, scene);
    let pos = SceneTree.nodePosition(rendered);
    let nonZero = not (pos#x == 0.0f32 and pos#y == 0.0f32);
    Assert.assertTrue(nonZero)

// ========================================================================
// stepAndCollide event firing
// ========================================================================

@Test
def testStepAndCollideBrickBodyEntered(): Unit \ Assert =
    // Ball(127.5, 83) overlaps Brick(100, 100) 80x20, unstuck
    let brick = SceneTree.staticBody2D("Brick_0",
        {x = 100.0f32, y = 100.0f32},
        {isSolid = false, destroyed = false},
        SceneTree.sprite2D("BrickSprite", Vec2D.zero(),
            {size = {x = 80.0f32, y = 20.0f32}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "block", visible = true}
        ) ::
        SceneTree.collisionShape2D("BrickCollider",
            SceneTree.CollisionShapeData.RectShape({x = 80.0f32, y = 20.0f32})
        ) :: Nil
    );
    let scene = SceneTree.node2D("Root", Vec2D.zero(),
        SceneTree.node2D("Bricks", Vec2D.zero(), brick :: Nil) ::
        SceneTree.kinematicBody2D("Paddle", {x = 350.0f32, y = 580.0f32},
            {velocity = Vec2D.zero()},
            SceneTree.collisionShape2D("PaddleCollider",
                SceneTree.CollisionShapeData.RectShape({x = playerWidth(), y = playerHeight()})
            ) :: Nil
        ) ::
        SceneTree.rigidBody2D("Ball", {x = 127.5f32, y = 83.0f32},
            {velocity = {x = 0.0f32, y = 350.0f32}, stuck = false},
            SceneTree.collisionShape2D("BallCollider",
                SceneTree.CollisionShapeData.CircleShape(ballRadiusConst())
            ) :: Nil
        ) ::
        SceneTree.area2D("DeathZone", {x = 0.0f32, y = 600.0f32},
            SceneTree.collisionShape2D("DeathZoneShape",
                SceneTree.CollisionShapeData.RectShape({x = 800.0f32, y = 20.0f32})
            ) :: Nil
        ) :: Nil
    );
    let screenSize = {x = 800.0f32, y = 600.0f32};
    region rc {
        let bodyCount = Ref.fresh(rc, 0);
        run {
            discard BreakoutScene.stepAndCollide(scene, 0.001f32, screenSize)
        } with handler SceneTree.OnCollision {
            def bodyEntered(_nodeName, _selfPos, _body, resume) = {
                Ref.put(Ref.get(bodyCount) + 1, bodyCount);
                resume(())
            }
            def areaEntered(_nodeName, _selfPos, _body, resume) = resume(())
        };
        Assert.assertTrue(Ref.get(bodyCount) > 0)
    }

@Test
def testStepAndCollideDeathZoneAreaEntered(): Unit \ Assert =
    // Ball(50, 585) overlaps DeathZone(0,600), not paddle
    let scene = SceneTree.node2D("Root", Vec2D.zero(),
        SceneTree.node2D("Bricks", Vec2D.zero(), Nil) ::
        SceneTree.kinematicBody2D("Paddle", {x = 350.0f32, y = 580.0f32},
            {velocity = Vec2D.zero()},
            SceneTree.collisionShape2D("PaddleCollider",
                SceneTree.CollisionShapeData.RectShape({x = playerWidth(), y = playerHeight()})
            ) :: Nil
        ) ::
        SceneTree.rigidBody2D("Ball", {x = 50.0f32, y = 585.0f32},
            {velocity = {x = 0.0f32, y = 350.0f32}, stuck = false},
            SceneTree.collisionShape2D("BallCollider",
                SceneTree.CollisionShapeData.CircleShape(ballRadiusConst())
            ) :: Nil
        ) ::
        SceneTree.area2D("DeathZone", {x = 0.0f32, y = 600.0f32},
            SceneTree.collisionShape2D("DeathZoneShape",
                SceneTree.CollisionShapeData.RectShape({x = 800.0f32, y = 20.0f32})
            ) :: Nil
        ) :: Nil
    );
    let screenSize = {x = 800.0f32, y = 600.0f32};
    region rc {
        let areaCount = Ref.fresh(rc, 0);
        run {
            discard BreakoutScene.stepAndCollide(scene, 0.001f32, screenSize)
        } with handler SceneTree.OnCollision {
            def bodyEntered(_nodeName, _selfPos, _body, resume) = resume(())
            def areaEntered(_nodeName, _selfPos, _body, resume) = {
                Ref.put(Ref.get(areaCount) + 1, areaCount);
                resume(())
            }
        };
        Assert.assertTrue(Ref.get(areaCount) > 0)
    }

@Test
def testStepAndCollideStuckNoEvents(): Unit \ Assert =
    // Stuck ball — no events should fire
    let brick = SceneTree.staticBody2D("Brick_0",
        {x = 100.0f32, y = 100.0f32},
        {isSolid = false, destroyed = false},
        SceneTree.collisionShape2D("BrickCollider",
            SceneTree.CollisionShapeData.RectShape({x = 80.0f32, y = 20.0f32})
        ) :: Nil
    );
    let scene = SceneTree.node2D("Root", Vec2D.zero(),
        SceneTree.node2D("Bricks", Vec2D.zero(), brick :: Nil) ::
        SceneTree.kinematicBody2D("Paddle", {x = 350.0f32, y = 580.0f32},
            {velocity = Vec2D.zero()},
            SceneTree.collisionShape2D("PaddleCollider",
                SceneTree.CollisionShapeData.RectShape({x = playerWidth(), y = playerHeight()})
            ) :: Nil
        ) ::
        SceneTree.rigidBody2D("Ball", {x = 127.5f32, y = 83.0f32},
            {velocity = {x = 0.0f32, y = 350.0f32}, stuck = true},
            SceneTree.collisionShape2D("BallCollider",
                SceneTree.CollisionShapeData.CircleShape(ballRadiusConst())
            ) :: Nil
        ) ::
        SceneTree.area2D("DeathZone", {x = 0.0f32, y = 600.0f32},
            SceneTree.collisionShape2D("DeathZoneShape",
                SceneTree.CollisionShapeData.RectShape({x = 800.0f32, y = 20.0f32})
            ) :: Nil
        ) :: Nil
    );
    let screenSize = {x = 800.0f32, y = 600.0f32};
    region rc {
        let eventCount = Ref.fresh(rc, 0);
        run {
            discard BreakoutScene.stepAndCollide(scene, 0.001f32, screenSize)
        } with handler SceneTree.OnCollision {
            def bodyEntered(_nodeName, _selfPos, _body, resume) = {
                Ref.put(Ref.get(eventCount) + 1, eventCount);
                resume(())
            }
            def areaEntered(_nodeName, _selfPos, _body, resume) = {
                Ref.put(Ref.get(eventCount) + 1, eventCount);
                resume(())
            }
        };
        Assert.assertEq(expected = 0, Ref.get(eventCount))
    }

// ========================================================================
// stepAndCollide (via SceneTree directly)
// ========================================================================

@Test
def testStepAndCollideBallMoves(): Unit \ Assert =
    let scene = mkSceneWith800x600();
    let scene1 = BreakoutScene.updateBall(bd -> {stuck = false | bd}, scene);
    let posBefore = (BreakoutScene.ballSnapshot(scene1))#pos;
    let screenSize = {x = 800.0f32, y = 600.0f32};
    let config: SceneTree.PhysicsConfig = {screenSize = screenSize, anchorPos = {x = 350.0f32, y = 580.0f32}, anchorWidth = playerWidth()};
    let scene2 = run {
        SceneTree.stepAndCollide(scene1, 0.1f32, config)
    } with handler SceneTree.OnCollision {
        def bodyEntered(_, _, _, k) = k(())
        def areaEntered(_, _, _, k) = k(())
    };
    let posAfter = (BreakoutScene.ballSnapshot(scene2))#pos;
    let moved = not (posBefore#x == posAfter#x and posBefore#y == posAfter#y);
    Assert.assertTrue(moved)
