// Tests for SceneTree — coordinate accumulation, draw order, utilities, and engine

use SceneTree.{collectDrawables,
               node2D, sprite2D, collisionShape2D, kinematicBody2D, staticBody2D, rigidBody2D, area2D}

def mkSpriteData(tex: String): SceneTree.SpriteData =
    {size = {x = 10.0f32, y = 10.0f32}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = tex, visible = true}

def mkPos(px: Float32, py: Float32): Vec2D.Vec2 =
    {x = px, y = py}

// --- 座標加算 ---

@Test
def testPositionAccumulation(): Unit \ Assert =
    let node = node2D("Parent", mkPos(100.0f32, 50.0f32),
        sprite2D("Child", mkPos(10.0f32, 5.0f32), mkSpriteData("block")) :: Nil
    );
    let result = collectDrawables(node, {x = 0.0f32, y = 0.0f32});
    let posX = List.head(result) |> Option.map(match (_, p) -> p#x);
    let posY = List.head(result) |> Option.map(match (_, p) -> p#y);
    Assert.assertEq(expected = Some(110.0f32), posX);
    Assert.assertEq(expected = Some(55.0f32), posY)

@Test
def testNestedPositionAccumulation(): Unit \ Assert =
    let leaf = sprite2D("Leaf", mkPos(5.0f32, 5.0f32), mkSpriteData("ball"));
    let inner = node2D("Inner", mkPos(20.0f32, 30.0f32), leaf :: Nil);
    let outer = node2D("Outer", mkPos(100.0f32, 200.0f32), inner :: Nil);
    let result = collectDrawables(outer, {x = 1.0f32, y = 2.0f32});
    let posX = List.head(result) |> Option.map(match (_, p) -> p#x);
    let posY = List.head(result) |> Option.map(match (_, p) -> p#y);
    Assert.assertEq(expected = Some(126.0f32), posX);
    Assert.assertEq(expected = Some(237.0f32), posY)

@Test
def testInvisibleSpriteSkipped(): Unit \ Assert =
    let sd = {size = {x = 10.0f32, y = 10.0f32}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "block", visible = false};
    let node = sprite2D("Hidden", Vec2D.zero(), sd);
    let result = collectDrawables(node, {x = 0.0f32, y = 0.0f32});
    Assert.assertEq(expected = 0, List.length(result))

// --- 描画順序 ---

@Test
def testDrawOrder(): Unit \ Assert =
    let node = node2D("Root", Vec2D.zero(),
        sprite2D("BG", Vec2D.zero(), mkSpriteData("background")) ::
        sprite2D("Block", Vec2D.zero(), mkSpriteData("block")) ::
        sprite2D("Paddle", Vec2D.zero(), mkSpriteData("paddle")) ::
        Nil
    );
    let textures = collectDrawables(node, {x = 0.0f32, y = 0.0f32})
        |> List.map(match (sd, _) -> sd#texture);
    Assert.assertEq(expected = "background" :: "block" :: "paddle" :: Nil, textures)

@Test
def testDrawOrderWithNestedGroup(): Unit \ Assert =
    let bricks = node2D("Bricks", Vec2D.zero(),
        sprite2D("B1", Vec2D.zero(), mkSpriteData("block")) ::
        sprite2D("B2", Vec2D.zero(), mkSpriteData("block_solid")) ::
        Nil
    );
    let root = node2D("Root", Vec2D.zero(),
        sprite2D("BG", Vec2D.zero(), mkSpriteData("background")) ::
        bricks ::
        sprite2D("Paddle", Vec2D.zero(), mkSpriteData("paddle")) ::
        Nil
    );
    let textures = collectDrawables(root, {x = 0.0f32, y = 0.0f32})
        |> List.map(match (sd, _) -> sd#texture);
    Assert.assertEq(expected = "background" :: "block" :: "block_solid" :: "paddle" :: Nil, textures)

// --- destroyed StaticBody2D のスキップ ---

@Test
def testDestroyedStaticBodySkipped(): Unit \ Assert =
    let brick = staticBody2D("Brick", mkPos(10.0f32, 10.0f32),
        {isSolid = false, destroyed = true},
        sprite2D("BrickSprite", Vec2D.zero(), mkSpriteData("block")) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), brick :: Nil);
    let result = collectDrawables(root, Vec2D.zero());
    Assert.assertEq(expected = 0, List.length(result))

// --- KinematicBody2D の子Spriteが描画される ---

@Test
def testKinematicBodyChildDrawable(): Unit \ Assert =
    let paddle = kinematicBody2D("Paddle", mkPos(100.0f32, 200.0f32),
        {velocity = Vec2D.zero()},
        sprite2D("PaddleSprite", Vec2D.zero(), mkSpriteData("paddle")) :: Nil
    );
    let result = collectDrawables(paddle, Vec2D.zero());
    Assert.assertEq(expected = 1, List.length(result));
    let posX = List.head(result) |> Option.map(match (_, p) -> p#x);
    Assert.assertEq(expected = Some(100.0f32), posX)

// --- findNode ---

@Test
def testFindNodeRoot(): Unit \ Assert =
    let root = node2D("Root", Vec2D.zero(), Nil);
    let found = SceneTree.findNode("Root", root) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Root"), found)

@Test
def testFindNodeNested(): Unit \ Assert =
    let child = sprite2D("Ball", Vec2D.zero(), mkSpriteData("ball"));
    let root = node2D("Root", Vec2D.zero(), child :: Nil);
    let found = SceneTree.findNode("Ball", root) |> Option.map(SceneTree.nodeName);
    Assert.assertEq(expected = Some("Ball"), found)

@Test
def testFindNodeMissing(): Unit \ Assert =
    let root = node2D("Root", Vec2D.zero(), Nil);
    let found = SceneTree.findNode("Missing", root);
    Assert.assertTrue(Option.isEmpty(found))

// --- mapNode ---

@Test
def testMapNodeModifiesTarget(): Unit \ Assert =
    let root = node2D("Root", Vec2D.zero(),
        sprite2D("Ball", mkPos(10.0f32, 20.0f32), mkSpriteData("ball")) :: Nil
    );
    let updated = SceneTree.mapNode("Ball", _ ->
        sprite2D("Ball", mkPos(99.0f32, 99.0f32), mkSpriteData("ball")),
        root
    );
    let pos = SceneTree.globalPosition("Ball", updated);
    let px = pos |> Option.map(p -> p#x);
    Assert.assertEq(expected = Some(99.0f32), px)

// --- globalPosition ---

@Test
def testGlobalPositionNested(): Unit \ Assert =
    let child = sprite2D("Ball", mkPos(10.0f32, 20.0f32), mkSpriteData("ball"));
    let parent = node2D("Root", mkPos(100.0f32, 200.0f32), child :: Nil);
    let pos = SceneTree.globalPosition("Ball", parent);
    let px = pos |> Option.map(p -> p#x);
    let py = pos |> Option.map(p -> p#y);
    Assert.assertEq(expected = Some(110.0f32), px);
    Assert.assertEq(expected = Some(220.0f32), py)

// --- vectorDirection ---

@Test
def testVectorDirectionUp(): Unit \ Assert =
    Assert.assertEq(expected = SceneTree.Direction.Up, SceneTree.vectorDirection({x = 0.0f32, y = 5.0f32}))

@Test
def testVectorDirectionRight(): Unit \ Assert =
    Assert.assertEq(expected = SceneTree.Direction.Right, SceneTree.vectorDirection({x = 5.0f32, y = 0.0f32}))

@Test
def testVectorDirectionDown(): Unit \ Assert =
    Assert.assertEq(expected = SceneTree.Direction.Down, SceneTree.vectorDirection({x = 0.0f32, y = -5.0f32}))

@Test
def testVectorDirectionLeft(): Unit \ Assert =
    Assert.assertEq(expected = SceneTree.Direction.Left, SceneTree.vectorDirection({x = -5.0f32, y = 0.0f32}))

// --- checkCircleRect ---

@Test
def testCheckCircleRectNoHit(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 0.0f32, y = 0.0f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    Assert.assertTrue(Option.isEmpty(result))

@Test
def testCheckCircleRectTopHit(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 112.5f32, y = 83.0f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    let dir = result |> Option.map(match (d, _) -> d);
    Assert.assertTrue(not Option.isEmpty(result));
    Assert.assertEq(expected = Some(SceneTree.Direction.Up), dir)

@Test
def testCheckCircleRectLeftHit(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 78.0f32, y = 97.5f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    let dir = result |> Option.map(match (d, _) -> d);
    Assert.assertEq(expected = Some(SceneTree.Direction.Right), dir)

@Test
def testCheckCircleRectRightHit(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 148.0f32, y = 97.5f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    let dir = result |> Option.map(match (d, _) -> d);
    Assert.assertEq(expected = Some(SceneTree.Direction.Left), dir)

@Test
def testCheckCircleRectBottomHit(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 112.5f32, y = 117.0f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    let dir = result |> Option.map(match (d, _) -> d);
    Assert.assertEq(expected = Some(SceneTree.Direction.Down), dir)

@Test
def testCheckCircleRectBarelyMiss(): Unit \ Assert =
    let result = SceneTree.checkCircleRect({x = 112.5f32, y = 74.9f32}, 12.5f32,
        {x = 100.0f32, y = 100.0f32}, {x = 50.0f32, y = 20.0f32});
    Assert.assertTrue(Option.isEmpty(result))

// --- resolveCollision ---

@Test
def testResolveCollisionHorizontal(): Unit \ Assert =
    let (newPos, newVel) = SceneTree.resolveCollision(
        {x = 100.0f32, y = 100.0f32}, {x = 200.0f32, y = -300.0f32}, 12.5f32,
        SceneTree.Direction.Left, {x = -5.0f32, y = 0.0f32});
    Assert.assertTrue(newPos#x > 100.0f32);
    Assert.assertTrue(newVel#x < 0.0f32);
    Assert.assertEq(expected = -300.0f32, newVel#y)

@Test
def testResolveCollisionVertical(): Unit \ Assert =
    let (newPos, newVel) = SceneTree.resolveCollision(
        {x = 100.0f32, y = 100.0f32}, {x = 200.0f32, y = -300.0f32}, 12.5f32,
        SceneTree.Direction.Up, {x = 0.0f32, y = -5.0f32});
    Assert.assertTrue(newPos#y < 100.0f32);
    Assert.assertEq(expected = 200.0f32, newVel#x);
    Assert.assertTrue(newVel#y > 0.0f32)

// --- collectBodies ---

@Test
def testCollectBodiesStaticBody(): Unit \ Assert =
    let brick = staticBody2D("Brick_0", mkPos(50.0f32, 50.0f32),
        {isSolid = false, destroyed = false},
        collisionShape2D("BrickCollider_0", SceneTree.CollisionShapeData.RectShape({x = 80.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), brick :: Nil);
    let bodies = SceneTree.collectBodies(root);
    Assert.assertEq(expected = 1, List.length(bodies));
    let name = List.head(bodies) |> Option.map(b -> b#name);
    Assert.assertEq(expected = Some("Brick_0"), name)

@Test
def testCollectBodiesSkipsDestroyed(): Unit \ Assert =
    let brick = staticBody2D("Brick_0", mkPos(50.0f32, 50.0f32),
        {isSolid = false, destroyed = true},
        collisionShape2D("BrickCollider_0", SceneTree.CollisionShapeData.RectShape({x = 80.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), brick :: Nil);
    let bodies = SceneTree.collectBodies(root);
    Assert.assertEq(expected = 0, List.length(bodies))

// --- physicsStep ---

@Test
def testPhysicsStepKinematicBody(): Unit \ Assert =
    let paddle = kinematicBody2D("Paddle", mkPos(100.0f32, 580.0f32),
        {velocity = {x = 500.0f32, y = 0.0f32}},
        collisionShape2D("PaddleCollider", SceneTree.CollisionShapeData.RectShape({x = 100.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), paddle :: Nil);
    let config = {screenSize = {x = 800.0f32, y = 600.0f32}, anchorPos = {x = 100.0f32, y = 580.0f32}, anchorWidth = 100.0f32};
    let newRoot = run {
        SceneTree.stepAndCollide(root, 0.1f32, config)
    } with handler SceneTree.OnCollision {
        def bodyEntered(_, _, _, k) = k(())
        def areaEntered(_, _, _, k) = k(())
    };
    let pos = SceneTree.globalPosition("Paddle", newRoot);
    let px = pos |> Option.map(p -> p#x);
    Assert.assertEq(expected = Some(150.0f32), px)

@Test
def testPhysicsStepRigidBodyStuck(): Unit \ Assert =
    let paddle = kinematicBody2D("Paddle", mkPos(350.0f32, 580.0f32),
        {velocity = Vec2D.zero()},
        collisionShape2D("PaddleCollider", SceneTree.CollisionShapeData.RectShape({x = 100.0f32, y = 20.0f32})) :: Nil
    );
    let ball = rigidBody2D("Ball", mkPos(0.0f32, 0.0f32),
        {velocity = {x = 100.0f32, y = -350.0f32}, stuck = true},
        collisionShape2D("BallCollider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), paddle :: ball :: Nil);
    let config = {screenSize = {x = 800.0f32, y = 600.0f32}, anchorPos = {x = 350.0f32, y = 580.0f32}, anchorWidth = 100.0f32};
    let newRoot = run {
        SceneTree.stepAndCollide(root, 0.1f32, config)
    } with handler SceneTree.OnCollision {
        def bodyEntered(_, _, _, k) = k(())
        def areaEntered(_, _, _, k) = k(())
    };
    let pos = SceneTree.globalPosition("Ball", newRoot);
    let px = pos |> Option.map(p -> p#x);
    let expectedX = 350.0f32 + 100.0f32 / 2.0f32 - 12.5f32;
    Assert.assertEq(expected = Some(expectedX), px)

// --- updateKinematicBody ---

@Test
def testUpdateKinematicBody(): Unit \ Assert =
    let paddle = kinematicBody2D("Paddle", mkPos(100.0f32, 200.0f32),
        {velocity = Vec2D.zero()},
        Nil
    );
    let root = node2D("Root", Vec2D.zero(), paddle :: Nil);
    let updated = SceneTree.updateKinematicBody("Paddle",
        _ -> {velocity = {x = 500.0f32, y = 0.0f32}}, root);
    let vel = SceneTree.findNode("Paddle", updated)
        |> Option.flatMap(SceneTree.kinematicBodyData)
        |> Option.map(bd -> bd#velocity#x);
    Assert.assertEq(expected = Some(500.0f32), vel)

// --- updateRigidBody ---

@Test
def testUpdateRigidBody(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(50.0f32, 50.0f32),
        {velocity = {x = 100.0f32, y = -200.0f32}, stuck = true},
        Nil
    );
    let root = node2D("Root", Vec2D.zero(), ball :: Nil);
    let updated = SceneTree.updateRigidBody("Ball",
        bd -> {stuck = false | bd}, root);
    let stuck = SceneTree.findNode("Ball", updated)
        |> Option.flatMap(SceneTree.rigidBodyData)
        |> Option.map(bd -> bd#stuck);
    Assert.assertEq(expected = Some(false), stuck)

// --- updateStaticBody ---

@Test
def testUpdateStaticBody(): Unit \ Assert =
    let brick = staticBody2D("Brick", mkPos(10.0f32, 10.0f32),
        {isSolid = false, destroyed = false},
        Nil
    );
    let root = node2D("Root", Vec2D.zero(), brick :: Nil);
    let updated = SceneTree.updateStaticBody("Brick",
        _ -> {isSolid = false, destroyed = true}, root);
    let found = SceneTree.findNode("Brick", updated);
    let destroyed = found |> Option.flatMap(SceneTree.staticBodyData)
        |> Option.map(bd -> bd#destroyed);
    Assert.assertEq(expected = Some(true), destroyed)

// --- setNodePosition ---

@Test
def testSetNodePosition(): Unit \ Assert =
    let child = sprite2D("Ball", mkPos(10.0f32, 20.0f32), mkSpriteData("ball"));
    let root = node2D("Root", Vec2D.zero(), child :: Nil);
    let updated = SceneTree.setNodePosition("Ball", {x = 99.0f32, y = 88.0f32}, root);
    let pos = SceneTree.globalPosition("Ball", updated);
    let px = pos |> Option.map(p -> p#x);
    let py = pos |> Option.map(p -> p#y);
    Assert.assertEq(expected = Some(99.0f32), px);
    Assert.assertEq(expected = Some(88.0f32), py)

// --- rigidBodyData ---

@Test
def testRigidBodyData(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(50.0f32, 50.0f32),
        {velocity = {x = 100.0f32, y = -200.0f32}, stuck = true},
        Nil
    );
    let result = SceneTree.rigidBodyData(ball);
    let vel = result |> Option.map(bd -> bd#velocity#x);
    Assert.assertEq(expected = Some(100.0f32), vel)

@Test
def testRigidBodyDataNonRigid(): Unit \ Assert =
    let spriteNode = sprite2D("Ball", mkPos(0.0f32, 0.0f32), mkSpriteData("ball"));
    let result = SceneTree.rigidBodyData(spriteNode);
    Assert.assertTrue(Option.isEmpty(result))

// --- kinematicBodyData ---

@Test
def testKinematicBodyData(): Unit \ Assert =
    let paddle = kinematicBody2D("Paddle", mkPos(100.0f32, 200.0f32),
        {velocity = {x = 500.0f32, y = 0.0f32}},
        Nil
    );
    let result = SceneTree.kinematicBodyData(paddle);
    let vel = result |> Option.map(bd -> bd#velocity#x);
    Assert.assertEq(expected = Some(500.0f32), vel)

// --- findRectShape ---

@Test
def testFindRectShape(): Unit \ Assert =
    let children =
        sprite2D("Sprite", Vec2D.zero(), mkSpriteData("block")) ::
        collisionShape2D("Collider", SceneTree.CollisionShapeData.RectShape({x = 80.0f32, y = 20.0f32})) ::
        Nil;
    let result = SceneTree.findRectShape(children);
    let w = result |> Option.map(s -> s#x);
    Assert.assertEq(expected = Some(80.0f32), w)

@Test
def testFindRectShapeNone(): Unit \ Assert =
    let children = sprite2D("Sprite", Vec2D.zero(), mkSpriteData("block")) :: Nil;
    let result = SceneTree.findRectShape(children);
    Assert.assertTrue(Option.isEmpty(result))

// --- findCircleShape ---

@Test
def testFindCircleShape(): Unit \ Assert =
    let children =
        sprite2D("Sprite", Vec2D.zero(), mkSpriteData("ball")) ::
        collisionShape2D("Collider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) ::
        Nil;
    let result = SceneTree.findCircleShape(children);
    Assert.assertEq(expected = Some(12.5f32), result)

@Test
def testFindCircleShapeNone(): Unit \ Assert =
    let children = sprite2D("Sprite", Vec2D.zero(), mkSpriteData("ball")) :: Nil;
    let result = SceneTree.findCircleShape(children);
    Assert.assertTrue(Option.isEmpty(result))

// --- detectCollisions ---

@Test
def testDetectCollisionsBallHitsBrick(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(112.5f32, 83.0f32),
        {velocity = {x = 0.0f32, y = 100.0f32}, stuck = false},
        collisionShape2D("BallCollider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) :: Nil
    );
    let brick = staticBody2D("Brick_0", mkPos(100.0f32, 100.0f32),
        {isSolid = false, destroyed = false},
        collisionShape2D("BrickCollider", SceneTree.CollisionShapeData.RectShape({x = 50.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), ball :: brick :: Nil);
    let events = SceneTree.detectCollisions(root);
    Assert.assertEq(expected = 1, List.length(events));
    let targetName = List.head(events) |> Option.map(e -> e#targetName);
    Assert.assertEq(expected = Some("Brick_0"), targetName)

@Test
def testDetectCollisionsNoHit(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(0.0f32, 0.0f32),
        {velocity = {x = 100.0f32, y = 100.0f32}, stuck = false},
        collisionShape2D("BallCollider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) :: Nil
    );
    let brick = staticBody2D("Brick_0", mkPos(200.0f32, 200.0f32),
        {isSolid = false, destroyed = false},
        collisionShape2D("BrickCollider", SceneTree.CollisionShapeData.RectShape({x = 50.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), ball :: brick :: Nil);
    let events = SceneTree.detectCollisions(root);
    Assert.assertEq(expected = 0, List.length(events))

@Test
def testDetectCollisionsStuckBallIgnored(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(112.5f32, 83.0f32),
        {velocity = {x = 0.0f32, y = 100.0f32}, stuck = true},
        collisionShape2D("BallCollider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) :: Nil
    );
    let brick = staticBody2D("Brick_0", mkPos(100.0f32, 100.0f32),
        {isSolid = false, destroyed = false},
        collisionShape2D("BrickCollider", SceneTree.CollisionShapeData.RectShape({x = 50.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), ball :: brick :: Nil);
    let events = SceneTree.detectCollisions(root);
    Assert.assertEq(expected = 0, List.length(events))

@Test
def testDetectCollisionsAreaContainsNode(): Unit \ Assert =
    let ball = rigidBody2D("Ball", mkPos(112.5f32, 83.0f32),
        {velocity = {x = 0.0f32, y = 100.0f32}, stuck = false},
        collisionShape2D("BallCollider", SceneTree.CollisionShapeData.CircleShape(12.5f32)) :: Nil
    );
    let brick = staticBody2D("Brick_0", mkPos(100.0f32, 100.0f32),
        {isSolid = false, destroyed = false},
        collisionShape2D("BrickCollider", SceneTree.CollisionShapeData.RectShape({x = 50.0f32, y = 20.0f32})) :: Nil
    );
    let root = node2D("Root", Vec2D.zero(), ball :: brick :: Nil);
    let events = SceneTree.detectCollisions(root);
    let areaName = List.head(events) |> Option.map(e -> SceneTree.nodeName(e#area));
    Assert.assertEq(expected = Some("Ball"), areaName)
