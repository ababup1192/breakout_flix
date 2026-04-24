// Tests for Breakout game logic — non-trivial computations only

use Game.Direction.{Up, Right, Down, Left}
use Game.{parseLevelLine, parseLevel, initGame,
          playerWidth, playerHeight, ballRadiusConst,
          screenWidth, screenHeight,
          initialBallVelocityX, initialBallVelocityY,
          moveBall, checkBallReset, processInput,
          keyA, keyD, keySpace,
          vectorDirection, checkCollisionCircleAABB}

// --- parseLevelLine ---

// 通常行のパース
@Test
def testParseLevelLineNormal(): Unit \ Assert =
    Assert.assertEq(expected = 1 :: 0 :: 3 :: Nil, parseLevelLine("1 0 3"))

// 空行は空リスト
@Test
def testParseLevelLineEmpty(): Unit \ Assert =
    Assert.assertEq(expected = (Nil: List[Int32]), parseLevelLine(""))

// 前後の空白を無視
@Test
def testParseLevelLineTrimmed(): Unit \ Assert =
    Assert.assertEq(expected = 2 :: 5 :: Nil, parseLevelLine("  2 5  "))

// --- parseLevel 統合テスト (parseLevelLine + buildBricks + buildBrickRow) ---
//
// parseLevel の処理フロー:
//   data (文字列) --> parseLevelLine (各行) --> buildBricks --> buildBrickRow
//                                                               |
//   "1 0 3\n2 0 1"  -->  [1,0,3], [2,0,1]  --> tileType=0 skip, それ以外は GameObject 生成

// レベル1データ (8行x15列) の非ゼロタイル数が正しいか
//   行ごと: 15+15+10+11+10+15+15+15 = 106
@Test
def testParseLevelBrickCount(): Unit \ Assert =
    let data = "5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n4 4 4 4 4 0 0 0 0 0 4 4 4 4 4\n4 1 4 1 4 0 0 1 0 0 4 1 4 1 4\n3 3 3 3 3 0 0 0 0 0 3 3 3 3 3\n3 3 1 3 3 3 3 3 3 3 3 3 1 3 3\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2";
    let level = parseLevel(data, 800, 300);
    Assert.assertEq(expected = 106, List.length(level#bricks))

// "1 0 2" → tileType 0 はスキップ → ブリック2個
@Test
def testParseLevelSkipsZero(): Unit \ Assert =
    let level = parseLevel("1 0 2", 300, 100);
    Assert.assertEq(expected = 2, List.length(level#bricks))

// position 計算:
//   入力 "0 0 3", 幅300 → 3列なので unitW = 300/3 = 100
//   col:   0     1     2
//        +-----+-----+-----+
//   row0 |  0  |  0  |  3  |  <- tileType 0 はスキップ
//        +-----+-----+-----+
//   唯一のブリックは col=2 → x = 2*100 = 200, y = 0*100 = 0
@Test
def testParseLevelBrickPosition(): Unit \ Assert =
    let level = parseLevel("0 0 3", 300, 100);
    let px = List.head(level#bricks) |> Option.map(b -> b#position#x);
    let py = List.head(level#bricks) |> Option.map(b -> b#position#y);
    Assert.assertEq(expected = Some(200.0f32), px);
    Assert.assertEq(expected = Some(0.0f32), py)

// tileType 1 → isSolid=true, テクスチャ=block_solid
@Test
def testParseLevelSolidBrick(): Unit \ Assert =
    let level = parseLevel("1", 100, 100);
    let solid = List.head(level#bricks) |> Option.map(b -> b#isSolid);
    let tex = List.head(level#bricks) |> Option.map(b -> b#texture);
    Assert.assertEq(expected = Some(true), solid);
    Assert.assertEq(expected = Some("block_solid"), tex)

// tileType 2 → isSolid=false, テクスチャ=block, 色=青(r=0.2)
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
//
//   画面 800x600:
//        0                 350  400  450                 800
//        |                  |    |    |                   |
//   580  +------------------[==PADDLE==]------------------+
//   600  +------------------------------------------------+
//
//   playerX = 800/2 - 100/2 = 350
//   playerY = 600 - 20 = 580
//   ballX   = 350 + 100/2 - 12.5 = 387.5
//   ballY   = 580 - 12.5*2 = 555

def dummyFileRead(f: Unit -> a \ ef): a \ ef - Fs.FileRead =
    run { f() } with handler Fs.FileRead {
        def read(_path, resume) = resume(Ok("1"))
        def readBytes(_path, resume) = resume(Ok(Vector#{}))
        def readLines(_path, resume) = resume(Ok("1" :: Nil))
    }

@Test
def testInitGamePlayerPosition(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    Assert.assertEq(expected = 350.0f32, state#player#position#x);
    Assert.assertEq(expected = 580.0f32, state#player#position#y)

@Test
def testInitGameBallPosition(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    Assert.assertEq(expected = 387.5f32, state#ballPos#x);
    Assert.assertEq(expected = 555.0f32, state#ballPos#y)

@Test
def testInitGameBallVelocity(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    Assert.assertEq(expected = 100.0f32, state#ballVelocity#x);
    Assert.assertEq(expected = -350.0f32, state#ballVelocity#y)

@Test
def testInitGameBallStuck(): Unit \ Assert =
    let state = run { initGame(800, 600) } with dummyFileRead;
    Assert.assertTrue(state#ballStuck)

// --- moveBall ---

// テスト用ヘルパー: 最小限のGameStateを構築
def mkTestState(
    px: Float32, py: Float32,
    bx: Float32, by: Float32,
    vx: Float32, vy: Float32,
    stuck: Bool
): Game.GameState =
    {
        levels = Nil,
        currentLevel = 0,
        player = {
            position = {x = px, y = py},
            size = {x = playerWidth(), y = playerHeight()},
            color = {r = 1.0f32, g = 1.0f32, b = 1.0f32},
            isSolid = true,
            destroyed = false,
            texture = "paddle"
        },
        ballPos = {x = bx, y = by},
        ballVelocity = {x = vx, y = vy},
        ballRadius = ballRadiusConst(),
        ballStuck = stuck
    }

// 固定時は動かない
@Test
def testMoveBallStuck(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let result = moveBall(state, 0.016f32);
    Assert.assertEq(expected = 387.5f32, result#ballPos#x);
    Assert.assertEq(expected = 555.0f32, result#ballPos#y)

// 通常移動
@Test
def testMoveBallNormal(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 300.0f32, 100.0f32, -350.0f32, false);
    let result = moveBall(state, 1.0f32);
    Assert.assertEq(expected = 500.0f32, result#ballPos#x);
    Assert.assertEq(expected = 0.0f32, result#ballPos#y)

// 左壁反射: x が 0 以下 → x=0, vx が正に
@Test
def testMoveBallLeftWall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 5.0f32, 300.0f32, -100.0f32, 0.0f32, false);
    let result = moveBall(state, 1.0f32);
    Assert.assertEq(expected = 0.0f32, result#ballPos#x);
    Assert.assertTrue(result#ballVelocity#x > 0.0f32)

// 右壁反射: x + ballSize >= screenWidth → クランプ, vx が負に
@Test
def testMoveBallRightWall(): Unit \ Assert =
    let ballSize = ballRadiusConst() * 2.0f32;
    let startX = Int32.toFloat32(screenWidth()) - ballSize - 10.0f32;
    let state = mkTestState(350.0f32, 580.0f32, startX, 300.0f32, 200.0f32, 0.0f32, false);
    let result = moveBall(state, 1.0f32);
    Assert.assertEq(expected = Int32.toFloat32(screenWidth()) - ballSize, result#ballPos#x);
    Assert.assertTrue(result#ballVelocity#x < 0.0f32)

// 上壁反射: y が 0 以下 → y=0, vy が正に
@Test
def testMoveBallTopWall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 5.0f32, 0.0f32, -100.0f32, false);
    let result = moveBall(state, 1.0f32);
    Assert.assertEq(expected = 0.0f32, result#ballPos#y);
    Assert.assertTrue(result#ballVelocity#y > 0.0f32)

// --- checkBallReset ---

// 画面内 → 変化なし
@Test
def testCheckBallResetInBounds(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, 300.0f32, 100.0f32, -350.0f32, false);
    let (result, _) = GameAction.unpack(checkBallReset(state));
    Assert.assertEq(expected = 400.0f32, result#ballPos#x);
    Assert.assertEq(expected = 300.0f32, result#ballPos#y);
    Assert.assertFalse(result#ballStuck)

// 画面下 → パドル上にリセット + stuck=true
@Test
def testCheckBallResetBelowScreen(): Unit \ Assert =
    let sh = Int32.toFloat32(screenHeight());
    let state = mkTestState(350.0f32, 580.0f32, 400.0f32, sh + 10.0f32, 100.0f32, 200.0f32, false);
    let (result, _) = GameAction.unpack(checkBallReset(state));
    let expectedX = 350.0f32 + playerWidth() / 2.0f32 - ballRadiusConst();
    let expectedY = 580.0f32 - ballRadiusConst() * 2.0f32;
    Assert.assertEq(expected = expectedX, result#ballPos#x);
    Assert.assertEq(expected = expectedY, result#ballPos#y);
    Assert.assertTrue(result#ballStuck);
    Assert.assertEq(expected = initialBallVelocityX(), result#ballVelocity#x);
    Assert.assertEq(expected = initialBallVelocityY(), result#ballVelocity#y)

// --- processInput ---

// テスト用モックハンドラ
def withMockInput(pressedKeys: Set[Int32], f: Unit -> a \ ef): a \ ef - Game.Game =
    run { f() } with handler Game.Game {
        def getInitialState(_, _k) = bug!("not used in test")
        def render(_, resume) = resume(())
        def shouldClose(_, resume) = resume(true)
        def getDeltaTime(_, resume) = resume(0.0f32)
        def isKeyPressed(keyCode, resume) = resume(Set.memberOf(keyCode, pressedKeys))
    }

// Aキーでパドルが左に移動
@Test
def testProcessInputMoveLeft(): Unit \ Assert =
    let state = mkTestState(100.0f32, 580.0f32, 137.5f32, 555.0f32, 100.0f32, -350.0f32, false);
    let (result, _) = GameAction.unpack(run { processInput(state, 0.1f32) } with withMockInput(Set#{keyA()}));
    Assert.assertTrue(result#player#position#x < 100.0f32)

// Dキーでパドルが右に移動
@Test
def testProcessInputMoveRight(): Unit \ Assert =
    let state = mkTestState(100.0f32, 580.0f32, 137.5f32, 555.0f32, 100.0f32, -350.0f32, false);
    let (result, _) = GameAction.unpack(run { processInput(state, 0.1f32) } with withMockInput(Set#{keyD()}));
    Assert.assertTrue(result#player#position#x > 100.0f32)

// Spaceキーでボール発射
@Test
def testProcessInputLaunchBall(): Unit \ Assert =
    let state = mkTestState(350.0f32, 580.0f32, 387.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let (result, _) = GameAction.unpack(run { processInput(state, 0.1f32) } with withMockInput(Set#{keySpace()}));
    Assert.assertFalse(result#ballStuck)

// 固定中にAキー → ボールもパドルに追従
@Test
def testProcessInputStuckBallFollowsPaddle(): Unit \ Assert =
    let state = mkTestState(100.0f32, 580.0f32, 137.5f32, 555.0f32, 100.0f32, -350.0f32, true);
    let (result, _) = GameAction.unpack(run { processInput(state, 0.1f32) } with withMockInput(Set#{keyA()}));
    let expectedBallX = result#player#position#x + playerWidth() / 2.0f32 - ballRadiusConst();
    Assert.assertEq(expected = expectedBallX, result#ballPos#x)

// --- vectorDirection ---

// 正のY方向 → Up
@Test
def testVectorDirectionUp(): Unit \ Assert =
    Assert.assertEq(expected = Up, vectorDirection({x = 0.0f32, y = 5.0f32}))

// 正のX方向 → Right
@Test
def testVectorDirectionRight(): Unit \ Assert =
    Assert.assertEq(expected = Right, vectorDirection({x = 5.0f32, y = 0.0f32}))

// 負のY方向 → Down
@Test
def testVectorDirectionDown(): Unit \ Assert =
    Assert.assertEq(expected = Down, vectorDirection({x = 0.0f32, y = -5.0f32}))

// 負のX方向 → Left
@Test
def testVectorDirectionLeft(): Unit \ Assert =
    Assert.assertEq(expected = Left, vectorDirection({x = -5.0f32, y = 0.0f32}))

// 斜め (3,4) → Yが大きいのでUp
@Test
def testVectorDirectionDiagonalUp(): Unit \ Assert =
    Assert.assertEq(expected = Up, vectorDirection({x = 3.0f32, y = 4.0f32}))

// 斜め (4,3) → Xが大きいのでRight
@Test
def testVectorDirectionDiagonalRight(): Unit \ Assert =
    Assert.assertEq(expected = Right, vectorDirection({x = 4.0f32, y = 3.0f32}))

// --- checkCollisionCircleAABB ---

def mkBrick(px: Float32, py: Float32, w: Float32, h: Float32, solid: Bool): Game.GameObject =
    {
        position = {x = px, y = py},
        size = {x = w, y = h},
        color = {r = 1.0f32, g = 1.0f32, b = 1.0f32},
        isSolid = solid,
        destroyed = false,
        texture = if (solid) "block_solid" else "block"
    }

// 衝突なし: ボールがブリックから離れている
@Test
def testCollisionNoHit(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    let (collision, _, _) = checkCollisionCircleAABB({x = 0.0f32, y = 0.0f32}, 12.5f32, brick);
    Assert.assertFalse(collision)

// 衝突あり: ボールがブリック上面に当たる
//   Brick (100,100) size(50,20), Ball pos(112.5, 83) r=12.5
//   BallCenter=(125, 95.5), AABBCenter=(125, 110)
//   closest=(125, 100), diff=(0, 4.5), dist=4.5 < 12.5 → 衝突
//   方向: (0, +4.5) → Up
@Test
def testCollisionTopHit(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    let (collision, dir, _) = checkCollisionCircleAABB({x = 112.5f32, y = 83.0f32}, 12.5f32, brick);
    Assert.assertTrue(collision);
    Assert.assertEq(expected = Up, dir)

// 衝突あり: ボールがブリック左面に当たる
//   Ball pos(78, 97.5) r=12.5 → Center=(90.5, 110)
//   closest=(100, 110), diff=(9.5, 0) → Right方向
@Test
def testCollisionLeftFaceHit(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    let (collision, dir, _) = checkCollisionCircleAABB({x = 78.0f32, y = 97.5f32}, 12.5f32, brick);
    Assert.assertTrue(collision);
    Assert.assertEq(expected = Right, dir)

// 衝突あり: ボールがブリック右面に当たる
//   Ball pos(148, 97.5) r=12.5 → Center=(160.5, 110)
//   closest=(150, 110), diff=(-10.5, 0) → Left方向
@Test
def testCollisionRightFaceHit(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    let (collision, dir, _) = checkCollisionCircleAABB({x = 148.0f32, y = 97.5f32}, 12.5f32, brick);
    Assert.assertTrue(collision);
    Assert.assertEq(expected = Left, dir)

// 衝突あり: ボールがブリック下面に当たる
//   Ball pos(112.5, 117) r=12.5 → Center=(125, 129.5)
//   AABBCenter=(125, 110), closest=(125, 120)
//   diff=(0, -9.5) → Down方向
@Test
def testCollisionBottomHit(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    let (collision, dir, _) = checkCollisionCircleAABB({x = 112.5f32, y = 117.0f32}, 12.5f32, brick);
    Assert.assertTrue(collision);
    Assert.assertEq(expected = Down, dir)

// ぎりぎり衝突しない: ボール半径ちょうどの距離
@Test
def testCollisionBarelyMiss(): Unit \ Assert =
    let brick = mkBrick(100.0f32, 100.0f32, 50.0f32, 20.0f32, false);
    // Ball center at (125, 100 - 12.5 - 0.1) = ballPos.y = 100 - 25.1 = 74.9
    let (collision, _, _) = checkCollisionCircleAABB({x = 112.5f32, y = 74.9f32}, 12.5f32, brick);
    Assert.assertFalse(collision)
