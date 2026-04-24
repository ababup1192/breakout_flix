// Tests for Breakout game logic — non-trivial computations only

def dummyTextures(): Textures = {
    background = 0,
    block = 1,
    blockSolid = 2,
    paddle = 3,
    ball = 4
}

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
    let level = parseLevel(data, 800, 300, dummyTextures());
    Assert.assertEq(expected = 106, List.length(level#bricks))

// "1 0 2" → tileType 0 はスキップ → ブリック2個
@Test
def testParseLevelSkipsZero(): Unit \ Assert =
    let level = parseLevel("1 0 2", 300, 100, dummyTextures());
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
    let level = parseLevel("0 0 3", 300, 100, dummyTextures());
    let px = List.head(level#bricks) |> Option.map(b -> b#position#x);
    let py = List.head(level#bricks) |> Option.map(b -> b#position#y);
    Assert.assertEq(expected = Some(200.0f32), px);
    Assert.assertEq(expected = Some(0.0f32), py)

// tileType 1 → isSolid=true, テクスチャ=blockSolid(=2)
@Test
def testParseLevelSolidBrick(): Unit \ Assert =
    let level = parseLevel("1", 100, 100, dummyTextures());
    let solid = List.head(level#bricks) |> Option.map(b -> b#isSolid);
    let texId = List.head(level#bricks) |> Option.map(b -> b#textureId);
    Assert.assertEq(expected = Some(true), solid);
    Assert.assertEq(expected = Some(dummyTextures()#blockSolid), texId)

// tileType 2 → isSolid=false, テクスチャ=block(=1), 色=青(r=0.2)
@Test
def testParseLevelNonSolidBrick(): Unit \ Assert =
    let level = parseLevel("2", 100, 100, dummyTextures());
    let solid = List.head(level#bricks) |> Option.map(b -> b#isSolid);
    let texId = List.head(level#bricks) |> Option.map(b -> b#textureId);
    let red = List.head(level#bricks) |> Option.map(b -> b#color#r);
    Assert.assertEq(expected = Some(false), solid);
    Assert.assertEq(expected = Some(dummyTextures()#block), texId);
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

@Test
def testInitGamePlayerPosition(): Unit \ Assert + IO =
    let state = initGame(800, 600, dummyTextures());
    Assert.assertEq(expected = 350.0f32, state#player#position#x);
    Assert.assertEq(expected = 580.0f32, state#player#position#y)

@Test
def testInitGameBallPosition(): Unit \ Assert + IO =
    let state = initGame(800, 600, dummyTextures());
    Assert.assertEq(expected = 387.5f32, state#ballPos#x);
    Assert.assertEq(expected = 555.0f32, state#ballPos#y)
