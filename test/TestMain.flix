// Tests for Breakout game logic — non-trivial computations only

def dummyTextures(): Textures = {
    background = 0,
    block = 1,
    blockSolid = 2,
    paddle = 3,
    ball = 4
}

// parseLevel + buildBricks + buildBrickRow + parseLevelLine の統合テスト
// 実レベルデータを通して非ゼロタイル数が正しいか検証する
@Test
def testParseLevelLevel1BrickCount(): Unit \ Assert =
    let data = "5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n5 5 5 5 5 5 5 5 5 5 5 5 5 5 5\n4 4 4 4 4 0 0 0 0 0 4 4 4 4 4\n4 1 4 1 4 0 0 1 0 0 4 1 4 1 4\n3 3 3 3 3 0 0 0 0 0 3 3 3 3 3\n3 3 1 3 3 3 3 3 3 3 3 3 1 3 3\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2\n2 2 2 2 2 2 2 2 2 2 2 2 2 2 2";
    let level = parseLevel(data, 800, 300, dummyTextures());
    // 8 rows x 15 cols, non-zero counts per row:
    // 15+15+10+11+10+15+15+15 = 106
    Assert.assertEq(expected = 106, List.length(level#bricks))

// playerX = width/2 - playerWidth/2, playerY = height - playerHeight
@Test
def testInitGamePlayerPosition(): Unit \ Assert + IO =
    let state = initGame(800, 600, dummyTextures());
    Assert.assertEq(expected = 350.0f32, state#player#position#x);
    Assert.assertEq(expected = 580.0f32, state#player#position#y)

// ballX = playerX + playerWidth/2 - ballRadius
// ballY = playerY - ballRadius * 2
@Test
def testInitGameBallPosition(): Unit \ Assert + IO =
    let state = initGame(800, 600, dummyTextures());
    Assert.assertEq(expected = 387.5f32, state#ballPos#x);
    Assert.assertEq(expected = 555.0f32, state#ballPos#y)
