// Tests for SceneTree.collectDrawables — coordinate accumulation and draw order

use SceneTree.{identityTransform, collectDrawables}
use SceneTree.Node.{Group, Sprite}

def mkSpriteData(tex: String): SceneTree.SpriteData =
    {size = {x = 10.0f32, y = 10.0f32}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = tex, visible = true}

def mkTransform(px: Float32, py: Float32): SceneTree.Transform2D =
    {position = {x = px, y = py}, scale = {x = 1.0f32, y = 1.0f32}}

// --- 座標加算 ---

// Group の position が子 Sprite のグローバル位置に加算される
@Test
def testPositionAccumulation(): Unit \ Assert =
    let node = Group("Parent", mkTransform(100.0f32, 50.0f32),
        Sprite("Child", mkTransform(10.0f32, 5.0f32), mkSpriteData("block"), Nil) :: Nil
    );
    let result = collectDrawables(node, {x = 0.0f32, y = 0.0f32});
    let posX = List.head(result) |> Option.map(match (_, p) -> p#x);
    let posY = List.head(result) |> Option.map(match (_, p) -> p#y);
    Assert.assertEq(expected = Some(110.0f32), posX);
    Assert.assertEq(expected = Some(55.0f32), posY)

// 3段ネスト: parentPos → Outer → Inner → Leaf の座標が全て加算される
@Test
def testNestedPositionAccumulation(): Unit \ Assert =
    let leaf = Sprite("Leaf", mkTransform(5.0f32, 5.0f32), mkSpriteData("ball"), Nil);
    let inner = Group("Inner", mkTransform(20.0f32, 30.0f32), leaf :: Nil);
    let outer = Group("Outer", mkTransform(100.0f32, 200.0f32), inner :: Nil);
    let result = collectDrawables(outer, {x = 1.0f32, y = 2.0f32});
    let posX = List.head(result) |> Option.map(match (_, p) -> p#x);
    let posY = List.head(result) |> Option.map(match (_, p) -> p#y);
    Assert.assertEq(expected = Some(126.0f32), posX);
    Assert.assertEq(expected = Some(237.0f32), posY)

// visible=false のSpriteは収集されない
@Test
def testInvisibleSpriteSkipped(): Unit \ Assert =
    let sd = {size = {x = 10.0f32, y = 10.0f32}, color = {r = 1.0f32, g = 1.0f32, b = 1.0f32}, texture = "block", visible = false};
    let node = Sprite("Hidden", identityTransform(), sd, Nil);
    let result = collectDrawables(node, {x = 0.0f32, y = 0.0f32});
    Assert.assertEq(expected = 0, List.length(result))

// --- 描画順序 ---

// Group 内の Sprite は宣言順（先頭が最初に描画される）
@Test
def testDrawOrder(): Unit \ Assert =
    let node = Group("Root", identityTransform(),
        Sprite("BG", identityTransform(), mkSpriteData("background"), Nil) ::
        Sprite("Block", identityTransform(), mkSpriteData("block"), Nil) ::
        Sprite("Paddle", identityTransform(), mkSpriteData("paddle"), Nil) ::
        Nil
    );
    let textures = collectDrawables(node, {x = 0.0f32, y = 0.0f32})
        |> List.map(match (sd, _) -> sd#texture);
    Assert.assertEq(expected = "background" :: "block" :: "paddle" :: Nil, textures)

// ネストした Group 内の順序も保持される（DFS 前順）
@Test
def testDrawOrderWithNestedGroup(): Unit \ Assert =
    let bricks = Group("Bricks", identityTransform(),
        Sprite("B1", identityTransform(), mkSpriteData("block"), Nil) ::
        Sprite("B2", identityTransform(), mkSpriteData("block_solid"), Nil) ::
        Nil
    );
    let root = Group("Root", identityTransform(),
        Sprite("BG", identityTransform(), mkSpriteData("background"), Nil) ::
        bricks ::
        Sprite("Paddle", identityTransform(), mkSpriteData("paddle"), Nil) ::
        Nil
    );
    let textures = collectDrawables(root, {x = 0.0f32, y = 0.0f32})
        |> List.map(match (sd, _) -> sd#texture);
    Assert.assertEq(expected = "background" :: "block" :: "block_solid" :: "paddle" :: Nil, textures)
