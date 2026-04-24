// Breakout - Faithful LearnOpenGL Recreation in Flix
// Using OpenGL 3.3 Core Profile with texture-based sprite rendering

import org.lwjgl.glfw.GLFW
import org.lwjgl.glfw.Callbacks
import org.lwjgl.glfw.GLFWKeyCallbackI
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL13
import org.lwjgl.opengl.GL15
import org.lwjgl.opengl.GL20
import org.lwjgl.opengl.GL30
import org.lwjgl.stb.STBImage
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil
import java.lang.Runtime
import java.lang.Thread

// ============================================================================
// Constants (from LearnOpenGL tutorial)
// ============================================================================

def screenWidth(): Int32 = 800
def screenHeight(): Int32 = 600
def playerWidth(): Float32 = 100.0f32
def playerHeight(): Float32 = 20.0f32
def playerVelocity(): Float32 = 500.0f32
def ballRadius(): Float32 = 12.5f32
def initialBallVelocityX(): Float32 = 100.0f32
def initialBallVelocityY(): Float32 = -350.0f32

// ============================================================================
// Type Definitions
// ============================================================================

type alias Vec2 = {x = Float32, y = Float32}
type alias Vec3 = {r = Float32, g = Float32, b = Float32}

type alias GameObject = {
    position = Vec2,
    size = Vec2,
    color = Vec3,
    isSolid = Bool,
    destroyed = Bool,
    textureId = Int32
}

type alias GameLevel = {
    bricks = List[GameObject]
}

type alias GameState = {
    levels = List[GameLevel],
    currentLevel = Int32,
    player = GameObject,
    ballPos = Vec2,
    ballRadius = Float32
}

// ============================================================================
// Shader Sources (LearnOpenGL sprite shader)
// ============================================================================

def vertexShaderSource(): String =
    "#version 330 core\n" +
    "layout (location = 0) in vec4 vertex;\n" +
    "out vec2 TexCoords;\n" +
    "uniform mat4 model;\n" +
    "uniform mat4 projection;\n" +
    "void main() {\n" +
    "    TexCoords = vertex.zw;\n" +
    "    gl_Position = projection * model * vec4(vertex.xy, 0.0, 1.0);\n" +
    "}\n"

def fragmentShaderSource(): String =
    "#version 330 core\n" +
    "in vec2 TexCoords;\n" +
    "out vec4 color;\n" +
    "uniform sampler2D image;\n" +
    "uniform vec3 spriteColor;\n" +
    "void main() {\n" +
    "    color = vec4(spriteColor, 1.0) * texture(image, TexCoords);\n" +
    "}\n"

// ============================================================================
// Shader Compilation
// ============================================================================

/// GLSLソースをコンパイルしてシェーダーIDを返す。失敗時は bug!
///
/// - `source`: GLSLシェーダーソースコード
/// - `shaderType`: `GL_VERTEX_SHADER` または `GL_FRAGMENT_SHADER`
/// - 戻り値: コンパイル済みシェーダーID
def compileShader(source: String, shaderType: Int32): Int32 \ IO =
    let shader = GL20.glCreateShader(shaderType);
    GL20.glShaderSource(shader, source);
    GL20.glCompileShader(shader);

    let success = GL20.glGetShaderi(shader, GL20.GL_COMPILE_STATUS);
    if (success == GL11.GL_FALSE) {
        let log = GL20.glGetShaderInfoLog(shader, 1024);
        println("Shader compilation error: ${log}");
        bug!("Shader failed")
    } else ();

    shader

/// 頂点・フラグメントシェーダーをリンクしてプログラムIDを返す
///
/// - `vertexSrc`: 頂点シェーダーのGLSLソース
/// - `fragmentSrc`: フラグメントシェーダーのGLSLソース
/// - 戻り値: リンク済みプログラムID
def createShaderProgram(vertexSrc: String, fragmentSrc: String): Int32 \ IO =
    let vs = compileShader(vertexSrc, GL20.GL_VERTEX_SHADER);
    let fs = compileShader(fragmentSrc, GL20.GL_FRAGMENT_SHADER);

    let program = GL20.glCreateProgram();
    GL20.glAttachShader(program, vs);
    GL20.glAttachShader(program, fs);
    GL20.glLinkProgram(program);

    let success = GL20.glGetProgrami(program, GL20.GL_LINK_STATUS);
    if (success == GL11.GL_FALSE) {
        let log = GL20.glGetProgramInfoLog(program, 1024);
        println("Shader linking error: ${log}");
        bug!("Linking failed")
    } else ();

    GL20.glDeleteShader(vs);
    GL20.glDeleteShader(fs);
    program

// ============================================================================
// Texture Loading (simplified white texture for now)
// ============================================================================

type alias Textures = {
    background = Int32,
    block = Int32,
    blockSolid = Int32,
    paddle = Int32,
    ball = Int32
}

/// STBImageで画像を読み込み、OpenGLテクスチャとして登録してIDを返す
///
/// - `path`: 画像ファイルのパス
/// - `alpha`: trueならRGBA、falseならRGBとして読み込む
/// - 戻り値: OpenGLテクスチャID
def loadTexture(path: String, alpha: Bool): Int32 \ IO =
    let stack = MemoryStack.stackPush();
    let w = stack.mallocInt(1);
    let h = stack.mallocInt(1);
    let c = stack.mallocInt(1);
    STBImage.stbi_set_flip_vertically_on_load(true);
    let channels = if (alpha) 4 else 3;
    let data = STBImage.stbi_load(path, w, h, c, channels);
    if (Object.isNull(data)) {
        discard MemoryStack.stackPop();
        bug!("Failed to load texture: ${path}")
    } else {
        let textureID = GL11.glGenTextures();
        GL11.glBindTexture(GL11.GL_TEXTURE_2D, textureID);
        let format = if (alpha) GL11.GL_RGBA else GL11.GL_RGB;
        GL11.glTexImage2D(GL11.GL_TEXTURE_2D, 0, format, w.get(0), h.get(0), 0, format, GL11.GL_UNSIGNED_BYTE, data);
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, GL11.GL_REPEAT);
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, GL11.GL_REPEAT);
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MIN_FILTER, GL11.GL_LINEAR);
        GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MAG_FILTER, GL11.GL_LINEAR);
        STBImage.stbi_image_free(data);
        discard MemoryStack.stackPop();
        textureID
    }

/// ゲームで使う全テクスチャ（背景・ブロック・パドル・ボール）を一括ロード
///
/// - 戻り値: 全テクスチャIDをまとめた `Textures` レコード
def loadTextures(): Textures \ IO =
    println("Loading textures...");
    let background = loadTexture("textures/background.jpg", false);
    println("  background.jpg loaded");
    let block = loadTexture("textures/block.png", true);
    println("  block.png loaded");
    let blockSolid = loadTexture("textures/block_solid.png", true);
    println("  block_solid.png loaded");
    let paddle = loadTexture("textures/paddle.png", true);
    println("  paddle.png loaded");
    let ball = loadTexture("textures/awesomeface.png", true);
    println("  awesomeface.png loaded");

    {
        background = background,
        block = block,
        blockSolid = blockSolid,
        paddle = paddle,
        ball = ball
    }

// ============================================================================
// Sprite Renderer
// ============================================================================

/// 2Dスプライト用の四角形メッシュ(VAO/VBO)を作成して返す
///
/// - 戻り値: `(VAO, VBO)` のタプル
def initSpriteRenderer(): (Int32, Int32) \ IO =
    let vao = GL30.glGenVertexArrays();
    let vbo = GL15.glGenBuffers();

    let vertices = Array#{
        0.0f32, 1.0f32, 0.0f32, 1.0f32,
        1.0f32, 0.0f32, 1.0f32, 0.0f32,
        0.0f32, 0.0f32, 0.0f32, 0.0f32,
        0.0f32, 1.0f32, 0.0f32, 1.0f32,
        1.0f32, 1.0f32, 1.0f32, 1.0f32,
        1.0f32, 0.0f32, 1.0f32, 0.0f32
    } @ Static;

    GL30.glBindVertexArray(vao);
    GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, vbo);
    GL15.glBufferData(GL15.GL_ARRAY_BUFFER, vertices, GL15.GL_STATIC_DRAW);

    GL20.glEnableVertexAttribArray(0);
    GL20.glVertexAttribPointer(0, 4, GL11.GL_FLOAT, false, 16, 0i64);

    GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, 0);
    GL30.glBindVertexArray(0);

    (vao, vbo)

/// 正射影行列を計算してシェーダーのuniformにセット
///
/// - `shader`: シェーダープログラムID
/// - `width`: 画面幅（ピクセル）
/// - `height`: 画面高さ（ピクセル）
def setProjectionMatrix(shader: Int32, width: Float32, height: Float32): Unit \ IO =
    GL20.glUseProgram(shader);

    // Orthographic projection matrix for LearnOpenGL Breakout
    // Maps (0,0) to top-left, (width,height) to bottom-right
    let left = 0.0f32;
    let right = width;
    let bottom = height;
    let top = 0.0f32;

    let proj = Array#{
        2.0f32 / (right - left), 0.0f32, 0.0f32, 0.0f32,
        0.0f32, 2.0f32 / (top - bottom), 0.0f32, 0.0f32,
        0.0f32, 0.0f32, -1.0f32, 0.0f32,
        -1.0f32 * (right + left) / (right - left), -1.0f32 * (top + bottom) / (top - bottom), 0.0f32, 1.0f32
    } @ Static;

    let loc = GL20.glGetUniformLocation(shader, "projection");
    GL20.glUniformMatrix4fv(loc, false, proj)

/// テクスチャ付きスプライトを指定位置・サイズ・色で描画
///
/// - `shader`: シェーダープログラムID
/// - `texture`: テクスチャID
/// - `vao`: 頂点配列オブジェクトID
/// - `pos`: 描画位置（左上座標）
/// - `size`: 描画サイズ（幅・高さ）
/// - `color`: スプライトに乗算するRGB色
def drawSprite(shader: Int32, texture: Int32, vao: Int32, pos: Vec2, size: Vec2, color: Vec3): Unit \ IO =
    GL20.glUseProgram(shader);

    let model = Array#{
        size#x, 0.0f32, 0.0f32, 0.0f32,
        0.0f32, size#y, 0.0f32, 0.0f32,
        0.0f32, 0.0f32, 1.0f32, 0.0f32,
        pos#x, pos#y, 0.0f32, 1.0f32
    } @ Static;

    let modelLoc = GL20.glGetUniformLocation(shader, "model");
    GL20.glUniformMatrix4fv(modelLoc, false, model);

    let colorLoc = GL20.glGetUniformLocation(shader, "spriteColor");
    GL20.glUniform3f(colorLoc, color#r, color#g, color#b);

    GL13.glActiveTexture(GL13.GL_TEXTURE0);
    GL11.glBindTexture(GL11.GL_TEXTURE_2D, texture);

    GL30.glBindVertexArray(vao);
    GL11.glDrawArrays(GL11.GL_TRIANGLES, 0, 6);
    GL30.glBindVertexArray(0)

// ============================================================================
// Level Data (from LearnOpenGL .lvl files)
// ============================================================================

/// .lvlファイルを読み込み、レベルデータをパースして GameLevel を返す
///
/// - `path`: .lvlファイルのパス
/// - `levelWidth`: レベル描画領域の幅（ピクセル）
/// - `levelHeight`: レベル描画領域の高さ（ピクセル）
/// - `textures`: テクスチャID群
/// - 戻り値: パース済み `GameLevel`
def loadLevel(path: String, levelWidth: Int32, levelHeight: Int32, textures: Textures): GameLevel \ IO =
    let result = Fs.FileRead.runWithIO(() -> Fs.FileRead.read(path));
    match result {
        case Ok(data) => parseLevel(data, levelWidth, levelHeight, textures)
        case Err(_) => bug!("Failed to load level: ${path}")
    }

/// レベルデータ文字列をパースし、ブリックのGameObjectリストに変換
///
/// - `data`: 改行区切りのレベルデータ文字列
/// - `levelWidth`: レベル描画領域の幅（ピクセル）
/// - `levelHeight`: レベル描画領域の高さ（ピクセル）
/// - `textures`: テクスチャID群
/// - 戻り値: ブリックを含む `GameLevel`
def parseLevel(data: String, levelWidth: Int32, levelHeight: Int32, textures: Textures): GameLevel =
    let lines = String.split({regex = "\n"}, data);
    let rows = List.map(parseLevelLine, lines);
    let validRows = List.filter(row -> not List.isEmpty(row), rows);
    let numRows = List.length(validRows);
    let numCols = List.head(validRows) |> Option.map(List.length) |> Option.getWithDefault(0);

    let unitWidth = if (numCols > 0) Int32.toFloat32(levelWidth) / Int32.toFloat32(numCols) else 0.0f32;
    let unitHeight = if (numRows > 0) Int32.toFloat32(levelHeight) / Int32.toFloat32(numRows) else 0.0f32;

    let bricks = buildBricks(validRows, unitWidth, unitHeight, textures);
    {bricks = bricks}

/// スペース区切りの1行をInt32のリストにパース
///
/// - `line`: スペース区切りのタイルタイプ文字列
/// - 戻り値: タイルタイプの `Int32` リスト
def parseLevelLine(line: String): List[Int32] =
    let trimmed = String.trim(line);
    if (String.isEmpty(trimmed)) Nil
    else
        trimmed
            |> String.split({regex = " "})
            |> List.filterMap(s -> {
                let t = String.trim(s);
                if (String.isEmpty(t)) None else Int32.fromString(t)
            })

/// パース済み全行からブリックのGameObjectリストを構築
///
/// - `rows`: パース済みの行リスト（各行はタイルタイプのリスト）
/// - `unitW`: 1ブリックあたりの幅
/// - `unitH`: 1ブリックあたりの高さ
/// - `textures`: テクスチャID群
/// - 戻り値: 全行分の `GameObject` リスト
def buildBricks(rows: List[List[Int32]], unitW: Float32, unitH: Float32, textures: Textures): List[GameObject] =
    rows |> (List.zipWithIndex >> List.flatMap(match (rowIdx, row) -> 
        buildBrickRow(row, rowIdx, unitW, unitH, textures)
    ))

/// 1行分のタイルタイプ列からブリックを生成。タイプ0はスキップ、1はsolid
///
/// - `row`: 1行分のタイルタイプリスト
/// - `rowIdx`: 行インデックス
/// - `unitW`: 1ブリックあたりの幅
/// - `unitH`: 1ブリックあたりの高さ
/// - `textures`: テクスチャID群
/// - 戻り値: 1行分の `GameObject` リスト
def buildBrickRow(row: List[Int32], rowIdx: Int32, unitW: Float32, unitH: Float32, textures: Textures): List[GameObject] =
    row |> (List.zipWithIndex >> List.filterMap(match (colIdx, tileType) ->
        if (tileType == 0) None
        else Some({
            position = {x = Int32.toFloat32(colIdx) * unitW, y = Int32.toFloat32(rowIdx) * unitH},
            size = {x = unitW, y = unitH},
            color = tileColor(tileType),
            isSolid = (tileType == 1),
            destroyed = false,
            textureId = if (tileType == 1) textures#blockSolid else textures#block
        })
    ))

/// タイルタイプ(1-5)をLearnOpenGLの配色に対応するRGB色に変換
///
/// - `tileType`: タイルタイプ（1=solid, 2=blue, 3=green, 4=yellow, 5=orange）
/// - 戻り値: 対応するRGB色
def tileColor(tileType: Int32): Vec3 =
    match tileType {
        case 1 => {r = 0.8f32, g = 0.8f32, b = 0.7f32}  // Solid - tan/gray
        case 2 => {r = 0.2f32, g = 0.6f32, b = 1.0f32}  // Blue
        case 3 => {r = 0.0f32, g = 0.7f32, b = 0.0f32}  // Green
        case 4 => {r = 0.8f32, g = 0.8f32, b = 0.4f32}  // Yellow
        case 5 => {r = 1.0f32, g = 0.5f32, b = 0.0f32}  // Orange
        case _ => {r = 1.0f32, g = 1.0f32, b = 1.0f32}  // White
    }

// ============================================================================
// Game Initialization
// ============================================================================

/// 4レベル・プレイヤー・ボールの初期状態を構築して GameState を返す
///
/// - `width`: 画面幅（ピクセル）
/// - `height`: 画面高さ（ピクセル）
/// - `textures`: テクスチャID群
/// - 戻り値: 初期化済み `GameState`
def initGame(width: Int32, height: Int32, textures: Textures): GameState \ IO =
    let level1 = loadLevel("levels/one.lvl", width, height / 2, textures);
    let level2 = loadLevel("levels/two.lvl", width, height / 2, textures);
    let level3 = loadLevel("levels/three.lvl", width, height / 2, textures);
    let level4 = loadLevel("levels/four.lvl", width, height / 2, textures);

    let playerX = Int32.toFloat32(width) / 2.0f32 - playerWidth() / 2.0f32;
    let playerY = Int32.toFloat32(height) - playerHeight();

    let player = {
        position = {x = playerX, y = playerY},
        size = {x = playerWidth(), y = playerHeight()},
        color = {r = 1.0f32, g = 1.0f32, b = 1.0f32},
        isSolid = true,
        destroyed = false,
        textureId = textures#paddle
    };

    let ballX = playerX + playerWidth() / 2.0f32 - ballRadius();
    let ballY = playerY - ballRadius() * 2.0f32;

    {
        levels = level1 :: level2 :: level3 :: level4 :: Nil,
        currentLevel = 0,
        player = player,
        ballPos = {x = ballX, y = ballY},
        ballRadius = ballRadius()
    }

// ============================================================================
// Rendering (LearnOpenGL style)
// ============================================================================

/// 背景・ブリック・パドル・ボールを1フレーム分描画
///
/// - `state`: 現在のゲーム状態
/// - `textures`: テクスチャID群
/// - `shader`: シェーダープログラムID
/// - `vao`: 頂点配列オブジェクトID
def render(state: GameState, textures: Textures, shader: Int32, vao: Int32): Unit \ IO =
    GL11.glClearColor(0.0f32, 0.0f32, 0.0f32, 1.0f32);
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT);

    // Draw background
    let bgPos = {x = 0.0f32, y = 0.0f32};
    let bgSize = {x = Int32.toFloat32(screenWidth()), y = Int32.toFloat32(screenHeight())};
    drawSprite(shader, textures#background, vao, bgPos, bgSize, {r = 1.0f32, g = 1.0f32, b = 1.0f32});

    // Draw level bricks
    let currentLevel = List.nth(state#currentLevel, state#levels);
    match currentLevel {
        case Some(level) => drawBricks(level#bricks, shader, vao)
        case None => ()
    };

    // Draw player paddle
    drawSprite(shader, textures#paddle, vao, state#player#position, state#player#size, state#player#color);

    // Draw ball
    let ballSize = {x = state#ballRadius * 2.0f32, y = state#ballRadius * 2.0f32};
    drawSprite(shader, textures#ball, vao, state#ballPos, ballSize, {r = 1.0f32, g = 1.0f32, b = 1.0f32})

/// ブリックリストを走査し、destroyedでないものを描画
///
/// - `bricks`: ブリックの `GameObject` リスト
/// - `shader`: シェーダープログラムID
/// - `vao`: 頂点配列オブジェクトID
def drawBricks(bricks: List[GameObject], shader: Int32, vao: Int32): Unit \ IO =
    List.forEach(brick -> {
        if (not brick#destroyed) {
            drawSprite(shader, brick#textureId, vao, brick#position, brick#size, brick#color)
        } else ()
    }, bricks)

// ============================================================================
// Main Entry Point
// ============================================================================

/// GLFW/OpenGL初期化→テクスチャロード→ゲームループ開始→終了処理
def main(): Unit \ IO =
    println("Breakout - LearnOpenGL Recreation");

    if (not GLFW.glfwInit()) {
        println("GLFW init failed");
        bug!("GLFW failed")
    } else ();

    // OpenGL 3.3 Core (LearnOpenGL standard)
    GLFW.glfwWindowHint(GLFW.GLFW_CONTEXT_VERSION_MAJOR, 3);
    GLFW.glfwWindowHint(GLFW.GLFW_CONTEXT_VERSION_MINOR, 3);
    GLFW.glfwWindowHint(GLFW.GLFW_OPENGL_PROFILE, GLFW.GLFW_OPENGL_CORE_PROFILE);
    GLFW.glfwWindowHint(GLFW.GLFW_OPENGL_FORWARD_COMPAT, GL11.GL_TRUE);
    GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, GL11.GL_FALSE);

    let window = GLFW.glfwCreateWindow(screenWidth(), screenHeight(), "Breakout", MemoryUtil.NULL, MemoryUtil.NULL);
    GLFW.glfwMakeContextCurrent(window);
    GLFW.glfwSwapInterval(1);
    discard GL.createCapabilities();

    // Retina display support (macOS typically uses 2x scale)
    let retinaScale = 2;
    let fbWidth = screenWidth() * retinaScale;
    let fbHeight = screenHeight() * retinaScale;
    println("Using ${retinaScale}x scale for Retina display: ${fbWidth}x${fbHeight}");

    GL11.glViewport(0, 0, fbWidth, fbHeight);
    GL11.glEnable(GL11.GL_BLEND);
    GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);

    let shader = createShaderProgram(vertexShaderSource(), fragmentShaderSource());
    setProjectionMatrix(shader, Int32.toFloat32(screenWidth()), Int32.toFloat32(screenHeight()));

    let (vao, _vbo) = initSpriteRenderer();
    let textures = loadTextures();

    println("Loading levels...");
    let state = initGame(screenWidth(), screenHeight(), textures);
    println("Game ready! Press ESC or close window to exit.");


    gameLoop(window, state, textures, shader, vao);

    Callbacks.glfwFreeCallbacks(window);
    GLFW.glfwDestroyWindow(window);
    GLFW.glfwTerminate();

    println("Game ended.")

/// ESC/ウィンドウ閉じるまで描画とイベント処理を繰り返す末尾再帰ループ
///
/// - `window`: GLFWウィンドウハンドル
/// - `state`: 現在のゲーム状態
/// - `textures`: テクスチャID群
/// - `shader`: シェーダープログラムID
/// - `vao`: 頂点配列オブジェクトID
@Tailrec
def gameLoop(window: Int64, state: GameState, textures: Textures, shader: Int32, vao: Int32): Unit \ IO =
    GLFW.glfwPollEvents();

    // Check ESC key (256 = GLFW_KEY_ESCAPE)
    let escPressed = GLFW.glfwGetKey(window, 256) == 1;  // 1 = GLFW_PRESS

    let windowClosing = GLFW.glfwWindowShouldClose(window);

    if (escPressed or windowClosing) {
        println("Closing game...")
    } else {
        render(state, textures, shader, vao);
        GLFW.glfwSwapBuffers(window);
        gameLoop(window, state, textures, shader, vao)
    }
