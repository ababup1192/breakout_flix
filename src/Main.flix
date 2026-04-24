// Breakout - Faithful LearnOpenGL Recreation in Flix

def main(): Unit \ IO =
    println("Breakout - LearnOpenGL Recreation");
    discard run {
        Game.startGame()
    } with LwjglLayer.withLwjgl;
    println("Game ended.")
