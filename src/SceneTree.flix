// Scene tree for declarative rendering

mod SceneTree {
    pub type alias Transform2D = {position = Game.Vec2, scale = Game.Vec2}

    pub enum Node {
        case Group(String, Transform2D, List[Node])
        case Sprite(String, Transform2D, SpriteData, List[Node])
        case CollisionShape(String, CollisionShapeData)
    }

    pub type alias SpriteData = {
        size = Game.Vec2,
        color = Game.Vec3,
        texture = String,
        visible = Bool
    }

    pub enum CollisionShapeData {
        case RectShape(Game.Vec2)
        case CircleShape(Float32)
    }

    pub def identityTransform(): Transform2D =
        {position = {x = 0.0f32, y = 0.0f32}, scale = {x = 1.0f32, y = 1.0f32}}

    /// Collect all visible sprites with their global positions
    pub def collectDrawables(node: Node, parentPos: Game.Vec2): List[(SpriteData, Game.Vec2)] =
        match node {
            case Node.Group(_, transform, children) =>
                let globalPos = {x = parentPos#x + transform#position#x, y = parentPos#y + transform#position#y};
                List.flatMap(child -> collectDrawables(child, globalPos), children)
            case Node.Sprite(_, transform, spriteData, children) =>
                let globalPos = {x = parentPos#x + transform#position#x, y = parentPos#y + transform#position#y};
                let self = if (spriteData#visible) (spriteData, globalPos) :: Nil else Nil;
                let childDrawables = List.flatMap(child -> collectDrawables(child, globalPos), children);
                List.append(self, childDrawables)
            case Node.CollisionShape(_, _) => Nil
        }
}
