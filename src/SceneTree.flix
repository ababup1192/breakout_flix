// Scene tree for declarative rendering

mod SceneTree {
    pub type alias Transform2D = {position = Vec2D.Vec2, scale = Vec2D.Vec2}

    pub enum Node {
        case Group(String, Transform2D, List[Node])
        case Sprite(String, Transform2D, SpriteData, List[Node])
        case CollisionShape(String, CollisionShapeData)
    }

    pub type alias SpriteData = {
        size = Vec2D.Vec2,
        color = Game.Vec3,
        texture = String,
        visible = Bool
    }

    pub enum CollisionShapeData {
        case RectShape(Vec2D.Vec2)
        case CircleShape(Float32)
    }

    pub def identityTransform(): Transform2D =
        {position = Vec2D.zero(), scale = Vec2D.one()}

    /// Collect all visible sprites with their global positions
    pub def collectDrawables(node: Node, parentPos: Vec2D.Vec2): List[(SpriteData, Vec2D.Vec2)] =
        match node {
            case Node.Group(_, transform, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectDrawables(child, globalPos), children)
            case Node.Sprite(_, transform, spriteData, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                let self = if (spriteData#visible) (spriteData, globalPos) :: Nil else Nil;
                let childDrawables = List.flatMap(child -> collectDrawables(child, globalPos), children);
                List.append(self, childDrawables)
            case Node.CollisionShape(_, _) => Nil
        }
}
