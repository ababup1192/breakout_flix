// Scene tree — Godot-inspired node system with built-in physics engine

mod SceneTree {
    pub type alias Transform2D = {position = Vec2D.Vec2, scale = Vec2D.Vec2}

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

    pub type alias KinematicBodyData = { velocity = Vec2D.Vec2 }
    pub type alias RigidBodyData = { velocity = Vec2D.Vec2, stuck = Bool }
    pub type alias StaticBodyData = { isSolid = Bool, destroyed = Bool }

    pub enum NodeKind {
        case Plain
        case Sprite(SpriteData)
        case Shape(CollisionShapeData)
        case Kinematic(KinematicBodyData)
        case Rigid(RigidBodyData)
        case Fixed(StaticBodyData)
        case Area
    }

    pub enum Node {
        case MkNode(String, Transform2D, NodeKind, List[Node])
    }

    pub enum Direction with Eq, ToString {
        case Up
        case Right
        case Down
        case Left
    }

    pub type alias BodyInfo = {
        name = String,
        position = Vec2D.Vec2,
        shape = CollisionShapeData,
        isSolid = Bool,
        destroyed = Bool
    }

    pub type alias PhysicsConfig = {
        screenSize = Vec2D.Vec2,
        anchorPos = Vec2D.Vec2,
        anchorWidth = Float32
    }

    // ========================================================================
    // Constructors (Godot vocabulary)
    // ========================================================================

    pub def node2D(name: String, transform: Transform2D, children: List[Node]): Node =
        Node.MkNode(name, transform, NodeKind.Plain, children)

    pub def sprite2D(name: String, transform: Transform2D, sd: SpriteData): Node =
        Node.MkNode(name, transform, NodeKind.Sprite(sd), Nil)

    pub def collisionShape2D(name: String, shape: CollisionShapeData): Node =
        Node.MkNode(name, identityTransform(), NodeKind.Shape(shape), Nil)

    pub def kinematicBody2D(name: String, transform: Transform2D, bd: KinematicBodyData, children: List[Node]): Node =
        Node.MkNode(name, transform, NodeKind.Kinematic(bd), children)

    pub def rigidBody2D(name: String, transform: Transform2D, bd: RigidBodyData, children: List[Node]): Node =
        Node.MkNode(name, transform, NodeKind.Rigid(bd), children)

    pub def staticBody2D(name: String, transform: Transform2D, bd: StaticBodyData, children: List[Node]): Node =
        Node.MkNode(name, transform, NodeKind.Fixed(bd), children)

    pub def area2D(name: String, transform: Transform2D, children: List[Node]): Node =
        Node.MkNode(name, transform, NodeKind.Area, children)

    // ========================================================================
    // Basic Helpers
    // ========================================================================

    pub def identityTransform(): Transform2D =
        {position = Vec2D.zero(), scale = Vec2D.one()}

    pub def nodeName(node: Node): String =
        let Node.MkNode(n, _, _, _) = node; n

    pub def nodeChildren(node: Node): List[Node] =
        let Node.MkNode(_, _, _, ch) = node; ch

    pub def nodeTransform(node: Node): Transform2D =
        let Node.MkNode(_, t, _, _) = node; t

    pub def nodeKind(node: Node): NodeKind =
        let Node.MkNode(_, _, k, _) = node; k

    // ========================================================================
    // Tree Search & Modification
    // ========================================================================

    pub def findNode(name: String, node: Node): Option[Node] =
        if (nodeName(node) == name) Some(node)
        else List.findMap(child -> findNode(name, child), nodeChildren(node))

    pub def mapNode(name: String, f: Node -> Node, node: Node): Node =
        if (nodeName(node) == name) f(node)
        else mapChildren(child -> mapNode(name, f, child), node)

    def mapChildren(f: Node -> Node, node: Node): Node =
        let Node.MkNode(n, t, k, children) = node;
        Node.MkNode(n, t, k, List.map(f, children))

    // ========================================================================
    // Node Update Helpers
    // ========================================================================

    pub def updateKinematicBody(name: String, f: KinematicBodyData -> KinematicBodyData, root: Node): Node =
        mapNode(name, node -> match nodeKind(node) {
            case NodeKind.Kinematic(bd) =>
                let Node.MkNode(n, t, _, ch) = node;
                Node.MkNode(n, t, NodeKind.Kinematic(f(bd)), ch)
            case _ => node
        }, root)

    pub def updateRigidBody(name: String, f: RigidBodyData -> RigidBodyData, root: Node): Node =
        mapNode(name, node -> match nodeKind(node) {
            case NodeKind.Rigid(bd) =>
                let Node.MkNode(n, t, _, ch) = node;
                Node.MkNode(n, t, NodeKind.Rigid(f(bd)), ch)
            case _ => node
        }, root)

    pub def updateStaticBody(name: String, f: StaticBodyData -> StaticBodyData, root: Node): Node =
        mapNode(name, node -> match nodeKind(node) {
            case NodeKind.Fixed(bd) =>
                let Node.MkNode(n, t, _, ch) = node;
                Node.MkNode(n, t, NodeKind.Fixed(f(bd)), ch)
            case _ => node
        }, root)

    pub def setNodePosition(name: String, pos: Vec2D.Vec2, root: Node): Node =
        mapNode(name, node -> {
            let Node.MkNode(n, t, k, ch) = node;
            Node.MkNode(n, {position = pos | t}, k, ch)
        }, root)

    pub def rigidBodyData(node: Node): Option[RigidBodyData] =
        match nodeKind(node) {
            case NodeKind.Rigid(bd) => Some(bd)
            case _ => None
        }

    pub def kinematicBodyData(node: Node): Option[KinematicBodyData] =
        match nodeKind(node) {
            case NodeKind.Kinematic(bd) => Some(bd)
            case _ => None
        }

    pub def staticBodyData(node: Node): Option[StaticBodyData] =
        match nodeKind(node) {
            case NodeKind.Fixed(bd) => Some(bd)
            case _ => None
        }

    pub def modifyStaticBody(f: StaticBodyData -> StaticBodyData, node: Node): Node =
        match nodeKind(node) {
            case NodeKind.Fixed(bd) =>
                let Node.MkNode(n, t, _, ch) = node;
                Node.MkNode(n, t, NodeKind.Fixed(f(bd)), ch)
            case _ => node
        }

    pub def findRectShape(children: List[Node]): Option[Vec2D.Vec2] =
        List.findMap(child -> match nodeKind(child) {
            case NodeKind.Shape(CollisionShapeData.RectShape(size)) => Some(size)
            case _ => None
        }, children)

    pub def findCircleShape(children: List[Node]): Option[Float32] =
        List.findMap(child -> match nodeKind(child) {
            case NodeKind.Shape(CollisionShapeData.CircleShape(r)) => Some(r)
            case _ => None
        }, children)

    pub def globalPosition(name: String, node: Node): Option[Vec2D.Vec2] =
        globalPositionHelper(name, node, Vec2D.zero())

    def globalPositionHelper(name: String, node: Node, parentPos: Vec2D.Vec2): Option[Vec2D.Vec2] =
        let currentPos = Vec2D.add(parentPos, (nodeTransform(node))#position);
        if (nodeName(node) == name) Some(currentPos)
        else List.findMap(child -> globalPositionHelper(name, child, currentPos), nodeChildren(node))

    // ========================================================================
    // Rendering: collectDrawables
    // ========================================================================

    pub def collectDrawables(node: Node, parentPos: Vec2D.Vec2): List[(SpriteData, Vec2D.Vec2)] =
        let Node.MkNode(_, transform, kind, children) = node;
        let globalPos = Vec2D.add(parentPos, transform#position);
        match kind {
            case NodeKind.Sprite(sd) =>
                if (sd#visible) (sd, globalPos) :: Nil else Nil
            case NodeKind.Fixed(bd) =>
                if (bd#destroyed) Nil
                else List.flatMap(child -> collectDrawables(child, globalPos), children)
            case NodeKind.Shape(_) => Nil
            case _ =>
                List.flatMap(child -> collectDrawables(child, globalPos), children)
        }

    // ========================================================================
    // Engine: Direction helpers
    // ========================================================================

    pub def vectorDirection(target: Vec2D.Vec2): Direction =
        let dotUp    = target#y;
        let dotRight = target#x;
        let dotDown  = -1.0f32 * target#y;
        let dotLeft  = -1.0f32 * target#x;
        let (d0, v0) = (Direction.Up, dotUp);
        let (d1, v1) = if (dotRight > v0) (Direction.Right, dotRight) else (d0, v0);
        let (d2, v2) = if (dotDown > v1) (Direction.Down, dotDown) else (d1, v1);
        let (d3, _)  = if (dotLeft > v2) (Direction.Left, dotLeft) else (d2, v2);
        d3

    // ========================================================================
    // Engine: Collision Detection
    // ========================================================================

    pub def checkCircleRect(circlePos: Vec2D.Vec2, radius: Float32,
        rectPos: Vec2D.Vec2, rectSize: Vec2D.Vec2): Option[(Direction, Vec2D.Vec2)] =
        let ballCenter = Vec2D.add(circlePos, {x = radius, y = radius});
        let halfExtents = Vec2D.div(rectSize, 2.0f32);
        let aabbCenter = Vec2D.add(rectPos, halfExtents);
        let diff = Vec2D.sub(ballCenter, aabbCenter);
        let clamped = Vec2D.clampSymmetric(diff, halfExtents);
        let closest = Vec2D.add(aabbCenter, clamped);
        let difference = Vec2D.sub(closest, ballCenter);
        let distSq = Vec2D.lengthSquared(difference);
        if (distSq < radius * radius)
            Some((vectorDirection(difference), difference))
        else
            None

    pub def resolveCollision(pos: Vec2D.Vec2, vel: Vec2D.Vec2, radius: Float32,
        dir: Direction, diff: Vec2D.Vec2): (Vec2D.Vec2, Vec2D.Vec2) =
        let horizontal = (dir == Direction.Left) or (dir == Direction.Right);
        if (horizontal) {
            let penetration = radius - Float32.abs(diff#x);
            let newX = if (dir == Direction.Left) pos#x + penetration else pos#x - penetration;
            ({x = newX, y = pos#y}, {x = -1.0f32 * vel#x, y = vel#y})
        } else {
            let penetration = radius - Float32.abs(diff#y);
            let newY = if (dir == Direction.Up) pos#y - penetration else pos#y + penetration;
            ({x = pos#x, y = newY}, {x = vel#x, y = -1.0f32 * vel#y})
        }

    // ========================================================================
    // Engine: collectBodies
    // ========================================================================

    pub def collectBodies(node: Node): List[BodyInfo] =
        collectBodiesHelper(node, Vec2D.zero())

    def extractShapes(name: String, globalPos: Vec2D.Vec2, isSolid: Bool, destroyed: Bool, children: List[Node]): List[BodyInfo] =
        List.filterMap(child -> match nodeKind(child) {
            case NodeKind.Shape(shape) => Some({
                name = name,
                position = globalPos,
                shape = shape,
                isSolid = isSolid,
                destroyed = destroyed
            })
            case _ => None
        }, children)

    def collectBodiesHelper(node: Node, parentPos: Vec2D.Vec2): List[BodyInfo] =
        let Node.MkNode(name, transform, kind, children) = node;
        let globalPos = Vec2D.add(parentPos, transform#position);
        match kind {
            case NodeKind.Fixed(bd) =>
                if (bd#destroyed) Nil
                else {
                    let shapes = extractShapes(name, globalPos, bd#isSolid, bd#destroyed, children);
                    let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                    List.append(shapes, childBodies)
                }
            case NodeKind.Kinematic(_) =>
                let shapes = extractShapes(name, globalPos, true, false, children);
                let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                List.append(shapes, childBodies)
            case NodeKind.Area =>
                let shapes = extractShapes(name, globalPos, false, false, children);
                let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                List.append(shapes, childBodies)
            case NodeKind.Plain =>
                List.flatMap(child -> collectBodiesHelper(child, globalPos), children)
            case NodeKind.Rigid(_) =>
                List.flatMap(child -> collectBodiesHelper(child, globalPos), children)
            case NodeKind.Sprite(_) => Nil
            case NodeKind.Shape(_) => Nil
        }

    // ========================================================================
    // Engine: physicsStep
    // ========================================================================

    pub def physicsStep(root: Node, dt: Float32, config: PhysicsConfig): Node =
        physicsStepNode(root, dt, config)

    def physicsStepNode(node: Node, dt: Float32, config: PhysicsConfig): Node =
        let Node.MkNode(name, transform, kind, children) = node;
        match kind {
            case NodeKind.Kinematic(bodyData) =>
                let newX = transform#position#x + bodyData#velocity#x * dt;
                let clampedX = Float32.max(0.0f32, Float32.min(config#screenSize#x - config#anchorWidth, newX));
                let newTransform = {position = {x = clampedX, y = transform#position#y} | transform};
                Node.MkNode(name, newTransform, kind, children)
            case NodeKind.Rigid(bodyData) =>
                if (bodyData#stuck) {
                    let ballRadius = findCircleShape(children) |> Option.getWithDefault(12.5f32);
                    let bx = config#anchorPos#x + config#anchorWidth / 2.0f32 - ballRadius;
                    let by = config#anchorPos#y - ballRadius * 2.0f32;
                    let newTransform = {position = {x = bx, y = by} | transform};
                    Node.MkNode(name, newTransform, kind, children)
                } else {
                    let ballSize = findCircleShape(children) |> Option.map(r -> r * 2.0f32) |> Option.getWithDefault(25.0f32);
                    let newX = transform#position#x + bodyData#velocity#x * dt;
                    let newY = transform#position#y + bodyData#velocity#y * dt;
                    let vx0 = bodyData#velocity#x;
                    let vy0 = bodyData#velocity#y;
                    let (nx1, vx1) = if (newX <= 0.0f32) (0.0f32, Float32.abs(vx0)) else (newX, vx0);
                    let (nx2, vx2) = if (nx1 + ballSize >= config#screenSize#x) (config#screenSize#x - ballSize, -1.0f32 * Float32.abs(vx1)) else (nx1, vx1);
                    let (ny1, vy1) = if (newY <= 0.0f32) (0.0f32, Float32.abs(vy0)) else (newY, vy0);
                    let newTransform = {position = {x = nx2, y = ny1} | transform};
                    let newBodyData = {velocity = {x = vx2, y = vy1} | bodyData};
                    Node.MkNode(name, newTransform, NodeKind.Rigid(newBodyData), children)
                }
            case NodeKind.Plain =>
                Node.MkNode(name, transform, kind, List.map(child -> physicsStepNode(child, dt, config), children))
            case _ => node
        }
}
