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

    pub enum Node {
        case Node2D(String, Transform2D, List[Node])
        case Sprite2D(String, Transform2D, SpriteData)
        case CollisionShape2D(String, CollisionShapeData)
        case KinematicBody2D(String, Transform2D, KinematicBodyData, List[Node])
        case StaticBody2D(String, Transform2D, StaticBodyData, List[Node])
        case RigidBody2D(String, Transform2D, RigidBodyData, List[Node])
        case Area2D(String, Transform2D, List[Node])
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

    // ========================================================================
    // Basic Helpers
    // ========================================================================

    pub def identityTransform(): Transform2D =
        {position = Vec2D.zero(), scale = Vec2D.one()}

    pub def nodeName(node: Node): String =
        match node {
            case Node.Node2D(name, _, _) => name
            case Node.Sprite2D(name, _, _) => name
            case Node.CollisionShape2D(name, _) => name
            case Node.KinematicBody2D(name, _, _, _) => name
            case Node.StaticBody2D(name, _, _, _) => name
            case Node.RigidBody2D(name, _, _, _) => name
            case Node.Area2D(name, _, _) => name
        }

    pub def nodeChildren(node: Node): List[Node] =
        match node {
            case Node.Node2D(_, _, children) => children
            case Node.Sprite2D(_, _, _) => Nil
            case Node.CollisionShape2D(_, _) => Nil
            case Node.KinematicBody2D(_, _, _, children) => children
            case Node.StaticBody2D(_, _, _, children) => children
            case Node.RigidBody2D(_, _, _, children) => children
            case Node.Area2D(_, _, children) => children
        }

    pub def nodeTransform(node: Node): Transform2D =
        match node {
            case Node.Node2D(_, t, _) => t
            case Node.Sprite2D(_, t, _) => t
            case Node.CollisionShape2D(_, _) => identityTransform()
            case Node.KinematicBody2D(_, t, _, _) => t
            case Node.StaticBody2D(_, t, _, _) => t
            case Node.RigidBody2D(_, t, _, _) => t
            case Node.Area2D(_, t, _) => t
        }

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
        match node {
            case Node.Node2D(n, t, children) =>
                Node.Node2D(n, t, List.map(f, children))
            case Node.Sprite2D(_, _, _) => node
            case Node.CollisionShape2D(_, _) => node
            case Node.KinematicBody2D(n, t, d, children) =>
                Node.KinematicBody2D(n, t, d, List.map(f, children))
            case Node.StaticBody2D(n, t, d, children) =>
                Node.StaticBody2D(n, t, d, List.map(f, children))
            case Node.RigidBody2D(n, t, d, children) =>
                Node.RigidBody2D(n, t, d, List.map(f, children))
            case Node.Area2D(n, t, children) =>
                Node.Area2D(n, t, List.map(f, children))
        }

    // ========================================================================
    // Node Update Helpers
    // ========================================================================

    pub def updateKinematicBody(name: String, f: KinematicBodyData -> KinematicBodyData, root: Node): Node =
        mapNode(name, node -> match node {
            case Node.KinematicBody2D(n, t, bd, children) =>
                Node.KinematicBody2D(n, t, f(bd), children)
            case other => other
        }, root)

    pub def updateRigidBody(name: String, f: RigidBodyData -> RigidBodyData, root: Node): Node =
        mapNode(name, node -> match node {
            case Node.RigidBody2D(n, t, bd, children) =>
                Node.RigidBody2D(n, t, f(bd), children)
            case other => other
        }, root)

    pub def updateStaticBody(name: String, f: StaticBodyData -> StaticBodyData, root: Node): Node =
        mapNode(name, node -> match node {
            case Node.StaticBody2D(n, t, bd, children) =>
                Node.StaticBody2D(n, t, f(bd), children)
            case other => other
        }, root)

    pub def setNodePosition(name: String, pos: Vec2D.Vec2, root: Node): Node =
        mapNode(name, node -> match node {
            case Node.Node2D(n, t, children) =>
                Node.Node2D(n, {position = pos | t}, children)
            case Node.Sprite2D(n, t, sd) =>
                Node.Sprite2D(n, {position = pos | t}, sd)
            case Node.KinematicBody2D(n, t, bd, children) =>
                Node.KinematicBody2D(n, {position = pos | t}, bd, children)
            case Node.StaticBody2D(n, t, bd, children) =>
                Node.StaticBody2D(n, {position = pos | t}, bd, children)
            case Node.RigidBody2D(n, t, bd, children) =>
                Node.RigidBody2D(n, {position = pos | t}, bd, children)
            case Node.Area2D(n, t, children) =>
                Node.Area2D(n, {position = pos | t}, children)
            case Node.CollisionShape2D(_, _) => node
        }, root)

    pub def rigidBodyData(node: Node): Option[RigidBodyData] =
        match node {
            case Node.RigidBody2D(_, _, bd, _) => Some(bd)
            case _ => None
        }

    pub def kinematicBodyData(node: Node): Option[KinematicBodyData] =
        match node {
            case Node.KinematicBody2D(_, _, bd, _) => Some(bd)
            case _ => None
        }

    pub def findRectShape(children: List[Node]): Option[Vec2D.Vec2] =
        List.findMap(child -> match child {
            case Node.CollisionShape2D(_, CollisionShapeData.RectShape(size)) => Some(size)
            case _ => None
        }, children)

    pub def findCircleShape(children: List[Node]): Option[Float32] =
        List.findMap(child -> match child {
            case Node.CollisionShape2D(_, CollisionShapeData.CircleShape(r)) => Some(r)
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
        match node {
            case Node.Node2D(_, transform, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectDrawables(child, globalPos), children)
            case Node.Sprite2D(_, transform, spriteData) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                if (spriteData#visible) (spriteData, globalPos) :: Nil else Nil
            case Node.CollisionShape2D(_, _) => Nil
            case Node.KinematicBody2D(_, transform, _, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectDrawables(child, globalPos), children)
            case Node.StaticBody2D(_, transform, bodyData, children) =>
                if (bodyData#destroyed) Nil
                else {
                    let globalPos = Vec2D.add(parentPos, transform#position);
                    List.flatMap(child -> collectDrawables(child, globalPos), children)
                }
            case Node.RigidBody2D(_, transform, _, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectDrawables(child, globalPos), children)
            case Node.Area2D(_, transform, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
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

    def collectBodiesHelper(node: Node, parentPos: Vec2D.Vec2): List[BodyInfo] =
        match node {
            case Node.StaticBody2D(name, transform, bodyData, children) =>
                if (bodyData#destroyed) Nil
                else {
                    let globalPos = Vec2D.add(parentPos, transform#position);
                    let shapes = List.filterMap(child -> match child {
                        case Node.CollisionShape2D(_, shape) => Some({
                            name = name,
                            position = globalPos,
                            shape = shape,
                            isSolid = bodyData#isSolid,
                            destroyed = bodyData#destroyed
                        })
                        case _ => None
                    }, children);
                    let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                    List.append(shapes, childBodies)
                }
            case Node.KinematicBody2D(name, transform, _, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                let shapes = List.filterMap(child -> match child {
                    case Node.CollisionShape2D(_, shape) => Some({
                        name = name,
                        position = globalPos,
                        shape = shape,
                        isSolid = true,
                        destroyed = false
                    })
                    case _ => None
                }, children);
                let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                List.append(shapes, childBodies)
            case Node.Area2D(name, transform, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                let shapes = List.filterMap(child -> match child {
                    case Node.CollisionShape2D(_, shape) => Some({
                        name = name,
                        position = globalPos,
                        shape = shape,
                        isSolid = false,
                        destroyed = false
                    })
                    case _ => None
                }, children);
                let childBodies = List.flatMap(child -> collectBodiesHelper(child, globalPos), children);
                List.append(shapes, childBodies)
            case Node.Node2D(_, transform, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectBodiesHelper(child, globalPos), children)
            case Node.RigidBody2D(_, transform, _, children) =>
                let globalPos = Vec2D.add(parentPos, transform#position);
                List.flatMap(child -> collectBodiesHelper(child, globalPos), children)
            case Node.Sprite2D(_, _, _) => Nil
            case Node.CollisionShape2D(_, _) => Nil
        }

    // ========================================================================
    // Engine: physicsStep
    // ========================================================================

    pub def physicsStep(root: Node, dt: Float32, screenSize: Vec2D.Vec2): Node =
        let paddlePos = findNode("Paddle", root) |> Option.map(n -> (nodeTransform(n))#position) |> Option.getWithDefault(Vec2D.zero());
        let paddleWidth = findNode("Paddle", root)
            |> Option.flatMap(n -> List.findMap(child -> match child {
                case Node.CollisionShape2D(_, CollisionShapeData.RectShape(size)) => Some(size#x)
                case _ => None
            }, nodeChildren(n)))
            |> Option.getWithDefault(100.0f32);
        physicsStepNode(root, dt, screenSize, paddlePos, paddleWidth)

    def physicsStepNode(node: Node, dt: Float32, screenSize: Vec2D.Vec2, paddlePos: Vec2D.Vec2, paddleWidth: Float32): Node =
        match node {
            case Node.KinematicBody2D(name, transform, bodyData, children) =>
                let newX = transform#position#x + bodyData#velocity#x * dt;
                let clampedX = Float32.max(0.0f32, Float32.min(screenSize#x - paddleWidth, newX));
                let newTransform = {position = {x = clampedX, y = transform#position#y} | transform};
                Node.KinematicBody2D(name, newTransform, bodyData, children)
            case Node.RigidBody2D(name, transform, bodyData, children) =>
                if (bodyData#stuck) {
                    let ballRadius = List.findMap(child -> match child {
                        case Node.CollisionShape2D(_, CollisionShapeData.CircleShape(r)) => Some(r)
                        case _ => None
                    }, children) |> Option.getWithDefault(12.5f32);
                    let bx = paddlePos#x + paddleWidth / 2.0f32 - ballRadius;
                    let by = paddlePos#y - ballRadius * 2.0f32;
                    let newTransform = {position = {x = bx, y = by} | transform};
                    Node.RigidBody2D(name, newTransform, bodyData, children)
                } else {
                    let ballSize = List.findMap(child -> match child {
                        case Node.CollisionShape2D(_, CollisionShapeData.CircleShape(r)) => Some(r * 2.0f32)
                        case _ => None
                    }, children) |> Option.getWithDefault(25.0f32);
                    let newX = transform#position#x + bodyData#velocity#x * dt;
                    let newY = transform#position#y + bodyData#velocity#y * dt;
                    let vx0 = bodyData#velocity#x;
                    let vy0 = bodyData#velocity#y;
                    let (nx1, vx1) = if (newX <= 0.0f32) (0.0f32, Float32.abs(vx0)) else (newX, vx0);
                    let (nx2, vx2) = if (nx1 + ballSize >= screenSize#x) (screenSize#x - ballSize, -1.0f32 * Float32.abs(vx1)) else (nx1, vx1);
                    let (ny1, vy1) = if (newY <= 0.0f32) (0.0f32, Float32.abs(vy0)) else (newY, vy0);
                    let newTransform = {position = {x = nx2, y = ny1} | transform};
                    let newBodyData = {velocity = {x = vx2, y = vy1} | bodyData};
                    Node.RigidBody2D(name, newTransform, newBodyData, children)
                }
            case Node.Node2D(name, transform, children) =>
                Node.Node2D(name, transform, List.map(child -> physicsStepNode(child, dt, screenSize, paddlePos, paddleWidth), children))
            case Node.StaticBody2D(_, _, _, _) => node
            case Node.Sprite2D(_, _, _) => node
            case Node.CollisionShape2D(_, _) => node
            case Node.Area2D(_, _, _) => node
        }
}
