// Writer monad for accumulating GameEvents alongside state transformations

mod GameAction {
    use Game.GameEvent
    use GameAction.GameAction

    pub enum GameAction[a](a, List[GameEvent])

    pub def pure(x: a): GameAction[a] = GameAction(x, Nil)

    pub def of(x: a, events: List[GameEvent]): GameAction[a] =
        GameAction(x, events)

    pub def unpack(action: GameAction[a]): (a, List[GameEvent]) =
        let GameAction(a, events) = action;
        (a, events)

    pub def emit(event: GameEvent): GameAction[Unit] =
        GameAction((), event :: Nil)

    pub def tell(events: List[GameEvent]): GameAction[Unit] =
        GameAction((), events)

    instance Functor[GameAction] {
        pub def map(f: a -> b \ ef, x: GameAction[a]): GameAction[b] \ ef =
            let GameAction(a, events) = x;
            GameAction(f(a), events)
    }

    instance Applicative[GameAction] {
        pub def point(x: a): GameAction[a] = GameAction(x, Nil)
        pub def ap(f: GameAction[a -> b \ ef], x: GameAction[a]): GameAction[b] \ ef =
            let GameAction(g, e1) = f;
            let GameAction(a, e2) = x;
            GameAction(g(a), List.append(e1, e2))
    }

    instance Monad[GameAction] {
        pub def flatMap(f: a -> GameAction[b] \ ef, x: GameAction[a]): GameAction[b] \ ef =
            let GameAction(a, e1) = x;
            let GameAction(b, e2) = f(a);
            GameAction(b, List.append(e1, e2))
    }

    pub def foldSignals(f: (a, s) -> GameAction[s],
                        state: s,
                        signals: List[a]): GameAction[s] =
        List.foldLeft((acc, sig) -> forM (
            s <- acc;
            s2 <- f(sig, s)
        ) yield s2, pure(state), signals)
}
