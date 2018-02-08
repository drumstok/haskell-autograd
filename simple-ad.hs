newtype Node v = Node (v -> v, v)

constant, variable :: Num v => v -> Node v
constant v = Node (const 0, v)
variable v = Node (id, v)

runNode :: Num v => Node v -> (v, v)
runNode (Node (g, v)) = (v, g 1)

instance (Num v) => Num (Node v) where
    fromInteger = constant . fromInteger

    Node (g1, v1) + Node (g2, v2) = Node (g', v1 + v2)
        where g' x = g1 x + g2 x

    Node (g1, v1) * Node (g2, v2) = Node (g', v1 * v2)
        where g' x = (v2 * g1 x) + (v1 * g2 x)

    Node (g1, v1) - Node (g2, v2) = Node (g', v1 - v2)
        where g' x = g1 x - g2 x

    abs (Node (g, v)) = Node (g', abs v)
        where g' x = signum v * g x

    signum (Node (_, v)) = constant $ signum v

main :: IO ()
main = do
    let nx = variable 5 :: Node Int
    print $ runNode $ -(3 + nx) * (3 - nx) -- (16, 10)
