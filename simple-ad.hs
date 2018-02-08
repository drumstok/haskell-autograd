newtype Node v = Node { fromNode :: (Node v -> Node v, v) }

constant, variable :: Num v => v -> Node v
constant v = Node (const 0, v)
variable v = Node (id, v)

value :: Num v => Node v -> v
value = snd . fromNode

grad :: Num v => Node v -> Node v
grad n = (fst $ fromNode n) (constant 1)

instance (Num v) => Num (Node v) where
    fromInteger = constant . fromInteger

    Node (g1, v1) + Node (g2, v2) = Node (g', v1 + v2)
        where g' x = g1 x + g2 x

    Node (g1, v1) - Node (g2, v2) = Node (g', v1 - v2)
        where g' x = g1 x - g2 x

    n1@(Node (g1, v1)) * n2@(Node (g2, v2)) = Node (g', v1 * v2)
        where g' x = (n2 * g1 x) + (n1 * g2 x)

    abs n@(Node (g, v)) = Node ((* signum n) . g, abs v)
    signum (Node (_, v)) = constant $ signum v

instance (Fractional v) => Fractional (Node v) where
    fromRational = constant . fromRational
    recip n@(Node (g, v)) = Node ((* (-recip (n * n))) . g, recip v)

instance (Floating v) => Floating (Node v) where
    pi = constant pi

    exp   n@(Node (g, v)) = Node ((* exp n)              . g, exp v)
    log   n@(Node (g, v)) = Node ((* recip n)            . g, log v)
    sin   n@(Node (g, v)) = Node ((* cos n)              . g, sin v)
    cos   n@(Node (g, v)) = Node ((* (-sin n))           . g, cos v)
    asin  n@(Node (g, v)) = Node ((* (1-n*n)**(-0.5))    . g, asin v)
    acos  n@(Node (g, v)) = Node ((* (-(1-n*n)**(-0.5))) . g, acos v)
    atan  n@(Node (g, v)) = Node ((* recip (1+n*n))      . g, atan v)
    sinh  n@(Node (g, v)) = Node ((* cosh n)             . g, sinh v)
    cosh  n@(Node (g, v)) = Node ((* sinh n)             . g, cosh v)
    asinh n@(Node (g, v)) = Node ((* (1+n*n)**(-0.5))    . g, asinh v)
    acosh n@(Node (g, v)) = Node ((* (n*n-1)**(-0.5))    . g, acosh v)
    atanh n@(Node (g, v)) = Node ((* recip (1-n*n))      . g, atanh v)

main :: IO ()
main = do
    let x = variable 2.0 :: Node Double
    let res = exp (cosh x * atan(sinh x + atanh (x - 1.5)))
    print $ value res -- 152.23405954065547
    print $ value $ grad res -- 895.7802709141566
    print $ value $ grad $ grad res -- 6143.755238016724
    print $ value $ grad $ grad $ grad res -- 47516.53980877573
