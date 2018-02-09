{-# LANGUAGE Rank2Types #-}
import Data.Monoid ((<>))
import Sum (Sum(..))
import Control.Lens

newtype Node g v = Node { fromNode :: (Node g v -> Node g g, v) }

constant :: (Monoid g, Num v) => v -> Node g v
constant v = Node (const mempty, v)

variable :: v -> Node v v
variable v = Node (id, v)

value :: Node g v -> v
value = snd . fromNode

grad :: (Monoid g, Num v) => Node g v -> Node g g
grad n = (fst $ fromNode n) (constant 1)

getVar :: Monoid s => Lens' s a -> Node s s -> Node s a
getVar ln (Node (g, s)) = Node (g . g', view ln s) where
    g' (Node (gg, gv)) = Node (gg . getVar ln, set ln gv mempty)

instance (Monoid g, Monoid v) => Monoid (Node g v) where
    mempty = Node (const mempty, mempty)
    Node (gf1, v1) `mappend` Node (gf2, v2) = Node (gf1 <> gf2, v1 <> v2)

instance (Monoid g, Num v) => Num (Node g v) where
    fromInteger = constant . fromInteger

    Node (g1, v1) + Node (g2, v2) = Node (g1 <> g2, v1 + v2)

    Node (g1, v1) - Node (g2, v2) = Node (g', v1 - v2)
        where g' x = g1 x <> g2 (-x)

    n1@(Node (g1, v1)) * n2@(Node (g2, v2)) = Node (g', v1 * v2)
        where g' x = g1 (n2 * x) <> g2 (n1 * x)

    abs n@(Node (g, v)) = Node (g . (* signum n), abs v)
    signum (Node (_, v)) = constant $ signum v

instance (Monoid g, Fractional v) => Fractional (Node g v) where
    fromRational = constant . fromRational
    recip n@(Node (g, v)) = Node (g . (* (-recip (n * n))), recip v)

instance (Monoid g, Floating v) => Floating (Node g v) where
    pi = constant pi

    exp   n@(Node (g, v)) = Node (g . (* exp n)             , exp v)
    log   n@(Node (g, v)) = Node (g . (* recip n)           , log v)
    sin   n@(Node (g, v)) = Node (g . (* cos n)             , sin v)
    cos   n@(Node (g, v)) = Node (g . (* (-sin n))          , cos v)
    asin  n@(Node (g, v)) = Node (g . (* (1-n*n)**(-0.5))   , asin v)
    acos  n@(Node (g, v)) = Node (g . (* (-(1-n*n)**(-0.5))), acos v)
    atan  n@(Node (g, v)) = Node (g . (* recip (1+n*n))     , atan v)
    sinh  n@(Node (g, v)) = Node (g . (* cosh n)            , sinh v)
    cosh  n@(Node (g, v)) = Node (g . (* sinh n)            , cosh v)
    asinh n@(Node (g, v)) = Node (g . (* (1+n*n)**(-0.5))   , asinh v)
    acosh n@(Node (g, v)) = Node (g . (* (n*n-1)**(-0.5))   , acosh v)
    atanh n@(Node (g, v)) = Node (g . (* recip (1-n*n))     , atanh v)

main :: IO ()
main = do
    let params = variable (2 :: Sum Double, 5 :: Sum Double)
    let x = getVar _1 params
    let y = getVar _2 params
    let res = x**3 + 2*y**3
    let fstgrad = getVar _1 . grad
    let sndgrad = getVar _2 . grad
    print $ value res                               -- 258
    print $ value $ grad res                        -- (dx, dy) = (12, 150)
    print $ value $ fstgrad $ fstgrad res           -- ddx  = 12
    print $ value $ fstgrad $ fstgrad $ fstgrad res -- dddx = 6
    print $ value $ sndgrad $ sndgrad res           -- ddy  = 60
    print $ value $ sndgrad $ sndgrad $ sndgrad res -- dddy = 12

    let z = variable (2 :: Sum Double)
    let res2 =  exp (cosh z * atan(sinh z + atanh (z - 1.5)))
    print $ value res2 -- 152.23405954065547
    print $ value $ grad res2 -- 895.7802709141566
    print $ value $ grad $ grad res2 -- 6143.755238016724
    print $ value $ grad $ grad $ grad res2 -- 47516.53980877573
