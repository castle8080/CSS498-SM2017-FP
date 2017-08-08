
module MIO (
    World(World),
    IO
) where
import qualified Prelude as P

data World = World deriving (P.Show)
type IO a = World -> (a, World)

return :: a -> IO a
return x = (\world -> (x, world))

(>>) :: IO a -> IO b -> IO b
(>>) action_a action_b =
  action_a >>= (\_ -> action_b)

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) action f =
  (\world ->
    let (a, world') = action world
    in (f a) world')
