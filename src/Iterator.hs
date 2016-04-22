-- Imitate the following Rust code:
-- pub fn polybind<I, U, F>(m: I, f: F) -> FlatMap<I, U, F>
-- where I: Iterator, U: Iterator, F: FnMut(<I as Iterator>::Item) -> U {
--     m.flat_map(f)
-- }
    
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs, UndecidableInstances, KindSignatures, OverlappingInstances #-}

-- Iterator --

class Iterator t item | t -> item where
  next :: t -> Maybe (t, item)

collect it =
  case next it of
  Just (it', v) -> v : collect it'
  Nothing -> []
  
collect_n n it =
  if n <= 0 then []
  else
    case next it of
    Just (it', v) -> v : collect_n (n - 1) it'
    Nothing -> []
  
instance Iterator [a] a where
  next [] = Nothing
  next (x:xs) = Just (xs, x)

instance Iterator (Maybe a) a where
  next (Just v) = Just (Nothing, v)
  next Nothing = Nothing

class Inc t where
  inc :: t -> t

instance (Enum t) => Inc t where
  inc = succ

instance (Ord a, Inc a) => Iterator (a, a) a where
  next (x, y) = if x < y then Just ((inc x, y), x)
                else Nothing

newtype Range a = Range (a, a)

instance (Ord a, Inc a) => Iterator (Range a) a where
  next (Range (x, y)) = if x < y then Just (Range (inc x, y), x)
                        else Nothing

-- Callable --
  
class Callable t a b | t -> a, t -> b where
  call :: t -> a -> b
  
instance Callable (a -> b) a b where
  call = ($)

newtype Closure a b = Closure (a -> b)

instance Callable (Closure a b) a b where
  call (Closure f) x =  f x

-- polybind --
  
data FlatMap cont1 cont2 f = FlatMap {
  core :: Maybe (cont1, cont2, f)
  }

flat_map :: (Iterator cont1 item1, Iterator cont2 item2, Callable f item1 cont2) => cont1 -> f -> FlatMap cont1 cont2 f
flat_map m f = 
  case next m of
   Just (m', v) -> FlatMap $ Just (m', call f v, f)
   Nothing -> FlatMap Nothing

polybind :: (Iterator cont1 item1, Iterator cont2 item2, Callable f item1 cont2) => cont1 -> f -> FlatMap cont1 cont2 f
polybind m f = flat_map m f

ret v = Just v

flap_map_next :: (Iterator cont1 item1, Iterator cont2 item2, Callable f item1 cont2) => FlatMap cont1 cont2 f -> Maybe (FlatMap cont1 cont2 f, item2)
flap_map_next (FlatMap o) = do
  (c1, c2, f) <- o
  case next c2 of
   Just (c2', v) -> return (FlatMap $ Just (c1, c2', f), v)
   Nothing -> do
     (c1', v) <- next c1
     flap_map_next $ FlatMap $ Just (c1', call f v, f)

instance (Iterator cont1 item1, Iterator cont2 item2, Callable f item1 cont2) => Iterator (FlatMap cont1 cont2 f) item2 where
  next = flap_map_next

-- main --
  
main = do
  let it = polybind (polybind [1,2,3] (\x -> Range (x, 4))) (Closure (\y -> ret $ y + 100))
  print $ collect it
  let it = polybind (polybind [1,2,3] (\x -> Range (x, 1000000000000))) (Closure (\y -> ret $ y + 100))
  print $ collect_n 10 it
