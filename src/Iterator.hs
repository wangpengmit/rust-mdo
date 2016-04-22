{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs, UndecidableInstances, KindSignatures, OverlappingInstances #-}

module Iterator where

    class Iterator item t where
                next :: t -> t * Maybe item
                
    class Callable a b t where
                call :: t -> a -> b
                             call
    data FlatMap item = FlatMap { flap_map_next :: FlatMap -> (FlatMap * Maybe item) }
        
        instance Iterator item (FlatMap item) where
                next = flap_map_next

    flap_map :: cont1 -> f -> Flat
    flap_map m f =
        let loop (c1, c2) =
            let (c2', o) = next c2 in
            case o of
              Just v => ((c1, c2'), Just v)
                   | Nothing =>
                     let (c1', o) = next c1
                                    
        FlatMap $ \(FlapMap )
        
    bind :: (Iterator item1 cont1, Iterator item2 cont2, Callable item1 cont2) => cont1 -> f -> FlatMap item2
    bind m f = flat_map m f
               
    pub fn bind<I, U, F>(m: I, f: F) -> FlatMap<I, U, F>
    where I: Iterator, U: Iterator, F: FnMut(<I as Iterator>::Item) -> U {
        m.flat_map(f)
    }
    
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

newtype TagT tag m a = TagT { runTag :: m a } deriving Show

instance Monad m => Monad (TagT tag m) where
   return a = TagT (return a)
   TagT x >>= f = TagT $ x >>= (runTag . f)

instance MonadTrans (TagT tag) where
   lift m = TagT m

instance Functor m => Functor (TagT tag m) where
  fmap f = TagT . fmap f . runTag

instance MonadState s m => MonadState s (TagT tag m) where
  get = TagT get
  put s = TagT $ put s

-- monad transformer stack n contains monad m with tag t
class Contains (m :: * -> *) t (n :: * -> *) | m t -> n where
   liftFrom :: t -> n a -> m a

instance (Monad m, m ~ n) => Contains (TagT tag m) tag n where
   liftFrom _ x = lift x

instance (MonadTrans t, Monad m, Contains m tag n) => Contains (t m) tag n where
   liftFrom tag x = lift (liftFrom tag x)

type TStateT tag s m = TagT tag (StateT s m)
runTStateT = runStateT . runTag

tput tag x = liftFrom tag (put x)
tget tag = liftFrom tag get

type TState tag s = TStateT tag s Identity
runTState m = runIdentity . runTStateT m

type TWriterT tag w m = TagT tag (WriterT w m)
runTWriterT = runWriterT . runTag

ttell tag x = liftFrom tag (tell x)

type TWriter tag w = TWriterT tag w Identity
runTWriter = runIdentity . runTWriterT

-- tests

test1 :: StateT Int (StateT Int (StateT Int (WriterT String Identity))) Int
test1 = do
   put 1
   lift $ put 2
   lift $ lift $ put 3
   a <- get
   b <- lift $ get
   c <- lift $ lift $ get
   lift $ lift $ lift $ tell $ show $ a+b+c
   return $ a*b*c

go1 = runIdentity (runWriterT (runStateT (runStateT (runStateT test1 0) 0) 0))

data A = A
data B = B
data C = C
data D = D

test2 :: TStateT A Int (TStateT B Int (TStateT C Int (TWriterT D String Identity))) Int

test2 = do
   tput A 1
   tput B 2
   tput C 3
   a <- tget A
   b <- tget B
   c <- tget C
   ttell D $ show $ a+b+c
   return $ a*b*c

go2 = runIdentity (runTWriterT (runTStateT (runTStateT (runTStateT test2 0) 0) 0))
