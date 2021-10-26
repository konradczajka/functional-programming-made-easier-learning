module Ch21 where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

newtype State s a = State (s -> Tuple a s)

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mg) = StateT \s -> mg s <#> \(Tuple a s') -> Tuple (f a) s'

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply :: ∀ a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  apply (StateT fmf) (StateT fmx) = StateT \s -> do
    Tuple f s' <- fmf s
    Tuple x s'' <- fmx s'
    pure $ Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind :: ∀ a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  bind (StateT fmx) f = StateT \s -> fmx s >>= \(Tuple x s') -> runStateT (f x) s'
  -- bind (StateT fmx) f = StateT \s -> do
  --   Tuple x s' <- fmx s
  --   runStateT (f x)

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadTransStateT :: MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \x -> (Tuple x s)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask =  lift ask

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError (StateT fmx) f = StateT \s -> 
                  catchError (fmx s) \e -> runStateT (f e) s

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

type AppM = AppStack String String Int Unit

type StackResult = Tuple (Tuple (Either String Unit) String) Int

type AppEffects =
  { log :: String
  , state :: Int
  , result :: Maybe Unit
  }

type AppResult = Tuple (Maybe String) AppEffects

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right result) l) s) = Tuple Nothing { log: l, state: s, result: Just result }

runApp :: Int -> AppM -> Effect AppResult
runApp st = (results <$> _)
  <<< flip runStateT st
  <<< runWriterT
  <<< runExceptT

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "Starting app..."
  n <- get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE 0 STATE!"
  put $ n + 1
  log "Incremented state"
  pure unit


-- liftStateT :: ∀ s m a. Functor m => m a -> StateT s m a
-- liftStateT mx = StateT \s -> mx <#> \x -> (Tuple x s)

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  Console.log $ show result1
  result2 <- runApp 1 app
  Console.log $ show result2