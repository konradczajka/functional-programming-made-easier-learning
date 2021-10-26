module Ch20 where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Control.Monad.Writer.Trans ( class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

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
results (Tuple (Tuple (Left err) l) s)
  = Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right _) l) s)
  = Tuple Nothing { log: l, state: s, result: Just unit }

runApp :: Int -> AppM -> Effect AppResult
runApp s = map results
  <<< flip runStateT s
  <<< runWriterT
  <<< runExceptT

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "Starting App..."
  n <- get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"
  put $ n + 1
  log "Incremented State"
  pure unit

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  Console.log $ show result1
  result2 <- runApp 99 app
  Console.log $ show result2