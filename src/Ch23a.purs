module Ch23a where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.AVar (AVar)
import Effect.Aff (Aff, delay, launchAff_, forkAff, joinFiber, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Exception (error)
import Effect (Effect)
import Effect.Class.Console (log)

data TickTock = Tick | Tock
derive instance eqTickTock :: Eq TickTock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  clock ttAVar

data BombState = WaitingTick | WaitingTock

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar destinationCount = go 0 WaitingTick where
  go :: Int -> BombState -> Aff Unit
  go count state = do
    if count == destinationCount then log "Boom!!"
    else do
      delay (Milliseconds 500.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
          if tt == Tick then log "Tick" *> go count WaitingTock
          else go count state
        WaitingTock ->
          if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
          else go count state

test :: Effect Unit
test = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar 3
  AVar.put Tick ttAVar
  void $ joinFiber bombFiber
  void $ killFiber (error "Exploded") clockFiber