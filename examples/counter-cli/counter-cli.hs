-- | This program shows very basic usage of an event store. We create a very
-- simple Counter Projection/CommandHandler that holds an integer between 0 and 100.
-- The CLI asks the user for commands and applies them to an in-memory event
-- store.

module Main where

import           Control.Concurrent.STM
import           Control.Monad          (forever, void)
import           Safe                   (readMay)

import           Eventful
import           Eventful.Store.Memory

main :: IO ()
main = do
  -- Create the event store and run loop forever
  tvar <- eventMapTVar
  let
    writer = tvarEventStoreWriter tvar
    reader = tvarEventStoreReader tvar
  forever (readAndHandleCommand writer reader)

readAndHandleCommand :: VersionedEventStoreWriter STM CounterEvent -> VersionedEventStoreReader STM CounterEvent -> IO ()
readAndHandleCommand writer reader = do
  -- Just use the nil uuid for everything
  let uuid = nil

  -- Get current state and print it out
  latestStreamProjection <- atomically $ getLatestStreamProjection reader (versionedStreamProjection uuid counterProjection)
  let
    currentState = streamProjectionState latestStreamProjection
  putStrLn $ "Current state: " ++ show currentState

  -- Ask user for command
  putStrLn "Enter a command. (IncrementCounter n, DecrementCounter n, ResetCounter, SetToTen):"
  input <- getLine

  -- Parse command and handle
  case readMay input of
    Nothing -> putStrLn "Unknown command"
    (Just command) -> do
      let events = commandHandlerHandler counterCommandHandler currentState command
      putStrLn $ "Events generated: " ++ show events
      void . atomically $ storeEvents writer uuid AnyPosition events

  putStrLn ""

-- | This is the state for our Counter projection.
newtype CounterState = CounterState { unCounterState :: Int }
  deriving (Eq, Show)

-- | This specifies the possible events we can use for our counter. In our
-- case, we only have one event to add an amount to a counter. Notice the use
-- of past-tense. Events record things that happened in the past.
data CounterEvent
  = CounterAmountAdded Int
  | CounterOutOfBounds Int
  deriving (Eq, Show)

-- | This ties together the state and event types into a 'Projection'.
type CounterProjection = Projection CounterState CounterEvent

counterProjection :: CounterProjection
counterProjection =
  Projection
  (CounterState 0)
  handleCounterEvent

handleCounterEvent :: CounterState -> CounterEvent -> CounterState
handleCounterEvent (CounterState k) (CounterAmountAdded x) = CounterState (k + x)
handleCounterEvent state (CounterOutOfBounds _) = state

-- | The commands we can use against our counter. We can increment or decrement
-- the counter, and also reset it.
data CounterCommand
  = IncrementCounter Int
  | DecrementCounter Int
  | ResetCounter
  | SetToTen
  deriving (Eq, Show, Read)


-- | This function validates commands and produces either an error or an event.
handlerCounterCommand :: CounterState -> CounterCommand -> [CounterEvent]
handlerCounterCommand (CounterState k) (IncrementCounter n) =
  if k + n <= 100
  then [CounterAmountAdded n]
  else [CounterOutOfBounds (k + n)]
handlerCounterCommand (CounterState k) (DecrementCounter n) =
  if k - n >= 0
  then [CounterAmountAdded (-n)]
  else [CounterOutOfBounds (k - n)]
handlerCounterCommand (CounterState k) SetToTen = [CounterAmountAdded (-k + 10)]
handlerCounterCommand (CounterState k) ResetCounter = [CounterAmountAdded (-k)]

-- | This ties all of the counter types into a CommandHandler.
type CounterCommandHandler = CommandHandler CounterState CounterEvent CounterCommand

counterCommandHandler :: CounterCommandHandler
counterCommandHandler = CommandHandler handlerCounterCommand counterProjection
