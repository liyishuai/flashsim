{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad
import           Data.Map.Strict
import           Foreign.C.Types
import           System.Process
import           Test.QuickCheck

data Event_type = R | W
  deriving (Show, Eq)

data Event = Event {
  event_type      :: Event_type,
  logical_address :: NonNegative Int
  } deriving Eq

instance Show Event where
  show (Event etype addr) =
    show etype ++ " " ++ show (getNonNegative addr)

instance Arbitrary Event_type where
  arbitrary = elements [R, W]

instance Arbitrary Event where
  arbitrary = liftM2 Event arbitrary arbitrary

step :: Event
  -> CreateProcess
  -> Map (NonNegative Int) Int
  -> IO (Map (NonNegative Int) Int)
step e p m =
  let input = show e in do
    output <- readCreateProcess p input
    return $
      case e of
        Event R laddr ->
          let paddr = read output in

            insert laddr paddr m
        Event W laddr ->
          delete laddr m

