module MazesForProgrammers.Types
    ( Loc
    , Maze(..)
    , Opening(..)
    , Side(..)
    ) where

import           Data.Map.Strict (Map)

data Side = N | E | S | W deriving (Eq, Show)

type Loc = (Int, Int)

data Opening = Opening Loc Side deriving Show

data Maze = Maze Int Int (Map Loc [Side]) deriving Show
