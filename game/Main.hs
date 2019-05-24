{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Fix

import Data.Distributive
import Data.Functor.Rep
import Data.FileEmbed
import Data.Traversable
import qualified Data.Text as T

import Language.Javascript.JSaddle.Types

import Reflex.Dom.Core


main :: IO ()
#ifdef ghcjs_HOST_OS
main = mainJSM
#else
main = putStrLn "Please use ghcjs" -- Allows ghcid to work
#endif

mainJSM :: JSM ()
mainJSM = mainWidgetWithCss css app
  where css = $(embedFile "style.css")

app :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => m ()
app = divClass "main" $ do
  rec
    game <- foldDyn doAction initialGame click
    click <- gameArea game
  pure ()


gameArea
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Game
  -> m (Event t Coord)
gameArea game = do
  fmap leftmost $ elClass "table" "game-area" $ forM allFour $ \row ->
    fmap leftmost $ el "tr" $ forM allFour $ \col -> do
      let coord = Coord{..}
          txt = dynText $ weightText coord <$> game
          attrs = ffor game $ \g -> "class" =: T.unwords
            ["tile" , selClass coord g , weightClass coord g]
      (btn,_) <- el "td" $ elDynAttr' "button" attrs txt
      pure $ coord <$ domEvent Click btn

weightText :: Coord -> Game -> T.Text
weightText coord = toText . (`index` coord) . gameGrid
  where
    toText 0 = ""
    toText n = T.pack . show @Int $ 2^n

weightClass :: Coord -> Game -> T.Text
weightClass coord = ("weight-" <>) . T.pack . show . (`index` coord) . gameGrid

selClass :: Coord -> Game -> T.Text
selClass coord = toText . (Just coord ==) . gameSel
  where
    toText False = "unselected"
    toText True  = "selected"


data Four = FourA | FourB | FourC | FourD
  deriving (Eq, Show, Ord, Enum, Bounded)

allFour :: [Four]
allFour = [FourA .. FourD]

distFour :: Four -> Four -> Int
distFour x y = abs (fromEnum x - fromEnum y)


data Line a = Line a a a a
  deriving (Eq, Show, Functor)

instance Distributive Line where
  distribute = distributeRep
  collect = collectRep

instance Representable Line where
  type Rep Line = Four
  tabulate f = Line (f FourA) (f FourB) (f FourC) (f FourD)
  index (Line a b c d) = \case
    FourA -> a
    FourB -> b
    FourC -> c
    FourD -> d


data Coord = Coord { row, col :: Four }
  deriving (Eq, Show)

topLeft :: Coord
topLeft = Coord FourA FourA

-- | Manhattan distance of `Coord`s
distCoord :: Coord -> Coord -> Int
distCoord x y = distFour (row x) (row y) + distFour (col x) (col y)

isNeighborOf :: Coord -> Coord -> Bool
isNeighborOf x y = 1 == distCoord x y


newtype Grid a = Grid { gridTable :: Line (Line a) }
  deriving (Eq, Show, Functor)

instance Distributive Grid where
  distribute = distributeRep
  collect = collectRep

instance Representable Grid where
  type Rep Grid = Coord
  tabulate f = Grid $ tabulate $ \row -> tabulate $ \col -> f Coord{..}
  index (Grid table) Coord{..} = index (index table row) col


type Weight = Int

data Game = Game
  { gameGrid :: Grid Weight
  , gameSel :: Maybe Coord
  } deriving (Eq, Show)

initialGame :: Game
initialGame = Game
  { gameGrid = tabulate $ fromEnum . (topLeft ==)
  , gameSel = Nothing
  }


type Action = Coord

doAction :: Action -> Game -> Game
doAction clicked game@Game{..} = case gameSel of
  Just sel -> case moveType gameGrid sel clicked of
    MoveCombine -> combine sel clicked game
    MoveShift -> game{gameSel = Nothing, gameGrid = shift sel clicked gameGrid}
    MoveInvalid -> game{gameSel = Nothing}
  Nothing
    | 0 == index gameGrid clicked -> game
    | otherwise -> game{gameSel = Just clicked}


data MoveType = MoveCombine | MoveShift | MoveInvalid

moveType :: Grid Weight -> Coord -> Coord -> MoveType
moveType grid from to =
  if | areNeighbors && atFrom == atTo -> MoveCombine
     | areNeighbors && atTo == 0 -> MoveShift
     | otherwise -> MoveInvalid
  where
    areNeighbors = from `isNeighborOf` to
    atFrom = index grid from
    atTo = index grid to

combine :: Coord -> Coord -> Game -> Game
combine from to Game{..} = Game
  { gameGrid = tabulate $ \coord ->
      if coord == from
      then fromEnum (from == topLeft)
      else index gameGrid coord + fromEnum (coord == to)
  , gameSel = Nothing
  }

shift :: Coord -> Coord -> Grid Weight -> Grid Weight
shift from to grid = tabulate $ \coord ->
  if | coord == from -> fromEnum (from == topLeft)
     | coord == to -> index grid from
     | otherwise -> index grid coord
