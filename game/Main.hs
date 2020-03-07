{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Fix
import Control.Monad.Trans.Maybe

import Data.Distributive
import Data.Functor.Rep
import Data.Maybe
import Data.Traversable
import qualified Data.Text as T

import Text.Read (readMaybe)

import Reflex.Dom

import JSDOM (currentWindow)
import JSDOM.Types (MonadDOM)
import JSDOM.Generated.Storage (Storage, getItem, setItem)
import JSDOM.Generated.Window (getLocalStorage)

import Style


main :: IO ()
main = do
  initialGame <- loadGame
  mainWidgetWithHead appHead (appBody initialGame)

appHead :: DomBuilder t m => m ()
appHead = do
  el "title" $ text "Four"
  elAttr "style" ("type" =: "text/css") $ text css
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: "https://code.cdn.mozilla.net/fonts/fira.css"
    ) blank

appBody
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
#ifdef ghcjs_HOST_OS
     , MonadDOM (PushM t)
#endif
     )
  => Game
  -> m ()
appBody initialGame = divClass "main" $ do
  reset <- gameHeader
  rec
    game <- foldDynM performAction initialGame $ leftmost
      [ ClickTile <$> click
      , Continue <$ continue
      , ResetGame <$ reset
      ]
    click <- gameArea game
    continue <- gameOverlay game
  pure ()


gameOverlay
  :: (DomBuilder t m, PostBuild t m, MonadHold t m)
  => Dynamic t Game
  -> m (Event t ())
gameOverlay game = do
  let attrs = ffor game $ \g -> "class" =:
        if gameWon g == WonOverlay then "overlay" else  "overlay-hidden"
  (overlay,_) <- elDynAttr' "div" attrs $ text "You Won!"
  pure $ domEvent Click overlay

gameHeader :: DomBuilder t m => m (Event t ())
gameHeader = divClass "game-header" $ do
  elAttr "span" ("class" =: "title") $ text "Four"
  (resetBtn,_) <- elAttr' "button" ("class" =: "reset") $ text "Reset"
  pure $ domEvent Click resetBtn

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


#ifdef ghcjs_HOST_OS

performAction :: MonadDOM m => Action -> Game -> m Game
performAction action oldGame = do
  let newGame = applyAction action oldGame
  saveGame newGame
  pure newGame

saveGame :: MonadDOM m => Game -> m ()
saveGame game = fmap (fromMaybe ()) . runMaybeT $ do
  storage <- getStorage
  setItem storage storageKey (show game)
  pure ()

loadGame :: MonadDOM m => m Game
loadGame = fmap (fromMaybe defaultGame) . runMaybeT $ do
  storage <- getStorage
  item <- MaybeT (getItem storage storageKey)
  MaybeT . pure $ readMaybe item

getStorage :: MonadDOM m => MaybeT m Storage
getStorage = do
  window <- MaybeT currentWindow
  MaybeT $ Just <$> getLocalStorage window

storageKey :: T.Text
storageKey = "four"

#else

performAction :: Monad m => Action -> Game -> m Game
performAction action game = pure $ applyAction action game

loadGame :: Applicative m => m Game
loadGame = pure defaultGame

#endif


data Four = FourA | FourB | FourC | FourD
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

allFour :: [Four]
allFour = [FourA .. FourD]

distFour :: Four -> Four -> Int
distFour x y = abs (fromEnum x - fromEnum y)


data Line a = Line a a a a
  deriving (Eq, Show, Read, Functor)

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
  deriving (Eq, Show, Read)

topLeft :: Coord
topLeft = Coord FourA FourA

-- | Manhattan distance of `Coord`s
distCoord :: Coord -> Coord -> Int
distCoord x y = distFour (row x) (row y) + distFour (col x) (col y)

isNeighborOf :: Coord -> Coord -> Bool
isNeighborOf x y = 1 == distCoord x y


newtype Grid a = Grid { gridTable :: Line (Line a) }
  deriving (Eq, Show, Read, Functor)

instance Distributive Grid where
  distribute = distributeRep
  collect = collectRep

instance Representable Grid where
  type Rep Grid = Coord
  tabulate f = Grid $ tabulate $ \row -> tabulate $ \col -> f Coord{..}
  index (Grid table) Coord{..} = index (index table row) col


type Weight = Int

data WonState = NotWon | WonOverlay | WonContinue
  deriving (Eq, Show, Read)

data Game = Game
  { gameGrid :: Grid Weight
  , gameSel :: Maybe Coord
  , gameWon :: WonState
  } deriving (Eq, Show, Read)

defaultGame :: Game
defaultGame = Game
  { gameGrid = tabulate $ fromEnum . (topLeft ==)
  , gameSel = Nothing
  , gameWon = NotWon
  }


data Action
  = ClickTile Coord
  | Continue
  | ResetGame

applyAction :: Action -> Game -> Game
applyAction (ClickTile clicked) game@Game{..} = case gameSel of
  Just sel -> case moveType gameGrid sel clicked of
    MoveCombine -> combine sel clicked game
    MoveShift -> game{gameSel = Nothing, gameGrid = shift sel clicked gameGrid}
    MoveInvalid -> game{gameSel = Nothing}
  Nothing
    | 0 == index gameGrid clicked -> game
    | otherwise -> game{gameSel = Just clicked}
applyAction Continue game = game{gameWon = WonContinue}
applyAction ResetGame _ = defaultGame


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
  , gameWon = case (gameWon, index gameGrid to) of
      (NotWon, 1) -> WonOverlay
      _ -> gameWon
  }

shift :: Coord -> Coord -> Grid Weight -> Grid Weight
shift from to grid = tabulate $ \coord ->
  if | coord == from -> fromEnum (from == topLeft)
     | coord == to -> index grid from
     | otherwise -> index grid coord
