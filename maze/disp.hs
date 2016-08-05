{-# LANGUAGE TemplateHaskell #-}

-- undefined
import Data.Void (Void)
import qualified Data.List as List
import qualified Control.Lens as L

-- nBetween :: (Integral n, Fractional a) => n -> a -> a -> [a]
-- nBetween n l r = 
--   let delta = (l + r) / fromIntegral n
--    in [

viewportWidth :: Int
viewportWidth = 20

-- | fixed length list of (display char, height)s
type Display = [(Char, Int)]

-- | in radians
type Angle = Double

type ShapeLoc = [Coords] 

type Coords = (Double, Double)

type Nat = Int
data Health 
  = Positive Nat -- ^ strictly positive
  | Dead
L.makeLenses ''Health

data PlayerData = PlayerData
  { _playerPosition :: Coords
  , _playerHealth :: Health
  , _playerViewLeftEdge :: Angle
  , _playerViewRightEdge :: Angle
  }
L.makeLenses ''PlayerData

data Wall = Wall 
  { _wallDisplayChar :: Void
  , _wallShapeLoc :: ShapeLoc
  }
L.makeLenses ''Wall

data Enemy = Enemy 
  { _damage :: Nat
  , _displayChar :: Void
  , _enemyShapeLoc :: ShapeLoc
  }
L.makeLenses ''Enemy

data Game = Game 
  { _playerData :: PlayerData
  , _enemies :: [Enemy]
  , _walls :: [Wall]
  }
L.makeLenses ''Game

testGame :: Game
testGame = Game
  { _playerData = PlayerData 
    { _playerPosition = (0,0)
    , _playerHealth = Positive 3
    , _playerViewLeftEdge = 0
    , _playerViewRightEdge = 1.7
    }
  , _enemies = []
  , _walls = []
  }

type MySetter s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type MyGetter s a = forall . (s -> Const a r) -> 
-- getTwo :: L.Getter s a -> L.Getter s b -> L.Getter s (a, b)
-- getTwo u v = undefined

disp :: Game -> Display
disp game = 
  let l = L.view (playerData . playerViewLeftEdge) game
      r = L.view (playerData . playerViewRightEdge) game
      deltaAng = (l + r) / fromIntegral viewportWidth
      angles = take viewportWidth [l, l + deltaAng, r]
      go angle = undefined
   in fmap go angles

screen :: Display -> String
screen disp = 
  let m = maximum . fmap snd $ disp 
      go (displayChar, height) = 
        let leftPad = (m - height) `div` 2
            rightPad = m - height - leftPad
         in replicate leftPad ' ' 
              ++ replicate height displayChar
              ++ replicate rightPad ' '
   in unlines . List.transpose . fmap go $ disp


-- maybe this should be more like an affine space...
-- instance Num Health where
--     (*) = error "tried to multiply health"
--     abs = error "tried to take abs of health"
--     signum = error "tried to take signum of health"
-- 
--     fromInteger n 
--         | n > 0 = Positive n
--         | n == 0 = Dead
--         | otherwise = error "tried to set negative health"
-- 
--     Positive a + Positive b = Positive $ a + b
--     Dead + Positive b = Positive b
--     Positive a + Dead = Positive a
--     Dead + Dead = Dead
-- 
--     Positive a - Positive b
