module Code where

import           Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

data Button = B | C | D | E | F | G
            | H | I | J | K | L | N
            | O deriving (Eq, Show, Enum)

data Door = Open | Closed
  deriving (Show, Eq)

data Corridor = Corridor { door1 :: Door
                         , door2 :: Door
                         , door3 :: Door
                         , door4 :: Door
                         , door5 :: Door
                         , door6 :: Door
                         } deriving (Show, Eq)

closedCorridor :: Corridor
closedCorridor = Corridor { door1 = Closed
                          , door2 = Closed
                          , door3 = Closed
                          , door4 = Closed
                          , door5 = Closed
                          , door6 = Closed
                          }

openCorridor :: Corridor
openCorridor = Corridor { door1 = Open
                        , door2 = Open
                        , door3 = Open
                        , door4 = Open
                        , door5 = Open
                        , door6 = Open
                        }

push :: Corridor -> Button -> Corridor
push c b = case b of
             B -> c { door4 = Open, door5 = Closed }
             C -> c { door5 = Open, door4 = Closed }
             D -> c { door1 = Open, door5 = Closed }
             E -> c { door5 = Open, door1 = Closed }
             F -> c { door2 = Open, door4 = Closed }
             G -> c { door6 = Open }
             H -> c { door3 = Open, door2 = Closed }
             I -> c { door2 = Open, door3 = Closed }
             J -> c { door2 = Open, door1 = Closed }
             K -> c { door6 = Open, door5 = Closed }
             L -> c { door3 = Open, door4 = Closed }
             N -> c { door1 = Open, door2 = Closed }
             O -> c { door5 = Open, door6 = Closed }

pushMultiple :: Corridor -> [Button] -> Corridor
pushMultiple = foldl push

genSteps :: Gen [Button]
genSteps = Gen.list (Range.linear 6 50) (Gen.enum B O)

propSolution :: Property
propSolution = withTests (20000 :: TestLimit) . property $ do
  steps <- forAll genSteps
  let corridor = pushMultiple closedCorridor steps
  assert $ corridor /= openCorridor
