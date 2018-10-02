module Prob where

type Chance = Double

-- type Prob a = [(a, Chance)]
newtype Prob a = Prob { unProb :: [(a, Chance)]}

always :: a -> Prob a
always a = Prob [(a, 1.0)]

uniform :: [a] -> Prob a
uniform a = let scale = fromIntegral $ length a
                chance = 1.0 / scale
            in Prob $ map (\b -> (b, chance)) a

dice :: Int -> Prob Int
dice n = uniform [1..n]

chanceOf :: (a -> Bool) -> Prob a -> Chance
chanceOf pred (Prob p) = go 0.0 p
  where
    go acc [] = acc
    go acc (a:as) =
      if pred (fst a)
      then go (acc + snd a) as
      else go acc as

instance Functor Prob where
  fmap f (Prob p) = Prob $ fmap (\(e, c) -> (f e, c)) p

instance Applicative Prob where
  pure = always

  u <*> x = Prob [ (v y, vc * yc)
                 | (v, vc) <- unProb u
                 , (y, yc) <- unProb x ]

instance Monad Prob where
  m >>= k = Prob . concat $ [ normalize xc $ unProb $ k x
                            | (x, xc) <- unProb m ]
    where
      normalize :: Chance -> [(a, Chance)] -> [(a, Chance)]
      normalize factor = map (\(a, ac) -> (a, factor * ac))

adventure :: Prob String
adventure = do
  strengthSave <- dice 6 -- bear attack
  strengthResult <- if strengthSave < 3
    then always "survived"
    else
    do
      healCheck <- dice 20
      healResult <- if healCheck < 13
                    then always "survived"
                    else always "died"
      return healResult
  return strengthResult

data Door = Prize | NoPrize deriving (Eq)

montyHall :: Prob Door
montyHall = do
  firstDoor <- uniform [ Prize, NoPrize, NoPrize ]
  -- Monty opens a not-chosen door with no prize behind it
  newDoor <- case firstDoor of
    Prize   -> always NoPrize
    NoPrize -> always Prize
  return newDoor

main :: IO ()
main = do
  putStr "Chance of surviving the adventure: "
  putStrLn.show $ chanceOf (=="survived") adventure
  putStr "Chance of the prize being behind the new door: "
  putStrLn.show $ chanceOf (==Prize) montyHall
