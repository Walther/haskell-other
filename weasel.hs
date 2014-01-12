-- Dawkins' Weasel http://en.wikipedia.org/wiki/Weasel_program
-- Walther // 2014
import Data.List
import Data.Random
import Data.Random.Extras
import Control.Monad (replicateM)

-- Parameters
target = "METHINKS IT IS LIKE A WEASEL"
len = length target
-- Chance, in percent, for mutation of an individual cell (Char)
chance = 5
-- Number of childs to spawn in each generation
childs = 100

-- Main
main = do
    child <- spawn
    loop 0 child
      where
        loop counter child = do
          child <- run (evolve child)
          let counter' = counter + 1
          prettyprint counter' child
          if child == target
            then return counter'
            else loop counter' child


-- Prettyprints the generation number, the child sentence, and the score
prettyprint :: Show a => a -> String -> IO ()
prettyprint n child = print (show n ++ ": " ++ child ++ " -- score: " ++ show (len - hamming target child))

-- Calculates the Hamming distance between two sentences
hamming :: String -> String -> Int
hamming a b = length $ filter id $ zipWith (/=) a b

-- Spawn one random cell (Char)
cell :: RVar Char
cell = choice $ ['A'..'Z'] ++ " "

-- Spawn a parent sentence that consists of cells
spawn :: IO String
spawn = replicateM len $ run cell

-- Helper runRVar function to reduce repetition
run :: MonadRandom m => RVar a -> m a
run r = runRVar r StdRandom

-- Mutation function
mutate :: String -> RVar String
mutate = mapM mutate_cell
    where
        mutate_cell :: Char -> RVar Char
        mutate_cell x = do roll <- d100
                           if roll > chance then return x
                           else cell

-- Evolution function. Gives birth to childs based on parent and mutation
evolve :: String -> RVar String
evolve parent = offspring >>= return . fittest
        where
              offspring :: RVar [String]
              offspring          = replicateM childs (mutate parent)
              comparefitness a b = compare (hamming target a) (hamming target b)
              fittest            = minimumBy comparefitness

-- A simple dice function
d100 = choice [1..100]