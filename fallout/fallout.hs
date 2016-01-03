import System.Random
import Data.List
import Data.Char

prompt :: String -> IO String
prompt s = putStrLn (s ++ " ") >> getLine

blanks :: String -> String -> String
blanks chosenWord guess = map (\(a, b) -> if a == b
                                             then b
                                             else '_') $
                          zip chosenWord guess

printWords :: [String] -> IO ()
printWords = mapM_ putStrLn . map (map toUpper)

getWords :: Int -> IO [String]
getWords num = do
  input <- readFile "dict.txt"
  g <- newStdGen
  let indexes = take num . nub $ (randomRs (1,40) g :: [Int])
  let words = filter ((==7) . length) $ lines input
  return $ map (\i -> words !! i) indexes

chooseWord :: [String] -> IO String
chooseWord words = do
  index <- getStdRandom (randomR (1,(length words)-1))
  return $ words !! index

game :: Int -> String -> [String] -> IO ()
game guessesLeft chosenWord words = do
  printWords words
  guess <- prompt "Your guess: "
  let blanked = blanks chosenWord guess
  if (length $ filter (\x -> x == '_') blanked) == 0
    then putStrLn "Congratulations! You have won!" 
    else do putStrLn "Wrong!"
            putStrLn blanked
            let left = guessesLeft - 1
            if left < 0
              then do putStrLn "You have lost!"
                      putStrLn $ "The words was: " ++ chosenWord
              else do putStrLn $ "Guesses left: " ++ show guessesLeft
                      game left chosenWord words

difficulties :: [Int]
difficulties = [4,7,9,12,15]

main :: IO ()
main = do
  diff <- prompt "Difficulty? (1-5)"
  words <- getWords (difficulties !! (read diff - 1))
  chosenWord <- chooseWord words
  let initialGuesses = 4
  game initialGuesses chosenWord words
