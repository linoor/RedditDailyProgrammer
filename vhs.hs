import Control.Applicative
import Data.List
import Data.Ord
import Text.ParserCombinators.Parsec as Parsec

data TVShow = TVShow {
  start :: Int,
  end   :: Int,
  name  :: String
}
 
instance Show TVShow where
  show (TVShow beg ending name) = "TVShow: " ++
      name     ++ " " ++
      show beg ++ " " ++
      show ending

parseShowName = Parsec.many anyChar 

parseShow = do
  beginning <- Parsec.many digit
  space
  end <- Parsec.many digit
  space
  name <- parseShowName
  return $ TVShow (read beginning) (read end) name

getTVShow :: String -> TVShow
getTVShow s = case parse parseShow "unknown" s of
  Right x -> x
  Left  m -> error $ show m

overlap :: TVShow -> TVShow -> Bool
overlap a b = start a < end b && start b < end a

main :: IO ()
main = do
  input <- readFile "last.data"
  let mustSeeShow = head $ lines input
  let alltvshows = map getTVShow . tail $ lines input
  let maxtvshows = maximumBy (comparing length) $
                   --map (nubBy overlap) $
                   filter (\subseq -> mustSeeShow `elem` map name subseq) $ 
                   subsequences alltvshows
  print $ length alltvshows
  mapM_ print maxtvshows
  putStr "max number of tv shows to record: "
  print $ length maxtvshows
