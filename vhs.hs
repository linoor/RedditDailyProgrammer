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
  input <- readFile "folks.data"
  let alltvshows = map getTVShow . tail $ lines input
  let mustSeeShowName = head $ lines input
  let mustSeeShow = head $ filter (\tvshow -> name tvshow == mustSeeShowName) alltvshows
  let maxtvshows =
        -- add to the acc only if the shows are not overlapping
        foldl helper [] $
        -- sort by ending time
        sortBy (comparing end) alltvshows where
            helper [] tvshow                = [tvshow]
            helper acc tvshow
                | overlap (last acc) tvshow = acc
                | otherwise                 = acc ++ [tvshow]
  let withMustSee =
        -- sort by ending time
        sortBy (comparing end) $ 
        -- add the must see show
        mustSeeShow : (foldl helper [] $
        -- sort by ending time
        sortBy (comparing end) $
        -- remove the shows that overlap with the must see show
        filter (\tvshow -> not (overlap mustSeeShow tvshow)) alltvshows) where
            helper [] tvshow                = [tvshow]
            helper acc tvshow
                | overlap (last acc) tvshow = acc
                | otherwise                 = acc ++ [tvshow]
  print $ length alltvshows
  mapM_ print maxtvshows
  putStr "max number of tv shows to record: "
  print $ length maxtvshows
  putStrLn "-----"
  mapM_ print withMustSee
  putStr "max number of tv shows to record (with the must see show included): "
  print $ length withMustSee
