module Helper
  ( wordsWhen
  , splitIntoTwoWords
  , tbind
  ) where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

splitIntoTwoWords :: (Char -> Bool) -> String -> (String, String)
splitIntoTwoWords splitFunc text =
  let (x:y:_) = take 2 $ wordsWhen splitFunc text ++ ["", ""]
   in (x, y)

tbind :: (Maybe a, Maybe b) -> Maybe (a, b)
tbind (Just x, Just y) = Just (x, y)
tbind _                = Nothing
