module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob (c:cs) (d:ds)
  | c == '?' = matchGlob cs ds
  | c == '[' = handleSet ([c] ++ cs) ([d] ++ ds)

matchGlob [] [] = True
matchGlob _ _ = False

handleSet :: GlobPattern -> String -> Bool
handleSet (c:cs) (d:ds) =
  any (== d) setChars && matchGlob rest ds
  where
    (setChars, _) = break (== ']') cs
    rest = drop (1 + (length setChars)) cs

--  | c == '*' = matchAfterAsterisks cs ([d] ++ ds)
