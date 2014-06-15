module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob (c:cs) (d:ds)
  | c == '?' = matchGlob cs ds
  | c == '[' = handleSet ([c] ++ cs) ([d] + ds)

matchGlob [] [] = True
matchGlob _ _ = False

handleSet :: GlobPattern -> String -> Bool
handleSet (c:cs) (d:ds) =
  let (setChars, _) = break (== ']') cs
  let rest = drop (1 + (length setChars)) cs
  any (== d) setChars && matchGlob rest ds

--  | c == '*' = matchAfterAsterisks cs ([d] ++ ds)


-- matchAfterSet - consume set expression in the pattern and call GlobPattern
-- again with the remainder of the pattern



