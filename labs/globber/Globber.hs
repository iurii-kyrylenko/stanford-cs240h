module Globber (matchGlob) where

type GlobPattern = String
data GlobTerm = Literal Char    -- a
              | AnyChar         -- ?
              | Substring       -- *
              | FromSet [Char]  -- [abc]
              deriving (Eq)

matchGlob :: GlobPattern -> String -> Bool
matchGlob g  s  = matchParsed (parseGlob g) s

matchParsed :: [GlobTerm] -> String -> Bool
matchParsed [] "" = True
matchParsed [] _ = False
matchParsed [Substring] _ = True
matchParsed ((Literal x):xs) (c:cs) = (x == c) && (matchParsed xs cs)
matchParsed (AnyChar:xs) (_:cs) = matchParsed xs cs
matchParsed ((FromSet ss):xs) (c:cs) = (c `elem` ss) && (matchParsed xs cs)
matchParsed _ "" = False
matchParsed (Substring:xs) s = anySubstring (matchParsed xs) s

getSubstrings :: String -> [String]
getSubstrings [] = []
getSubstrings s@(_:xs) =  s : getSubstrings xs

anySubstring :: (String -> Bool) -> String -> Bool
anySubstring f = (any f) . getSubstrings

parseGlob :: GlobPattern -> [GlobTerm]
parseGlob "" = []
-- TO DO --
parseGlob _ = undefined

------
t1 = [ FromSet "0123456789"
     , Literal 'q'
     , Literal 'w'
     , Literal 'e'
     , Literal 'r'
     , Literal 't'
     , Literal 'y'
     , AnyChar
     , Literal 'i'
     ]
r1  = matchParsed t1 "5qwertyXi" -- True
r1' = matchParsed t1 "5qwertyi"  -- False

t2 = [ Substring
     , FromSet "0123456789"
     , Literal 'a'
     , Substring
     , Literal 'q'
     , Literal 'w'
     , Literal 'e'
     , Literal 'r'
     , Literal 't'
     , Literal 'y'
     , Substring
     , Literal 'y'
     , Substring
     ]
r2   = matchParsed t2 "7sada5awwwaqwertyy789" -- True
r2'  = matchParsed t2 "7ada5awwwaqwertyy789"  -- True
r2'' = matchParsed t2 "7sada5awwwaqwertYy789" -- False
