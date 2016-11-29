module Globber (matchGlob) where

import Data.List (nub)

type GlobPattern = String
data GlobTerm = Literal Char    -- a
              | AnyChar         -- ?
              | Substring       -- *
              | FromSet [Char]  -- [abc]
              deriving (Eq, Show)

matchGlob :: GlobPattern -> String -> Bool
matchGlob g s = matchParsed (parseGlob g []) s

-- match parsed --

matchParsed :: [GlobTerm] -> String -> Bool
matchParsed [] "" = True
matchParsed [] _ = False
matchParsed [Substring] _ = True
matchParsed ((Literal x):xs) (c:cs) = (x == c) && (matchParsed xs cs)
matchParsed (AnyChar:xs) (_:cs) = matchParsed xs cs
matchParsed ((FromSet ss):xs) (c:cs) = (c `elem` ss) && (matchParsed xs cs)
matchParsed _ "" = False
matchParsed (Substring:xs) s = anySubstring (matchParsed xs) s

-- parse glob

parseGlob :: GlobPattern -> [GlobTerm] -> [GlobTerm]
parseGlob "" s = s
parseGlob ('*':xs) s = parseSubstring xs s           -- Substring
parseGlob ('?':xs) s = parseGlob xs (s ++ [AnyChar]) -- AnyChar
parseGlob ('\\':xs) s = parseEscaped xs s            -- Escaped literal
parseGlob ('[':xs) s = parseSet xs s []              -- FromSet
parseGlob (x:xs) s = parseGlob xs (s ++ [Literal x]) -- Literal

parseSubstring :: GlobPattern -> [GlobTerm] -> [GlobTerm]
parseSubstring ('*':xs) s = parseSubstring xs s
parseSubstring xs s = parseGlob xs (s ++ [Substring])

parseEscaped :: GlobPattern -> [GlobTerm] -> [GlobTerm]
parseEscaped [] s = s ++ [Literal '\\']
parseEscaped (x:xs) s = parseGlob xs (s ++ [Literal x])

parseSet :: GlobPattern -> [GlobTerm] -> [Char] -> [GlobTerm]
parseSet [] s _ = s ++ [FromSet []]
parseSet (']':xs) s t = parseGlob xs (s ++ [FromSet t])
parseSet (a:'-':']':xs) s t = parseSet ('-':']':xs) s (nub (a:t)) -- open range set
parseSet (a:'-':c:xs) s t = parseSet xs s (nub ([a..c] ++ t))     -- range set
parseSet (x:xs) s t = parseSet xs s (nub (x:t))

-- utilities --

getSubstrings :: String -> [String]
getSubstrings [] = []
getSubstrings s@(_:xs) =  s : getSubstrings xs

anySubstring :: (String -> Bool) -> String -> Bool
anySubstring f = (any f) . getSubstrings

-- inner tests --

-- t1 = [ FromSet "0123456789"
--      , Literal 'q'
--      , Literal 'w'
--      , Literal 'e'
--      , Literal 'r'
--      , Literal 't'
--      , Literal 'y'
--      , AnyChar
--      , Literal 'i'
--      ]
-- r1  = matchParsed t1 "5qwertyXi" -- True
-- r1' = matchParsed t1 "5qwertyi"  -- False

-- t2 = [ Substring
--      , FromSet "0123456789"
--      , Literal 'a'
--      , Substring
--      , Literal 'q'
--      , Literal 'w'
--      , Literal 'e'
--      , Literal 'r'
--      , Literal 't'
--      , Literal 'y'
--      , Substring
--      , Literal 'y'
--      , Substring
--      ]
-- r2   = matchParsed t2 "7sada5awwwaqwertyy789" -- True
-- r2'  = matchParsed t2 "7ada5awwwaqwertyy789"  -- True
-- r2'' = matchParsed t2 "7sada5awwwaqwertYy789" -- False

-- r3 = parseGlob "q[123abb2*?]*" []
-- r4 = parseGlob "q[123abb2*?*" []

-- r5 = parseGlob "[a-fd-h]" []
-- r6 = parseGlob "[qwertyu-" []
-- r7 = parseGlob "[qwertyu-]" []
-- r8 = parseGlob "[-qwertyu]" []

