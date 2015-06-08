module Language.Snx (decode) where

import Debug.Trace

type Xml = String
type Snx = String
type LineNum = Int

-- |
-- 下記コードから
-- @
-- a
--   b
--     c
--   d
-- @
-- 下記 XML を生成します。
-- @
-- \<a>
--   \<b>
--     \<c />
--   \</b>
--   \<d />
-- \</a>
-- @
-- >>> decode "a\n  |b\n    c\n  d"
-- "<a><b><c /></b><d />< a>"
decode :: Snx -> Xml
decode = decodeElem 0 1 . lines

decodeElem :: Int -> LineNum -> [Snx] -> Xml
decodeElem nest ln snxs@(snx : rest)
  | trace ("decodeElem " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\"") False = error "never come here"
  | countIndent ln snx /= nest = syntaxError "illegal indent (decodeElem)" ln snx
  | ':' == head (unshift snx)  = decodeTextElem nest ln snxs
  | otherwise                  = decodeTagElem nest ln snxs
decodeElem _ _ [] = ""

decodeTextElem :: Int -> LineNum -> [Snx] -> Xml
decodeTextElem nest ln snxs@(snx : rest) =
  trace ("text elem: " ++ text) undefined
  where
    text = drop 2 snx
decodeTextElem _ _ [] = undefined

decodeTagElem :: Int -> LineNum -> [Snx] -> Xml
decodeTagElem nest ln snxs@(snx : rest) = trace ("decodeTagElem " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\"") $
  shift nest $ "<" ++ tag ++ decodeInTagElem tag nest (ln + 1) rest
  where
    tag = unshift snx
decodeTagElem _ _ [] = undefined

decodeInTagElem :: String -> Int -> LineNum -> [Snx] -> Xml
decodeInTagElem tag nest ln snxs@(snx : rest) = trace ("decodeInTagElem " ++ tag ++ " " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\"") $
  case countIndent ln snx of
    i | i == nest + 2 -> "\n" ++ snx ++ decodeInTagElem tag nest (ln + 1) rest
      | i == nest + 1 -> ">\n" ++ decodeElem (nest + 1) ln snxs ++ shift nest ("</" ++ tag ++ ">\n")
      | i <= nest     -> " />\n" ++ decodeElem i ln snxs
      | otherwise     -> syntaxError ("illegal indent (actual " ++ (show i) ++ ", expexted " ++ (show nest) ++ " or " ++ (show (nest + 1)) ++ " or " ++ (show (nest + 2)) ++ ")") ln snx
decodeInTagElem _ _ _ [] = " />\n"

-- | count leading spaces
countIndent :: LineNum -> String -> Int
countIndent ln text =
  let
    n = length (takeWhile (== ' ') text)
  in
    if n `mod` indent == 0
    then n `div` indent
    else syntaxError ("illegal indent (not any multiples of " ++ (show indent)) ln text

indent :: Int
indent = 2

shift :: Int -> String -> String
shift n text = replicate (n * indent) ' ' ++ text

unshift :: String -> String
unshift = dropWhile (== ' ')

syntaxError :: String -> LineNum -> Snx -> a
syntaxError msg ln snx = error $ msg ++ " at " ++ (show ln) ++ " (" ++ snx ++ ")"
