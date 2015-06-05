module Language.Snx (decode) where

import Debug.Trace

type Xml = String
type Snx = String

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
decode = decodeElem 0 . lines

decodeElem :: Int -> [Snx] -> Xml
decodeElem nest snxs@(snx : rest)
  | countIndent snx /= nest                  = error "illegal indent"
  | ':' == head (unshift snx) = decodeTextElem nest snxs
  | otherwise                            = decodeTagElem nest snxs
decodeElem _ [] = undefined

decodeTextElem :: Int -> [Snx] -> Xml
decodeTextElem nest snxs@(snx : rest) =
  trace ("text elem: " ++ text) undefined
  where
    text = drop 2 snx
decodeTextElem _ [] = undefined

decodeTagElem :: Int -> [Snx] -> Xml
decodeTagElem nest snxs@(snx : rest) =
  shift nest $ "<" ++ tag ++ decodeInTagElem tag nest rest ++ "\n"
  where
    tag = unshift snx
decodeTagElem _ [] = undefined

decodeInTagElem :: String -> Int -> [Snx] -> Xml
decodeInTagElem tag nest snxs@(snx : rest) =
  case countIndent snx of
    i | i == nest + 2 -> "\n" ++ snx ++ decodeInTagElem tag nest rest
      | i == nest + 1 -> ">\n" ++ decodeElem (nest + 1) snxs ++ shift nest ("</" ++ tag ++ ">")
      | i == nest     -> trace "next elem" undefined
    _                 -> error "illegal indent"
decodeInTagElem _ _ [] = " />"

-- | count leading spaces
countIndent :: String -> Int
countIndent text =
  let
    n = length (takeWhile (== ' ') text)
  in
    if n `mod` indent == 0
    then n `div` indent
    else error "illegal indent"

indent :: Int
indent = 2

shift :: Int -> String -> String
shift n text = replicate (n * indent) ' ' ++ text

unshift :: String -> String
unshift = dropWhile (== ' ')
