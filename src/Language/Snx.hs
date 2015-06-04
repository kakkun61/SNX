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
  | ind snx /= 2 * nest                  = fail "illegal indent"
  | ':' == head (dropWhile (== ' ') snx) = decodeTextElem nest snxs
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
  if length rest == 0
  then
    trace ("closing tag: " ++ tag) undefined
  else
    decodeInTagElem tag nest rest
  where
    tag = snx
decodeTagElem _ [] = undefined

decodeInTagElem :: String -> Int -> [Snx] -> Xml
decodeInTagElem tag nest snxs@(snx : rest) =
  case ind snx of
    i | i == 2 * (nest + 2) -> trace "attr" undefined
      | i == 2 * (nest + 1) -> trace "sub elem" undefined
      | i == 2 * nest       -> trace "next elem" undefined
    otherwise               -> fail "illegal indent"
decodeInTagElem _ _ [] = undefined

-- | count leading spaces
ind :: String -> Int
ind = length . takeWhile (== ' ')
