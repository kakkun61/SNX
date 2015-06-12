module Language.Snx (decode) where

import Debug.Trace
import Control.Monad.State

type Xml = String
type Snx = String
type LineNum = Int
type Nest = Int

data Context = Context Nest LineNum [Snx]

type Decoder = State Context Xml

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
decode = fst . decodeSnx . (Context 0 1) . lines

decodeSnx :: Decoder
decodeSnx = do
  ctx@(Context nest ln snxs@(snx : rest)) <- get
  trace ("decodeSnx (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $
  go ""
  where
    go xml = do
      ctx <- get
      case ctx of
        Context _ _ [] -> return ""
        Context nest' _ _
          | nest == nest' -> xml' <- decodeElem
                             go (xml ++ xml')
          | otherwise     -> return xml

decodeElem :: Context -> (Xml, Context)
decodeElem ctx@(Context nest ln snxs@(snx : rest))
  | trace ("decodeElem (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") False = error "never come here"
  | countIndent ln snx /= nest = syntaxError "illegal indent (decodeElem)" ln snx
  | ':' == head (unshift snx)  = decodeTextElem ctx
  | otherwise                  = decodeTagElem ctx

decodeTextElem :: Context -> (Xml, Context)
decodeTextElem (Context nest ln snxs@(snx : rest)) = trace ("text elem: " ++ text) $
  (text ++ "\n", Context nest (ln + 1) rest)
  where
    text = shift nest $ drop 2 $ unshift snx

decodeTagElem :: Context -> (Xml, Context)
decodeTagElem (Context nest ln snxs@(snx : rest)) = trace ("decodeTagElem (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $
  (shift nest $ "<" ++ tag ++ xml', ctx')
  where
    tag = unshift snx
    (xml', ctx') = decodeInTagElem tag (Context nest (ln + 1) rest)

decodeInTagElem :: String -> Context -> (Xml, Context)
decodeInTagElem tag (Context nest ln snxs@(snx : rest)) = trace ("decodeInTagElem (Context " ++ tag ++ " " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $
  case countIndent ln snx of
    i | i == nest + 2 -> let (xml', ctx') = decodeInTagElem tag (Context nest (ln + 1) rest)
                         in ("\n" ++ snx ++ xml', ctx')
      | i == nest + 1 -> let (xml', Context _ ln' rest') = decodeSnx (Context (nest + 1) ln snxs)
                         in (">\n" ++ xml' ++ shift nest ("</" ++ tag ++ ">\n"), Context nest ln' rest')
      | i <= nest     -> (" />\n", Context i ln snxs)
      | otherwise     -> syntaxError ("illegal indent (actual " ++ (show i) ++ ", expexted " ++ (show nest) ++ " or " ++ (show (nest + 1)) ++ " or " ++ (show (nest + 2)) ++ ")") ln snx
decodeInTagElem tag ctx@(Context _ _ []) = (" />\n", ctx)

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
