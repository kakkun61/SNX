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
decode = fst . runState decodeSnx . (Context 0 1) . lines

decodeSnx :: Decoder
decodeSnx = do
  Context nest ln snxs@(snx : rest) <- get
  trace ("decodeSnx (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $ go nest ""
  where
    go :: Nest -> Xml -> Decoder
    go nest xml = do
      ctx <- get
      case ctx of
        Context _ _ [] -> return xml
        Context nest' _ _
          | nest == nest' -> do
                               xml' <- decodeElem
                               go nest (xml ++ xml')
          | otherwise     -> return xml

decodeElem :: Decoder
decodeElem = do
  Context nest ln snxs@(snx : rest) <- get
  trace ("decodeElem (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $ return ()
  if countIndent ln snx /= nest
    then syntaxError "illegal indent (decodeElem)" ln snx
    else if ':' == head (unshift snx)
      then decodeTextElem
      else decodeTagElem

decodeTextElem :: Decoder
decodeTextElem = do
  Context nest ln snxs@(snx : rest) <- get
  let text = shift nest $ drop 2 $ unshift snx
  trace ("text elem: " ++ text) $ return ()
  nextline
  return $ text ++ "\n"

decodeTagElem :: Decoder
decodeTagElem = do
  Context nest ln snxs@(snx : rest) <- get
  trace ("decodeTagElem (Context " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $ return ()
  let tag = unshift snx
  nextline
  xml <- decodeInTagElem tag
  return $ shift nest $ "<" ++ tag ++ xml

decodeInTagElem :: String -> Decoder
decodeInTagElem tag = do
  ctx <- get
  case ctx of
    Context nest ln snxs@(snx : rest) -> trace ("decodeInTagElem (Context " ++ tag ++ " " ++ (show nest) ++ " " ++ (show ln) ++ " \"" ++ snx ++ "...\")") $ do
      case countIndent ln snx of
        i | i == nest + 2 -> do
              nextline
              xml <- decodeInTagElem tag
              return $ "\n" ++ snx ++ xml
          | i == nest + 1 -> do
              modifyNest (+ 1)
              xml <- decodeSnx
              return $ ">\n" ++ xml ++ shift nest ("</" ++ tag ++ ">\n")
          | i <= nest -> do
              modifyNest $ const i
              return " />\n"
          | otherwise ->
              syntaxError ("illegal indent (actual " ++ (show i) ++ ", expexted " ++ (show nest) ++ " or " ++ (show (nest + 1)) ++ " or " ++ (show (nest + 2)) ++ ")") ln snx
    Context _ _ [] -> return " />\n"

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

nextline :: State Context ()
nextline = state $ \(Context nest ln (snx:snxs)) -> ((), Context nest (ln + 1) snxs)

modifyNest :: (Nest -> Nest) -> State Context ()
modifyNest f = state $ \(Context nest ln snxs) -> ((), Context (f nest) ln snxs)
