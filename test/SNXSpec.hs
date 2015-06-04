module SnxSpec where

import Test.Hspec
import Language.Snx

spec :: Spec
spec = do
  it "single tag element" $ do
    decode "a" `shouldBe` "<a />\n"
