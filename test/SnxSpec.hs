module SnxSpec where

import Test.Hspec
import Language.Snx

spec :: Spec
spec = do
  it "single tag element" $ do
    decode "a" `shouldBe` "<a />\n"

  it "single tag element with an attribute" $ do
    decode "a\n    p\n" `shouldBe` "<a\n    p />\n"
