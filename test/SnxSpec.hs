module SnxSpec where

import Test.Hspec
import Language.Snx

spec :: Spec
spec = do
  describe "normal" $ do
    it "single tag element" $ do
      decode "a" `shouldBe` "<a />\n"

    it "single tag element with an attribute" $ do
      decode "a\n    p\n" `shouldBe` "<a\n    p />\n"

    it "1 child tag elem" $ do
      decode "a\n  b\n" `shouldBe` "<a>\n  <b />\n</a>\n"

    it "2-nested tag elem" $ do
      decode "a\n  b\n    c\n" `shouldBe` "<a>\n  <b>\n    <c />\n  </b>\n</a>\n"
