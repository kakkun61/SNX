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

    it "2 child tag elem" $ do
      decode "a\n  b\n  c\n" `shouldBe` "<a>\n  <b />\n  <c />\n</a>\n"

    -- a
    --     p
    --   b
    --       q
    --     c
    --         s
    --     d
    --   e
    -- ↓
    -- <a
    --     p>
    --   <b
    --       q
    --       r>
    --     <c
    --         s />
    --     <d />
    --   </b>
    --   <e />
    -- </a>
    it "<a p><b q r><c s /><d /></b><e /></a>" $ do
      decode "a\n    p\n  b\n      q\n      r\n    c\n        s\n    d\n  e\n"
        `shouldBe`
          "<a\n    p>\n  <b\n      q\n      r>\n    <c\n        s />\n    <d />\n  </b>\n  <e />\n</a>\n"

    -- <a>
    --   Hi
    -- </a>
    it "text elem" $ do
      decode "a\n  : Hi\n" `shouldBe` "<a>\n  Hi\n</a>\n"
