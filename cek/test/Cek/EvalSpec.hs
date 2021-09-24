module Cek.EvalSpec where

import Cek
import Test.Hspec

spec :: Spec
spec = describe "evaluation of cek machine" $ do
  context "evaluation of cek machine" $ do
    it "A variable of course should be terminated" $ do
      let exp = Ref "y"
      isFinal (evaluate exp) `shouldBe` True
    it "An identity function applied with a variable should be terminated" $ do
      let exp = Lam ("x" :=> Ref "x") :@ Ref "y"
      isFinal (evaluate exp) `shouldBe` True
