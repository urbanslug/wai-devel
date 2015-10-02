module Tests.ReverseProxy where

import Network.Socket
import Devel.ReverseProxy
import Test.Hspec

main :: IO ()
main = hspec $ before_ (createSocket 5445) $ do
  describe "Ports" $ do
    it "Confirms that socket 4234 is free" $ do
      isPortFree <- checkPort 4234 
      isPortFree `shouldBe` True
    it "Successfull port cycling" $ do
      port <- cyclePorts 5445
      port `shouldBe` 5446


