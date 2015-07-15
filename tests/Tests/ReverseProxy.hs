module Tests.ReverseProxy where

import Network.Socket

testCreateSocket :: IO Socket
testCreateSocket = TestCase $
  assertEqual
    "creates a socket."
    
