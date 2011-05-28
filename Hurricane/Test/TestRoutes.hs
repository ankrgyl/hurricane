{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import qualified Hurricane.Internal.Routes as R
import qualified Network.HTTP.Types as HTTP

data TestResult = Pass | Fail T.Text
                  deriving (Show, Eq)

trivialRoutes = [
  ("/a", "1"),
  ("/b/c", "2"),
  ("/d/e/f", "3")]

trivialTests = ("Trivial Tests", 
  [("/a", [], Just "1"),
   ("/b/c", [], Just "2"),
   ("/d/e/f", [], Just "3")] 
  )


testRoutes routes tests = iter tests 
  where 
    tree = R.buildRouteTree routes
    iter [] = Pass
    iter ((url, vars, handler) : testList)
      | R.matchRoute (HTTP.decodePathSegments url) tree == (vars, handler) = iter testList
      | otherwise = Fail (E.decodeUtf8 url)

runTestSuite routes (name, tests) =
  case testRoutes routes tests of
    Pass -> putStrLn (name ++ " Passed!")
    Fail e -> putStrLn (name ++ " Failed with " ++ (show e))

main = do
  runTestSuite trivialRoutes trivialTests
  return ()
