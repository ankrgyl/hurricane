-- Copyright 2011 Ankur Goyal and other github contributors

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

import qualified Hurricane.Internal.Routes as R
import qualified Network.HTTP.Types as HTTP

import qualified Data.Map as M

import Control.Monad.Error

data TestResult = Pass | Fail String
                  deriving (Show, Eq)

trivialRoutes = [
  ("/a", "1", R.Full),
  ("/b/c", "2", R.Full),
  ("/d/e/f", "3", R.Full)]

trivialTests = 
  ("Trivial Tests", 
    [
      ("/a", [], R.FullMatch "1"),
      ("/b/c", [], R.FullMatch "2"),
      ("/d/e/f", [], R.FullMatch "3"),
      ("foo", [], R.FailMatch),
      ("/a/", [], R.FailMatch),
      ("/a/b", [], R.FailMatch),
      ("/b/k", [], R.FailMatch)
    ]
  )

trivialParamRoutes = 
  [
    ("/a/:par", "1", R.Full),
    ("/a/:foo/bar", "2", R.Full)
  ]

trivialParamTests = 
  ("Trivial Param Test",
    [
      ("/a/1", [("par", "1")], R.FullMatch "1"),
      ("/a/1/bar", [("foo", "1")], R.FullMatch "2")
    ]
  )

multParamRoutes =
  [
    ("/a/:b/c/:d", "1", R.Full)
  ]
multParamTests =
  ("Multiple Param Tests",
    [
      ("/a/b/c/d", [("b", "b"), ("d", "d")], R.FullMatch "1")
    ]
  )

overlappingParamRoutes =
  [
    ("/a/:k/foo/bar", "1", R.Full),
    ("/a/:k/foo/baz", "2", R.Full)
  ]
overlappingParamTests =
  ("Overlapping Param Tests",
    [
      ("/a/one/foo/bar", [("k", "one")], R.FullMatch "1"),
      ("/a/two/foo/baz", [("k", "two")], R.FullMatch "2")
    ]
  )

staticTreeRoutes =
  [
    ("/static", "S", R.Full),
    ("/static/bar", "S", R.Prefix)
  ]
staticTreeTests =
  ("Static Tree Tests",
    [
      ("/static/folder/file.txt", [], R.PrefixMatch "S" ["folder", "file.txt"])
    ]
  )

main = do
  runTestSuite trivialRoutes trivialTests
  runTestSuite trivialParamRoutes trivialParamTests
  runTestSuite multParamRoutes multParamTests
  runTestSuite overlappingParamRoutes overlappingParamTests
  runTestSuite staticTreeRoutes staticTreeTests
  print $ R.buildRouteTree staticTreeRoutes
  return ()


testRoutes routes tests = 
  case R.buildRouteTree routes of
    (Left e) -> Fail (show e)
    (Right tree) -> iter tests
      where 
        iter [] = Pass
        iter ((url, vars, handler) : testList) =
          let 
            (vars', handler') = R.matchRoute tree (HTTP.decodePathSegments url)
          in
            if (M.fromList vars, handler) == (M.fromList vars', handler')
              then iter testList
              else Fail ((show url) ++ "\t" ++ (show vars') ++ "\t" ++ (show handler') ++ "\n" ++ (show tree))

runTestSuite routes (name, tests) =
  case testRoutes routes tests of
    Pass -> putStrLn (name ++ " Passed!")
    Fail e -> putStrLn (name ++ " Failed with: " ++ e)


