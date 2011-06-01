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

data TestResult = Pass | Fail String
                  deriving (Show, Eq)

trivialRoutes = [
  ("/a", "1"),
  ("/b/c", "2"),
  ("/d/e/f", "3")]

trivialTests = 
  ("Trivial Tests", 
    [
      ("/a", [], Just "1"),
      ("/b/c", [], Just "2"),
      ("/d/e/f", [], Just "3"),
      ("foo", [], Nothing),
      ("/a/", [], Nothing),
      ("/a/b", [], Nothing),
      ("/b/k", [], Nothing)
    ]
  )

trivialParamRoutes = 
  [
    ("/a/:par", "1"),
    ("/a/:foo/bar", "2")
  ]

trivialParamTests = 
  ("Trivial Param Test",
    [
      ("/a/1", [("par", "1")], Just "1"),
      ("/a/1/bar", [("foo", "1")], Just "2")
    ]
  )

multParamRoutes =
  [
    ("/a/:b/c/:d", "1")
  ]
multParamTests =
  ("Multiple Param Tests",
    [
      ("/a/b/c/d", [("b", "b"), ("d", "d")], Just "1")
    ]
  )

overlappingParamRoutes =
  [
    ("/a/:k/foo/bar", "1"),
    ("/a/:k/foo/baz", "2")
  ]
overlappingParamTests =
  ("Overlapping Param Tests",
    [
      ("/a/one/foo/bar", [("k", "one")], Just "1"),
      ("/a/two/foo/baz", [("k", "two")], Just "2")
    ]
  )

main = do
  runTestSuite trivialRoutes trivialTests
  runTestSuite trivialParamRoutes trivialParamTests
  runTestSuite multParamRoutes multParamTests
  runTestSuite overlappingParamRoutes overlappingParamTests
  return ()


testRoutes routes tests = iter tests 
  where 
    tree = R.buildRouteTree routes
    iter [] = Pass
    iter ((url, vars, handler) : testList) =
      let 
        (vars', handler') = R.matchRoute (HTTP.decodePathSegments url) tree
      in
        if (M.fromList vars, handler) == (M.fromList vars', handler')
          then iter testList
          else Fail ((show url) ++ "\t" ++ (show vars') ++ "\t" ++ (show handler') ++ "\n" ++ (show tree))

runTestSuite routes (name, tests) =
  case testRoutes routes tests of
    Pass -> putStrLn (name ++ " Passed!")
    Fail e -> putStrLn (name ++ " Failed with " ++ e)


