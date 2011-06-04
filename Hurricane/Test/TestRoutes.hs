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

data BuildResult = BuildFail R.InvalidRoutes | BuildPass
                   deriving (Show, Eq)
data TestResult = Pass | Fail String
                  deriving (Show, Eq)


emptyRoutes = ("Empty Routes", [], BuildPass)
emptyTests = ("Empty Tests", [])

trivialRoutes = 
  ("Trivial Routes",
    [
      ("/a", "1", R.Full),
      ("/b/c", "2", R.Full),
      ("/d/e/f", "3", R.Full)
    ],
    BuildPass
  )
    

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
  ("Trivial Param Routes",
    [
      ("/a/:par", "1", R.Full),
      ("/a/:foo/bar", "2", R.Full)
    ],
    BuildPass
  )

trivialParamTests = 
  ("Trivial Param Tests",
    [
      ("/a/1", [("par", "1")], R.FullMatch "1"),
      ("/a/1/bar", [("foo", "1")], R.FullMatch "2")
    ]
  )

multParamRoutes =
  ("Multiple Param Routes",
    [
      ("/a/:b/c/:d", "1", R.Full)
    ],
    BuildPass
  )
multParamTests =
  ("Multiple Param Tests",
    [
      ("/a/b/c/d", [("b", "b"), ("d", "d")], R.FullMatch "1")
    ]
  )

overlappingParamRoutes =
  ("Overlapping Param Routes",
    [
      ("/a/:k/foo/bar", "1", R.Full),
      ("/a/:k/foo/baz", "2", R.Full)
    ],
    BuildPass
  )
overlappingParamTests =
  ("Overlapping Param Tests",
    [
      ("/a/one/foo/bar", [("k", "one")], R.FullMatch "1"),
      ("/a/two/foo/baz", [("k", "two")], R.FullMatch "2")
    ]
  )

staticTreeRoutes =
  ("Static Tree Routes",
    [
      ("/static", "S", R.Prefix),
      ("/longer/static", "L", R.Prefix)
    ],
    BuildPass
  )
staticTreeTests =
  ("Static Tree Tests",
    [
      ("/static/folder/file.txt", [], R.PrefixMatch "S" ["folder", "file.txt"]),
      ("/longer/static/folder/file.txt", [], R.PrefixMatch "L" ["folder", "file.txt"])
    ]
  )

prefixParamRoutes =
  ("Prefix Param Routes",
    [
      ("/static", "S", R.Prefix),
      ("/:param/static", "P", R.Prefix)
    ],
    BuildPass
  )
prefixParamTests =
  ("Prefix Param Tests",
    [
      ("/static/folder/file.txt", [], R.PrefixMatch "S" ["folder", "file.txt"]),
      ("/open/static/folder/file.txt", [("param", "open")], R.PrefixMatch "P" ["folder", "file.txt"])
    ]
  )

conflictingParamRoutes =
  ("Conflicting Param Routes",
    [
      ("/a/:p/b/c", "A", R.Full),
      ("/a/:k/b/c", "B", R.Full)
    ],
    BuildFail (R.TooManyParams "k" "p")
  )

overlappingPrefixRoutes =
  ("Overlapping Prefix Routes",
    [
      ("/a", "A", R.Prefix),
      ("/a/b", "B", R.Prefix)
    ],
    BuildFail (R.PrefixOverlap "a")
  )


main = do
  runTestSuite trivialRoutes trivialTests
  runTestSuite trivialParamRoutes trivialParamTests
  runTestSuite multParamRoutes multParamTests
  runTestSuite overlappingParamRoutes overlappingParamTests
  runTestSuite staticTreeRoutes staticTreeTests
  runTestSuite prefixParamRoutes prefixParamTests
  runTestSuite conflictingParamRoutes emptyTests
  runTestSuite overlappingParamRoutes emptyTests
  return ()


testRoutes (routeName, routes, buildResult) tests = 
  case R.buildRouteTree routes of
    (Left e) ->
      case buildResult of BuildPass -> Fail (show e)
                          (BuildFail e') ->
                            if e == e'
                               then Pass
                               else Fail (show e)
    (Right tree) -> 
      case buildResult of 
        (BuildFail e) -> Fail ("build should have failed with " ++ (show e))
        BuildPass ->
          iter tests
          where 
            iter [] = Pass
            iter ((url, vars, handler) : testList) =
              let 
                (vars', handler') = R.matchRoute tree (HTTP.decodePathSegments url)
              in
                if (M.fromList vars, handler) == (M.fromList vars', handler')
                  then iter testList
                  else Fail ((show url) ++ "\t" ++ (show vars') ++ "\t" ++ (show handler') ++ "\n" ++ (show tree))

padToLength len str = (take (len - (length str)) (repeat ' ')) ++ str
routesAndTestToString (name, _, _) testName = (padToLength 30 name) ++ "\t" ++ (padToLength 30 testName)
    
runTestSuite routes (name, tests) =
  case testRoutes routes tests of
    Pass -> putStrLn ((routesAndTestToString routes name) ++ "\t\t\tPassed!")
    Fail e -> putStrLn ((routesAndTestToString routes name) ++ "\t\t\tFailed!\t\t" ++ e)


