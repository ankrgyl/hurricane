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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

{-| 
Implementation of routing for Hurricane. Routes are of the form:

@
Route ::= FixedString | ParamString | \'\/\'
FixedString ::= (Char \\ \':\') (Char)\*
ParamString ::= ':' (Char)\*
@
-}

module Hurricane.Internal.Routes 
(
  RouteTree,
  emptyRouteTree,
  MatchSpec(..),
  MatchResult(..),
  InvalidRoutes(..),
  buildRouteTree,
  matchRoute,
) where

import Control.Monad.Error
import Control.Exception (Exception(..))

import Data.Typeable (Typeable)

import qualified Data.Text as T
import qualified Data.ByteString as B

import qualified Data.HashMap as HM

import qualified Network.HTTP.Types as HTTP

{-| 
We traverse a route tree by "consuming" individual components of a route, and
landing from RouteNode to RouteNode. Each node contains fix-named subtrees,
parameter subtrees, and a specification for the type of match it expects.
-}
data RouteTree h = RouteNode {
                    subtrees :: HM.Map T.Text (RouteTree h), -- ^ Mapping from name -> tree
                    params :: HM.Map T.Text (RouteTree h), -- ^ Mapping from param -> tree
                    matchSpec :: CompiledSpec h -- ^ Compiled version of match specification
                   } deriving (Show)

{-|
Empty route tree
-}
emptyRouteTree :: forall a . RouteTree a
emptyRouteTree = RouteNode { subtrees = HM.empty, params = HM.empty, matchSpec = CEmpty }

{-|
Various modes of failure in parsing routes.
-}
data InvalidRoutes = TooManyParams T.Text T.Text -- ^ Occurs when the specified parameter names 
                                                 -- make it impossible to pick a specific route.
                                                   
                   | DuplicateParamName T.Text   -- ^ Occurs when two routes have the same parameter name.

                   | PrefixOverlap T.Text        -- ^ Occurs when two prefixes overlap, as the current 
                                                 -- implementation is not prefix-greedy. This may change
                                                 -- in future versions.
                     deriving (Show, Typeable, Eq)
instance Error InvalidRoutes
instance Exception InvalidRoutes

{-|
Specification for the kind of match required for a current route.
-}
data MatchSpec = Full   -- ^ Matches exactly the specified route
               | Prefix -- ^ Matches any URL where the specified route is a prefix.
               deriving (Show)

-- Not exposed at all
data CompiledSpec h = CEmpty
                    | CFull h
                    | CPrefix h
                    deriving (Show)

{-|
Result of matching a URL to a Routing Tree. Match types correspond to MatchSpecs
-}
data MatchResult h = FullMatch h -- ^ Corresponds to a successful, full match,
                                 -- and provides the corresponding handler.
                   | PrefixMatch h [T.Text] -- ^ Corresponds to a succesful prefix match, and 
                                            -- includes both the handler and unmatched suffix
                   | FailMatch -- ^ Match failed
                   deriving (Show, Eq)


{-| 
Converts a routing specification into a Route Tree. Takes in a list of triples 
of the form @(routeString, handler, matchSpec)@ and tries to return a @'RouteTree'@, otherwise
an @'InvalidRoutes'@ on error.
-}
buildRouteTree :: [(B.ByteString, h, MatchSpec)] -> Either InvalidRoutes (RouteTree h)

{-| 
Matches a parsed route against a route tree, returning an association
list of matched parameters and a handler if one exists.

Takes in a 'RouteTree' and a parsed route, and returns a pair of @(paramList, matchResult)@
-}
matchRoute :: (RouteTree h) -> [T.Text] -> ([(T.Text, T.Text)], MatchResult h)

buildRouteTree routes =
  foldM (\t (routeString, h, spec) -> addRoute t (HTTP.decodePathSegments routeString, h, spec) "")
        emptyRouteTree
        routes

{-- addRoute takes a pre-split route (<Text Route>) like ["path", "to", "url"] and handler
 -- for this route (<handler>), which it builds into the passed in RouteTree <Current Tree>.
 -- For debugging, i.e. the exceptions it throws, we also pass around the piece of the route
 -- that we just consumed (<Previous Prefix>).
 --
 -- addRoute (<Text Route>, <handler>) <Previous Prefix> <Current Tree> -> <New Tree>
 --}
addRoute :: (RouteTree h) -> ([T.Text], h, MatchSpec) -> T.Text -> Either InvalidRoutes (RouteTree h)

addRoute t ([], h, spec) pre =
  -- Make sure that a handler doesn't already exist for this route
  case (matchSpec t, spec) of
    (CEmpty, Full) -> return (RouteNode { subtrees = subtrees t, params = params t, matchSpec = CFull h})
    (CEmpty, Prefix) -> return (RouteNode { subtrees = subtrees t, params = params t, matchSpec = CPrefix h})
    _ -> Left (PrefixOverlap pre)

addRoute t (r : l, h, mt) pre =
  case extractParam r of
    -- If it's not a param, then merge it with the appropriate subtree
    Nothing -> do
      t' <- case (matchSpec t) of 
              CPrefix _ -> Left (PrefixOverlap pre)
              _ -> return t
      subtree <- case HM.lookup r (subtrees t') of 
                  Nothing -> addRoute emptyRouteTree (l, h, mt) r
                  (Just t'') -> addRoute t'' (l, h, mt) r
      return RouteNode {
               subtrees = HM.insert r subtree (subtrees t'), 
               params = params t',
               matchSpec = matchSpec t'
             }
    Just p ->
      -- Otherwise, make sure it doesn't conflict with *any* param routes, and
      -- then add it to the appropriate subtree
      let 
        --Try and match against every existing param route to make sure no duplicates.
        dupRoute = HM.foldWithKey
                      (\p' -> \t' -> \dup -> case matchRoute t' l of
                                              (_, FailMatch) -> dup -- This subtree doesn't contain t'
                                              (_, _) -> Just (p, p')) -- Found a handler corresponding to this subtree!
                      Nothing
                      (params t)

      in
        case dupRoute of 
          (Just (d1, d2)) -> Left (TooManyParams d1 d2)
          Nothing -> do
            subtree <- case HM.lookup p (params t) of
                         Nothing -> addRoute emptyRouteTree (l, h, mt) r
                         (Just t') -> addRoute t' (l, h, mt) r

            return RouteNode {
                      subtrees = subtrees t,
                      params = HM.insert p subtree (params t),
                      matchSpec = matchSpec t
                    }

{-- Given a piece of a route, figures out whether or not it represents a param, and if so
 -- extracts the name of the param.
 --
 -- extractParam <Component> -> <Param>
 --}
extractParam :: T.Text -> Maybe T.Text 
extractParam s 
  | s == "" = Nothing
  | T.head s == ':' = Just (T.tail s)
  | otherwise = Nothing

matchRoute t [] = 
  case matchSpec t of
    CEmpty -> ([], FailMatch)
    CFull h -> ([], FullMatch h)
    CPrefix h -> ([], PrefixMatch h [])
matchRoute t (r : l) =
  case matchSpec t of
    CPrefix h -> ([], PrefixMatch h (r : l))
    _ ->
      case (HM.lookup r (subtrees t)) of 
        (Just t') -> matchRoute t' l
        Nothing ->
          case iter (HM.toList (params t)) of
            Nothing -> ([], FailMatch)
            Just ans -> ans
          where
            iter [] = Nothing
            iter ((p, t') : pl) =
              case matchRoute t' l of
                (_, FailMatch) -> iter pl
                (pmap, match) -> Just ((p, r) : pmap, match)
