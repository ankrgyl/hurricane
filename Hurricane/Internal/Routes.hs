{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hurricane.Internal.Routes 
(
  RouteTree,
  InvalidRoutes,
  buildRouteTree,
  matchRoute,
) where

import Control.Exception
import Data.Typeable (Typeable)

import qualified Data.Text as T
import qualified Data.ByteString as B

import qualified Data.HashMap as HM
import qualified Data.HashSet as HS

import qualified Network.HTTP.Types as HTTP

import qualified Hurricane.Internal.Util as U

{-- We traverse a route tree by "consuming" individual components of a route, and
 -- landing from RouteNode to RouteNode. Each node contains fix-named subtrees,
 -- parameter subtrees, and potentially a handler.
 --
 -- This tree is "heavy" by design. We need to route requests really fast, and
 -- the number of routes is generally trivially small.
 --}
data RouteTree h = RouteNode {
                    subtrees :: HM.Map T.Text (RouteTree h), -- name -> tree
                    params :: HM.Map T.Text (RouteTree h), -- param -> tree
                    handler :: Maybe h
                   } deriving (Show)

emptyRouteTree = RouteNode { subtrees = HM.empty, params = HM.empty, handler = Nothing }

data InvalidRoutes = TooManyParams T.Text T.Text
                   | PrefixOverlap T.Text
                   | DuplicateParamName T.Text
                     deriving (Show, Typeable)
instance Exception InvalidRoutes

{-- Converts a [(<Route String>, <Handler>)] to a Route Tree 
 --
 -- buildRouteTree [(<Route String>, <Handler>)] -> <Route Tree>
 --}
buildRouteTree :: [(B.ByteString, h)] -> RouteTree h

{-- Matches a parsed route against a route tree, returning an association
 -- list of matched parameters and a handler if one exists.
 --
 -- matchRoute <Parsed Route> -> <Route Tree> -> [(<Param Assoc List>, <Val>), <Handler>]
 --}
matchRoute :: [T.Text] -> (RouteTree h) -> ([(T.Text, T.Text)], Maybe h)

buildRouteTree routes =
  foldr (\(routeString, h) -> \t -> addRoute (HTTP.decodePathSegments routeString, h) "" t)
        emptyRouteTree
        routes

{-- addRoute takes a pre-split route (<Text Route>) like ["path", "to", "url"] and handler
 -- for this route (<handler>), which it builds into the passed in RouteTree <Current Tree>.
 -- For debugging, i.e. the exceptions it throws, we also pass around the piece of the route
 -- that we just consumed (<Previous Prefix>).
 --
 -- addRoute (<Text Route>, <handler>) <Previous Prefix> <Current Tree> -> <New Tree>
 --}
addRoute :: ([T.Text], h) -> T.Text -> (RouteTree h) -> (RouteTree h)

addRoute ([], h) pre t =
  -- Make sure that a handler doesn't already exist for this route
  case (handler t) of
    Nothing -> RouteNode { subtrees = subtrees t, params = params t, handler = Just h }
    (Just h') -> throw (PrefixOverlap pre)

addRoute (r : l, h) pre t =
  case extractParam r of
    -- If it's not a param, then merge it with the appropriate subtree
    Nothing -> 
      let subtree = case HM.lookup r (subtrees t) of 
                      Nothing -> addRoute (l, h) r emptyRouteTree
                      (Just t') -> addRoute (l, h) r t'
      in
        RouteNode {
          subtrees = HM.insert r subtree (subtrees t), 
          params = params t,
          handler = handler t
        }
    Just p ->
      -- Otherwise, make sure it doesn't conflict with *any* param routes, and
      -- then add it to the appropriate subtree
      case (HM.lookup p (params t)) of 
        (Just _) -> throw (DuplicateParamName p)
        Nothing ->
          --Try and match against every existing param route to make sure no duplicates.
          let 
            dupRoute = HM.foldWithKey
                          (\p' -> \t' -> \dup -> case matchRoute l t' of
                                                  (_, Nothing) -> dup
                                                  (_, Just h) -> Just (p, p'))
                          Nothing
                          (params t)
            subtree = case HM.lookup r (params t) of
                         Nothing -> addRoute (l, h) r emptyRouteTree
                         (Just t') -> addRoute (l, h) r t'
          in
            case dupRoute of 
              (Just (d1, d2)) -> throw (TooManyParams d1 d2)
              Nothing -> RouteNode {
                          subtrees = subtrees t,
                          params = HM.insert p subtree (params t),
                          handler = handler t
                        }

{-- Given a piece of a route, figures out whether or not it represents a param, and if so
 -- extracts the name of the param.
 --
 -- extractParam <Component> -> <Param>
 --}
extractParam :: T.Text -> Maybe T.Text 
extractParam s | s == "" = Nothing
extractParam s | T.head s == ':' = Just (T.tail s)
extractParam s = Nothing

matchRoute [] t = ([], handler t)
matchRoute (r : l) t =
  case (HM.lookup r (subtrees t)) of 
    (Just t') -> matchRoute l t'
    Nothing ->
      case iter (HM.toList (params t)) of
        Nothing -> ([], Nothing)
        Just ans -> ans
      where
        iter [] = Nothing
        iter ((p, t') : pl) =
          case matchRoute l t' of
            (_, Nothing) -> iter pl
            (pmap, Just h) -> Just ((r, p) : pmap, Just h)
