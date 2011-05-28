{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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
--import qualified Network.HTTP.Types as HTTP

import qualified Hurricane.Web as W

{-- Each leaf of a RouteTree has an identifier, a mapping from next piece to subtree,
 -- and potentially a (last resort) parameter, with its own subtree.
 --
 -- This is redundant by design. Specifically, this uses heavy data structures, but the
 -- number of routes is generally small, and doesn't change with load. But, it needs to
 -- route very quickly because this is done on every request.
 --}
data RouteTree = RouteNode (HM.Map T.Text RouteTree) (Maybe RouteTree) (Maybe W.Handler)

data InvalidRoutes = TooManyParams String String
                   | PrefixOverlap String
                   | EmptyRoute 
                     deriving (Show, Typeable)

instance Exception InvalidRoutes

buildRouteTree :: [(B.ByteString, W.Handler)] -> RouteTree
matchRoute :: [T.Text] -> W.Handler

buildRouteTree = throw (TooManyParams "not" "implemented")

-- addRoute (<Text Route>, <handler>) <Previous Prefix> <Current Tree> -> <New Tree>
-- <Previous Prefix> is for debugging
addRoute :: ([T.Text], W.Handler) T.Text RouteTree -> RouteTree
addRoute ([], h) pre (RouteNode m p Nothing) = RouteNode m p h
addRoute ([], h) pre (RouteNode m p (Just h')) = throw PrefixOverlap pre
addRoute (r :: l, h) pre t = 
  -- Lookup r in t's map. If it exists, then continue to merge, with r as the prefix.
  -- If it doesn't, then add a new node, and then put it into the map.

extractParam :: T.Text -> Maybe T.Text 
extractParam s | s == "" = Nothing
extractParam s | T.head s == ':' = Just (T.tail s)
extractParam s = Nothing

matchRoute = throw (TooManyParams "not" "implemented")
