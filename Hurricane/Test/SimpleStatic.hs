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
import qualified Network.Wai.Handler.Warp as Warp
import qualified Hurricane.Web as Web

options = Web.ApplicationOptions {
            Web.static_url_prefix = "static",
            Web.static_path = "Hurricane"
          }

routes = []

app = Web.makeApplication options routes

main = Warp.run 3000 app
