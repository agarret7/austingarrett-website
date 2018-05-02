module Paths where

import System.FilePath.Posix

-- rootPath :: String
-- -- rootPath = "~" </> "web_scripts" </> "personal"
-- rootPath = "/" </> "home" </> "agarret7" </> "programming" </> "web" </> "haskell-website"

backendPath :: String -> String
backendPath rootPath = rootPath </> "backend"

blogPath :: String -> String
blogPath rootPath = rootPath </> "blog"

resumePath :: String -> String
resumePath rootPath = rootPath </> "GarrettResume.pdf"

contactPath :: String -> String
contactPath rootPath = rootPath </> "contact.html"

cssPath :: String -> String
cssPath rootPath = rootPath </> "backend" </> "css"

jsPath :: String -> String
jsPath rootPath = backendPath rootPath </> "js"

mediaPath :: String -> String
mediaPath rootPath = backendPath rootPath </> "media"

contentPath :: String -> String
contentPath rootPath = backendPath rootPath </> "content"

postsPath :: String -> String
postsPath rootPath = contentPath rootPath </> "posts"
