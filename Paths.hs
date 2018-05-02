module Paths where

import System.FilePath.Posix

rootPath :: String
rootPath = joinPath ["/", "home", "agarret7", "programming", "web", "haskell-website"]

backendPath = rootPath </> "backend"

cssPath :: String
cssPath = backendPath </> "css"

blogPath :: String
blogPath = rootPath </> "blog"

resumePath :: String
resumePath = rootPath </> "GarrettResume.pdf"

contactPath :: String
contactPath = rootPath </> "contact.html"

contentPath :: String
contentPath = backendPath </> "content"

jsPath :: String
jsPath = backendPath </> "js"

postsPath :: String
postsPath = contentPath </> "posts"

mediaPath :: String
mediaPath = backendPath </> "media"
