{-# LANGUAGE OverloadedStrings #-}

-- Other
import System.FilePath.Posix

-- Blaze
import Text.Blaze.Html5 hiding (main, map)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String

-- Local
import Paths
import Templating
import Util
import Blog

-- Main HTML Generation --

main :: IO ()
main = do
  -- Home
  homeContent <- readFile $ "." </> "content" </> "home.html"
  writeHtml (".." </> "home.html") (formatTemplate AtHome (whitePage $ preEscapedToHtml homeContent) ".")

  -- Contact
  contactContent <- readFile $ "." </> "content" </> "contact.html"
  writeHtml (".." </> "contact.html") (formatTemplate AtContact (whitePage $ preEscapedToHtml contactContent) ".")

  -- Blog
  blog <- readBlog $ postsPath ".."
  let writeIdx (path, htmlIdx) = writeHtml (blogPath ".." </> path) $ formatTemplate AtBlog htmlIdx ".." in do

    -- Main Index
    mapM_ writeIdx (blogToIndexHtml blog)

    -- Categories
    mapM_ writeIdx (blogToCatHtml blog)

  -- Posts
  let writePost (path, htmlPost) = writeHtml (blogPath ".." </> path) $ formatTemplate AtBlog htmlPost ".." in
    mapM_ writePost (blogToPostHtml blog)
