{-# LANGUAGE OverloadedStrings #-}

module Templating where

-- Standard
import System.FilePath.Posix

-- Other
import Text.Blaze.Html5 as Blaze
import Text.Blaze.Html5.Attributes as BlazeAttr

-- Local
import Paths
import Util

-- Takes the current WebState, and some content, and generates an Html page.
formatTemplate :: WebState -> Html -> String -> Html
formatTemplate state content rootPath = docTypeHtml $ do

  -- Head Section --
  Blaze.head $ do
    meta ! charset "utf-8"
    meta ! name "viewport"
         ! BlazeAttr.content "width=device-width"
    Blaze.title "Austin Garrett"
    link ! href "https://fonts.googleapis.com/css?family=Open+Sans+Condensed:300|Sonsie+One"
         ! rel "stylesheet"
         ! type_ "text/css"
    link ! href (toValue (cssPath rootPath </> "style.css"))
         ! rel "stylesheet"
    emptyScript ! type_ "text/javascript"
                ! async ""
                ! src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"
    emptyScript ! async ""
                ! src "https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"
    script ! type_ "text/x-mathjax-config" $
      "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}});"

  -- Top Navigation --
  let
    titleElements isMain = do

      let mainString = if isMain then "main" else "" :: String
      let blogString = if state == AtBlog then "active " else "" :: String
      let contactString = if state == AtContact then "active " else "" :: String

      let blogHtml = li $ a ! href (toValue $ blogPath rootPath </> "index.html")
                            ! class_ (toValue $ blogString ++ mainString) $
                              "Blog"
      let contactHtml = li $ a ! href (toValue $ contactPath rootPath)
                            ! class_ (toValue $ contactString ++ mainString) $
                              "Contact"
      let resumeHtml = li $ a ! href (toValue $ resumePath rootPath)
                            ! class_ (toValue $ mainString) $
                              "Resume"

      -- Needs reversed order for weird HTML quirk.
      if isMain then do
        resumeHtml; contactHtml; blogHtml
      else do
        blogHtml; contactHtml; resumeHtml

  nav $ do
    ul $ do
      li $ a ! class_ "name"
             ! href (toValue $ rootPath </> "home.html") $
               "Austin Garrett"
      li $ do
        Blaze.div ! class_ "mobile" $ do
          img ! src (toValue $ mediaPath rootPath </> "menu.png")
              ! onclick "myFunction()"
              ! class_ "dropbtn"
          Blaze.div ! BlazeAttr.id "myDropdown"
                    ! class_ "dropdown-content" $ do
            ul $ titleElements False
        emptyScript ! src (toValue (jsPath rootPath </> "dropdown.js"))
      titleElements True

  -- Body Section --
  body $ do
    content
    (footer . p) $ do
      "© Copyright 2017 by Austin Garrett. Powered by "
      a ! href "https://hackage.haskell.org/package/blaze-html" $ "Blaze HTML"
      ". This website is "
      a ! href "https://github.com/agarret7/austingarrett-website" $ "open source"
      "."
