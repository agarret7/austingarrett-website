{-# LANGUAGE OverloadedStrings #-}

module Util where

-- Standard
import Data.Set as Set hiding (foldr)
import Numeric.Natural
import System.Directory
import System.IO.Error

-- Other
import Text.Blaze.Html5 as Blaze
import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.String

-- Some useful things --

data WebState = AtHome | AtBlog | AtContact deriving Eq

-- Analogous function to concatMap, but for sets.
unionMap :: Ord b => (a -> Set b) -> [a] -> Set b
unionMap f = foldr (Set.union . f) Set.empty

-- Turns a function taking an inner HTML to empty constant.
emptyInner :: (Html -> Html) -> Html
emptyInner em = em ""

emptyScript :: Html
emptyScript = emptyInner script

emptyA :: Html
emptyA = emptyInner a

emptyDiv :: Html
emptyDiv = emptyInner Blaze.div

-- | Write Html to a path.
writeHtml :: FilePath -> Html -> IO ()
writeHtml fp h = writeFile fp (renderHtml h)

line :: String -> Natural -> String
line s n = (lines s) !! (fromIntegral . toInteger) n

whitePage :: Html -> Html
whitePage = main . article

subtitle :: Html -> Html
subtitle = Parent "subtitle" "<subtitle" "</subtitle>"

smallNav :: Html -> Html
smallNav = Parent "smallNav" "<smallNav" "</smallNav>"

preview :: Html -> Html
preview = Parent "preview" "<preview" "</preview>"
