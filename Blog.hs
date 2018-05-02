{-# LANGUAGE OverloadedStrings #-}

module Blog where

-- Standard
import Control.Monad (forM_)
import Control.Monad.Zip (mzip)
import Data.Char
import Data.Dates as Dates
import Data.List as List hiding (map)
import Data.List.Split
import Data.Maybe
import Data.Set as Set hiding (map)
import Numeric.Natural
import System.Directory
import System.FilePath.Posix
import qualified Data.Map.Lazy as Map

-- Other
import Text.Blaze.Internal
import Text.Blaze.Html5 as Blaze hiding (map)
import Text.Blaze.Html5.Attributes as BlazeAttr

-- Local
import Util
import Templating
import Paths

type Category = String
data Post = Post Natural String deriving Show
type Blog = [Post]
data Index = Index Natural [Post]

indexPosts :: [Post] -> [Index]
indexPosts x = zipWith Index (map fromIntegral [len,len-1..0]) (chunksOf 10 x)
  where len = quot (length x) 10

compareDates :: String -> String -> Ordering
compareDates s s' = case getDate s of
  Left _   -> error "Invalid date in posts."
  Right dt -> case getDate s' of
    Left _    -> error "Invalid date in posts."
    Right dt' -> compare dt' dt
  where dateOrigin = Dates.DateTime 0 0 0 0 0 0
        getDate s = Dates.parseDate dateOrigin (line s 1)

readBlog :: FilePath -> IO Blog
readBlog dir = do
  rel_dirs <- (listDirectory dir)
  let abs_dirs = map (dir </>) $ rel_dirs
  content <- mapM readFile abs_dirs
  let sortedContent = sortBy compareDates content
  let len = length sortedContent in 
    return $ zipWith Post (map fromIntegral [len-1,len-2..0]) sortedContent

getPostCategories :: Post -> Set Category
getPostCategories (Post _ p) = fromList . splitOn ", " . last . lines $ p

-- | Turns a blog into a set of categories.
getCategories :: Blog -> Set Category
getCategories = unionMap getPostCategories

catToUrl' :: Category -> String
catToUrl' c = (map (\l -> if l == ' ' then '_' else toLower l) c)

-- | Turns category into navigable URL.
catToUrl :: Category -> String
catToUrl c = (catToUrl' c) ++ "_0.html"

getPostHeader :: Post            -- ^ Post to get title/date/number of comments from.
              -> Bool            -- ^ Whether or not to link to post from title.
              -> (Html -> Html)  -- ^ Header element to surround the thing.
              -> Html            -- ^ title/date/number of comments
getPostHeader post@(Post n p) linked em = em $ do
  -- Title
  (if linked then
    a ! href (toValue $ (show n) ++ ".html") $ preEscapedToHtml (List.head pLines)
  else
    preEscapedToHtml (List.head pLines));
  br
  -- Date and Comments
  subtitle $ do
    img ! height "10" ! width "10" ! src (toValue $ mediaPath </> "clock.png")
    preEscapedToHtml $ " " ++ (pLines !! 1) ++ " | "
    img ! height "10" ! width "10" ! src (toValue $ mediaPath </> "speech_bubble.png")
    a ! href (toValue $ show n ++ ".html#disqus_thread")
      ! BlazeAttr.content (toValue $ "data-disqus-identifier=" ++ show n) $ " Comments"
  where pLines = lines p

getStub :: Post -> Html
getStub post@(Post n p) = Blaze.p $ do

  -- Title, date, and comments
  getPostHeader post True h3

  -- Paragraph preview
  Blaze.p $ do
    let preview = firstParagraph post
    preEscapedToHtml preview
    " "
    a ! href (toValue $ (show n) ++ ".html") $
      preEscapedToHtml $ "[" ++ (show $ wc p - wc preview) ++ " more words]"

  -- Categories
  let categories = getPostCategories post in
    h3 $ do
      subtitle $ do
        "Posted in "
        let orderedCategories = toList categories
        mapM_ (\c -> do catLink c; ", ") $ init $ orderedCategories
        subtitle $ do catLink $ last orderedCategories
  where catLink :: Category -> Html
        catLink c = a ! href (toValue $ catToUrl c) $ preEscapedToHtml c

        firstParagraph :: Post -> String
        firstParagraph (Post _ p) = (splitOn "\n\n" p) !! 1

        wc :: String -> Natural
        wc = fromIntegral . length . words

blogHeader :: Html
blogHeader = h1 ! class_ "raisedbox" $ do
  "\\(\\alpha\\beta\\gamma\\)"
  br
  subtitle "The blog of Austin Garrett"

-- | Generates an aside from a blog.
aside :: Blog -> Html
aside b = (Blaze.aside . ul) $ do
  li $ h3 "Featured Posts"
  li "Placeholder to be replaced."
  -- TODO: Implement featured posts functionality.

  li $ h3 "Categories"
  forM_ (getCategories b) (\c -> li $ a ! href (toValue $ catToUrl c) $ toHtml ("» " ++ c))

blogToCategories :: Blog -> Map.Map Category [Post]
blogToCategories []     = Map.empty
blogToCategories (p:ps) = insertMultipleWith (++) cs (repeat $ [p]) (blogToCategories ps)
  where insertMultipleWith :: Ord k => (a -> a -> a) -> [k] -> [a] -> Map.Map k a -> Map.Map k a
        insertMultipleWith _ []     _      m = m
        insertMultipleWith _ _      []     m = m
        insertMultipleWith f (k:ks) (a:as) m = Map.insertWith f k a (insertMultipleWith f ks as m)

        cs :: [Category]
        cs = toList $ getPostCategories p

data Navigator = Navigator Natural       -- ^ Current location number
                           Natural       -- ^ Maximum number of pages
                           String        -- ^ Prefix for HTML url
                           (Maybe String)  -- ^ Text for previous navigation
                           (Maybe String)  -- ^ Text for next navigation

-- | Gets navigation for bottom of page.
getPageNavigation :: Navigator -> Html
getPageNavigation (Navigator n numPages prefix prevText nextText) = smallNav $ do

  -- Forward Navigation
  case nextText of
    Nothing -> ""
    Just nextText' -> 
      a ! href (toValue (prefix ++ (show $ fromIntegral n + 1) ++ ".html")) ! class_ "right index" $
        preEscapedToHtml $ nextText' ++ " »"

  -- Backward Navigation
  case prevText of
    Nothing -> ""
    Just prevText' ->
      a ! href (toValue (prefix ++ (show $ fromIntegral n - 1) ++ ".html")) ! class_ "left index" $
        preEscapedToHtml $ "« " ++ prevText'

blogToCatHtml :: Blog -> [(String, Html)]
blogToCatHtml b = concatMap blogToCatHtml' (Map.toList $ blogToCategories b)
  where blogToCatHtml' (c, posts) = [(catToUrl' c ++ "_" ++ show n ++ ".html",
          Blaze.main $ do
            preview $ do
              blogHeader
              mapM_ getStub chunkedPosts
              let numPages = fromIntegral $ length indices
              getPageNavigation (Navigator n numPages (catToUrl' c ++ "_") (if n > 0 then Just "Older Posts" else Nothing) (if n < numPages - 1 then Just "Newer Posts" else Nothing))
              br
            emptyScript ! BlazeAttr.id "dsq-count-src"
                      ! src "//austingarrett.disqus.com/count.js"
                      ! async ""
            Blog.aside b)
          | (Index n chunkedPosts) <- indices]
          where indices = indexPosts posts
  
blogToIndexHtml :: Blog -> [(String, Html)]
blogToIndexHtml b = ("index.html", redirect) : [("index_" ++ show n ++ ".html",
  Blaze.main $ do
    preview $ do
      blogHeader
      mapM_ getStub chunkedPosts
      let numPages = fromIntegral $ length indices in
        getPageNavigation (Navigator n numPages "index_" (if n > 0 then Just "Older Posts" else Nothing) (if n < numPages - 1 then Just "Newer Posts" else Nothing))
      br
    emptyScript ! BlazeAttr.id "dsq-count-src"
                ! src "//austingarrett.disqus.com/count.js"
                ! async ""
    Blog.aside b)
  | (Index n chunkedPosts) <- indices]
  where indices = indexPosts b
        redirect = script $ preEscapedToHtml $ "window.location.href = \"" ++ blogPath </> "index_" ++ (show $ length indices - 1) ++ ".html" ++ "\""

blogToPostHtml :: Blog -> [(String, Html)]
blogToPostHtml b = [(show n ++ ".html",
  main $ article $ do
    smallNav $ do
      let numPages = fromIntegral $ length b
          prevTitle = if n > 0 then Just (case reverse b !! ((fromIntegral n :: Int) - 1) of (Post _ p') -> line p' 0) else Nothing
          nextTitle = if n < numPages - 1 then Just (case reverse b !! ((fromIntegral n :: Int) + 1) of (Post _ p') -> line p' 0) else Nothing in
            getPageNavigation (Navigator n (fromIntegral $ length b) "" prevTitle nextTitle)
    getPostHeader post False h2
    mapM_ (Blaze.p . preEscapedToHtml) $ init . tail $ (splitOn "\n\n" p)

    -- Share
    Blaze.div ! class_ "a2a_kit a2a_kit_size_32 a2a_default_style" $ do
      emptyA ! class_ "a2a_dd" ! href "https://www.addtoany.com/share"
      emptyA ! class_ "a2a_button_facebook"
      emptyA ! class_ "a2a_button_twitter"
      emptyA ! class_ "a2a_button_google_plus"

    emptyScript ! async "" ! src "https://static.addtoany.com/menu/page.js"
    emptyDiv ! BlazeAttr.id "disqus_thread"
    script $ preEscapedToHtml disqus_script
    noscript $ do
      "Please enable JavaScript to view the"
      a ! href "https://disqus.com/?ref_noscript" $
        "comments powered by Disqus.")
    | post@(Post n p) <- b]
  where disqus_script = unlines $ [
          "var disqus_config = function () {{",
          "this.page.identifier = \"{post_number}\";",
          "}};",
          "(function() {{ // DON'T EDIT BELOW THIS LINE",
          "var d =document, s = d.createElement('script');",
          "s.src ='https://austingarrett.disqus.com/embed.js';",
          "s.setAttribute('data-timestamp', +new Date());",
          "(d.head|| d.body).appendChild(s);",
          "}})();"
          ]
