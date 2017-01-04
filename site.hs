--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.HTML.TagSoup      as TS
import           Data.List(partition)
import qualified GHC.IO.Encoding as E

--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" thisContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ postCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    thisContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    activeClassField `mappend` listField "posts" postCtx (return posts) `mappend`
                    thisContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    thisContext

thisContext :: Context String
thisContext = activeClassField `mappend` defaultContext

-- | Mark activeClass functions as active or inactive.
activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
  path <- toFilePath <$> getUnderlying
  return $ if path == p then "active" else "inactive"

-- | Add a class to the given tag.
addClass :: String -> TS.Tag String -> TS.Tag String
addClass cls (TS.TagOpen name attr) = case partition ((== "class") . fst) attr of
   ([],         _)     -> TS.TagOpen name $ ("class", cls) : attr
   ((_,cls'):_, attr') -> TS.TagOpen name $ ("class", cls ++ ' ': cls') : attr'
addClass _ tag = tag

-- | Custom post compiler which adds the classes "table" and "table-striped" to all table elements.
postCompiler :: Compiler (Item String)
postCompiler = fmap (withTags process) `fmap` pandocCompiler
  where process tag | TS.isTagOpenName "table" tag = addClass "table table-striped" tag
                    | otherwise                    = tag
