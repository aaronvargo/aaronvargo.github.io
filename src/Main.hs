{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Hakyll
import           System.FilePath
import           Text.Pandoc
import           Tut.Hakyll

main :: IO ()
main = hakyll $ do
    mirc "images/*" copyFileCompiler
    mirc "css/*" compressCssCompiler
    match "code/**" (compile getResourceString)
    mrc postsPattern (setExtension "html") postCompiler
    mirc "*.html" pageCompiler
    match ("templates/*") (compile templateCompiler)
    createFeed "atom.xml" renderAtom
    createFeed "rss.xml" renderRss

mrc m r c = match m (route r >> compile c)

mirc m c = mrc m idRoute c

contentSnapshot :: String
contentSnapshot = "content"

titledContentSnapshot :: String
titledContentSnapshot = "titledContent"

postCompiler :: Compiler (Item String)
postCompiler =
    myPandocCompiler
    >>= loadAndApplyTemplate "templates/post.html" postCtx
    >>= saveSnapshot contentSnapshot
    >>= loadAndApplyTemplate "templates/title.html" postCtx
    >>= saveSnapshot titledContentSnapshot
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

pageCompiler :: Compiler (Item String)
pageCompiler = do
    posts <- recentFirst =<< loadAllSnapshots postsPattern contentSnapshot
    let ctx = mconcat [ listField "posts" postCtx (return posts)
                      , listField "recentPosts"
                                  teaserCtx
                                  (return $ take 5 posts)
                      , defaultContext
                      ]
    getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , defaultContext
  ]

teaserCtx :: Context String
teaserCtx = mconcat [ teaserField "teaser" contentSnapshot , postCtx]

postsPattern :: Pattern
postsPattern = "posts/*"

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransformM defaultHakyllReaderOptions
                                                defaultHakyllWriterOptions
                                                defaultHakyllTut

withToc :: WriterOptions
withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "$toc$\n$body$"
        , writerStandalone = True
        }

myFeedConfig :: FeedConfiguration
myFeedConfig = FeedConfiguration
  { feedTitle = "Programming = Math: Applying Math to Programming"
  , feedDescription = "Recent posts"
  , feedAuthorName = "Aaron Vargo"
  , feedAuthorEmail = "vargosblog@gmail.com"
  , feedRoot = "http://aaronvargo.github.io"
  }

createFeed s f =
  create [s] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots postsPattern titledContentSnapshot
        f myFeedConfig feedCtx posts
