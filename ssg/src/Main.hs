{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Slugger qualified as Slugger
import Hakyll
import System.FilePath (takeFileName)
import Text.Pandoc
  ( Extension (Ext_fenced_code_attributes, Ext_footnotes, Ext_gfm_auto_identifiers, Ext_implicit_header_references, Ext_smart),
    Extensions,
    ReaderOptions,
    WriterOptions,
    extensionsFromList,
    githubMarkdownExtensions,
    readerExtensions,
    writerExtensions,
  )
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

--------------------------------------------------------------------------------
-- PERSONALIZATION

mySiteName :: String
mySiteName = "dendry"

mySiteRoot :: String
mySiteRoot = "https://dendry.net"

myFeedTitle :: String
myFeedTitle = "dendron"

myFeedDescription :: String
myFeedDescription = "blog"

myFeedAuthorName :: String
myFeedAuthorName = "Gabriel Talbert Bunt"

myFeedAuthorEmail :: String
myFeedAuthorEmail = "web@dendry.net"

myFeedRoot :: String
myFeedRoot = mySiteRoot

--------------------------------------------------------------------------------
-- CONFIG

-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "dist",
      ignoreFile = ignoreFile',
      previewHost = "127.0.0.1",
      previewPort = 8000,
      providerDirectory = "src",
      storeDirectory = "ssg/_cache",
      tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | "." `isPrefixOf` fileName = False
      | "#" `isPrefixOf` fileName = True
      | "~" `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = hakyllWith config $ do
  forM_
    [ "CNAME",
      "favicon.ico",
      "robots.txt",
      "_config.yml",
      "images/*",
      "js/*",
      "fonts/*"
    ]
    $ \f -> match f $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- Main post compilation: produces /posts/YYYY/title-slug.html
  match "posts/*" $ do
    let ctx = constField "type" "article" <> postCtx

    route $ metadataRoute postRoute
    compile $
      pandocCompilerCustom
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)

      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "root" mySiteRoot
              <> constField "feedTitle" myFeedTitle
              <> constField "siteName" mySiteName
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match "templates/*" $
    compile templateBodyCompiler

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)

      let pages = posts
          sitemapCtx =
            constField "root" mySiteRoot
              <> constField "siteName" mySiteName
              <> listField "pages" postCtx (return pages)

      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  create ["rss.xml"] $ do
    route idRoute
    compile (feedCompiler renderRss)

  create ["atom.xml"] $ do
    route idRoute
    compile (feedCompiler renderAtom)

  create ["css/code.css"] $ do
    route idRoute
    compile (makeStyle pandocHighlightStyle)

--------------------------------------------------------------------------------
-- COMPILER HELPERS

makeStyle :: Style -> Compiler (Item String)
makeStyle =
  makeItem . compressCss . styleToCss

--------------------------------------------------------------------------------
-- CONTEXT

feedCtx :: Context String
feedCtx =
  titleCtx
    <> postCtx
    <> bodyField "description"

postCtx :: Context String
postCtx =
  constField "root" mySiteRoot
    <> constField "feedTitle" myFeedTitle
    <> constField "siteName" mySiteName
    <> titleCtx
    <> descriptionCtx
    <> publishedDateField "date" "%Y-%m-%d"
    <> defaultContext

-- | Extract the title from aliases[0]
titleCtx :: Context String
titleCtx =
  field "title" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    case lookupStringList "aliases" metadata of
      Just (t : _) -> return $ replaceAmp t
      _ -> fail "no title (aliases[0]) found"

-- | Extract description from metadata
descriptionCtx :: Context String
descriptionCtx =
  field "desc" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    case lookupString "description" metadata of
      Just d -> return d
      Nothing -> fail "no description found"

-- | Date field that reads from "published" instead of the filename
publishedDateField :: String -> String -> Context a
publishedDateField key fmt =
  field key $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    case lookupString "published" metadata of
      Just d -> return d
      Nothing -> fail "no published date found"

--------------------------------------------------------------------------------
-- TITLE HELPERS

replaceAmp :: String -> String
replaceAmp =
  replaceAll "&" (const "&amp;")

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  pandocCompilerWith pandocReaderOpts pandocWriterOpts

pandocExtensionsCustom :: Extensions
pandocExtensionsCustom =
  githubMarkdownExtensions
    <> extensionsFromList
      [ Ext_fenced_code_attributes,
        Ext_gfm_auto_identifiers,
        Ext_implicit_header_references,
        Ext_smart,
        Ext_footnotes
      ]

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  defaultHakyllReaderOptions
    { readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  defaultHakyllWriterOptions
    { writerExtensions = pandocExtensionsCustom
    }

pandocHighlightStyle :: Style
pandocHighlightStyle =
  breezeDark -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

--------------------------------------------------------------------------------
-- FEEDS

type FeedRenderer =
  FeedConfiguration ->
  Context String ->
  [Item String] ->
  Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< recentFirst
    =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = myFeedTitle,
      feedDescription = myFeedDescription,
      feedAuthorName = myFeedAuthorName,
      feedAuthorEmail = myFeedAuthorEmail,
      feedRoot = myFeedRoot
    }

--------------------------------------------------------------------------------
-- CUSTOM ROUTES

-- | Route a post to /posts/YYYY/title-slug.html
-- Reads year from "published" and title from aliases[0]
postRoute :: Metadata -> Routes
postRoute metadata =
  let year = takeYear $ fromMaybe "0000" $ lookupString "published" metadata
      title = case lookupStringList "aliases" metadata of
        Just (t : _) -> t
        _ -> "untitled"
      slug = T.unpack $ Slugger.toSlug $ T.pack title
   in constRoute $ "posts/" <> year <> "/" <> slug <> ".html"

-- | Extract the 4-digit year from a date string like "2026-03-14"
takeYear :: String -> String
takeYear = take 4
