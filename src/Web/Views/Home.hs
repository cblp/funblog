{-# LANGUAGE OverloadedStrings #-}

module Web.Views.Home where

import           Prelude                     hiding (div)

import           Data.Foldable
import           Database.Persist
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes

import           Model.CoreTypes
import           Web.Views.Site              (SiteView (..))

homeView :: [Entity Post] -> SiteView -> Html
homeView posts sv = do
    div ! class_ "blog-header" $ do
        h1 ! class_ "blog-title" $ toHtml $ sv_blogName sv
        p ! class_ "lead blog-description" $ toHtml $ sv_blogDesc sv
    div ! class_ "row" $ do
        div ! class_ "col-sm-8 blog-main" $
            for_ posts $ \post ->
                div ! class_ "blog-post" $ do
                    h2 ! class_ "blog-post-title" $ toHtml $ postTitle (entityVal post)
                    p ! class_ "blog-post-meta" $ toHtml $ show $ postDate (entityVal post)
                    p (toHtml $ postContent (entityVal post))
        div ! class_ "col-sm-3 col-sm-offset-1 blog-sidebar" $ do
            div ! class_ "sidebar-module sidebar-module-inset" $ do
                h4 "About"
                p "This blog is very cool and written in Haskell."
            div ! class_ "sidebar-module" $ do
                h4 "Archives"
                ol ! class_ "list-unstyled" $
                    li $ a ! href "#" $ "Fooo"
