{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

-- getRegisterR :: Handler Html
-- getRegisterR = do
--     defaultLayout $ do
--         addStylesheet (StaticR css_bootstrap_css)
--         $(whamletFile "templates/register.hamlet")

getAboutR :: Handler Html
getAboutR = do 
    defaultLayout $ do
        addStylesheet (StaticR css_bootstrap_css) 
        $(whamletFile "templates/about.hamlet")

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do 
        usuario <- lookupSession "_ID" 
        addStylesheet (StaticR css_bootstrap_css)
        -- js
        -- toWidgetHead $(juliusFile "templates/home.julius")
        -- css
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")
