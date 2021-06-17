{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Login where

import Import
import Handler.Auxiliar

formLogin :: Form Usuario
formLogin = renderDivs $ Usuario    
    <$> areq textField (FieldSettings "Email: "
                            (Just "")
                            (Just "n1")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
    <*> areq passwordField (FieldSettings "Senha: "
                            (Just "")
                            (Just "n2")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
    

getAutR :: Handler Html
getAutR = do 
    (widget,_) <- generateFormPost formLogin 
    msg <- getMessage
    defaultLayout (formWidget widget msg AutR "Entrar") 

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario "root@root.com" "root") -> do  
            setSession "_ID" "Admin"
            redirect AdminR
        FormSuccess (Usuario email senha) -> do   
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do 
                    setMessage [shamlet|
                        Usuario nao cadastradado
                    |]      
                    redirect AutR   
                Just (Entity _ usuario) -> do
                    if senha == usuarioSenha usuario then do 
                        setSession "_ID" (usuarioEmail usuario)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            Usuario e/ou senha nao conferem
                        |]   
                        redirect AutR       
        _-> redirect HomeR

postSairR :: Handler Html 
postSairR = do
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
        Bem vindo Admin
    |]