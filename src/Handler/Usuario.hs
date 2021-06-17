{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Usuario where

import Import
import Handler.Auxiliar

formLogin :: Form (Usuario, Text)
formLogin = renderDivs $ (,) 
    <$> (Usuario
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
        ) Nothing)
    <*> areq passwordField  (FieldSettings "Confirmação: "
                            (Just "")
                            (Just "n3")
                            Nothing
                            [("class", "form-control")]
        ) Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,_) <- generateFormPost formLogin 
    msg <- getMessage
    defaultLayout (formWidget widget msg UsuarioR "Cadastrar")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (usuario@(Usuario email senha), conf) -> do   
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div>
                            Email Já cadastrado
                        |]
                    redirect UsuarioR
                Nothing -> do
                    if senha == conf then do 
                        runDB $ insert usuario
                        setMessage [shamlet|
                            <div>
                                Usuario INSERIDO COM SUCESSO
                        |]
                        redirect UsuarioR
                    else do
                        setMessage [shamlet|
                            <div>
                                Senha incompativel
                        |]
                        redirect UsuarioR
        _-> redirect HomeR