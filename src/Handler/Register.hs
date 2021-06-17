{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Register where

import Import

formRegister :: Form Register
formRegister = renderDivs $ Register
    <$> areq textField (FieldSettings "Ocorrência: "
                            (Just "")
                            (Just "n1")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
    <*> areq textField (FieldSettings "Endereço: "
                            (Just "")
                            (Just "n1")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
    <*> areq textField  (FieldSettings "Status: "
                            (Just "")
                            (Just "n1")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
getRegisterR :: Handler Html
getRegisterR = do 
    (widget,_) <- generateFormPost formRegister
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <h1>
                CADASTRO DE NOVA OCORRÊNCIA
            <form method=post action=@{RegisterR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postRegisterR :: Handler Html
postRegisterR = do
    ((result,_),_) <- runFormPost formRegister
    case result of
        FormSuccess register -> do   
            runDB $ insert register
            setMessage [shamlet|
                <div>
                    REGISTRO INSERIDO COM SUCESSO
            |]
            redirect RegisterR
        _-> redirect HomeR

getOcorrenciaR :: RegisterId -> Handler Html
getOcorrenciaR cid = do
    register <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            Ocorrencia: #{registerOcorrencia register}
        <h2>
            Endereco: #{registerEndereco register}
        <h2>
            Status: #{registerStatus register}
    |]

getListaRegR :: Handler Html
getListaRegR = do
    registers <- runDB $ selectList [] [Asc RegisterOcorrencia]
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        $(whamletFile "templates/register.hamlet")

postApagarRegR :: RegisterId -> Handler Html
postApagarRegR cid = do
    runDB $ delete cid
    redirect ListaRegR