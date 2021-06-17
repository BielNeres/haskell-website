{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Ocorr where

import Import
import Database.Persist.Postgresql

formOcorr :: ClienteId -> Form Ocorr
formOcorr cid = renderDivs $ Ocorr
    <$> pure cid
    <*> areq (selectField prodCB) (FieldSettings "Ocorrencia: "
                            (Just "")
                            (Just "n1")
                            Nothing
                            [("class", "form-control")]
        ) Nothing
    <*> areq textField (FieldSettings "Descricao: "
                            (Just "")
                            (Just "n2")
                            Nothing
                            [("class", "form-control")]
        ) Nothing

prodCB = do 
    rows <- runDB $ selectList [] [Asc RegisterOcorrencia]
    optionsPairs $
        map(\r -> (registerOcorrencia $ entityVal r, entityKey r)) rows

getAtualizaR :: ClienteId -> Handler Html
getAtualizaR cid = do 
    (widget,_) <- generateFormPost (formOcorr cid)
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg 
                <div>
                    ^{mensa}
            <h1>
                CADASTRO DE OCORRÊNCIA DO USUARIO
        
            <form method=post action=@{AtualizaR cid}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postAtualizaR :: ClienteId -> Handler Html
postAtualizaR cid = do
    ((result,_),_) <- runFormPost (formOcorr cid)
    case result of
        FormSuccess ocorrencia -> do   
            runDB $ insert ocorrencia
            setMessage [shamlet|
                <div>
                    OCORRENCIA INCLUIDA COM SUCESSO!
            |]
            redirect (ListagemR cid)
        _-> redirect HomeR


getListagemR :: ClienteId -> Handler Html
getListagemR cid = do
    let sql = "SELECT ??, ??, ?? FROM ?? \
            \ INNER JOIN ocorr ON occor.regisid = register.id \
            \ INNER JOIN cliente ON ocorr.cliid = cliente.id \
            \ WHERE cliente.id = ?"
    cliente <- runDB $ get404 cid
    lista <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Register,Entity Ocorr,Entity Cliente)]
    defaultLayout $ do
        [whamlet|
            <h1>
                OCORRENCIAS DE #{clienteNome cliente}
            <ul>
                $forall (Entity _ register, Entity _ _, Entity _ _) <- lista
                    <li>
                        #{registerOcorrencia register}
        |]

