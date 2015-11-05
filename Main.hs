{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where

import Control.Applicative
import Data.Text (Text)
import Yesod
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool, fromSqlKey
    )

data Sitio = Sitio { connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Pessoa
   nome Text
   idade Int
   salario Double
   deptoid DepartamentoId
   deriving Show

Departamento
   nome Text
   sigla Text 2
   deriving Show
|]

mkYesod "Sitio" [parseRoutes|
    / HomeR GET
    /pessoas/listar ListarPessoasR GET
    /pessoas/cadastrar CadastrarPessoaR GET POST
    /pessoas/pessoa/#PessoaId PessoaR GET
    /departamentos/listar ListarDepartamentosR GET
    /departamentos/cadastrar CadastrarDepartamentoR GET POST
    /departamentos/departamento/#DepartamentoId DepartamentoR GET
    /creditos CreditosR GET
|]

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

formDepartamento :: Form Departamento
formDepartamento = renderDivs $ Departamento <$>
             areq textField "Nome" Nothing <*>
             areq textField "Sigla" Nothing 

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField departamento) "Departamento" Nothing

departamento = do
        entities <- runDB $ selectList [] [Asc DepartamentoNome]
        optionsPairs $ Prelude.map (\cat -> (departamentoNome $ entityVal cat, entityKey cat)) entities

widgetFormPessoa :: Enctype -> Widget -> Widget
widgetFormPessoa enctype widget = [whamlet|
            <form method=post action=@{CadastrarPessoaR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

widgetFormDepartamento :: Enctype -> Widget -> Widget
widgetFormDepartamento enctype widget = [whamlet|
            <form method=post action=@{CadastrarDepartamentoR} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|]

wHead :: String -> Widget
wHead title = toWidgetHead [hamlet|

    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="https://code.jquery.com/jquery-2.1.4.min.js" crossorigin="anonymous">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" integrity="sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" crossorigin="anonymous">    
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" integrity="sha512-K1qjQ+NcF2TYO/eI3M6v8EiNYZfA95pQumfvcVrTHtwQVDG+aHRqLi/ETn2uB+1JqwYqVG3LIvdm9lj6imS/pQ==" crossorigin="anonymous">
    <title>#{title}
|]

wNav :: Widget
wNav = [whamlet|
    <nav class="navbar navbar-inverse">
        <div class="container-fluid">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                    <span class="sr-only">Toggle navigation
                    <span class="icon-bar">
                    <span class="icon-bar">
                    <span class="icon-bar">
                <a class="navbar-brand" href=@{HomeR}>Início
            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                <ul class="nav navbar-nav">
                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                        <span class="glyphicon glyphicon-user">
                        Pessoas
                        <span class="caret">
                      <ul class="dropdown-menu">
                        <li>
                          <a href=@{CadastrarPessoaR}>Cadastrar
                        <li>
                          <a href=@{ListarPessoasR}>Ver Todos

                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                        <span class="glyphicon glyphicon-th-large">
                        Departamentos
                        <span class="caret">
                      <ul class="dropdown-menu">
                        <li>
                          <a href=@{CadastrarDepartamentoR}>Cadastrar
                        <li>
                          <a href=@{ListarDepartamentosR}>Ver Todos

                <ul class="nav navbar-nav navbar-right">
                    <li>
                        <a href=@{CreditosR}>
                           <span class="glyphicon glyphicon-user">
                           Créditos
|]

wContainer :: String -> Widget -> Widget
wContainer title content = do
    wHead title
    [whamlet|
    <div class="container">
         ^{wNav}
         ^{content}
|]

wHome :: Widget
wHome = [whamlet|
    <div class="row">
        <div class="col-sm-12">
            <div class="jumbotron">
              <h1>Olá, Seja Bem Vindo!
              <p>Este é o nosso segundo site feito em Haskell.
|]

wCreditos :: Widget
wCreditos = do
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-user">
                        Créditos
        <div class="row">
            <div class="col-sm-6">
                <div class="table-responsive">
                    <table class="table">
                      <thead>
                        <tr>
                          <th>Alunos
                          <th>Matrícula
                      <tbody>
                        <tr>
                          <td>André Silva Santos
                          <td>1310002-1
                        <tr>
                          <td>Leonardo Machado
                          <td>1310066-7
    |]

wCadastrarPessoa :: Widget
wCadastrarPessoa = do
    (widget, enctype) <- handlerToWidget $ generateFormPost formPessoa
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-download-alt">
                        Cadastrar Pessoas
        <div class="row">
            <div class="col-sm-6">
                ^{widgetFormPessoa enctype widget}
    |]

wPessoa :: PessoaId -> Widget
wPessoa pid = do
    pessoa <- handlerToWidget $ runDB $ get404 pid
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-user">
                        Pessoa
        <div class="row">
            <div class="col-sm-6">
                <div class="table-responsive">
                    <table class="table">
                        <tbody>
                            <tr>
                                <th>Nome:
                                <td>#{pessoaNome pessoa}
                            <tr>
                                <th>Salário:
                                <td>#{pessoaNome pessoa}
                            <tr>
                                <th>Idade:
                                <td>#{pessoaNome pessoa}
    |]

wDepartamento :: DepartamentoId -> Widget
wDepartamento pid = do
    departamento <- handlerToWidget $ runDB $ get404 pid
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-th-large">
                        Departamento
        <div class="row">
            <div class="col-sm-6">
                <div class="table-responsive">
                    <table class="table">
                        <tbody>
                            <tr>
                                <th>Nome:
                                <td>#{departamentoNome departamento}
                            <tr>
                                <th>Salário:
                                <td>#{departamentoSigla departamento}
    |]

wCadastrarDepartamento :: Widget
wCadastrarDepartamento = do
    (widget, enctype) <- handlerToWidget $ generateFormPost formDepartamento
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-download-alt">
                        Cadastrar Departamentos
        <div class="row">
            <div class="col-sm-6">
                ^{widgetFormDepartamento enctype widget}
    |]

wListarPessoas :: Widget
wListarPessoas = do
    pessoas <- handlerToWidget $ runDB $ selectList [] [Desc PessoaNome]
    [whamlet|
    <div class="row">
      <div class="col-sm-12">
        <div class="col-sm-12 page-header">
          <h1>
            <span class="glyphicon glyphicon-list-alt">
            Lista de Pessoas
    <div class="row">
      <div class="col-sm-12">
        $if null pessoas
          <div class="jumbotron">
            <h1>Sem nomes!
            <p>Se quiser cadastrar pessoas clique no botão abaixo.
            <p>
              <a class="btn btn-primary" href=@{CadastrarPessoaR} role="button">Cadastrar Pessoas
        $else
          <div class="table-responsive">
            <table class="table">
              <thead>
                <tr>
                  <th>Pessoas Cadastrados
              <tbody>
                $forall Entity pessoaId pessoa <- pessoas
                  <tr>
                    <td>
                        <a href=@{PessoaR pessoaId}>#{pessoaNome pessoa}
    |]

wListarDepartamentos :: Widget
wListarDepartamentos = do
    departamentos <- handlerToWidget $ runDB $ selectList [] [Desc DepartamentoNome]
    [whamlet|
    <div class="row">
      <div class="col-sm-12">
        <div class="col-sm-12 page-header">
          <h1>
            <span class="glyphicon glyphicon-list-alt">
            Lista de Departamentos
    <div class="row">
      <div class="col-sm-12">
        $if null departamentos
          <div class="jumbotron">
            <h1>Sem departamentos!
            <p>Se quiser cadastrar departamentos clique no botão abaixo.
            <p>
              <a class="btn btn-primary" href=@{CadastrarDepartamentoR} role="button">Cadastrar Departamentos
        $else
          <div class="table-responsive">
            <table class="table">
              <thead>
                <tr>
                  <th>Departamentos Cadastrados
              <tbody>
                $forall Entity departamentoId departamento <- departamentos
                  <tr>
                    <td>
                        <a href=@{DepartamentoR departamentoId}>#{departamentoNome departamento}
    |]


wInserirPessoa :: Widget
wInserirPessoa = [whamlet|
            <div class="row">
              <div class="col-sm-12 page-header">
                <h1>
                  <span class="glyphicon glyphicon-download-alt">
                  Inserir Pessoa
              <div class="col-sm-4">
                <div class="alert alert-success">
                  <p>Inserido com sucesso!
                <a class="btn btn-primary" href=@{CadastrarPessoaR} role="button">Adicionar Outro
        |]

wInserirDepartamento :: Widget
wInserirDepartamento = [whamlet|
            <div class="row">
              <div class="col-sm-12 page-header">
                <h1>
                  <span class="glyphicon glyphicon-download-alt">
                  Inserir Departamento
              <div class="col-sm-4">
                <div class="alert alert-success">
                  <p>Inserido com sucesso!
                <a class="btn btn-primary" href=@{CadastrarDepartamentoR} role="button">Adicionar Outro
        |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do (wContainer "Home" wHome)

getCreditosR :: Handler Html
getCreditosR = defaultLayout $ do (wContainer "Créditos" wCreditos)

getCadastrarDepartamentoR :: Handler Html
getCadastrarDepartamentoR = defaultLayout $ do (wContainer "Cadastrar Departamento" wCadastrarDepartamento)

getCadastrarPessoaR :: Handler Html
getCadastrarPessoaR = defaultLayout $ do (wContainer "Cadastrar Pessoa" wCadastrarPessoa)

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = defaultLayout $ do (wContainer "Cadastrar Pessoa" (wPessoa pid))

getDepartamentoR :: DepartamentoId -> Handler Html
getDepartamentoR pid = defaultLayout $ do (wContainer "Cadastrar Pessoa" (wDepartamento pid))

getListarPessoasR :: Handler Html
getListarPessoasR = defaultLayout $ do (wContainer "Lista de Pessoas" wListarPessoas)

getListarDepartamentosR :: Handler Html
getListarDepartamentosR = defaultLayout $ do (wContainer "Lista de Departamentos" wListarDepartamentos)

postCadastrarPessoaR :: Handler Html
postCadastrarPessoaR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa
                       defaultLayout (wContainer "Cadastrar Pessoa" wInserirPessoa)
                    _ -> redirect CadastrarPessoaR

postCadastrarDepartamentoR :: Handler Html
postCadastrarDepartamentoR = do
                ((result, _), _) <- runFormPost formDepartamento
                case result of
                    FormSuccess dpto -> do
                       runDB $ insert dpto
                       defaultLayout (wContainer "Cadastrar Departamento" wInserirDepartamento)
                    _ -> redirect CadastrarDepartamentoR

main::IO()
main = do
       pool <- runStdoutLoggingT $ createSqlitePool "sitio.db3" 10 -- create a new pool
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Sitio pool) 

