wInsert::Text -> Widget
wInsert t = do
    runDB $ insert (Pessoa t)
    defaultLayout $ do
      [whamlet|
        <div class="row">
          <div class="col-sm-12 page-header">
            <h1>
              <span class="glyphicon glyphicon-download-alt">Inserir Nomes
          <div class="col-sm-4">
            <div class="alert alert-success">
              <h1>#{show t}
              <p>Inserido com sucesso!
            <a class="btn btn-primary" href=@{NovoR} role="button">Adicionar Outro
      |]


getInsertR :: Text -> Handler Html
getInsertR t = defaultLayout $ do (wContainer "Inserir Nomes" (wInsert t))