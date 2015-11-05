wListar :: Widget
wListar = do $
    pessoas <- runDB $ selectList [] [Desc PessoaNome]
        defaultLayout $ do
          [whamlet|
            <div class="row">
              <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                  <h1>
                    <span class="glyphicon glyphicon-list-alt">Lista de Nomes
            <div class="row">
              <div class="col-sm-12">
                $if null pessoas
                  <div class="jumbotron">
                    <h1>Sem nomes!
                    <p>Se quiser cadastrar nomes clique no botão abaixo.
                    <p>
                      <a class="btn btn-primary" href=@{NovoR} role="button">Cadastrar Nomes
                $else
                  <div class="table-responsive">
                    <table class="table">
                      <thead>
                        <tr>
                          <th>Nomes Cadastrados
                      <tbody>
                        $forall Entity pessoaId pessoa <- pessoas
                          <tr>
                            <td>#{pessoaNome pessoa}
          |]

getListarR :: Handler Html
getListarR = defaultLayout $ do (wContainer "Lista de Nomes" wListar)
