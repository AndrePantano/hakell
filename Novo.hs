wNovo :: Widget
wNovo = [whamlet|
    <div class="row">
      <div class="col-sm-12 page-header">
        <h1>
          <span class="glyphicon glyphicon-download-alt">Inserir Nomes
      <div class="col-sm-4">
        <form class="form-vertical" id="form" method="GET" action="#">
          <div class="form-group">
            <label>Nome:
            <input type="text" class="form-control" id="texto" placeholder="Novo nome">
          <button type="button" class="btn btn-primary" id="btn-enviar">Enviar
    |]
    toWidgetHead [julius|
        $(document).ready(function(){
            $("#btn-enviar").click(function(){
                var texto = $("#texto").val();
                if(texto.length > 0){
                    window.location.href = "/insert/"+texto;
                }else{
                    alert("Informe o texto corretamente!");
                }
            });
        });
    |]

getNovoR::Handler Html
getNovoR = defaultLayout $ do (wContainer "Inserir Nomes" wNovo)

