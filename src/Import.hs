{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
   / CadastroR GET POST
   /listar ListarR GET
   /pessoa/#PessoaId PessoaR GET
   /depto DeptoR GET POST
   /usuario UsuarioR GET POST
   /listusuario ListUsuarioR GET
|]