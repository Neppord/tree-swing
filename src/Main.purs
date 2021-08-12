module Main where

import Prelude

import Data.Lens.Fold (toArrayOf)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Prism (prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter ((<>~))
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration(..), Ident(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..))
import Type.Proxy (Proxy(..))
import Data.Lens.Fold (filtered)
import Data.Lens.Getter (view)

parse :: String -> Module Void
parse myModuleSource = case parseModule myModuleSource of
  ParseSucceeded cst -> cst
  _ -> unsafeCrashWith "Faild parsing"

newtype Project = Project (Array (Module Void))

derive instance newtypeProject :: Newtype Project _

_modules = unto Project <<< traversed
_module = unto Module
_header = prop (Proxy :: Proxy "header")
_body = prop (Proxy :: Proxy "body")
_moduleBody = unto ModuleBody
_moduleHeader = unto ModuleHeader
_decls = prop (Proxy :: Proxy "decls")
_declValue = prism' DeclValue case _ of
    DeclValue a -> Just a
    _ -> Nothing
_nameFiled = prop (Proxy :: Proxy "name")
_name = unto Name
_ident = unto Ident
_guarded = prop (Proxy :: Proxy "guarded")


_modulesDeclartaions =
    _module <<< _body <<< _moduleBody <<< _decls

_modulesValueDeclarationNames =
    _modulesDeclartaions <<< traversed
        <<< _declValue <<< _nameFiled <<< _name <<< _nameFiled <<< _ident

_moduleName =
    _module
    <<< _header <<< _moduleHeader
    <<< _nameFiled <<< _name
    <<< _nameFiled <<< unto ModuleName

_valueReference = ""

code :: Module Void
code = parse
  """
  module Main where

  f = 1
  add a b = a + b

  """

project :: Project
project = Project
    [ parse """
      module Main where
      import Lib (world)
      main = do
          log $ "hello" <> world
      """
    , parse """
      module Lib where
      world = "world"
      """
     ]

_moduleWithName name = filtered (\ c -> view (_moduleName ) c == name )

main :: Effect Unit
main = do
    let query = _modules  <<< _moduleWithName "Main" <<< _modulesValueDeclarationNames
    let newCode = project # query <>~ "Value"
    logShow $ toArrayOf (_modules <<< _modulesValueDeclarationNames) newCode
