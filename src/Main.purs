module Main where

import Prelude

import Data.Lens.Fold (filtered, toArrayOf)
import Data.Lens.Getter (view)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Prism (prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter ((<>~))
import Data.Lens.Traversal (traversed)
import Data.Lens.Types (Iso', Lens', Prism', Traversal')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Choice (class Choice)
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration(..), Ident(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), ValueBindingFields)
import Type.Proxy (Proxy(..))

parse :: String -> Module Void
parse myModuleSource = case parseModule myModuleSource of
  ParseSucceeded cst -> cst
  _ -> unsafeCrashWith "Faild parsing"

newtype Project = Project (Array (Module Void))

derive instance newtypeProject :: Newtype Project _

_modules :: Traversal' Project (Module Void)
_modules = unto Project <<< traversed

_header :: Lens' (Module Void) (ModuleHeader Void)
_header = unto Module <<< prop (Proxy :: Proxy "header")

_body :: Lens' (Module Void) (ModuleBody Void)
_body = unto Module <<< prop (Proxy :: Proxy "body")

_decls :: Lens' (ModuleBody Void) (Array (Declaration Void))
_decls = unto ModuleBody <<< prop (Proxy :: Proxy "decls")

_headerName :: Lens' (ModuleHeader Void) (Name ModuleName)
_headerName = unto ModuleHeader <<< prop (Proxy :: Proxy "name")

_declValue :: Prism' (Declaration Void) (ValueBindingFields Void)
_declValue = prism' DeclValue case _ of
    DeclValue a -> Just a
    _ -> Nothing

_ident :: Iso' Ident String
_ident = unto Ident

_nameValue :: forall a. Lens' (Name a) a
_nameValue = unto Name <<< prop (Proxy :: Proxy "name")

_modulesDeclartaions :: Lens' (Module Void) (Array (Declaration Void))
_modulesDeclartaions = _body <<< _decls

_modulesValueDeclarationNames :: Traversal' (Module Void) String
_modulesValueDeclarationNames =
    _modulesDeclartaions <<< traversed
        <<< _declValue <<< prop (Proxy :: Proxy "name") <<< _nameValue <<< _ident

_moduleName :: Lens' (Module Void) String
_moduleName = _header <<< _headerName <<< _nameValue <<< unto ModuleName

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

_moduleWithName :: forall t137. Choice t137 => String -> t137 (Module Void) (Module Void) -> t137 (Module Void) (Module Void)
_moduleWithName name = filtered (\ c -> view (_moduleName ) c == name )

main :: Effect Unit
main = do
    let query = _modules  <<< _moduleWithName "Main" <<< _modulesValueDeclarationNames
    let newCode = project # query <>~ "Value"
    logShow $ toArrayOf (_modules <<< _modulesValueDeclarationNames) newCode
