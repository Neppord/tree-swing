module Main where

import Prelude

import Data.Lens.Fold (filtered, toArrayOf)
import Data.Lens.Getter (view)
import Data.Lens.Internal.Wander (class Wander)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Prism (prism')
import Data.Lens.Record (prop)
import Data.Lens.Setter ((<>~))
import Data.Lens.Traversal (traversed)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
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

_modules :: forall t29. Profunctor t29 => Wander t29 => t29 (Module Void) (Module Void) -> t29 Project Project
_modules = unto Project <<< traversed

_header :: forall t58. Profunctor t58 => Strong t58 => t58 (ModuleHeader Void) (ModuleHeader Void) -> t58 (Module Void) (Module Void)
_header = unto Module <<< prop (Proxy :: Proxy "header")

_body :: forall t175. Profunctor t175 => Strong t175 => t175 (ModuleBody Void) (ModuleBody Void) -> t175 (Module Void) (Module Void)
_body = unto Module <<< prop (Proxy :: Proxy "body")

_decls :: forall t143. Profunctor t143 => Strong t143 => t143 (Array (Declaration Void)) (Array (Declaration Void)) -> t143 (ModuleBody Void) (ModuleBody Void)
_decls = unto ModuleBody <<< prop (Proxy :: Proxy "decls")

_headerName :: forall t66 t67. Profunctor t67 => Strong t67 => t67 (Name ModuleName) (Name ModuleName) -> t67 (ModuleHeader t66) (ModuleHeader t66)
_headerName = unto ModuleHeader <<< prop (Proxy :: Proxy "name")

_declValue :: forall p. Choice p => p  (ValueBindingFields Void) (ValueBindingFields Void) -> p (Declaration Void) (Declaration Void)
_declValue = prism' DeclValue case _ of
    DeclValue a -> Just a
    _ -> Nothing

_ident :: forall p. Profunctor p => p String String -> p Ident Ident
_ident = unto Ident

_nameValue :: forall t27 t28. Profunctor t28 => Strong t28 => t28 t27 t27 -> t28 (Name t27) (Name t27)
_nameValue = unto Name <<< prop (Proxy :: Proxy "name")

_modulesDeclartaions :: forall s. Profunctor s => Strong s => s (Array (Declaration Void)) (Array (Declaration Void)) -> s (Module Void) (Module Void)
_modulesDeclartaions = _body <<< _decls

_modulesValueDeclarationNames :: forall t200. Profunctor t200 => Strong t200 => Wander t200 => t200 String String -> t200 (Module Void) (Module Void)
_modulesValueDeclarationNames =
    _modulesDeclartaions <<< traversed
        <<< _declValue <<< prop (Proxy :: Proxy "name") <<< _nameValue <<< _ident

_moduleName :: forall t113. Profunctor t113 => Strong t113 => t113 String String -> t113 (Module Void) (Module Void)
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
