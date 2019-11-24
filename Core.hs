module Core where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.Map.Lazy as M

import AbsCapp
import LexCapp
import ParCapp
import SkelCapp
import ErrM
import PrintCapp

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value

data Value = VInt Integer | VBool Bool | VString String | VFunc Env [Arg] Block | VNull
    deriving (Eq, Ord)

instance Show Value where
    show (VInt val)             = show val
    show (VBool b)              = show b
    show (VString str)          = show str
    show (VFunc env args block) = "Func"
    show (VNull)                = "Null"

type InterpreterM a = ReaderT Env (ErrorT String (StateT Store IO)) a

runInterM :: Env -> Store -> InterpreterM a -> IO (Either String a, Store)
runInterM env state ev = runStateT (runErrorT (runReaderT ev env)) state

data RetInfo = Return Value | ReturnNothing | Break | Continue

initEnv :: Env
initEnv = M.empty

initStore :: Store
initStore = M.empty
