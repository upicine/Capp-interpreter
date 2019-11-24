module Interpreter where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Debug.Trace
import System.IO

import qualified Data.Map.Lazy as M

import AbsCapp
import LexCapp
import ParCapp
import SkelCapp
import ErrM
import PrintCapp
import Core


debug = flip trace


getVarLoc :: Ident -> InterpreterM Loc
getVarLoc id = do
    loc <- asks $ M.lookup id
    case loc of
        Nothing   -> throwError $ "Error: variable " ++ show id ++ " is not declared"
        Just addr -> return addr


getVarValue :: Ident -> InterpreterM Value
getVarValue id = do
    loc <- getVarLoc id
    getValueStore loc

-- Only for stmt 'for' purpose

removeVarStore :: Ident -> InterpreterM ()
removeVarStore id = do
    loc <- getVarLoc id
    s'  <- M.delete loc <$> get
    asks $ M.delete id
    put s'

isReadOnly :: Ident -> InterpreterM Bool
isReadOnly id = do
    loc <- asks $ M.lookup id
    case loc of
        Just addr -> return $ addr < 0
        Nothing   -> return False

varReadOnlyDeclaration :: Ident -> Value -> InterpreterM Env
varReadOnlyDeclaration id val = do
    readonly <- isReadOnly id
    if readonly then throwError $ "Error: " ++ show id ++ " is read-only variable"
    else do 
        loc <- getNextLoc <$> get
        env <- asks $ M.insert id (-loc)
        insertValueStore (-loc) val
        return env


assignReadOnlyValue :: Ident -> Value -> InterpreterM ()
assignReadOnlyValue id val = do
    loc <- getVarLoc id
    insertValueStore loc val

----------------------------

assignValue :: Ident -> Value -> InterpreterM ()
assignValue id val = do
    loc <- getVarLoc id
    if loc < 0 then
        throwError $ "Error: " ++ show id ++ " is read-only variable"
    else 
        insertValueStore loc val


getNextLoc :: Store -> Int
getNextLoc s = M.size s + 1


getValueStore :: Loc -> InterpreterM Value
getValueStore loc = do
    val <- M.lookup loc <$> get
    case val of
        Nothing    -> throwError "CRITICAL ERROR: This never should've happen"
        Just VNull -> throwError "Error: variable is not assigned"
        Just v     -> return v


insertValueStore :: Loc -> Value -> InterpreterM ()
insertValueStore loc val = do
    s' <- M.insert loc val <$> get
    put s'


varDeclaration :: Type -> Ident -> InterpreterM Env
varDeclaration t id = do
    loc <- getNextLoc <$> get
    env <- asks $ M.insert id loc
    insertValueStore loc VNull
    return env


varsDeclaration :: Type -> [Ident] -> InterpreterM Env
varsDeclaration t [] = ask
varsDeclaration t (x:xs) = do
    env <- varDeclaration t x
    local (const env) $ varsDeclaration t xs


declaration :: Decl -> InterpreterM Env
declaration (DefVarExp t id expr) = do
    val <- evalExpr expr
    env <- varDeclaration t id
    local (const env) $ assignValue id val
    return env

declaration (DefVar t ids) = varsDeclaration t ids

declaration (DefFunc t id args block) = do
    env <- varDeclaration t id
    local (const env) $ assignValue id $ VFunc env args block
    return env


declarations :: [Decl] -> InterpreterM Env
declarations [] = ask
declarations (x:xs) = do
    env <- declaration x
    local (const env) $ declarations xs


runInterpreter :: Program -> InterpreterM Integer
runInterpreter (Program dec) = do
    env      <- declarations dec
    VInt val <- local (const env) $ evalExpr $ EApp (Ident "main") []
    return val

-- Expressions --

---- Expression Functions

notVBool :: Value -> Value
notVBool (VBool b) = VBool $ not b


logVBool :: OpLog -> Value -> Value -> Value
logVBool OAnd (VBool b1) (VBool b2) = VBool $ and [b1, b2]
logVBool OOr  (VBool b1) (VBool b2) = VBool $ or  [b1, b2]


cmpVInt :: OpCmp -> Value -> Value -> Value
cmpVInt OLt  (VInt v1) (VInt v2) = VBool $ v1 < v2 
cmpVInt OGt  (VInt v1) (VInt v2) = VBool $ v1 > v2
cmpVInt OLte (VInt v1) (VInt v2) = VBool $ v1 <= v2
cmpVInt OGte (VInt v1) (VInt v2) = VBool $ v1 >= v2
cmpVInt OEq  (VInt v1) (VInt v2) = VBool $ v1 == v2


mulVInt :: OpMul -> Value -> Value -> Value
mulVInt OMul (VInt v1) (VInt v2) = VInt $ v1 * v2
mulVInt ODiv (VInt v1) (VInt v2)
    | v2 == 0   = error "Error: Dividing by 0 is not defined"
    | otherwise = VInt $ v1 `div` v2
mulVInt OMod (VInt v1) (VInt v2)
    | v2 == 0   = error "Error: Modulo by 0 is not defined"
    | otherwise = VInt $ v1 `mod` v2


addVInt :: OpAdd -> Value -> Value -> Value
addVInt OPlus  (VInt v1) (VInt v2) = VInt $ v1 + v2
addVInt OMinus (VInt v1) (VInt v2) = VInt $ v1 - v2


evalFuncExprs :: [Expr] -> InterpreterM [Value]
evalFuncExprs [] = return []
evalFuncExprs (x:xs) = (:) <$> evalExpr x <*> evalFuncExprs xs


assignFuncArgs :: [Arg] -> [Value] -> InterpreterM Env
assignFuncArgs [] [] = ask
assignFuncArgs ((Arg t id):args) (v:vs) = do
    env <- varDeclaration t id
    local (const env) $ assignValue id v
    local (const env) $ assignFuncArgs args vs

---- Applying
evalExpr :: Expr -> InterpreterM Value
evalExpr (EInt val)            = return $ VInt val
evalExpr (EStr str)            = return $ VString str
evalExpr ETrue                 = return $ VBool True
evalExpr EFalse                = return $ VBool False
evalExpr (EVar id)             = getVarValue id
evalExpr (EUnar not expr)      = notVBool    <$> evalExpr expr
evalExpr (ELog expr1 op expr2) = logVBool op <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (ECmp expr1 op expr2) = cmpVInt  op <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (EMul expr1 op expr2) = mulVInt  op <$> evalExpr expr1 <*> evalExpr expr2
evalExpr (EAdd expr1 op expr2) = addVInt  op <$> evalExpr expr1 <*> evalExpr expr2

evalExpr (EApp id exprs)       = do
    VFunc env args block <- evalExpr $ EVar id
    vs                   <- evalFuncExprs exprs
    env'                 <- local (const env)  $ assignFuncArgs args vs
    ret                  <- local (const env') $ executeBlock block
    case ret of
        ReturnNothing   -> throwError "Error: missing return statement"
        Return val      -> return val
        breakOrCont     -> throwError "Error: break/continue out of loop"

-- Stmts --

---- Stmts Functions
executeBlock :: Block -> InterpreterM RetInfo
executeBlock (BDeclBlock decls []) = return ReturnNothing
executeBlock (BDeclBlock [] (s:ss)) = do
    ret <- executeStmt s
    case ret of
        Return val      -> return $ Return val
        ReturnNothing   -> executeBlock $ BDeclBlock [] ss
        breakOrCont     -> return breakOrCont

executeBlock (BDeclBlock decls ss) = do
    env <- declarations decls
    local (const env) $ executeBlock $ BDeclBlock [] ss 


executeAssOp :: OpAss -> Ident -> Expr -> InterpreterM Value
executeAssOp OAssP id expr = addVInt OPlus <$> getVarValue id <*> evalExpr expr
executeAssOp OAssM id expr = addVInt OMinus <$> getVarValue id <*> evalExpr expr


executeForLoop :: Ident -> Value -> Block -> InterpreterM RetInfo
executeForLoop id (VInt end) block = do
    (VInt val) <- getVarValue id
    if val == end then return ReturnNothing
    else do
        env <- ask
        ret <- local (const env) $ executeBlock block
        local (const env) $ assignReadOnlyValue id $ VInt $ val + 1
        case ret of
            Break    -> return ReturnNothing
            Return v -> return $ Return v
            other    -> local (const env) $ executeForLoop id (VInt end) block

---- Execute
executeStmt :: Stmt -> InterpreterM RetInfo
executeStmt (SAss id expr) = do
    val <- evalExpr expr
    assignValue id val
    return ReturnNothing

executeStmt (SReturnE expr) = Return <$> evalExpr expr

executeStmt (SIncr id) = do
    VInt v <- getVarValue id
    assignValue id $ VInt $ v + 1
    return ReturnNothing

executeStmt (SDecr id) = do
    VInt v <- getVarValue id
    assignValue id $ VInt $ v - 1
    return ReturnNothing

executeStmt (SAssOp id op expr) = do
    val <- executeAssOp op id expr
    assignValue id val
    return ReturnNothing

executeStmt (SIf expr block) = do
    VBool b <- evalExpr expr
    if b then executeBlock block
    else return ReturnNothing

executeStmt (SIfEl expr ifBlock elseBlock) = do
    VBool b <- evalExpr expr
    if b then executeBlock ifBlock
    else executeBlock elseBlock

executeStmt SCont = return Continue

executeStmt SBreak = return Break

executeStmt (SPrint expr) = do
    val <- evalExpr expr
    liftIO $ putStrLn $ show val
    return ReturnNothing

executeStmt (SExpr expr) = do
    evalExpr expr
    return ReturnNothing

executeStmt while@(SWhile expr block) = do
    VBool b <- evalExpr expr
    if b then do
        ret <- executeBlock block
        case ret of
            Break    -> return ReturnNothing
            Return v -> return $ Return v
            _        -> executeStmt while
    else return ReturnNothing

executeStmt for@(SFor id expr1 expr2 block) = do
    start <- evalExpr expr1
    env   <- ask
    env'  <- local (const env) $ varReadOnlyDeclaration id start
    end   <- evalExpr expr2
    ret   <- local (const env') $ executeForLoop id end block
    local (const env') $ removeVarStore id
    case ret of
        Break -> return ReturnNothing
        other -> return other
