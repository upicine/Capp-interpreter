{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintCapp where

-- pretty-printer generated by the BNF converter

import AbsCapp
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Program decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print Block where
  prt i e = case e of
    BDeclBlock decls stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decls, prt 0 stmts, doc (showString "}")])

instance Print Stmt where
  prt i e = case e of
    SBlock block -> prPrec i 0 (concatD [prt 0 block])
    SWhile expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    SFor id expr1 expr2 block -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 id, doc (showString ":="), prt 0 expr1, doc (showString "to"), prt 0 expr2, doc (showString ")"), prt 0 block])
    SAss id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr, doc (showString ";")])
    SIncr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "++"), doc (showString ";")])
    SDecr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "--"), doc (showString ";")])
    SAssOp id opass expr -> prPrec i 0 (concatD [prt 0 id, prt 0 opass, prt 0 expr, doc (showString ";")])
    SIf expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    SIfEl expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    SReturnE expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    SCont -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    SBreak -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    SPrint expr -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ";")])
    SExpr expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print OpAss where
  prt i e = case e of
    OAssP -> prPrec i 0 (concatD [doc (showString "+=")])
    OAssM -> prPrec i 0 (concatD [doc (showString "-=")])

instance Print Decl where
  prt i e = case e of
    DefVar type_ ids -> prPrec i 0 (concatD [prt 0 type_, prt 0 ids])
    DefVarExp type_ id expr -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString ":="), prt 0 expr])
    DefFunc type_ id args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Arg where
  prt i e = case e of
    Arg type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    Int -> prPrec i 0 (concatD [doc (showString "int")])
    Str -> prPrec i 0 (concatD [doc (showString "string")])
    Bool -> prPrec i 0 (concatD [doc (showString "bool")])

instance Print Expr where
  prt i e = case e of
    EStr str -> prPrec i 5 (concatD [prt 0 str])
    EVar id -> prPrec i 5 (concatD [prt 0 id])
    EInt n -> prPrec i 5 (concatD [prt 0 n])
    ETrue -> prPrec i 5 (concatD [doc (showString "true")])
    EFalse -> prPrec i 5 (concatD [doc (showString "false")])
    EApp id exprs -> prPrec i 5 (concatD [prt 0 id, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EUnar opunar expr -> prPrec i 4 (concatD [prt 0 opunar, prt 5 expr])
    EMul expr1 opmul expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 opmul, prt 4 expr2])
    EAdd expr1 opadd expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 opadd, prt 3 expr2])
    ECmp expr1 opcmp expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 opcmp, prt 2 expr2])
    ELog expr1 oplog expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 oplog, prt 0 expr2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print OpMul where
  prt i e = case e of
    OMod -> prPrec i 0 (concatD [doc (showString "%")])
    OMul -> prPrec i 0 (concatD [doc (showString "*")])
    ODiv -> prPrec i 0 (concatD [doc (showString "/")])

instance Print OpAdd where
  prt i e = case e of
    OPlus -> prPrec i 0 (concatD [doc (showString "+")])
    OMinus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print OpCmp where
  prt i e = case e of
    OLt -> prPrec i 0 (concatD [doc (showString "<")])
    OGt -> prPrec i 0 (concatD [doc (showString ">")])
    OLte -> prPrec i 0 (concatD [doc (showString "<=")])
    OGte -> prPrec i 0 (concatD [doc (showString ">=")])
    OEq -> prPrec i 0 (concatD [doc (showString "==")])

instance Print OpLog where
  prt i e = case e of
    OAnd -> prPrec i 0 (concatD [doc (showString "&&")])
    OOr -> prPrec i 0 (concatD [doc (showString "||")])

instance Print OpUnar where
  prt i e = case e of
    ONot -> prPrec i 0 (concatD [doc (showString "!")])


