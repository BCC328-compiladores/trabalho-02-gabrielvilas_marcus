module Frontend.Pretty.SlPretty (prettyPrint) where

import Frontend.Syntax.SlSyntax
import Text.PrettyPrint
import Prelude hiding ((<>))

-- Função principal exportada
prettyPrint :: Sl -> String
-- Usamos vcat (vertical concat) para separar as definições
prettyPrint (Sl defs) = render $ vcat (map ppDef defs)

-- ============================================================================
-- Definições (Top Level)
-- ============================================================================

-- Auxiliar: Imprime o corpo do bloco e a chave de fechamento
-- Esta função será usada para o conteúdo INDENTADO + '}'
ppBlockContent :: Block -> Doc
ppBlockContent stmts = 
    -- Conteúdo (statements) indentado em 4 espaços
    nest 4 (vcat (map ppStmt stmts)) 
    -- Chave de fechamento na mesma coluna do comando de controle/função
    $$ rbrace

ppFuncRetType :: Maybe Type -> Doc
ppFuncRetType Nothing = empty 
ppFuncRetType (Just t) = colon <+> ppType t

ppDef :: Definition -> Doc
-- Struct
ppDef (DStruct name fields) = 
    (text "struct" <+> text name <+> lbrace) -- { na mesma linha
    $$ nest 4 (vcat (map ppField fields)) 
    $$ rbrace
    $$ empty 

-- Função
ppDef (Dfunc name typeVars params retType block) = 
    (ppGenerics typeVars <+> text "func" <+> text name
    <> parens (ppParams params)
    <> ppFuncRetType retType <+> lbrace) -- <--- CHAVE DE ABERTURA AQUI
    
    $$ ppBlockContent block -- Conteúdo e chave de fechamento na nova linha
    $$ empty 

-- Helper para generics
ppGenerics :: [TypeVar] -> Doc
ppGenerics [] = empty
ppGenerics vars = text "forall" <+> hsep (punctuate (char ' ') (map text vars)) <+> char '.'

-- Helper para campos de struct
ppField :: Field -> Doc
ppField (Field name t) = text name <+> colon <+> ppType t <> semi

-- Helper para parâmetros de função
ppParam :: Param -> Doc
ppParam (Param name Nothing) = text name
ppParam (Param name (Just t)) = text name <+> colon <+> ppType t

ppParams :: [Param] -> Doc
ppParams params = hsep (punctuate comma (map ppParam params))

-- ============================================================================
-- Blocos e Statements
-- ============================================================================



ppStmt :: Stmt -> Doc
ppStmt stmt = case stmt of
    -- Let, Assign, Read, Print: sem alterações
    SLet var t mExp -> 
        case mExp of
            Nothing -> 
                text "let" <+> text var <+> colon <+> ppType t <> semi
            Just exp ->
                text "let" <+> text var <+> colon <+> ppType t <+> equals <+> ppExp exp <> semi
    
    SLetInfer var exp ->
        text "let" <+> text var <+> equals <+> ppExp exp <> semi

    SAssign lexp rexp ->
        ppExp lexp <+> equals <+> ppExp rexp <> semi
        
    SRead exp ->
        text "read" <+> ppExp exp <> semi

    SPrint exp ->
        text "print" <> parens (ppExp exp) <> semi

    -- IF
    SIf cond thenBlock elseBlock ->
        (text "if" <+> parens (ppExp cond) <+> lbrace) -- <--- Chave na mesma linha
        $$ ppBlockContent thenBlock                   
        $$ ppElse elseBlock
    
    -- WHILE
    SWhile cond block ->
        (text "while" <+> parens (ppExp cond) <+> lbrace) -- <--- Chave na mesma linha
        $$ ppBlockContent block

    -- FOR
    SFor init cond step block ->
        (text "for" <+> parens (ppStmtNoSemi init <> semi <+> ppExp cond <> semi <+> ppStmtNoSemi step) <+> lbrace) 
        $$ ppBlockContent block

    SReturn exp ->
        text "return" <+> ppExp exp <> semi

    SExpr exp ->
        ppExp exp <> semi

ppElse :: Block -> Doc
ppElse [] = empty
ppElse block = 
    (text "else" <+> lbrace) -- <--- Chave na mesma linha
    $$ ppBlockContent block
    
-- Helper para imprimir statements dentro do 'for' sem o ponto e vírgula final
ppStmtNoSemi :: Stmt -> Doc
ppStmtNoSemi (SAssign l r) = ppExp l <+> equals <+> ppExp r

ppStmtNoSemi (SLetInfer v e) = text "let" <+> text v <+> equals <+> ppExp e

ppStmtNoSemi (SLet v t mExp) = 
    case mExp of
        Nothing -> 
            text "let" <+> text v <+> colon <+> ppType t
        Just exp -> 
            text "let" <+> text v <+> colon <+> ppType t <+> equals <+> ppExp exp

ppStmtNoSemi other = ppStmt other -- Fallback seguro

-- ============================================================================
-- Tipos (sem mudanças)
-- ============================================================================

ppType :: Type -> Doc
ppType t = case t of
    TInt          -> text "int"
    TFloat        -> text "float"
    TString       -> text "string"
    TBool         -> text "bool"
    TVoid         -> text "void"
    TStruct name  -> text name
    TVar name     -> text name
    TVector inner -> ppType inner <> text "[]"
    TVectorN inner n -> ppType inner <> brackets (int n)
    TFunc args ret -> parens (hcat $ punctuate comma (map ppType args)) <+> text "->" <+> ppType ret

-- ============================================================================
-- Expressões (sem mudanças)
-- ============================================================================

ppExp :: Exp -> Doc
ppExp e = case e of
    EValue v -> ppValue v
    EVar name -> text name
    EIncrement exp -> ppExp exp <> text "++"
    EStruct name exps -> 
        text name <> text "{" <> hsep (punctuate comma (map ppExp exps)) <> text "}"
    EVector exps -> brackets (hcat $ punctuate (comma <> space) (map ppExp exps))
    ENew t size -> text "new" <+> ppType t <> brackets (ppExp size)
    EIndex arr idx -> ppExp arr <> brackets (ppExp idx)
    EField obj field -> ppExp obj <> char '.' <> text field
    EArraySize exp -> ppExp exp <> text ".size"
    ECall func args -> ppExp func <> parens (hcat $ punctuate (comma <> space) (map ppExp args))
    ENot exp -> char '!' <> ppParens exp
    EMinus exp -> char '-' <> ppParens exp
    l :+: r -> ppExp l <+> char '+' <+> ppExp r
    l :-: r -> ppExp l <+> char '-' <+> ppExp r
    l :*: r -> ppExp l <+> char '*' <+> ppExp r
    l :/: r -> ppExp l <+> char '/' <+> ppExp r
    l :%: r -> ppExp l <+> char '%' <+> ppExp r
    l :&&: r -> ppExp l <+> text "&&" <+> ppExp r
    l :||: r -> ppExp l <+> text "||" <+> ppExp r
    l :==: r -> ppExp l <+> text "==" <+> ppExp r
    l :!=: r -> ppExp l <+> text "!=" <+> ppExp r
    l :<: r  -> ppExp l <+> text "<" <+> ppExp r
    l :>: r  -> ppExp l <+> text ">" <+> ppExp r
    l :<=: r -> ppExp l <+> text "<=" <+> ppExp r
    l :>=: r -> ppExp l <+> text ">=" <+> ppExp r

ppParens :: Exp -> Doc
ppParens e@(EValue _) = ppExp e
ppParens e@(EVar _) = ppExp e
ppParens e = parens (ppExp e)

ppValue :: Value -> Doc
ppValue v = case v of
    VInt n -> int n
    VFloat f -> double f
    VString s -> doubleQuotes (text s)
    VBool True -> text "true"
    VBool False -> text "false"