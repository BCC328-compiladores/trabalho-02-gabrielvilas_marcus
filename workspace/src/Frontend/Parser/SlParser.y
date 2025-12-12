{
module Frontend.Parser.SlParser where

import Frontend.Lexer.Token
import Frontend.Lexer.SlLexer
import Frontend.Syntax.SlSyntax
}

-- O Parser retorna uma AST do tipo 'Sl'
%name slParser Sl
%monad { Alex } { (>>=) } { return }
%tokentype { Token }
%error { parseError }
%lexer { alexLexer } { Token _ TokEOF }

-- Mapeamento dos Tokens (Do Token.hs para nomes usáveis na gramática)
%token
    func    { Token _ KW_Func }
    struct  { Token _ KW_Struct }
    let     { Token _ KW_Let }
    return  { Token _ KW_Return }
    if      { Token _ KW_If }
    else    { Token _ KW_Else }
    while   { Token _ KW_While }
    for     { Token _ KW_For }
    new     { Token _ KW_New }
    forall  { Token _ KW_Forall }
    void    { Token _ KW_Void }
    read    { Token _ KW_Read }
    print   { Token _ KW_Print }

    int     { Token _ KW_Int }
    float   { Token _ KW_Float }
    string  { Token _ KW_String }
    bool    { Token _ KW_Bool }
    
    true    { Token _ KW_True }
    false   { Token _ KW_False }

    id      { Token _ (TokId $$) }
    int_lit { Token _ (TokInt $$) }
    float_lit { Token _ (TokFloat $$) }
    str_lit { Token _ (TokString $$) }

    '='     { Token _ TokAssign }
    '+'     { Token _ TokPlus }
    '-'     { Token _ TokMinus }
    '*'     { Token _ TokTimes }
    '/'     { Token _ TokDiv }
    '%'     { Token _ TokMod }
    '&&'    { Token _ TokAnd }
    '||'    { Token _ TokOr }
    '!'     { Token _ TokNot }
    '++'    { Token _ TokIncrement }
    '=='    { Token _ TokEq }
    '!='    { Token _ TokNeq }
    '<'     { Token _ TokLt }
    '>'     { Token _ TokGt }
    '<='    { Token _ TokLeq }
    '>='    { Token _ TokGeq }
    '->'    { Token _ TokArrow }
    '.'     { Token _ TokDot }

    '('     { Token _ TokLParen }
    ')'     { Token _ TokRParen }
    '{'     { Token _ TokLBrace }
    '}'     { Token _ TokRBrace }
    '['     { Token _ TokLBracket }
    ']'     { Token _ TokRBracket }
    ';'     { Token _ TokSemi }
    ':'     { Token _ TokColon }
    ','     { Token _ TokComma }

-- Precedência de Operadores (Do menor para o maior)
-- Isso resolve conflitos e elimina a necessidade de muitos parênteses
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '>' '<=' '>=' 
%left '+' '-'
%left '*' '/' '%'
%left '!'           -- Not unário
%left NEG           -- Menos unário (definido na regra)
%nonassoc '++'
%left '[' ']' '.'   -- Acesso a array e struct tem prioridade máxima
%left '(' ')'       -- Chamada de função

%%

-- ============================================================================
-- Gramática Principal
-- ============================================================================

Sl : Definitions { Sl (reverse $1) }

Definitions 
    : Definitions Definition { $2 : $1 }
    | {- empty -}           { [] }

Definition
    : StructDef { $1 }
    | FuncDef   { $1 }

-- Definição de Struct
-- struct Name { field : Type; ... }
StructDef 
    : struct id '{' Fields '}' { DStruct $2 (reverse $4) }

Fields
    : Fields Field ';' { $2 : $1 }
    | {- empty -}      { [] }

Field 
    : id ':' Type { Field $1 $3 }

-- Definição de Função
-- func name(params) : Type { Block }
-- forall a b . func map (params) : Type { Block }
FuncDef
    : func id '(' Params ')' OptRetType '{' Block '}' 
      { Dfunc $2 [] (reverse $4) $6 $8 } -- $6 é o tipo de retorno opcional

    | forall TypeVars '.' func id '(' Params ')' OptRetType '{' Block '}'
      { Dfunc $5 (reverse $2) (reverse $7) $9 $11 } -- $9 é o tipo de retorno opcional

OptRetType
    : ':' Type      { Just $2 }  -- Caso: : Type (Retorno explícito)
    | {- empty -}   { Nothing }  -- Caso: Sem tipo de retorno (Inferência)

-- Variáveis de Tipo (Generics): a b c ...
TypeVars
    : TypeVars id { $2 : $1 }
    | id          { [$1] }

Params
    : ParamsList { $1 }
    | {- empty -} { [] }

ParamsList
    : ParamsList ',' Param { $3 : $1 }
    | Param                { [$1] }

Param
    : id ':' Type { Param $1 (Just $3) } -- Explicita (e.g., x : int)
    | id          { Param $1 Nothing }   -- Inferido (e.g., x)

-- ============================================================================
-- Tipos
-- ============================================================================

Type
    : int         { TInt }
    | float       { TFloat }
    | string      { TString }
    | bool        { TBool }
    | void        { TVoid }
    | id          { if isTypeVar $1 then TVar $1 else TStruct $1 } 
    | Type '[' ']' { TVector $1 }          -- int[]
    | Type '[' int_lit ']' { TVectorN $1 $3 } -- int[5]
    -- Tipos de função: (int, float) -> bool
    | '(' TypeList ')' '->' Type { TFunc (reverse $2) $5 }

TypeList
    : TypeList ',' Type { $3 : $1 }
    | Type              { [$1] }

-- ============================================================================
-- Statements (Comandos)
-- ============================================================================

Block 
    : Stmts { reverse $1 }

Stmts
    : Stmts Stmt { $2 : $1 }
    | {- empty -} { [] }

Stmt
    -- let x : int = 10;
    : let id ':' Type '=' Exp ';'  { SLet $2 $4 (Just $6) }

    | let id ':' Type ';'          { SLet $2 $4 Nothing }
    
    -- let x = 10; (Inferência)
    | let id '=' Exp ';'           { SLetInfer $2 $4 }
    
    -- Atribuição: x = 10; ou v[i] = 10;
    -- Importante: Usamos Exp no lado esquerdo (L-Value)
    | Exp '=' Exp ';'              { SAssign $1 $3 }

    | read '(' Exp ')' ';'              { SRead $3 }
    | print '(' Exp ')' ';'             {SPrint $3} 

    
    -- Controle de Fluxo
    | if '(' Exp ')' '{' Block '}' else '{' Block '}' { SIf $3 $6 $10 }
    | if '(' Exp ')' '{' Block '}'                    { SIf $3 $6 [] }
    | while '(' Exp ')' '{' Block '}'                 { SWhile $3 $6 }
    | for '(' Stmt Exp ';' StmtNoSemi ')' '{' Block '}'     { SFor $3 $4 $6 $9 } -- Stmt já inclui o ';'
    
    -- Retorno e Expressão solta
    | return Exp ';'               { SReturn $2 }
    | Exp ';'                      { SExpr $1 } -- Chamada de função void: f();

StmtNoSemi
    : Exp '++'                { SExpr (EIncrement $1) } -- i++

-- ============================================================================
-- Expressões
-- ============================================================================

Exp
    -- Literais
    : int_lit       { EValue (VInt $1) }
    | float_lit     { EValue (VFloat $1) }
    | str_lit       { EValue (VString $1) }
    | true          { EValue (VBool True) }
    | false         { EValue (VBool False) }
    | id            { EVar $1 }
    | id '{' ExpList '}'  { EStruct $1 (reverse $3) }

    -- Arrays e Structs
    | '[' ExpList ']'      { EVector (reverse $2) } -- [1, 2, 3]
    | new Type '[' Exp ']' { ENew $2 $4 }           -- new int[size]
    
    -- Acessos (Precedência alta via %left)
    | Exp '[' Exp ']'      { EIndex $1 $3 }         -- arr[i]
    | Exp '.' id           { EField $1 $3 }         -- struct.field

    | Exp '++'             { EIncrement $1 }    -- Incremento: x++
    
    -- Chamada de Função: f(a, b) ou v.f(a)
    | Exp '(' ExpList ')'  { ECall $1 (reverse $3) }
    | Exp '(' ')'          { ECall $1 [] }

    -- Operações Matemáticas
    | Exp '+' Exp   { $1 :+: $3 }
    | Exp '-' Exp   { $1 :-: $3 }
    | Exp '*' Exp   { $1 :*: $3 }
    | Exp '/' Exp   { $1 :/: $3 }
    | Exp '%' Exp   { $1 :%: $3 }
    | '-' Exp %prec NEG { EMinus $2 } -- Menos unário
    
    -- Operações Lógicas
    | Exp '&&' Exp  { $1 :&&: $3 }
    | Exp '||' Exp  { $1 :||: $3 }
    | '!' Exp       { ENot $2 }
    
    -- Comparação
    | Exp '==' Exp  { $1 :==: $3 }
    | Exp '!=' Exp  { $1 :!=: $3 }
    | Exp '<' Exp   { $1 :<: $3 }
    | Exp '>' Exp   { $1 :>: $3 }
    | Exp '<=' Exp  { $1 :<=: $3 }
    | Exp '>=' Exp  { $1 :>=: $3 }
    
    -- Parenteses
    | '(' Exp ')'   { $2 }

ExpList
    : ExpList ',' Exp { $3 : $1 }
    | Exp             { [$1] }

{
parseError :: Token -> Alex a
parseError (Token (l, c) lexeme) = 
    alexError $ "Erro sintático na linha " ++ show l ++ ", coluna " ++ show c ++ 
                ". Token inesperado: " ++ show lexeme

-- Helper simples para distinguir type var de struct type
-- Por convenção neste parser, vamos assumir que se é 'id', virou Struct ou Var
-- A verificação real se é genérico acontece na Análise Semântica.
-- Aqui, simplificamos: se for uma letra minúscula isolada, pode ser considerado generic no contexto de tipo?
-- Para simplificar o parser, tratamos tudo como Struct/Var e o type checker decide.
-- Mas adicionei uma lógica básica na regra 'Type : id' usando essa função:
isTypeVar :: String -> Bool
isTypeVar (c:cs) = null cs && c `elem` ['a'..'z'] -- Ex: 'a', 'b' são type vars. 'Person' não.
isTypeVar _ = False
}