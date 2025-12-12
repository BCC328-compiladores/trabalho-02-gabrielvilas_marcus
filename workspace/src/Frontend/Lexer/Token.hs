module Frontend.Lexer.Token where

data Token
  = Token {
      pos :: (Int, Int) -- Linha e Coluna
    , lexeme :: Lexeme
    }
  deriving (Eq, Show)

data Lexeme
  -- Palavras Chave (Keywords)
  = KW_Func | KW_Struct | KW_Let | KW_Return
  | KW_If | KW_Else | KW_While | KW_For
  | KW_New | KW_Forall | KW_Void | KW_Read | KW_Print
  -- Tipos Primitivos (como keywords)
  | KW_Int | KW_Float | KW_String | KW_Bool
  -- Literais Booleanos
  | KW_True | KW_False
  
  -- Identificadores e Literais
  | TokId String -- Palavras NAO reservadas (identificadores)
  | TokInt Int
  | TokFloat Double
  | TokString String

  -- Operadores
  | TokIncrement    -- ++
  | TokAssign       -- =
  | TokPlus         -- +
  | TokMinus        -- -
  | TokTimes        -- *
  | TokDiv          -- /
  | TokMod          -- %
  | TokAnd          -- &&
  | TokOr           -- ||
  | TokNot          -- !
  | TokEq           -- ==
  | TokNeq          -- !=
  | TokLt           -- <
  | TokGt           -- >
  | TokLeq          -- <=
  | TokGeq          -- >=
  | TokArrow        -- -> (usado em tipos de função)
  | TokDot          -- .  (acesso a registro/generics)

  -- Delimitadores
  | TokLParen       -- (
  | TokRParen       -- )
  | TokLBrace       -- {
  | TokRBrace       -- }
  | TokLBracket     -- [
  | TokRBracket     -- ]
  | TokSemi         -- ;
  | TokColon        -- :
  | TokComma        -- ,
  
  -- Especial
  | TokEOF
  deriving (Eq, Show)