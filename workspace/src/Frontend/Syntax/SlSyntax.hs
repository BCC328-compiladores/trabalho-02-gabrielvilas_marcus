module Frontend.Syntax.SlSyntax where

data Sl 
    = Sl [Definition]
  deriving (Eq, Ord, Show)

type Var = String
type Block =  [Stmt]
type TypeVar = String

data Field 
    = Field String Type
  deriving (Eq, Ord, Show)

data Param 
    = Param String (Maybe Type)
  deriving (Eq, Ord, Show)

data Definition 
    = DStruct String [Field]
    | Dfunc String [TypeVar] [Param] (Maybe Type) Block-- Generics adicionados: [TypeVar]
    -- Ex: func map<a,b>(...) vira DFunc "map" ["a","b"] ...
  deriving (Eq, Ord, Show)
    -- | Dfunc String [Param] Type Block

data Type 
    = TInt | TFloat | TString | TBool | TVoid
    | TVector Type -- vetor dinamico vetor[]
    | TVectorN Type Int --vetor estatico vetor[5]
    | TStruct String
    | TVar TypeVar --variavel de tipo generico 
    | TFunc [Type] Type
    deriving (Eq, Ord, Show)

data Stmt
    = SAssign Exp Exp 
    | SLet Var Type (Maybe Exp) -- declaraçao de variavel explicitando tipo 
    | SLetInfer Var Exp -- declaraçao de variavel sem tipo explicito
    | SRead Exp -- verificar se havera leitura na linguagem
    | SPrint Exp 
    | SIf Exp Block Block 
    | SFor Stmt Exp Stmt Block 
    | SWhile Exp Block 
    | SReturn Exp 
    | SExpr Exp           -- ÚTIL: Para chamadas de função que ignoram retorno: f(x);
    deriving (Eq, Ord, Show)

data Exp
    = EValue Value 
    | EVector [Exp]
    | EStruct String [Exp]
    | EIndex Exp Exp -- acesso a índice do array
    | EField Exp String -- acesso a campo de struct
    | EArraySize Exp -- retorna o tamanho do array 
    | ENew Type Exp -- declara um novo vetor
    | ECall Exp [Exp] -- chamada de funcao
    | EIncrement Exp
    | EVar Var 
    | ENot Exp 
    | EMinus Exp
    | Exp :+: Exp
    | Exp :-: Exp
    | Exp :*: Exp
    | Exp :/: Exp
    | Exp :%: Exp
    | Exp :&&: Exp
    | Exp :||: Exp
    | Exp :<:  Exp
    | Exp :>:  Exp
    | Exp :<=: Exp
    | Exp :>=: Exp
    | Exp :==: Exp
    | Exp :!=: Exp
    deriving (Eq, Ord, Show)

data Value
    = VInt Int
    | VFloat Double
    | VString String
    | VBool Bool
  deriving (Eq, Ord, Show)