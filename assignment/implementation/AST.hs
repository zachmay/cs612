module AST where

type Program = [Statement]

data Statement = Print Expression
               | PrintString String
               | Read Ident
               | Assign Ident Expression
               | While Expression Statement
               | IfThen Expression Statement
               | IfThenElse Expression Statement Statement
               | Compound [Statement]
               | Empty
               deriving (Show)

data Expression = Var Ident
                | Const Integer
                | BinaryOp BinaryOperator Expression Expression
                | UnaryOp UnaryOperator Expression
                deriving (Show)

data Ident = Ident String deriving (Show, Eq)

data BinaryOperator = Add
                    | Sub
                    | Mul
                    | Div
                    | Mod
                    | Exp
                    | Equal
                    | NotEqual
                    | GreaterThan
                    | LessThan
                    | GreaterThanEq
                    | LessThanEq
                    | And
                    | Or
                    deriving (Eq, Show, Enum)

data UnaryOperator = LogicalNegate deriving (Show)


