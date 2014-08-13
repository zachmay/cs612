module AST where

-- | A LANG program consists of a list of Statements.
type Program = [Statement]

-- | A LANG statement is one of:
--   - Print, which writes the value of an integer expression to the console
--   - String print, which echoes a string to the console
--   - Read, which reads an integer value from the console into a variable
--   - Assign, which stores the value of an integer expression into a variable
--   - While, a while loop with a control expression and a single-Statement body
--   - If-Then, a conditional statement with a control expression and a single-Statement body
--   - If-Then-Else, a conditional statement with a control expression, and two alternative
--     single-Statement bodies
--   - Compound statement that wrapes up multiple statements into a single statement for Whil
--     or If-Then bodies
data Statement = Print Expression
               | PrintString String
               | Read Ident
               | Assign Ident Expression
               | While Expression Statement
               | IfThen Expression Statement
               | IfThenElse Expression Statement Statement
               | Compound [Statement]
               deriving (Show)

-- | A LANG expression is one of:
--   - A variable reference
--   - An integer constant
--   - A binary operator expression
--   - A unary operator expression
data Expression = Var Ident
                | Const Integer
                | BinaryOp BinaryOperator Expression Expression
                | UnaryOp UnaryOperator Expression
                deriving (Show)

-- | A LANG identifier simply wraps a string value.
data Ident = Ident String deriving (Show, Eq)

-- | These are the supported binary operators in LANG.
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

-- | This is the only supported unary operator in lang.
data UnaryOperator = LogicalNegate deriving (Show)
