module AssemblyRepresentation (
    AssemblyProgram (AssemblyProgram),

    Mnemonic (
        NoOp,
        Load,
        Read,
        Store,
        ADD,
        ADDmod2,
        MUL,
        SUB,
        DIV,
        MOD,
        CompEq,
        CompGt,
        Mark,
        Jump,
        JumpIfZ,
        Push,
        Pop
    ),

    VarName,

    VarIndex,

    Reg (A, B),

    Label,

    delta
) where


-- Assembly Representation

data AssemblyProgram = AssemblyProgram [Mnemonic]

data Mnemonic = NoOp
              | Load    Reg Int
              | Read    Reg VarName | Store   Reg VarName
              | ADD     Reg Reg     | ADDmod2 Reg Reg
              | MUL     Reg Reg     | SUB     Reg Reg
              | DIV     Reg Reg     | MOD     Reg Reg
              | CompEq  Reg Reg     | CompGt  Reg Reg
              | Mark    Label
              | Jump    Label       | JumpIfZ Reg Label
              | Push    Reg         | Pop     Reg
   deriving Show

type VarName  = String
type VarIndex = Int
data Reg      = A | B deriving (Eq, Show)
type Label    = String


-- delta

delta True  = 1
delta False = 0

