module MayFail (

  MayFail (
    Raise,
    Ok
  ),

  Exception (
    DivByZero,
    NotFound,
    NullPointer,
    InvalidCase,
    Other
  ),

  ExHandler,

  tryCatch
) where

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero   | NotFound     | NullPointer
               | InvalidCase | Other String

type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise e) g f = f e
tryCatch (Ok a)    g f = g a

