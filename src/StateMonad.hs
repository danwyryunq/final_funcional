module StateMonad (
  State,

  evalState,
  execState,
  getState,
  updState,
  errState,
  runState
) where

import MayFail

newtype State s a = State { runState :: s -> MayFail (a,s) }

-- Ejercicio 2

instance Monad (State s) where
    -- return :: a -> State MayFail (a,s)
    return x = State $ \s -> Ok (x,s)

    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State mayfail) f = State $ \s -> case mayfail s of
                                            (Raise e)       -> Raise e 
                                            (Ok (a,estado)) -> let (State nMayfail) = f a
                                                                in nMayfail estado
evalState :: State s a -> s -> MayFail a
evalState (State f) = \s -> case  f s of  
                                (Ok (x,s')) -> Ok x 
                                (Raise e)  -> Raise e

execState :: State s a -> s -> MayFail s
execState (State f) = \s -> case  f s of  
                                (Ok (x,s' )) -> Ok s' 
                                (Raise e)  -> Raise e

getState :: State s s
getState =  State $ \s -> Ok (s,s)

updState :: (s -> s) -> State s ()
updState f =  State $ \s -> Ok ((), f s) 

errState :: Exception -> State s a
errState = \e -> State $ \s -> Raise e
