import Control.Monad

{-
data TurnstileState = Locked | Unlocked
data TurnstileOutput = Tut | Thank | Open
data TurnstileInput = Push | Coin


data StateMachine = StateMachine TurnstileState

advance :: StateMachine -> TurnstileInput -> (StateMachine, TurnstileOutput)
advance sm@(StateMachine Locked) Push = (sm, Tut)
advance sm@(StateMachine Locked) Coin = (StateMachine Unlocked, Thank)
advance sm@(StateMachine Unlocked) Push = (StateMachine Locked, Open)
advance sm@(StateMachine Unlocked) Coin = (sm, Thank)
-}

data TurnstileState = Locked | Unlocked
    deriving (Show, Eq)
data TurnstileOutput = Tut | Thank | Open
    deriving (Show, Eq)

type TurnstileAction = TurnstileState -> (TurnstileOutput, TurnstileState)
type TurnstileHistory = ([TurnstileOutput], TurnstileState)

push, coin :: TurnstileAction
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

coin Locked = (Thank, Unlocked)
coin Unlocked = (Thank, Unlocked)

takeAction :: TurnstileHistory -> TurnstileAction -> TurnstileHistory
takeAction (outputHistory, state) action = 
    let (output, newState) = action state
    in (outputHistory ++ [output], newState) 


takeActions :: TurnstileHistory -> [TurnstileAction] -> TurnstileHistory
takeActions  = foldl takeAction 

regularPerson, distractedPerson, hastyPerson :: TurnstileState -> TurnstileHistory
regularPerson s = takeActions ([], s) [coin, push]
distractedPerson s = takeActions ([], s) [coin]
hastyPerson s = 
    let hist@(outputs, state) = takeActions ([], s) [push]
    in case last outputs of 
        Tut -> takeActions hist [coin, push]
        Open -> hist
        _ -> error "Impossible"

        

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

tuesday :: TurnstileState ->  TurnstileHistory
tuesday s = 
    let (regOut, regState) = regularPerson s
        (hastyOut, hastyState) = hastyPerson regState
        (distractedOut, distractedState) = distractedPerson hastyState
        (hasty2Out, hasty2state) = hastyPerson distractedState
    in  (regOut ++ hastyOut ++ distractedOut ++ hasty2Out, hasty2state)
    
    
luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair who s1 =
    let first = if who then regularPerson else distractedPerson
        (_, s2) = first s1 
        (r, s3) = push s2 
    in (r == Open, s3)
  
