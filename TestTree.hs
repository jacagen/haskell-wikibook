import AT
import Control.Monad

{-

Sample tree:

+--3
|
+--+--5
   |
   +--+--4
      |
      +--2
-}

testTree = 
    B
        (L 3)
        (B
            (L 5)
            (B 
                (L 4)
                (L 2)
            )
        )


testFunTree =
    B
        (L (* 10))
        (L (* 100))

-- ap testFunTree testTree
testAp = 
    B 
        (B 
            (L 30) 
            (B 
                (L 50) 
                (B 
                    (L 40) 
                    (L 20)
                )
            )
        ) 
        (B 
            (L 300) 
            (B 
                (L 500) 
                (B 
                    (L 400) 
                    (L 200)
                )
            )
        )

testMonad n = B (L (n * 10)) (L (n * 100)) 

pattern = AT' (L (* 10))

lopsided = AT' (B (L (* 10)) (L (*100))) <*> AT' testTree
