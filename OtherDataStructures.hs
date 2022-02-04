data Weird a b = First a
               | Second b
               | Third [(a,b)]
               | Fourth (Weird a b)
               | Fifth [Weird a b] a (Weird a a, Maybe (Weird a b))

weirdMap :: (a -> c) -> (b  -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
    where
        g (First a) = First $ fa a
        g (Second b) = Second $ fb b
        g (Third ts) = Third [(fa x, fb y) | (x, y) <- ts]
        g (Fourth w) = Fourth (g w)

weirdFold :: (a -> x) -> (b -> x) -> ([(a, b)] -> x) -> (x -> x) -> ([x] -> a -> (Weird a a, Maybe x) -> x) -> Weird a b -> x
weirdFold fax fbx fabx fw ff = g
    where 
        g (First a) = fax a
        g (Second b) = fbx b
        g (Third l) = fabx l
        g (Fourth w) = fw (g w)
        g (Fifth wab a (waa, mwab)) = ff (map g wab) a (waa, maybeMap g mwab)
            where
                maybeMap f Nothing = Nothing    
                maybeMap f (Just w) = Just (f w)