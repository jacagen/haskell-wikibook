import Distribution.FieldGrammar (Set')
bunnyGenerate :: Int -> [String]
bunnyGenerate n = foldr (\_ -> concat . replicate 3) ["bunny"] [1..n]



