(<**>) :: [a -> b] -> [a] -> [b]
mab <**> ma = [f a | f <- mab, a <- ma]
