import Distribution.Simple.Utils (xargs)
r               = 5.0
area        r   = pi * r ^ 2
double      x   = 2 * x
quadruple   x   = double (double x)
square      x   = x * x
half        x   = x / 2
halfM12     x   = half x - 12
areaRect l w = l * w
areaTriangle b h = (b * h) / 2
boxVol l w h = l * w * h

heron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

absolute x 
    | x < 0 = -x
    | otherwise = x

numOfRealSolutions a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
            disc = b^2 - 4*a*c