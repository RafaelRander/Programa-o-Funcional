dobraPos::[Int]->[Int]
dobraPos [] = []
dobraPos (x:xs)
                |x>0 = 2*x : dobraPos xs
                |otherwise = dobraPos xs

dobraPosOS lista = map (2*) (filter (>0) lista)

maiorQueZero x = x>0

dobraPosFOS lista = map (2*) (filter maiorQueZero lista)

--fatores n = [i|i<-[1..n], n mod i == 0]

primeiros :: [(a, b)]->[a]
primeiros lista = [fst x| x<-lista]
