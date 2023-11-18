--segundo :: [a]->a
segundo xs = head (tail xs)

trocar :: (a,b)->(b,a)
trocar (x,y) = (y,x)

parear :: a->b->(a,b)
parear x y = (x, y)

dobro :: (Num a)=>a->a
dobro x = x * 2

palindromo :: Eq a=>[a]->Bool
palindromo xs = reverse xs == xs

cabeca :: [a]->a
cabeca (x:xs) = x

--2

{-
iguais :: (Eq a) => a->a->a->Integer
iguais x y z
            |x==y && x==z = 3
            |x==y || x==z = 2
            |otherwise = 0
-}

iguais :: (Eq a) =>(a,a,a)->Integer
iguais (x,y,z)
            |x==y && x==z = 3
            |x==y || x==z = 2
            |otherwise = 0

--4
maiorMenor :: (Ord a)=>(a,a,a)->(a,a)
maiorMenor (x,y,z)
                  |x>y && y>z = (x,z)
                  |x>y && z>y && x>z = (x,y)
                  |y>x && x>z = (y,z)
                  |y>x && z>x && y>z = (y,x)
                  |z>x && x>y = (z,y)
                  |otherwise = (z,x)

--5a
comprimento :: [a]->Integer
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

--5b
somatorio:: (Num a)=>[a]->a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs


--5c
somatorioImpares :: Integral a => [a] -> a
somatorioImpares [] = 0
somatorioImpares (x:xs)
                        |x `mod` 2 /= 0 = x + somatorioImpares xs
                        |otherwise = somatorioImpares xs

--5d
somatorioMultTres :: Integral a => [a] -> a
somatorioMultTres [] = 0
somatorioMultTres (x:xs)
                        |x `mod` 3 == 0 = x + somatorioMultTres xs
                        |otherwise = somatorioMultTres xs

--5e
produtorio :: (Num a)=>[a]->a
produtorio [] = error "Lista deve possuir ao menos um elemento."
produtorio [x] = x
produtorio (x:xs) = x * produtorio xs

--5f
enesimo :: [a]->Integer->a
enesimo [] elemento = error "Lista não possui elementos suficientes."
enesimo [x] 0 = x
enesimo (x:xs) elemento
                        |elemento == x = x
                        |otherwise = enesimo xs (elemento-1)










