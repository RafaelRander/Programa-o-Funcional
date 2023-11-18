cabeca :: [Float]->Float
cabeca (x:_) = x

cauda :: [Float]->[Float]
cauda (x:xs) = xs

produto :: [Float] -> Float
produto [x] = x
produto (x:xs) = x * produto xs

soma :: (Num a)=>[a]->a
soma [x] = x
soma (x:xs) = x + soma xs

comprimento :: String -> Int
comprimento [] = 0
comprimento (_:xs) = 1 + comprimento xs

inversa :: (Num a)=>[a]->[a]
inversa [] = []
inversa (x:xs) = (inversa xs)++[x]

impar :: Integer->Bool
impar n
        |n `mod` 2 /= 0 = True
        |otherwise = False

nImpar :: [Integer]->Integer
nImpar [] = 0
nImpar (x:xs)
             |impar x = x + (nImpar xs)
             |otherwise = nImpar xs

mult3 :: Integer->Bool
mult3 n
        |n `mod` 3 == 0 = True
        |otherwise = False
    
somaMult3 :: [Integer]->Integer
somaMult3 [] = 0
somaMult3 (x:xs)
                |mult3 x = x + somaMult3 xs
                |otherwise = somaMult3 xs

produtoLista :: [Integer]->Integer
produtoLista [x] = x
produtoLista (x:xs) = x * produtoLista xs

n_esimo :: [a]->Integer->a
n_esimo [] n = error "Lista não possui elementos suficientes."
n_esimo (x:xs) n
                      |n == 0 = x
                      |otherwise = n_esimo xs (n-1)

ultimo :: [a]->a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--i
duplica :: [a] -> [a]
duplica [] = []
duplica (x:xs) = [x,x] ++ duplica xs

--k
substitui :: Integer->Integer->[Integer]->[Integer]
substitui _ _ [] = error "Lista não possui elementos suficientes para substituição"
substitui x y (h:t)
                    |h==x = [y] ++ substitui x y (t)
                    |otherwise = substitui x y t
                    
--l
substituiPri :: Int->Int->[Int]->[Int]
substituiPri _ _ [] = error "Não existe elementos suficientes para substituição."
substituiPri x y (h:t)
                     |x==h = [y] ++ t
                     |otherwise = [h] ++ substituiPri x y t

--m
produtoInterno :: [Integer]->[Integer]->Integer
produtoInterno [] [] = 0
produtoInterno (h1:t1) (h2:t2) = h1*h2 + produtoInterno t1 t2


--n
maiorElemento :: [Integer]->Integer
maiorElemento [x] = x
maiorElemento (x:x2:xs)
                    |x>x2 = maiorElemento (x:xs)
                    |otherwise = maiorElemento (x2:xs)

--o
desduplica :: [a] -> [a]
desduplica [] = []
desduplica (x:y:xs) = [x] ++ desduplica xs

--p
veriImpares :: [Integer]->Bool
veriImpares [] = True
veriImpares (x:xs)
                  |impar x = True && veriImpares xs
                  |otherwise = False

--q
insereOrd :: Integer->[Integer]->[Integer]
insereOrd x [] = [x]
insereOrd n (x:xs)
                  |x>n = n:x:xs
                  |otherwise = [x] ++ insereOrd n xs
 
--r
quadLista :: [Integer]->[Integer]
quadLista [x] = [x^2]
quadLista (x:xs) = [x^2] ++ quadLista xs

--s
pertence :: Integer->[Integer]->Bool
pertence n [] = False
pertence n (x:xs)
                  |n==x = True
                  |otherwise = pertence n xs

--t
remover_todos :: Integer->[Integer]->[Integer]
remover_todos n [] = []
remover_todos n (x:xs)
                      |n==x = remover_todos n xs
                      |otherwise = x:remover_todos n xs

--u
primeiros :: [(Integer, Integer)]->[Integer]
primeiros [] = []
primeiros (x:xs) = [fst(x)] ++ primeiros xs

--v
concatList :: [[Integer]]->[Integer]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

--w
difAbs :: [Integer]->[Integer]->[Integer]
difAbs [x] [y] = [x-y]
difAbs (x:y) (z:w) = (x-z):difAbs y w

--5
alternada :: [Integer]->Bool
alternada [x] = True
alternada (x:y:z) 
                  |(par x && impar y)|| (par y && impar x) = True && alternada z
                  |otherwise = False

--6
converte1 :: Integer->[Integer]
converte1 0 = []
converte1 x = converte1 (x `div` 2) ++ [x `mod` 2]

--7
somaN :: [Integer]->Integer
somaN [] = 0
somaN (x:xs) = 1 + (somaN xs)

converte :: [Integer]->Integer
converte [x] = 1
converte (x:xs) = (x * (2 ^ ((somaN (x:xs))-1))) + converte xs

--8
digitos :: Integer->[Integer]
digitos valor
             |valor == 0 = []
             |valor `mod` 10 == 0 = [valor]
             |otherwise =  digitos (valor `div` 10) ++ [valor `mod` 10]

--[0,1,0,1] = 1 * 2 ^0 + 0 * 2^1 + 1 * 2 ^ 2 + 0 * 2 ^ 3 = 5
