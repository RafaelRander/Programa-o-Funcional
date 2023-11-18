import Data.Char

--a
primeiros :: [(a,b)]->[a]
primeiros [] = []
primeiros (x:y) = (fst x):(primeiros y)

primeirosMap lista = map fst lista

--b
maiusculasN :: [Char]->[Char]
maiusculasN [] = []
maiusculasN (x:xs) = (toUpper x): maiusculasN xs

maiusculas lista = map toUpper lista

--c
dobra ::(Num a)=>[a]->[a]
dobra [] = []
dobra (x:xs) = (2 * x):(dobra xs)

dobraOS lista = map (*2) lista


--pega_letras
pega_letras :: [Char]->[Char]
pega_letras [] = []
pega_letras (x:xs)
                  |isAlpha x = x:(pega_letras xs)
                  |otherwise = pega_letras xs

pega_letrasOS lista = filter isAlpha lista

--7 a) pega pares OS
pegaPares :: (Integral a)=>[a]->[a]
pegaPares [] = []
pegaPares (x:xs)
                |even x = x:(pegaPares xs)
                |otherwise = pegaPares xs

pegaParesOS lista = filter even lista


--7 c)
remove ::(Eq a)=>a->[a]->[a]
remove _ [] = []
remove e (x:xs)
                |e==x = remove e xs
                |otherwise = x:(remove e xs)

removeOS ::(Eq a)=>a->[a]->[a]
removeOS e lista = filter (/=e) lista

somatorio :: Num a=>[a]->a
somatorio lista = foldr (+) 0 lista

--7e)
--desiguais


--8a)
produto :: (Num a) => [a] -> a
produto [x] = x
produto (x:xs) = x * (produto xs)

produtoF :: (Num a) => [a] -> a
produtoF lista  = foldr (*) 1 lista

--8b)
eLogico :: [Bool]->Bool
eLogico [] = error "Deve existir ao menos um elemento."
eLogico [x] = x
eLogico (x:xs) = x && (eLogico xs)

eLogicoF :: [Bool]->Bool
eLogicoF lista = foldr (&&) True lista

--8c)
concatena :: [String]->String
concatena [] = ""
concatena (x:xs) = x ++ concatena xs

concatenaF lista = foldr (++) [] lista

--9a)
--numof :: Num a=>[a]->a
numof lista e =  (foldr (+) 0  (map (div e) (filter (==e) lista)))


