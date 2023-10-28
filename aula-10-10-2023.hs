tamanho :: [Int]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

separa :: [ Int ] -> [ Int ]
separa (p:s:r) = (s:r)

separab :: [ Int ] -> [ Int ]
--separab (p:s:r) = (r:s:p)
separab [x] = [x]
separab (p:s:r) = separab r ++ [s] ++ [p]

separac :: [ Int ] -> [ Int ]
separac (p:r) = r

impar :: Int -> Bool
impar x
        |x `mod` 2 /= 0 = True
        |otherwise = False

par :: Int -> Bool
{-
par x
        |x `mod` 2 == 0 = True
        |otherwise = False
-}
par x = not (impar x)

soma_digitos :: Int -> Int
soma_digitos 0 = 0
soma_digitos n = (soma_digitos (n `div` 10)) + (n `mod` 10)

multiplo3 :: Int->Bool
multiplo3 n
            |(soma_digitos) n == 0 = True
            |otherwise = False

somatorio_imp :: [Int]->Int
somatorio_imp [] = 0
somatorio_imp (x:xs)
                |x `mod` 2 /= 0 = x + somatorio_imp xs
                |otherwise = somatorio_imp xs


somatorio_i :: [Int]->Int
somatorio_i [] = 0
somatorio_i (x:xs)
                |impar x = x + somatorio_i xs
                |otherwise = somatorio_i xs

somatorio_p :: [Int]->Int
somatorio_p [] = 0
somatorio_p (x:xs)
                |par x = x + somatorio_p xs
                |otherwise = somatorio_p xs

somaQuadrados :: [Int]->Int
somaQuadrados [] = 0
somaQuadrados (x:xs) = x^2 + somaQuadrados xs

--soma_mult_3 :: [Int]->Int





