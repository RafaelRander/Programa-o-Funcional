{- 

menor :: Int -> Int -> Int
menor x y
  |x<=y = x
  |x>y = y 

-}

--Lista conteúdo
menor :: Int -> Int -> Int
menor x y
  |x<=y = x
  |otherwise = y

--1)
nIguais :: Int -> Int -> Int -> Int
nIguais x y z
            |x==y && x==z = 3
            {- 
            |(x==y) && (x/=z) || (x==z) && (x/= y) || (y==z) && (y/=x) = 2
            |((x==y)||(x==z))&&(y\=z)= 2
            -}
            |(x==y)||(x==z)||(y==z)= 2
            |otherwise = 0

menor2 :: Float -> Float -> Float
menor2 n1 n2
            |n1 < n2 = n1
            |otherwise = n2
        
menor3 :: (Float, Float, Float) -> Float
menor3 (n1, n2, n3) = menor2 n1 (menor2 n2 n3)

--3)
menorTres :: (Float, Float, Float) -> Float
menorTres (n1,n2, n3)
                    |(n1 < n2) && (n1 < n3) = n1
                    {-|(n1 < n3) && (n1 > n2) = n2-}
                    |n2 < n3 = n2
                    |otherwise = n3

--4)
maiorMenorTres :: (Float, Float, Float) -> (Float, Float)
maiorMenorTres (n1, n2, n3) 
                        |(n1 > n2) && (n1 > n3) = (n1, menorTres(n1, n2, n3))
                        |(n2 > n3) = (n2, menorTres (n1, n2, n3))
                        |otherwise = (n3, menorTres(n1, n2, n3))

imc :: Float -> Float -> String
imc peso altura 
                |imc <= 18.5 = "A pessoa esta abaixo do peso."
                |imc <= 25 = "A pessoa esta no peso desejavel."
                |imc <= 30 = "A pessoa esta acima do peso."
                |otherwise = "A pessoa esta obesa."
                 where 
                  imc = peso / altura ^ 2

{-
saudacao :: String -> String
saudacao "Joana" = saudacaoLegal ++ "Joana"  
saudacao "Fernando" = saudacaoLegal ++ "Fernando"
 
saudacao nome = saudacaoInfeliz ++ nome
    where
      saudacaoInfeliz = "Nao pensei que ainda estivesse vivo, "
-}

saudacao :: String -> String
saudacao nome
        |nome == "Joana" = saudacaoLegal ++ nome
        |nome == "Fernando" = saudacaoLegal ++ nome
        |otherwise = saudacaoInfeliz ++ nome
        where
          saudacaoLegal = "Ola! Que bom encontrar voce,"
          saudacaoInfeliz = "Nao pensei que ainda estivesse vivo, "

raizes :: Float->Float->Float->(Float, Float)


raizes a b c =
  let
    delta = b^2 - 4 * a * c
    raiz1 = (-b + sqrt delta) / (2 * a)
    raiz2 = (-b - sqrt delta) / (2 * a)
  in
    |delta >= 2 = (raiz1, raiz2)
    |delta == 1 = (raiz1, raiz1)
    |otherwise = error "Nao existem raízes reais."

{-
raizes a b c
            |delta >= 2 = ((-b + sqrt delta)/(2*a), (-b - sqrt delta)/(2*a))
            |delta == 0 =  ((-b + sqrt delta)/(2*a) , (-b + sqrt delta)/(2*a))
            |otherwise  = error "Não existem raízes reais."
             where delta = b^2-4*a*c 
-}









