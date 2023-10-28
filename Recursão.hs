module Exercise where
import Data.Char (isLetter, toLower)
import Data.List (group)


-- Defina as seguintes funções USANDO recursão.



{- 
Nos seguintes exercícios, implemente suas soluções de forma recursiva
Defina os tipos das funções.
-}


{-
Uma função que calcule x * y usando apenas o operador de adição na recursão.

Entrada:
    - x
    - y

Resultado: 
    - x * y

Exemplos:
>>>multiplique 2 3
6
>>>multiplique 3 3
9

>>>multiplique 3 (-3)
-9

>>>multiplique (-3) 3
-9

>>>multiplique (-3) (-3)
9

-}

multiplique :: Int -> Int -> Int
multiplique x n
                |n==0 = 0
                |n==1 = x
                |n>=1 = (x+x) + multiplique x (n-1)
                |otherwise = (-1)*(x+x) + multiplique x (n+1)

{-
Uma função que calcule a n-ésima potência de um número x.

Entrada:
    - x: base
    - n: expoente

Resultado: 
    - x elevado a n

Exemplos:
>>>potência 2 3
8
>>>potência (-3) 2
9
-}

potência :: Int -> Int -> Int
potência x n 
            |n==0 = 1
            |n==1 = x
            |n==(-1) = 1 `div` x
            |n<1 = (1 `div`x) * potência x (n+1)
            |otherwise = x * potência x (n-1)

{-
Uma função que calcule log base 2 de n por divisões sucessivas por 2.

Entrada:
    - n

Resultado: '

    - log_2 (n)

Exemplos:
>>>logBase2 100
6

>>>logBase2 16
4

-}
logBase2::Int->Int
logBase2 n 
            |n==2 = 1
            |n>2 = 1 + logBase2 (n `div` 2)
            |otherwise = error "n deve ser maior ou igual a 1."



{-Escreva uma função **recursiva**  que calcula a soma dos quadrados dos números inteiros entre os parâmetros passados, inclusive.

Entrada:
    - i - Inteiro
    - n - Inteiro
Resultado
    - i^2 + (i+1)^2 + ... + n^2

>>>somaDosQuadrados 1 3
14

>>>somaDosQuadrados 3 6
86

>>>somaDosQuadrados 5 2
54

>>>somaDosQuadrados 3 (-2)
19
-}

somaDosQuadrados i n 
                    |i==n = (i*i)
                    |i<n = (i*i) + somaDosQuadrados (i+1) n
                    |otherwise = (n*n) + somaDosQuadrados i (n+1)


{- 
Defina uma função que calcule a união de duas listas, isto é, sua concatenação, sem repetições.
Assuma que as entradas não possuem repetições.

Entrada:
    - l1, l2: listas de inteiros.

Resultados:
    - lista com a união.
    
>>> união [1,2,3] [3,4,5]
[1,2,3,4,5]
-}

união l1 l2 
           |l1 ==[] && l2 == [] = []
           |l1 == [] = l1
           |l2 == [] = l2

união (x:xs) (y:ys)
                   |x<y = x:união xs (y:ys)
                   |x==y = x: união xs ys
                   |otherwise = y: união (x:xs) ys



{- 
Defina uma função que calcule a diferença de duas listas, isto é, a lista com os elementos da primeira lista que não estão na segunda lista.

Entrada:
    - l1, l2: listas de inteiros.

Resultados:
    - lista com a diferença.
    
>>> diferença [1,2,3] [3,4,5]
[1,2]
-}

diferenca (x:xs) (y:ys)
                       |(y:ys)==[] = (x:xs)
                       |(x:xs)==[] = (y:ys)
                       |x>y = x: diferenca xs (y:ys)
                       |x<y = y: diferenca (x:xs) ys
                       |otherwise = x : diferenca xs ys




{-
Defina uma função que remova as primeiras duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram removidas.

>>>removeDuplicatas [1,2,3,4,5,3,7,8,3]
[1,2,4,5,7,8,3]

-}

removeDuplicatas [] = []
removeDuplicatas (x:[]) = [x]
removeDuplicatas (x1:x2:xs)
                            |x1==x2 = removeDuplicatas (x2:xs)
                            |otherwise = x1 : removeDuplicatas (x2:xs)


{-
Defina uma função que remova as últimas duplicatas de uma lista de inteiros.

Entrada:
    - l - lista de inteiros.

Resultado:
    - lista em que as primeiras ocorrências repetidas de qualquer valor foram mantidas.

>>>removeDuplicatas2 [1,2,3,4,5,3,7,8,3]
[1,2,3,4,5,7,8]

-}

removeDuplicatas2 [] = []
removeDuplicatas2 (x:[]) = [x]
removeDuplicatas2 (x1:x2:xs)
                            |x1==x2 = removeDuplicatas2 (x1:xs)
                            |otherwise = x1 : removeDuplicatas2 (x2:xs)

{-
Uma função que rotacione os elementos de uma tupla n vezes.

Entrada:
    - t: tupla de 5 inteiros.
    - n: número de rotações a ser feito. Rotacionar à direita se n é positivo e a esquerda se n é negativo.

Resultado: 
    - t rotacionado n vezes.

Exemplos:
>>>rotacionar (1,2,3,4,5) 2
(4,5,1,2,3)
>>>rotacionar (1,2,3,4,5) (-2)
(3,4,5,1,2)
-}

rotacionar:: (Int, Int, Int, Int, Int)->Int-> (Int, Int, Int, Int, Int)
rotacionar (x1,x2,x3,x4,x5) 0 = (x1,x2,x3,x4,x5)
rotacionar (x1,x2,x3,x4,x5) n 
              |n<0 = rotacionar (x5,x1,x2,x3,x4) (n+1)
              |otherwise = rotacionar (x2,x3,x4,x5,x1) (n-1)



{-
Desafio!!

A fórmula de Leibniz para pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
estabelece que a constante pode ser calculada como a série

pi = (4/1) - (4/3) + (4/5) - (4/7)...

Implemente uma função recursiva que calcule a constante até uma quantidade n de termos.

Entrada:
    - n: quantidade de termos

Resultado:
    - pi, calculado com n passos da série

Exemplos:

>>>piDeLeibniz 1
4.0

>>>piDeLeibniz 2
2.666666666666667

>>>piDeLeibniz 3
3.466666666666667

>>>piDeLeibniz 2000
3.1410926536210413

>>>piDeLeibniz 3000
3.1412593202657186
-}

comparaInt::Float->Float
comparaInt n
            |n>0 && (n `mod` 2 == 0) = 1
            |n>0 && (n `mod` 2 /= 0) = 0
            |otherwise = error "Número menor que zero não é permitido."    

piDeLeibniz::Float->Float
piDeLeibniz 1 = 4
piDeLeibniz n
             |(comparaInt n) == 1 = (-1)*(4/n) + piDeLeibniz (n-1)
             |(comparaInt n) == 0 = 4/n - piDeLeibniz n-1
             |otherwise = error "Valor não permitido."       




{-
Defina uma função que retorne a sub-lista com t elementos começando na posição i da lista dada.

Entrada:
    - l: lista 
    - i: inteiro com posição inicial da sub-lista
    - t: inteiro com tamanho da sub-lista.

>>> subListaDeAte "entrada" 2 4
"trad"

>>> subListaDeAte [1..10] 2 4
[3,4,5,6]

>>> subListaDeAte [1..4] 2 4
[3,4]

>>> subListaDeAte [] 2 4
[]

-}

subListaDeAte [] i t = []
subListaDeAte l i t = undefined 



{-
Defina uma função que retorne a sub-lista com os últimos u elementos da lista de entrada.

Entrada:
    - l: lista
    - u: inteiro com a quantidade de elementos da sub-lista que termina a string de entrada.

>>> últimosUElementos [1..10] 5
[6,7,8,9,10]

>>> últimosUElementos [1..4] 5
[1,2,3,4]

>>> últimosUElementos [] 5
[]

-}
últimosUElementos l u = undefined 

{-
Defina uma função que receba duas listas e retorne a resultado da concatenação das listas de t elementos começando na posição i das listas de entrada.

Entrada
    - l1: lista
    - l2: lista
    - i: inteiro com posição de início das sub-listas.
    - t: inteiro com tamanho das sub-listas.

>>> subStringDeAteAppend [1..10] [20..45] 3 5
[4,5,6,7,8,23,24,25,26,27]

>>> subStringDeAteAppend [1..3] [20..24] 3 5 
[23,24]

>>> subStringDeAteAppend [1..5] [20..24] 3 5 
[4,5,23,24]
-}

subStringDeAteAppend l1 l2 i u = undefined 



{-
Defina uma função que jogue fora os elementos inicias de uma lista s até que o restante da lista se inicie com um elemento c ou que a lista termine.

Entrada:
    - l: lista
    - e: elemento do mesmo tipo.

Resultado: 
    - a string resultante.

Exemplos:
>>>jogarForaAté "Eu quis dizer, voce nao quis escutar." ','
", voce nao quis escutar."

>>>jogarForaAté "Eu quis dizer, voce nao quis escutar." 'z'
"zer, voce nao quis escutar."

>>>jogarForaAté "Eu quis dizer, voce nao quis escutar." 'v'
"voce nao quis escutar."

-}

jogarForaAté l e = undefined 


{-
Escreva uma função que retorne duplas formadas pelos por elementos das duas metades da lista, sendo o primeiro elemento do resultado formado pelo 
primeiro elemento da primeira metade da lista mais o primeiro da segunda metade da lista, o segundo elemento formado pelo segundo elemento da primeira
metade mais o segundo elemento da segunda metade e assim por diante.

Dica: splitAt e length

>>>combinaMetades [1,2,3,4,5,6]
[(1,4),(2,5),(3,6)]

>>>combinaMetades [1,2,3,4,5,6,7]
[(1,4),(2,5),(3,6)]

-}

combinaMetades l = undefined 

{-
Escreva uma função que reverta combinaMetades. Ou seja
>>>descombinaMetades [(1,4),(2,5),(3,6)] 
[1,2,3,4,5,6]
-}
descombinaMetades l = undefined 





{-
Escreva uma função que separe repetições consecutivas dentro de uma lista.

Dica: investigue a função group.

>>>empacote "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]

>>>empacote ""
[]


>>>empacote [1,1,12,2,2,3,3,3,4,4,4,3,3,3,2,2,2,1,1,1]
[[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]

-}

empacote :: (Eq a) => [a] -> [[a]]
empacote l = undefined 


{-
Dado uma lista empacotada, como a gerada pela função anterior, gere uma lista de duplas tal que:
- para cada pacote haja uma dupla no resultado.
- a dupla tem como primeiro elemento o dado repetido na lista correspondente e como segundo elemento o comprimento de tal lista.

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3],[2,2,2],[1,1,1]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3),(2,3),(1,3)]

>>>compacte ["aaaa","b","cc","aa","d","eeee"]
[('a',4),('b',1),('c',2),('a',2),('d',1),('e',4)]

>>>compacte []
[]

>>>compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
[(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]

-}
compacte l = undefined 


{-
Escreva uma função que reverta a função compacte, definida acima, ou seja, tal que
>>>descompacte (compacte [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]) == [[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]
True

>>> descompacte [(1,2),(12,1),(2,2),(3,3),(4,3),(3,3)]
[[1,1],[12],[2,2],[3,3,3],[4,4,4],[3,3,3]]


Dica: use replicate

-}

descompacte l = undefined 


{-
Escreva uma função que reverta a função empacote, acima, definida acima, ou seja, tal que
>>>desempacote (empacote "aaaabccaadeeee") == "aaaabccaadeeee"
True

>>> desempacote ["aaaa","b","cc","aa","d","eeee"]
"aaaabccaadeeee"

-}

desempacote l = undefined 
