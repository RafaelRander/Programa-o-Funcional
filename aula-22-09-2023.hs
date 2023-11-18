
fatorial :: Integer->Integer
fatorial n
            |n == 0 = 1
            |n>1 = n * fatorial (n-1)
            |otherwise = error "Valor deve ser maior que 0."

{-
soma :: (Num a)=>a->a
soma n = m + n
-}

soma :: Integer->Integer
soma 0 = 0
soma n 
      |n>1 = n + soma (n-1)
      |otherwise = n + soma (n+1)

soma' :: Integer->Integer
soma' n
        |n == 1 = 1
        |n > 1 = n + soma (n-1)
        |otherwise = error "Valor deve ser positivo."

mdc :: Integer->Integer->Integer
mdc m n
        |n == 0 = m
        |n > 0 = mdc n (m `mod` n)
        |otherwise = error "Valor de m e n deve ser maior que 0."


mmc :: Integer->Integer->Integer
mmc m n = (m * n) `div` (mdc m n)


--4


--5
comb :: Integer->Integer->Integer
comb n k
        |k==1 = n
        |k==n = 1
        |k<n = comb (n-1) (k-1)
        |otherwise = error "Valores não permitidos."

--6
serieTaylor :: Integer->Integer
serieTaylor x
             |x==0 = 1
             |x>0 = 1 + (x^x `div` (fatorial x)) + serieTaylor (x - 1)
             |otherwise = erro "Valor não permitido, x deve ser maior que 0."
  
