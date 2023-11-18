fatorial :: Int->Int
fatorial n = if n==1
                then 1
                else n * fatorial n-1