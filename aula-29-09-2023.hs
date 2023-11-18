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

ultimo :: (Num a)=>[a]->a
ultimo [x] = x
ultimo (_:xs) = ultimo xs 