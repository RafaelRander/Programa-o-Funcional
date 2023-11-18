ls1 :: [Int]
ls1 = [3,1,4,1,5]

ls2 :: [Int]
ls2 = [1,6,1,8]

{-terceiro :: [Int]->Int
terceiro x = if (length x) < 3 then x
             else head (tail (tail x))-}

terceiro :: [Int]->Int
terceiro x = head (tail (tail x))

ultimo :: String->Char
ultimo lista = head (reverse lista)

penultimo :: String->Char
penultimo lista = head (tail (reverse lista))

inicio :: String->String
inicio lista = reverse (tail (reverse lista))

iniciais :: [Char]->[Char]->(Char, Char)
iniciais name1 name2 = (head name1, head name2)

