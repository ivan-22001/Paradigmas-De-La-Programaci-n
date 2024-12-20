-- Ejercicio 2----------------------------
valorAbsoluto   ::  Float -> Float
valorAbsoluto x  |  x < 0 = -x
                 |  otherwise = x

bisiesto :: Int -> Bool
bisiesto x = if x `mod` 4 == 0 then True else False

factorial :: Int -> Int
factorial x | x == 0 = 1
            | otherwise = x* factorial(x-1)

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = length [ y | y <- [2..x], x `mod` y == 0 && esPrimo y] 


esPrimo :: Int -> Bool
esPrimo n = null [ x | x <- [2..n-1], n `mod` x == 0]

--Ejercicio 3------------------------


inverso :: Float -> Maybe Float
inverso a = if a == 0 then Nothing else Just (1 / a)

aEntero :: Either Int Bool -> Int
aEntero a = if a == Right True then 1 else if a == Right False then 0 else case a of  Left x -> x
--Ejercicio 4------------------------

limpiar :: String -> String -> String
limpiar [] ts = ts
limpiar (x:xs) ts = limpiar xs (filter (/= x) ts)

difPromedio :: [Float] -> [Float]
difPromedio x = dif x (promedio x)

promedio :: [Float] -> Float
promedio [] = 0
promedio xs = sum xs / fromIntegral (length xs)

dif :: [Float] -> Float -> [Float]
dif [] p = []
dif (x:xs) p = x-p : dif xs p 

todosIguales :: [Int] -> Bool
todosIguales (x:xs) = foldr (\y acc -> y == x && acc) True xs

-- Ejercicio 5 ------------------------

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

foldAB :: ( b -> a -> b -> b) -> b -> AB a -> b
foldAB _ cb Nil = cb
foldAB f cb (Bin i r d) = f (foldAB f cb i) r (foldAB f cb d)

recAB :: (a -> AB a -> AB a -> b -> b -> b) -> b -> AB a -> b
recAB _ cb Nil = cb
recAB f cb (Bin i r d) = f r i d (recAB f cb i) (recAB f cb d)

altura :: AB a -> Integer
altura = foldAB (\i _ d -> 1 + max i d) 0

cantNodos :: AB a -> Integer
cantNodos = foldAB (\i _ d -> 1 + i + d) 0

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin izq ra der)= foldAB(\ i r d -> if f r i && f r d then r else if f i r then i else d) ra (Bin izq ra der) 

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq r der) = Bin (negacionAB izq) (not r) (negacionAB der)


productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq r der) = productoAB izq * r * productoAB der

merge :: [Int] -> [Int] -> [Int]
merge [] x = x
merge x [] = x
merge (x:xs) (t:ts) = if x < t then x : merge xs (t:ts) else t : merge (x:xs) ts

halve :: [a] -> ([a],[a])
halve xs = (take lhx xs, drop lhx xs)
           where lhx = length xs `div` 2

mergeSort :: [Int] -> [Int]
mergeSort [] = [] 
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left ) (mergeSort right) 
               where (left,right) = halve xs


instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin left val right) = "Bin (" ++ show left ++ ") " ++ show val ++ " (" ++ show right ++ ")"
    
myTree :: AB Bool
myTree = negacionAB (Bin (Bin Nil True Nil) False (Bin Nil True Nil))




--- Ejercicio 10 


generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []
generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])




foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f cb 0 = cb
foldNat f cb n = f n (foldNat f cb (n-1))

potencia :: Integer -> Integer -> Integer
potencia n = foldNat (\_ acc-> acc*n) 1 

main :: IO ()
main = do
    print (potencia 2 3)  -- Esto imprimirá 8
    print (potencia 5 0)  -- Esto imprimirá 1
    print (potencia 3 4)  -- Esto imprimirá 81
