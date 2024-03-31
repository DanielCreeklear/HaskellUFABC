module Main where

-- Exercicio 1
raiz2Grau :: Floating a => a -> a -> a -> (a, a) 
raiz2Grau a b c = ((-b + sqrt (b^2 - 4 * a * c))/ (2 * a), (-b - sqrt (b^2 - 4 * a * c))/ (2 * a))

-- Exercicio 2
raiz2GrauV2 :: Floating a => a -> a -> a -> (a, a)
raiz2GrauV2 a b c = (x1, x2)
  where
    x1 = (-b + sqrt delta) / (2*a)
    x2 = (-b - sqrt delta) / (2*a)
    delta = b^2 - 4 * a * c

-- Exercicio 3
raiz2GrauV3 :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2GrauV3 a b c = (x1, x2)
  where
    x1 = if delta < 0 then 0 else (-b + sqrt delta) / (2*a)
    x2 = if delta < 0 then 0 else (-b - sqrt delta) / (2*a)
    delta = b^2 - 4 * a * c

-- Exercicio 4
raiz2GrauV4 :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2GrauV4 a b c
  -- | delta < 0 = error "Delta negativo."
  | delta < 0 = (0, 0)
  | otherwise = (x1, x2)
  where
    x1 = (-b + sqrt delta) / (2*a)
    x2 = (-b - sqrt delta) / (2*a)
    delta = b^2 - 4 * a * c

-- Exercicio 5
(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False

main :: IO ()
main = do
  -- Exercicio 1
  let (aX1, aX2) = raiz2Grau 4 3 (-5)
      (bX1, bX2) = raiz2Grau 4 3 5
  putStrLn $ "Resultado Ex 1 - a: (" ++ show aX1 ++ ", " ++ show aX2 ++ ")"
  putStrLn $ "Resultado Ex 1 - b: (" ++ show bX1 ++ ", " ++ show bX2 ++ ")"

  -- Exercicio 2
  let (x1, x2) = raiz2GrauV2 4 3 (-5)
  putStrLn $ "Resultado Ex 2: (" ++ show x1 ++ ", " ++ show x2 ++ ")"

  -- Exercicio 3
  let (aX1, aX2) = raiz2GrauV3 4 3 (-5)
      (bX1, bX2) = raiz2GrauV3 4 3 5
  putStrLn $ "Resultado Ex 3 - a: (" ++ show aX1 ++ ", " ++ show aX2 ++ ") "
  putStrLn $ "Resultado Ex 3 - b: (" ++ show bX1 ++ ", " ++ show bX2 ++ ") "

  -- Exercicio 4
  let (x1, x2) = raiz2GrauV4 4 3 5
  putStrLn $ "Resultado Ex 4: (" ++ show x1 ++ ", " ++ show x2 ++ ") "

  -- Exercicio 5
  let andFunctionTrue = True &&& True
  let andFunctionFalse = True &&& False
  putStrLn $ "Resultado Ex 5: True &&& True = " ++ show andFunctionTrue ++ "; True &&& False = " ++ show andFunctionFalse