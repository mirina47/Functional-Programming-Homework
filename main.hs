data Root = Two Double Double | One Double | No deriving Show
 
solve :: Double -> Double -> Double -> Root
solve a b c | a==0 && b==0 || d < 0.0 = No
            | a == 0.0 = (One (c/b))
            | d == 0.0 = (One ((-b)/(2.0*a)))
            | d > 0.0 = (Two (((-b) + sqrt d)/(2.0*a)) (((-b) - sqrt d)/(2.0*a)))
            where d = b * b - 4.0 * a * c

main = do 
   print(solve 0 0 0)
   print(solve 0 5 3)
   print(solve 1 0 (-25))
   print(solve 4 5 0)
   print(solve 4 5 3)
   print(solve 3 1 7)
   putStrLn "Введите коэффициенты: " 
   a <- getLine
   b <- getLine
   c <- getLine
   print $ solve (read a :: Double) (read b :: Double) (read c :: Double)