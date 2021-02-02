areaGivenDiameter :: Double -> Double
areaGivenDiameter size = (size / 2) ^ 2 * pi

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizza :: Pizza -> Pizza -> Pizza
comparePizza p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

descrivePizza :: Pizza -> String
descrivePizza (s, c) = mconcat ["The ", show s, " Pizza", "is cheaper at ", show $ costPerInch (s, c), " per square inch"]

main :: IO ()
main = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine

  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizza pizza1 pizza2

  putStrLn $ descrivePizza betterPizza