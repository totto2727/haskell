import qualified Data.Map as Map
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

describePizza :: Pizza -> String
describePizza (s, c) = mconcat ["The ", show s, " Pizza", " is cheaper at ", show $ costPerInch (s, c), " per square inch"]

monadMain::Monad m=>m Double->m Double->m Double ->m Double->m String
monadMain costM1 sizeM1 costM2 sizeM2=do
  cost1<-costM1
  size1<-sizeM1
  cost2<-costM2
  size2<-sizeM2
  let pizza1=(cost1,size1)
      pizza2=(cost2,size2)
  return .describePizza $comparePizza pizza1 pizza2


main::IO ()
main=
  putStrLn "size1">>getLine>>=
    (\size1->putStrLn "cost1">>getLine >>=
      (\cost1->putStrLn "size2">>getLine >>=
        (\size2->putStrLn "cost2">>getLine >>=
          (\cost2->putStrLn $ describePizza $ comparePizza (read size1,read cost1) (read size2,read cost2)))))

-- main :: IO ()
-- main = do
--   putStrLn "What is the size of pizza 1"
--   size1 <- getLine
--   putStrLn "What is the cost of pizza 1"
--   cost1 <- getLine
--   putStrLn "What is the size of pizza 2"
--   size2 <- getLine
--   putStrLn "What is the cost of pizza 2"
--   cost2 <- getLine

--   let pizza1 = (read size1, read cost1)
--   let pizza2 = (read size2, read cost2)
--   let betterPizza = comparePizza pizza1 pizza2

--   putStrLn $ describePizza betterPizza