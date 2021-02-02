import Control.Monad (replicateM)
import System.Environment (getArgs)

myReplicate :: Monad m => Int -> m a -> m [a]
myReplicate n func = mapM (const func) [1 .. n]

main :: IO ()
main = do
        args <- getArgs
        let linesToRead =
                if not $ null args
                        then read $ head args
                        else 0 :: Int
        numbers <- replicateM linesToRead getLine
        let ints = map read numbers :: [Int]
        print $ sum ints
