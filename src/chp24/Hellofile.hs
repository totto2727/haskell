import           System.IO

main :: IO ()
main = do
    helloFile     <- openFile "./hello.txt" ReadMode
    hasFirstLine  <- hIsEOF helloFile
    firstLine <- if not hasFirstLine then hGetLine helloFile else return "empty"
    hasSecondLine <- hIsEOF helloFile
    secondLine    <- if not hasSecondLine
        then hGetLine helloFile
        else return "empty"
    hClose helloFile
    putStrLn firstLine
    putStrLn secondLine

    -- helloFile<-openFile "./hello.txt" ReadMode
    -- firstLines<-hGetLine helloFile
    -- putStrLn firstLines
    -- secondLines<-hGetLine helloFile
    -- goodbyeFile<-openFile "./goodbye.txt" WriteMode
    -- hPutStrLn goodbyeFile secondLines
    -- hClose goodbyeFile
    -- hClose helloFile
    -- putStrLn "Done"

-- main=do
--     helloFile<-openFile "./hello.txt" ReadMode
--     hClose helloFile
--     putStrLn "done!"
