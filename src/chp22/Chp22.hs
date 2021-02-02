main::IO ()
main=do
    inputs<-mapM (const getLine) [1 .. 3]
    mapM_ print inputs