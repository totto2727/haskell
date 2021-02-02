import System.IO
import System.Environment

getCounts::String ->(Int ,Int ,Int )
getCounts str=(charCounts,wordCounts,lineCounts)
    where
        charCounts=length str
        wordCounts=length . words $ str
        lineCounts=length . lines $ str

countsText::(Int ,Int ,Int )->String
countsText (cc,wc,lc)=unwords [
    "chars: ",
    show cc,
    " words: ",
    show wc,
    " lines: ",
    show lc
    ]

main::IO ()
main=do
    args<-getArgs
    let fileName=head args
    input<-readFile fileName
    let summary = countsText . getCounts $ input
    appendFile "stats.dat" $ mconcat [fileName,"\t",summary,"\n"]
    putStrLn summary
