minOfThree::(Ord a)=>a->a->a->a
minOfThree v1 v2 v3= min v1 $ min v2 v3

readInt::IO Int
readInt=read<$>getLine

minOfInts::IO Int
minOfInts=minOfThree<$>readInt<*>readInt<*>readInt

main::IO ()
main=do
    minInt<-minOfInts
    print minInt