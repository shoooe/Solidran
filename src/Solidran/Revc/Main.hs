import Solidran.Revc.Detail (complementDna)

main :: IO ()
main = getLine >>= (putStr . complementDna)
