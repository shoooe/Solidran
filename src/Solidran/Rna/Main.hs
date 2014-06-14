import Solidran.Rna.Detail (dnaToRna)

main :: IO ()
main = getLine >>= (putStr . dnaToRna)
