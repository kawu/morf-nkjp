import           System.Environment (getArgs)

import qualified NLP.MorfNCP as MN

main :: IO ()
main = do
  [teiPath] <- getArgs
  MN.temp teiPath
