import           System.Environment (getArgs)

import qualified NLP.MorfNCP as MN

main :: IO ()
main = do
  [tagsetPath, teiPath] <- getArgs
  MN.temp tagsetPath teiPath
