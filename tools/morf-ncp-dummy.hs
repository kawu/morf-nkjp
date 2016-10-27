import           System.Environment (getArgs)

import qualified NLP.MorfNCP.Dummy as D

main :: IO ()
main = do
  [teiPath] <- getArgs
  D.dummy teiPath
