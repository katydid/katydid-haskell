module Main where

import qualified Criterion.Main                as B
import           Suite                          ( readBenches
                                                , BenchSuiteCase(..)
                                                , stretch
                                                , runBench
                                                )

main :: IO ()
main = do
  benches <- readBenches
  B.defaultMain $ map
    (\benchcase -> B.bench (benchname benchcase)
      $ B.perBatchEnv (stretch benchcase) runBench
    )
    benches
