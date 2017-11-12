module Main where

import qualified Criterion.Main as B
import Suite (readBenches)

setupEnv :: IO ([Int], [Int])
setupEnv = do
    let small = replicate 1000 (1 :: Int)
    big <- map length . words <$> readFile "../testsuite/Readme.md"
    return (small, big)

main :: IO ()
main = B.defaultMain
    -- notice the lazy pattern match here!
    [ B.env setupEnv $ \ ~(small,big) -> B.bgroup "main" [
        B.bgroup "small" [
            B.bench "length" $ B.whnf length small
            , B.bench "length . filter" $ B.whnf (length . filter (==1)) small
        ]
        , B.bgroup "big" [
            B.bench "length" $ B.whnf length big
            , B.bench "length . filter" $ B.whnf (length . filter (==1)) big
        ]
    ] ]