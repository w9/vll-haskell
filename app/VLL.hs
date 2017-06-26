module Main where

import System.Process
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let vl = proc "vl" ("-z" : args)
  (_, Just hout, _, _) <- createProcess vl { std_out = CreatePipe }
  (_, _, _, p) <- createProcess (proc "less" ["-S", "-R"]){ std_in = UseHandle hout }
  _ <- waitForProcess p
  return ()
