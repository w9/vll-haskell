{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( module Lib
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Csv.Streaming
import Data.List
import Data.Bool
import Data.String.Utils
import Data.Char
import Data.Foldable
import Data.Vector (Vector)
import Data.Text (Text)
import Debug.Trace
import System.Environment
import System.Exit
import System.Console.ANSI
import System.Console.GetOpt

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Text as T

   
data Flag = Verbose
          | Version 
          | Help
          deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output on stderr"
          , Option ['V'] ["version"] (NoArg Version) "show version number"
          , Option ['h'] ["help"]    (NoArg Help)    "display this message"
          ]

alternating :: a -> a -> [a]
alternating x y = x : alternating y x

{-decodeCSV :: BS.ByteString -> [[String]]-}
{-decodeCSV = V.toList . either error id . decode NoHeader-}

zipWithLonger :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger _ _ [] [] = []
zipWithLonger c f [] (b:bs) = f c b : zipWithLonger c f [] bs
zipWithLonger c f (a:as) [] = f a c : zipWithLonger c f as []
zipWithLonger c f (a:as) (b:bs) = f a b : zipWithLonger c f as bs

makeWidth :: Int -> String -> String
makeWidth l s = take l $ s ++ repeat ' '

rewidthLine :: [Int] -> [String] -> [String]
rewidthLine = zipWith makeWidth

wrapLineInColors :: [String] -> [String]
wrapLineInColors = zipWith wrapInColor (alternating Yellow Blue)

wrapInColor :: Color -> String -> String
wrapInColor x s = setSGRCode [SetColor Foreground Vivid x] ++ s ++ setSGRCode [Reset]

updateLineWidths :: [Int] -> [Int] -> [Int]
updateLineWidths = zipWithLonger 0 max

formatLine :: [Int] -> [String] -> [String]
formatLine widths = wrapLineInColors . rewidthLine widths

iterateThrough :: [Int] -> [[String]] -> [[String]]
iterateThrough _  []     = []
iterateThrough ws (r:rs) = let ws' = updateLineWidths (map length r) ws
                      in formatLine ws' r : iterateThrough ws' rs

processOneLine :: [String] -> StateT (Int, [Int], [[String]]) IO ()
processOneLine l = do
    (i, ws, cache) <- get
    let ws' = updateLineWidths (map length l) ws
    if i < 80 then put (i+1, ws', l : cache)
              else do
                  liftIO . putStr . unlines . map (unwords . wrapLineInColors . formatLine ws') $ reverse cache
                  liftIO . putStrLn . unwords . wrapLineInColors . formatLine ws' $ l
                  put (80, ws', [])

cmdLine :: IO ()
cmdLine = do
    args <- getArgs
    (options, fnames) <-
        case getOpt Permute options args of
          (o,n,[]  ) -> if Help `elem` o
                           then do
                               putStrLn $ usageInfo "VLL" options
                               exitSuccess
                           else return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "VLL" options))

    {-putStrLn $ "(options, fnames) = " ++ show (options, fnames)-}
    let fname = head fnames
    fcontent <- BS.readFile fname
    let fcontentWithComments = BS.lines fcontent

    let isCSV = endswith ".csv" fname
    let csvColSepChar = fromIntegral . ord $ bool '\t' ',' isCSV
    let csvOptions = CSV.DecodeOptions csvColSepChar

    void . flip runStateT (1, [], []) . traverse_ processOneLine $ decodeWith csvOptions NoHeader fcontent
