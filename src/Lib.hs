{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( module Lib
    ) where

import Control.Applicative
import Control.Monad
import Data.Csv
import Data.Char
import Data.List
import Data.Vector (Vector)
import Data.Text (Text)
import Debug.Trace
import System.Environment
import System.Exit
import System.Console.ANSI
import System.Console.GetOpt

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

decodeCSV :: BS.ByteString -> [[String]]
decodeCSV = V.toList . either error id . decodeWith (DecodeOptions . fromIntegral $ ord '\t') NoHeader

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

calculateWidths :: [[String]] -> [Int]
calculateWidths cells = foldr updateLineWidths [] $ map (map length) cells

formatLine :: [Int] -> [String] -> [String]
formatLine widths = wrapLineInColors . rewidthLine widths

iterateThrough :: [Int] -> [[String]] -> [[String]]
iterateThrough _  []     = []
iterateThrough ws (r:rs) = let ws' = updateLineWidths (map length r) ws
                      in formatLine ws' r : iterateThrough ws' rs

formatCells :: [[String]] -> [[String]]
formatCells cells = let (previewCells, restCells) = splitAt 3 cells
                        widths = calculateWidths previewCells
                     in map (formatLine widths) previewCells ++ iterateThrough widths restCells


cmdLine :: IO ()
cmdLine = do
    args <- getArgs
    (options, filenames) <-
        case getOpt Permute options args of
          (o,n,[]  ) -> if Help `elem` o
                           then do
                               putStrLn $ usageInfo "VLL" options
                               exitSuccess
                           else return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "VLL" options))

    {-putStrLn $ "(options, filenames) = " ++ show (options, filenames)-}

    fileContent <- BS.readFile $ head filenames
    let cells = decodeCSV fileContent

    putStrLn . unlines . map (intercalate " ") $ formatCells cells

