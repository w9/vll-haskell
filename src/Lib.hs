{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( module Lib
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Data.Csv.Incremental
import Data.List
import Data.Bool
import Data.String.Utils
import Data.Char
import Data.Int
import Data.Foldable
import Data.Monoid
import Data.Vector (Vector)
import Data.Text (Text)
import Debug.Trace
import System.Environment
import System.Exit
import System.Console.ANSI
import System.Console.GetOpt

import qualified Data.Csv as CSV
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
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

{-decodeCSV :: BSL.ByteString -> [[String]]-}
{-decodeCSV = V.toList . either error id . decode NoHeader-}

zipWithLonger :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger _ _ [] [] = []
zipWithLonger c f [] (b:bs) = f c b : zipWithLonger c f [] bs
zipWithLonger c f (a:as) [] = f a c : zipWithLonger c f as []
zipWithLonger c f (a:as) (b:bs) = f a b : zipWithLonger c f as bs

makeWidth :: Int64 -> BSL.ByteString -> BSL.ByteString
makeWidth l s = BSL.take l $ s <> BSL.repeat ' '

rewidthLine :: [Int64] -> [BSL.ByteString] -> [BSL.ByteString]
rewidthLine = zipWith makeWidth

wrapLineInColors :: [BSL.ByteString] -> [BSL.ByteString]
wrapLineInColors = zipWith wrapInColor (cycle [Yellow, Blue])

wrapInColor :: Color -> BSL.ByteString -> BSL.ByteString
wrapInColor x s = pre <> s <> post
  where
    pre  = BSL.pack $ setSGRCode [SetColor Foreground Vivid x]
    post = BSL.pack $ setSGRCode [Reset]

updateLineWidths :: [Int64] -> [Int64] -> [Int64]
updateLineWidths = zipWithLonger 0 max

formatLine :: [Int64] -> [BSL.ByteString] -> BSL.ByteString
formatLine widths = BSL.concat . wrapLineInColors . rewidthLine widths

--iterateThrough :: [Int64] -> [[String]] -> [[String]]
--iterateThrough _  []     = []
--iterateThrough ws (r:rs) = let ws' = updateLineWidths (map length r) ws
--                      in formatLine ws' r : iterateThrough ws' rs

--processOneLine :: [String] -> StateT (Int64, [Int64], [[String]]) IO ()
--processOneLine l = do
--    (i, ws, cache) <- get
--    let ws' = updateLineWidths (map length l) ws
--    if i < 80 then put (i+1, ws', l : cache)
--              else do
--                  liftIO . putStr . unlines . map (unwords . wrapLineInColors . formatLine ws') $ reverse cache
--                  liftIO . putStrLn . unwords . wrapLineInColors . formatLine ws' $ l
--                  put (80, ws', [])

--cmdLine :: IO ()
--cmdLine = do
--    args <- getArgs
--    (options, fnames) <-
--        case getOpt Permute options args of
--          (o,n,[]  ) -> if Help `elem` o
--                           then do
--                               putStrLn $ usageInfo "VLL" options
--                               exitSuccess
--                           else return (o,n)
--          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "VLL" options))
--
--    {-putStrLn $ "(options, fnames) = " ++ show (options, fnames)-}
--    let fname = head fnames
--    fcontent <- BSL.readFile fname
--    let fcontentWithComments = BSL.lines fcontent
--
--    let isCSV = endswith ".csv" fname
--    let csvColSepChar = fromIntegral . ord $ bool '\t' ',' isCSV
--    let csvOptions = CSV.DecodeOptions csvColSepChar
--
--    void . flip runStateT (1, [], []) . traverse_ processOneLine $ decodeWith csvOptions NoHeader fcontent




-- new strategy -----------------------------

lines :: FilePath -> IO [BSL.ByteString]
lines fp = BSL.lines <$> BSL.readFile fp

readingLoop :: [BSL.ByteString] -> State [Int64] [BSL.ByteString]
readingLoop [] = return []
readingLoop lls@(l:ls)  =  if BSL.head l == '#' then (:) <$> return l <*> readingLoop ls
                                                else parsingLoop lls

parsingLoop :: [BSL.ByteString] -> State [Int64] [BSL.ByteString]
parsingLoop lls = let p = decode NoHeader
                   in loop p lls
                  where
                    loop :: Parser [BSL.ByteString] -> [BSL.ByteString] -> State [Int64] [BSL.ByteString]
                    loop p [] = loop p [""]
                    loop p lls@(l:ls) = case p of
                                          Many _ f -> loop (f (BSL.toStrict l <> "\n")) ls  -- I don't expect rs be returned here
                                          Done [r] -> do   -- I only expect one parsed line here
                                            ws <- get
                                            let ws' = updateLineWidths ws (map (fromIntegral . BSL.length) $ either undefined id r)
                                            (:) <$> (return . formatLine ws' $ either undefined id r) <*> readingLoop lls
