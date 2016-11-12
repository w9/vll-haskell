{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( module Lib
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.RWS.Lazy
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
import Control.Lens

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
formatLine widths = BSL.intercalate " " . wrapLineInColors . rewidthLine widths


type BS = BSL.ByteString
type Parserf a = BS.ByteString -> Parser a

data ParsedLine = Comment BSL.ByteString
                | Cells [BSL.ByteString]

data ParsingEnv = ParsingEnv
  { _initParserf :: Parserf [BSL.ByteString] }  -- it is fed as a env variable because the col_seq needs to be specified
makeLenses ''ParsingEnv

data ParsingState = ParsingState
  { _cellWidths  :: [Int64]
  , _lastParserf :: Maybe (Parserf [BSL.ByteString])
  , _cachedLines :: [ParsedLine] }
makeLenses ''ParsingState

type ParsingRWS = RWS ParsingEnv [BS] ParsingState

procedure :: [BSL.ByteString] -> ParsingRWS ()
procedure lines = do
  let (probingLines, remainingLines) = splitAt 80 lines
  sequence_ $ map parseAndCache probingLines
           ++ [releaseCache]
           ++ map parseAndPrint remainingLines

-- this function should not know whether `pf` is an initParser or
-- a cached parser
tryParse :: ([BS] -> ParsingRWS ()) -> Parserf [BS] -> BS -> ParsingRWS ()
tryParse cont pf l = do
  case pf (BSL.toStrict $ l <> "\n") of
    Fail _ errMsg  -> error $ errMsg
    Done rs -> error $ "Parser `Done` not expected. Current line: " ++ show l
    Many rs pf' -> case rs of
                     [] -> lastParserf ?= pf'
                     [ecs] -> do
                       lastParserf .= Nothing
                       let cs = either error id ecs
                       cellWidths %= updateLineWidths (map BSL.length cs)
                       cont cs
                     _ -> error $ "Unexpected: more than one (" ++ show (length rs) ++ ") records received in `Many`"
  


parseAndCache :: BS -> ParsingRWS ()
parseAndCache l = do
  let cont :: [BS] -> ParsingRWS ()
      cont cs = cachedLines %= (++ [Cells cs])
  pf <- use lastParserf
  case pf of
    Nothing -> if BSL.null l || BSL.head l == '#'
                    then cachedLines %= (++ [Comment l])
                    else do
                        pf <- view initParserf
                        tryParse cont pf l
    Just pf -> tryParse cont pf l


releaseCache :: ParsingRWS ()
releaseCache = do
  cls <- use cachedLines
  forM_ cls $ \case
    Comment c -> tell [c]
    Cells cs  -> do
        ws <- use cellWidths
        tell [formatLine ws cs]
  cachedLines .= []


parseAndPrint :: BS -> ParsingRWS ()
parseAndPrint l = do
  let cont :: [BS] -> ParsingRWS ()
      cont cs = do
        ws <- use cellWidths
        tell [formatLine ws cs]

  pf <- use lastParserf
  case pf of
    Nothing -> if BSL.null l || BSL.head l == '#'
                    then tell [l]
                    else do
                        pf <- view initParserf
                        tryParse cont pf l
    Just pf -> tryParse cont pf l


test :: [String] -> [String]
test flines =
  let Many _ pf = decode NoHeader
      initEnv = ParsingEnv
          { _initParserf = pf }
      initState = ParsingState
          { _cellWidths  = []
          , _lastParserf = Nothing
          , _cachedLines = [] }
   in map BSL.unpack . (^. _2) . (\rws -> evalRWS rws initEnv initState) . procedure . map BSL.pack $ flines


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
    fcontent <- BSL.readFile fname
    let fcontentWithComments = BSL.lines fcontent

    let isCSV = endswith ".csv" fname
    let csvColSepChar = fromIntegral . ord $ bool '\t' ',' isCSV
    let csvOptions = CSV.DecodeOptions csvColSepChar
    let flines = BSL.lines fcontent

    let Many _ pf = decodeWith csvOptions NoHeader
    let initEnv = ParsingEnv { _initParserf = pf }
    let initState = ParsingState { _cellWidths  = [] , _lastParserf = Nothing , _cachedLines = [] }

    mapM_ BSL.putStrLn . (^. _2) . (\rws -> evalRWS rws initEnv initState) $ procedure flines


-- |               | _cellWidths | _lastParser | _cachedLines | r _initParserf | w _output |
-- | parseAndCache |      x      |      x      |       x      |        x       |           |
-- |  releaseCache |      x      |             |       x      |        x       |           |
-- | parseAndPrint |      x      |      x      |              |        x       |     x     |
