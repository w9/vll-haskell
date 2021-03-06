-- TODO: add "maximum-num-columns"

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( module Lib
    ) where

import           Control.Applicative
import           Control.Lens                 hiding (argument)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.RWS.Lazy
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Except
import           Data.Bool
import           Data.Char
import           Data.Csv.Incremental
import           Data.Foldable
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String.Utils
import           Data.Text                    (Text)
import           Data.Vector                  (Vector)
import           Data.Word
import           Debug.Trace
import           Options.Applicative.Simple   hiding (Parser)
import           Options.Applicative.Types    hiding (Parser)
import           Safe
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Console.Terminal.Size
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix
import           System.Posix.Terminal

import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as BSL
import qualified Data.Csv                     as CSV
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Options.Applicative.Simple   as OPT



zipWithLonger :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger _ _ [] []         = []
zipWithLonger c f [] (b:bs)     = f c b : zipWithLonger c f [] bs
zipWithLonger c f (a:as) []     = f a c : zipWithLonger c f as []
zipWithLonger c f (a:as) (b:bs) = f a b : zipWithLonger c f as bs

makeWidth :: Int64 -> Int64 -> BSL.ByteString -> BSL.ByteString
makeWidth m l s = BSL.take (m+l) $ s <> BSL.repeat ' '

rewidthLine :: Int64 -> [Int64] -> [BSL.ByteString] -> [BSL.ByteString]
rewidthLine marginWidth = zipWith (makeWidth marginWidth)

wrapLineInColors :: [Color] -> [BSL.ByteString] -> [BSL.ByteString]
wrapLineInColors colors = zipWith wrapInColor (cycle colors)

wrapInColor :: Color -> BSL.ByteString -> BSL.ByteString
wrapInColor x s = pre <> s <> post
  where
    pre  = BSL.pack $ setSGRCode [SetColor Foreground Vivid x]
    post = BSL.pack $ setSGRCode [Reset]

updateLineWidths :: [Int64] -> [Int64] -> [Int64]
updateLineWidths = zipWithLonger 0 max

type BS = BSL.ByteString
type Parserf a = (BS.ByteString -> Parser a)

haha :: Int -> Int
haha = do
  let a = 1
  let b = 2
  return 12

data ParsedLine = Comment BSL.ByteString
                | Cells [BSL.ByteString]

-- | magnify the env (options) to the sub-routines
data ParsingEnv = ParsingEnv
  { _colSep          :: Char
  , _commentPrefix   :: Char
  , _naiveParsing    :: Bool
  , _marginWidth     :: Int64
  , _useColors       :: Bool
  , _colors          :: [Color]
  , _numProbingLines :: Int
  , _chopLen         :: Int64 }
makeLenses ''ParsingEnv


--formatLine :: Bool -> [Color] -> Int64 -> [Int64] -> [BSL.ByteString] -> BSL.ByteString
--formatLine useColors colors marginWidth widths = (<> "$")
--                                     . BSL.concat
--                                     . (if useColors then wrapLineInColors colors else id)
--                                     . rewidthLine marginWidth widths

listReader :: Read a => ReadM [a]
listReader = do
  s <- readerAsk
  if head s == '['
     then auto
     else case readMay $ "[" ++ s ++ "]" of
            Nothing -> readerError $ "Only comma separated values are allowed. Got " ++ show s
            Just c  -> pure c

charReader :: ReadM Char
charReader = do
  s <- readerAsk
  case readMay $ "'" ++ s ++ "'" of
    Nothing -> readerError $ "Only a single ASNI character is allowed. Got " ++ show s
    Just c  -> pure c

maybeReader :: (String -> Maybe a) -> ReadM a
maybeReader f = eitherReader $ \arg ->
  maybe (Left $ "cannot parse value `" ++ arg ++ "'") pure . f $ arg

envParser :: OPT.Parser ParsingEnv
envParser = ParsingEnv
  <$> option charReader
      ( long "column-separator" <> short 's' <> metavar "CHAR" <> value '\NUL' <> showDefault
     <> help
        ( "'\\NUL' means ',' for *.csv and '\\t' for all other extensions, "
       ++ "including pipes. You might want to use --naive as well if you set this manually" ))

  <*> option charReader
      ( long "comment-prefix"
     <> short 'c'
     <> metavar "CHAR"
     <> value '#'
     <> showDefault )

  <*> switch
      ( long "naive"
     <> help
        ( "Disable quote parsing, this might be the quick and dirty solution "
       ++ "for most parsing failures due to the file not compling the CSV specification "
       ++ "precisely" ) )

  <*> option auto
      ( long "margin-width"
     <> short 'm'
     <> metavar "INT"
     <> value 1
     <> showDefault
     <> help "Num of spaces between columns" )

  <*> switch
      ( long "use-colors"
     <> short 'z'
     <> help "Enable colors" )

  <*> option listReader
      ( long "colors"
     <> metavar "COLORS"
     <> value [Yellow, Blue]
     <> showDefault
     <> help
        ( "Comma delimited colors that will be cycled through. A color can be one of "
       ++"Black, Red, Green, Yellow, Blue, Magenta, Cyan, or White" ))

  <*> option auto
      ( long "num-probing-lines"
     <> short 'p'
     <> metavar "INT"
     <> value (-1)
     <> showDefault
     <> help "If set to -1, will use twice the $LINES env variable" )

  <*> option auto
      ( long "chop-length"
     <> short 'g'
     <> metavar "INT"
     <> value 1000
     <> showDefault
     <> help "Chop the column if longer than this length." )



optparser :: OPT.Parser (ParsingEnv, [FilePath])
optparser = liftA2 (,) envParser (many $ argument str (metavar "FILENAME..."))

data ParsingState = ParsingState
  { _cellWidths  :: [Int64]
  , _lastParserf :: Maybe (Parserf [BSL.ByteString])
  , _cachedLines :: [ParsedLine] }
makeLenses ''ParsingState


type ParsingRWS = RWS ParsingEnv [BS] ParsingState

formatLine :: [BSL.ByteString] -> ParsingRWS BSL.ByteString
formatLine l = do
  useColors   <- view useColors
  colors      <- view colors
  marginWidth <- view marginWidth
  chopLen     <- view chopLen

  cellWidths  <- use cellWidths

  let cellWidths' = zipWith min cellWidths (repeat chopLen)

  pure
     . (<> "$")
     . BSL.concat
     . (if useColors then wrapLineInColors colors else id)
     . rewidthLine marginWidth cellWidths'
     $ l

procedure :: [BSL.ByteString] -> ParsingRWS ()
procedure lines = do
  numProbingLines <- view numProbingLines
  let (probingLines, remainingLines) = splitAt numProbingLines lines
  sequence_ $ map parseAndCache probingLines
           ++ [releaseCache]
           ++ map parseAndPrint remainingLines

releaseCache :: ParsingRWS ()
releaseCache = do
  cls <- use cachedLines
  forM_ cls $ \case
    Comment c -> tell [c]
    Cells cs  -> do
        output <- formatLine cs
        tell [output]
  cachedLines .= []


tryParseCells :: ([BS] -> ParsingRWS ()) -> Parserf [BS] -> BS -> ParsingRWS ()
tryParseCells contCells pf l = do
  case pf (BSL.toStrict $ l <> "\n") of
    Fail _ errMsg  -> error $ errMsg ++ ": " ++ BSL.unpack l
                        -- ^ TODO: This error should be thrown to the base stack so that the line number
                        --         can be printed with the line
    Done rs -> error $ "Parser `Done` not expected. Current line: " ++ show l
    Many rs pf' -> case rs of
                     [] -> lastParserf ?= pf'
                     [ecs] -> do
                       lastParserf .= Nothing
                       let cs = map (BSL.pack . tail . init . show) $ either error id ecs
                       cellWidths %= updateLineWidths (map BSL.length $ cs)
                       contCells cs
                     _ -> error $ "Unexpected: more than one (" ++ show (length rs) ++ ") records received in `Many`"


-- this function should not know whether `pf` is an initParser or
-- a cached parser
tryCSVParse :: (BS -> ParsingRWS ()) -> ([BS] -> ParsingRWS ()) -> BS -> ParsingRWS ()
tryCSVParse contComment contCells l = do
  pf <- use lastParserf
  cp <- view commentPrefix
  case pf of
    Nothing -> if BSL.null l || BSL.head l == cp
                    then contComment l
                    else do
                        csvColSepChar <- view colSep
                        let csvColSepWord8 = fromIntegral $ ord csvColSepChar
                        let csvOptions = CSV.DecodeOptions csvColSepWord8
                        let Many _ pf = decodeWith csvOptions NoHeader
                        tryParseCells contCells pf l
    Just pf -> tryParseCells contCells pf l

tryNaiveParse :: (BS -> ParsingRWS ()) -> ([BS] -> ParsingRWS ()) -> BS -> ParsingRWS ()
tryNaiveParse contComment contCells l = do
  cp <- view commentPrefix
  if BSL.null l || BSL.head l == cp
        then contComment l
        else do
            csvColSepChar <- view colSep
            let cs = BSL.split csvColSepChar l
            cellWidths %= updateLineWidths (map BSL.length $ cs)
            contCells cs

tryParse :: (BS -> ParsingRWS ()) -> ([BS] -> ParsingRWS ()) -> BS -> ParsingRWS ()
tryParse contComment contCells l = do
  naive <- view naiveParsing
  if naive then tryNaiveParse contComment contCells l
           else tryCSVParse contComment contCells l

parseAndCache :: BS -> ParsingRWS ()
parseAndCache l = do
  let contComment l = cachedLines %= (++ [Comment l])
  let contCells cs = cachedLines %= (++ [Cells cs])
  tryParse contComment contCells l


parseAndPrint :: BS -> ParsingRWS ()
parseAndPrint l = do
  let contComment l = tell [l]
  let contCells cs = do
        output <- formatLine cs
        tell [output]
  tryParse contComment contCells l


-- TODO: add options to not show $ at the end


cmdLine :: IO ()
cmdLine = do
    ((initEnv, fnames),()) <- simpleOptions "0.0.1" "(V)iew (L)arge table. Fast." "" optparser empty

    isPipe <- liftM not $ queryTerminal stdInput

    let fname | not (null fnames) = head fnames
              | isPipe = ""
              | otherwise = error "Filenames needed."

    let isCSV = endswith ".csv" fname
    let csvColSepChar = if isCSV then ',' else '\t'

    terminalHeight <- liftM ((*2) . head . (++ [80]) . map height . catMaybes)
                    . sequence . map hSize
                    $ [stdout, stderr, stdin]

    let initEnv' = initEnv & colSep %~ (\ x -> if (x == '\NUL') then csvColSepChar else x)
                           & numProbingLines %~ (\ x -> if (x == (-1)) then terminalHeight else x)

    fcontent <- liftM BSL.concat . sequence . (if isPipe then (BSL.getContents:) else id) . map BSL.readFile $ fnames

    let flines = BSL.lines fcontent
    let initState = ParsingState
          { _cellWidths  = []
          , _lastParserf = Nothing
          , _cachedLines = [] }

    mapM_ BSL.putStrLn . (^. _2) . (\rws -> evalRWS rws initEnv' initState) $ procedure flines


-- |               | _cellWidths | _lastParser | _cachedLines | r _initParserf | w _output |
-- | parseAndCache |      x      |      x      |       x      |        x       |           |
-- |  releaseCache |      x      |             |       x      |        x       |           |
-- | parseAndPrint |      x      |      x      |              |        x       |     x     |
