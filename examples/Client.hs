{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad (forever)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict as State

import Control.Concurrent (threadDelay)

import Data.Attoparsec.ByteString.Char8 as Atto hiding (isSpace)
import qualified Data.ByteString.Char8 as S
import Data.Char (isSpace)
import Data.Serialize
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Data.List (intersperse, isPrefixOf, sort, sortBy)
import qualified Data.List as List
import Data.Maybe (fromMaybe)

import Network

import Network.Socket (Socket)
import Network.Stream as Stream
import Network.Stream.Socket as Stream
import Network.Stream.Types as Stream

import System.Console.Haskeline
import System.Environment (getArgs, getEnvironment)
import System.FilePath ((</>))
import System.IO

import ClientTypes

data Client k v = Client
  { -- leaderSocket :: Maybe Handle
    leaderStream :: Maybe Stream.Stream
  }

data Command k v = Command
    { commandParsers :: [(S.ByteString, Atto.Parser (ClientCommand k v))]
    , commandHelp :: [(String, String)]
    , commandExamples :: [(S.ByteString, ClientCommand k v)]
    }

emptyClient :: Client S.ByteString Int
emptyClient = Client Nothing

main :: IO ()
main = do
    args <- getArgs
    Stream.bracketSocket (Endpoint "localhost" 44444) (shell commands)

shell :: (Show k, Show v, Serialize k, Serialize v) => [Command k v] -> Socket -> IO ()
shell commands0 socket = do
    S.putStrLn $ "type help for help"
    stream <- mkSocketStream socket
    home <- fromMaybe "." . lookup "HOME" <$> getEnvironment
    let settings = setComplete (completeWordWithPrev Nothing " \t" (clientComplete commands0))
            defaultSettings { historyFile = Just $ home </> ".raft_history" }
    runInputT settings loop `evalStateT` emptyClient { leaderStream = Just stream }
  where
    comment = fst . break ('#' ==)
    chomp = fst . S.spanEnd isSpace . snd . S.span isSpace . S.pack
    -- loop :: (Show k, Show v) => InputT (StateT (Client k v) IO) ()
    loop = do
        m'ln <- fmap (chomp . comment) <$> getInputLine ("> ")
        for_ m'ln $ \ln -> if S.null ln then loop else do
            env <- lift State.get
            case parseOnly (parseCommand commands0) ln of
                Left err -> outputStrLn $ "parse error: " ++ show err
                Right cmd -> lift $ execCommand commands0 cmd
        loop

clientComplete :: Monad m => [Command k v] -> String -> String -> m [Completion]
clientComplete commands0 rPrev = return . map simpleCompletion .
    possible (words (reverse rPrev))
  where

    possible :: [String] -> String -> [String]
    possible prior current = case prior of
        [] -> prefixes cmdNames
        _  -> []
      where
        prefixes = filter (current `isPrefixOf`)
        cmdNames = concatMap (map (S.unpack . fst) . commandParsers) commands0

parseCommand :: [Command k v] -> Atto.Parser (ClientCommand k v)
parseCommand commands0 = prefixChoice (concatMap commandParsers commands0)
  where
    prefixChoice = choice . fmap both . sortBy (flip $ on compare fst)
    both (s, p) = try (string s *> p) -- try, in case p fails

getStream :: StateT (Client k v) IO Stream.Stream
getStream = do
    env <- State.get
    case leaderStream env of
        Just s -> return s
        Nothing -> threadDelayMS 10 >> getStream

execCommand :: (Show k, Show v, Serialize k, Serialize v) => [Command k v] -> ClientCommand k v -> StateT (Client k v) IO ()
execCommand commands0 cmd = case cmd of
    CmdHelp mCmd -> liftIO . putStrLn $ showMaybeCommandHelp commands0 mCmd
    _ -> do
        stream <- getStream
        liftIO $ do
            Stream.runPut stream $ put cmd
            rsp <- Stream.runGet stream (get :: Get (ClientResponse S.ByteString Int))
            putStrLn $ "Got response " ++ show rsp
            -- response <- hGetLine h
            -- putStrLn $ "Got response " ++ response

{-
    CmdSet k v -> liftIO . putStrLn $ "Set " ++ show k ++ " to " ++ show v
    CmdGet k -> liftIO . putStrLn $ "Get " ++ show k
    CmdSleep n -> do
        h <- getSocket
        liftIO $ do
            putStrLn $ "Sleep for " ++ show n
            hPutStrLn h $ show n
            response <- hGetLine h
            putStrLn $ "Got response " ++ response
-}
        

showMaybeCommandHelp :: [Command k v] -> Maybe S.ByteString -> String
showMaybeCommandHelp commands0 = maybe (showCommandsHelp False commands0) $ \cmd ->
    showCommandsHelp True (filter (elem cmd . fmap fst . commandParsers) commands0)

showCommandsHelp :: Bool -> [Command k v] -> String
showCommandsHelp showExamples = removeBlanks . concat . sort .
    map (showCommandHelp showExamples)
  where
    removeBlanks = unlines . filter (not . blank) . lines
    blank l = null l || all isSpace l

showCommandHelp :: Bool -> Command k v -> String
showCommandHelp showExamples Command{..} = unlines $ case showExamples of
    False -> blurb
    True  -> blurb ++ examples
  where
    blurb :: [String]
    blurb = concatMap b commandHelp
    b :: (String, String) -> [String]
    b (synopsis, explanation) = synopsis : [indent 4 (para [explanation])]
    examples :: [String]
    examples = "Examples:" : [indent 8 (unlines (map (S.unpack . fst) commandExamples))]


------------------------------------------------------------
-- Paragraph rendering
--  
    
para :: [String] -> String
para ss = concat $ intersperse "\n" (map (\s -> breakLines 76 s) ss)
        
indent :: Int -> String -> String 
indent i s = unlines $ map (\x -> indentation ++ x) (lines s)
    where
        indentation = List.take i $ repeat ' '

-- breakLines leftIndent columnWidth text
breakLines :: Int -> String -> String
breakLines n s
    | length s < n = s ++ "\n"
    | otherwise    = line' ++ "\n" ++ breakLines n rest'
    where 
        (line, rest) = splitAt n s
        (rSpill, rLine) = break isSpace (reverse line)
        line' = reverse rLine
        rest' = reverse rSpill ++ rest


----------------------------------------------------------------------

commands :: [Command S.ByteString Int]
commands = [cmdHelp, cmdGet, cmdSet, cmdSleep]

cmdHelp :: Command S.ByteString Int
cmdHelp = Command [("help", parser)]
    []
    []
  where
    parser = CmdHelp <$ skipSpace <*> ((Just <$> (takeWhile1 (not . isSpace))) <|> pure Nothing)

cmdGet :: Command S.ByteString Int
cmdGet = Command [("get", parser)]
    [("get", "Request a value from the log")]
    [("get x", CmdGet "x")]
  where
    parser = CmdGet <$ skipSpace <*> takeWhile1 (not . isSpace)

cmdSet :: Command S.ByteString Int
cmdSet = Command [("set", parser)]
    [("set", "Request to set a value in the log")]
    [("set x=7", CmdSet "x" 7)]
  where
    parser = CmdSet <$ skipSpace
                 <*> (takeWhile1 (\x -> not (isSpace x) && x /= '=')
                         <* skipMany space <* char '=' <* skipMany space)
                 <*> decimal

cmdSleep :: Command S.ByteString Int
cmdSleep = Command [("sleep", parser)]
    []
    []
  where
    parser = CmdSleep <$ skipSpace <*> decimal

----------------------------------------------------------------------

threadDelayMS :: MonadIO m => Int -> m ()
threadDelayMS = liftIO . threadDelay . (*1000)
