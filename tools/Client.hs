{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad (forever)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import qualified Control.Monad.Trans.State.Strict as State

import Data.Attoparsec.ByteString.Char8 as Atto hiding (isSpace)
import qualified Data.ByteString.Char8 as S
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (fromMaybe)

import System.Console.Haskeline
import System.Environment (getArgs, getEnvironment)
import System.FilePath ((</>))
import System.IO

data Client k v = Client
  { 
  }

data Cmd k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump

data Command k v = Command
    { commandParsers :: [(S.ByteString, Atto.Parser (Cmd k v))]
    , commandHelp :: [(String, String)]
    , commandExamples :: [(S.ByteString, Cmd k v)]
    }

emptyClient :: Client S.ByteString Int
emptyClient = Client

main :: IO ()
main = do
    args <- getArgs
    shell commands emptyClient

shell :: (Show k, Show v) => [Command k v] -> Client k v -> IO ()
shell commands0 env0 = do
    S.putStrLn $ "type help for help"
    home <- fromMaybe "." . lookup "HOME" <$> getEnvironment
    let settings = setComplete (completeWordWithPrev Nothing " \t" (clientComplete commands0))
            defaultSettings { historyFile = Just $ home </> ".raft_history" }
    runInputT settings loop `evalStateT` env0
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
                Right cmd -> lift $ execCommand cmd
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

parseCommand :: [Command k v] -> Atto.Parser (Cmd k v)
parseCommand commands0 = prefixChoice (concatMap commandParsers commands0)
  where
    prefixChoice = choice . fmap both . sortBy (flip $ on compare fst)
    both (s, p) = try (string s *> p) -- try, in case p fails

execCommand :: (Show k, Show v) => Cmd k v -> StateT (Client k v) IO ()
execCommand cmd = case cmd of
    CmdSet k v -> liftIO . putStrLn $ "Set " ++ show k ++ " to " ++ show v
    CmdGet k -> liftIO . putStrLn $ "Get " ++ show k
    CmdSleep n -> liftIO . putStrLn $ "Sleep for " ++ show n

----------------------------------------------------------------------

commands :: [Command S.ByteString Int]
commands = [cmdGet, cmdSet, cmdSleep]

cmdGet :: Command S.ByteString Int
cmdGet = Command [("get", CmdGet <$ skipSpace1 <*> takeWhile1 (not . isSpace))]
    []
    []

cmdSet :: Command S.ByteString Int
cmdSet = Command [("set", CmdSet <$ skipSpace1 <*> takeWhile1 (not . isSpace) <* skipSpace1 <*> decimal)]
    []
    []

cmdSleep :: Command S.ByteString Int
cmdSleep = Command [("sleep", CmdSleep <$ skipSpace1 <*> decimal)]
    []
    []

----------------------------------------------------------------------

skipSpace1 :: Atto.Parser ()
skipSpace1 = satisfy isSpace >> skipSpace
