{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Test.WebDriver (WDConfig, defaultConfig, defaultCaps, wdCapabilities, browser, chrome, runSession,finallyClose, WD, findElem, openPage, click, clearInput, sendKeys, Selector(..))

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 
    then putStrLn "Usage: runghc ivona.hs <file-with-text-to-read>"
    else do
      text <- T.readFile $ head args
      readChunks $ chunkText text

readChunks :: [Text] -> IO ()
readChunks cs = runSession myConfig . finallyClose $ do
  openPage "http://www.ivona.com/"
  --selectVoice "TODO"
  mapM_ readChunk cs
  where 
    readChunk :: Text -> WD ()
    readChunk c = do   
      setTextToRead c
      play
      wait 10 -- TODO: explicit wait az span pod id=voiceTesterLogicspk bude obsahovat class speakerPlay

chunkText :: Text -> [Text]
chunkText txt = [txt] --TODO: chunk text into pieces of 250 characters (but not in the middle of words

setTextToRead :: Text -> WD ()
setTextToRead txt = do
  inp <- findElem $ ById "VoiceTesterForm_text"
  clearInput inp
  sendKeys txt inp

play :: WD ()
play = findElem (ById "voiceTesterLogicpbut") >>= click

wait :: Int -> WD ()
wait secs = liftIO . threadDelay $ secs * 1000000

myConfig :: WDConfig
myConfig = defaultConfig {
    wdCapabilities = defaultCaps {browser = chrome}
  }
