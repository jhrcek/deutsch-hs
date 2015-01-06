{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Test.WebDriver (WDConfig, defaultConfig, defaultCaps, wdCapabilities, browser, chrome, runSession,finallyClose, WD, findElem, openPage, click, clearInput, sendKeys, Selector(..))
import Test.WebDriver.Commands.Wait (waitUntil)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: runghc ivona.hs <file-with-text-to-read>"
    else do
      text <- TIO.readFile $ head args
      readChunks $ chunkText 250 text

readChunks :: [Text] -> IO ()
readChunks cs = runSession myConfig . finallyClose $ do
  openPage "http://www.ivona.com/"
  selectVoice
  mapM_ readChunk cs
  where 
    readChunk :: Text -> WD ()
    readChunk c = do   
      setTextToRead c
      play
      waitPlayDone

-- Input chunking
chunkText :: Int -> Text -> [Text]
chunkText maxChnkSize txt = reverse $ foldl chunk [] (T.words txt)
  where 
    chunk []     word = [word]
    chunk (c:cs) word = if 1 + T.length word + T.length c <= maxChnkSize
                          then (c `T.snoc` ' ' `T.append` word) : cs --append word to existing chunk
                          else word : c : cs --make word seed of new chunk


-- Ivona UI controls
play :: WD ()
play = findElem (ById "voiceTesterLogicpbut") >>= click

waitPlayDone :: WD ()
waitPlayDone = void . waitUntil 30 . findElem $ ByXPath "//span[@id='voiceTesterLogicpbuttext'][contains(text(),'Play')]"

setTextToRead :: Text -> WD ()
setTextToRead txt = do
  inp <- findElem $ ById "VoiceTesterForm_text"
  clearInput inp
  sendKeys txt inp

selectVoice :: WD ()
selectVoice = do
  findElem (ByClass "voiceSelectorValue") >>= click --open menu
  wait 1 -- TODO explicit wait
  clickItem "German" --select lang
  wait 1
  clickItem "Marlene" --select voice
  where clickItem str = findElem (ByXPath $ T.concat ["//div/a/span/span[contains(.,'", str, "')]"]) >>= click

wait :: Int -> WD ()
wait secs = liftIO . threadDelay $ secs * 1000000

myConfig :: WDConfig
myConfig = defaultConfig {
    wdCapabilities = defaultCaps {browser = chrome}
  }
