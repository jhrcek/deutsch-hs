{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Test.WebDriver (WDConfig, defaultConfig, defaultCaps, wdCapabilities, browser, chrome, runSession,finallyClose, WD, findElem, findElems, getText, Element, attr, openPage, click, clearInput, sendKeys, Selector(..), FailedCommandType(..), FailedCommand(..))

myConfig :: WDConfig
myConfig = defaultConfig {
    wdCapabilities = defaultCaps {browser = chrome}
  }

main :: IO ()
main =
  runSession myConfig . finallyClose $ do
    openPage "http://www.ivona.com/"
    --selectVoice "TODO"
    setTextToRead "Hello, this is some random text"
    play
    threadSleep 10 -- TODO: explicit wait az span pod id=voiceTesterLogicspk bude obsahovat class speakerPlay

setTextToRead :: Text -> WD ()
setTextToRead txt = do
  inp <- findElem $ ById "VoiceTesterForm_text"
  clearInput inp
  sendKeys txt inp

play :: WD ()
play = findElem (ById "voiceTesterLogicpbut") >>= click

threadSleep :: Int -> WD ()
threadSleep secs = liftIO $ threadDelay (secs * 1000000)
