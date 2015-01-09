module WdUtil
    ( clickElem
    , myWdConfig
    , setInput
    ) where 

import Control.Monad ((>=>))
import Data.Text (Text) 
import Test.WebDriver (WDConfig, Selector, WD, findElem, click, clearInput, sendKeys, defaultConfig, defaultCaps, browser, chrome, wdCapabilities)

clickElem :: Selector -> WD()
clickElem = findElem >=> click

setInput :: Selector -> Text -> WD ()
setInput sel txt = do
  inp <- findElem sel
  clearInput inp
  sendKeys txt inp

myWdConfig :: WDConfig
myWdConfig = defaultConfig {
    wdCapabilities = defaultCaps {browser = chrome}
  }
