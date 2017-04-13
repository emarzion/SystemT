{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Reflex.Dom
import qualified Data.Text as T
import Data.FileEmbed
import Data.Text.Encoding
import Data.Monoid ((<>))
import Control.Applicative

import InputStuff

--sample file
examples = decodeUtf8 $(embedFile "exampleTerms")

--css file
css = $(embedFile "css.css")

main :: IO()
main = mainWidgetWithCss css $ do
    elClass "h1" "mainTitle" $ text "System T Calculator"
    el "div" $ do
        rec stuff <- foldDyn (\t _ -> fullUpdate t) (reservedTerms , []) update
            let myLib = fmap fst stuff
            let myMsgs = fmap (T.unlines . snd) stuff
            update <- el "div" $ do
                u2 <- el "left" $ do
                    code <- textArea $ def & textAreaConfig_initialValue .~ examples
                                        & attributes .~ constDyn ( "rows" =: "45" <> "cols" =: "125" <> "style" =: "resize:none" )
                    el "br" $ return ()
                    load <- button "Load"
                    el "br" $ return ()
                    msgs <- textArea $ def & attributes .~ constDyn ( "readonly" =: "readonly" <> "rows" =: "10" <> "cols" =: "125" <> "style" =: "resize:none" )
                                           & setValue   .~ (updated myMsgs)
                    return (fmap getLines $ tag (current (value code)) load)
                return u2
            el "right" $ do
                rec text "Input:"
                    el "br" $ return ()
                    textIn <- textInput $ def & attributes .~ constDyn ( "size" =: "100"  )
                    command <- button "Evaluate command"
                    el "p" $ return ()
                    text "Output:"
                    el "br" $ return ()
                    textOut <- textArea $ def & attributes .~ constDyn ( "rows" =: "5" <> "cols" =: "100" <> "style" =: "resize:none" <> "readonly" =: "readonly" )
                                              & setValue   .~ attachPromptlyDynWith evalCommand myLib (tag (current (fmap T.strip (value textIn))) command)
                return ()
        return ()