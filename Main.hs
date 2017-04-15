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
examples = decodeUtf8 $(embedFile "docs/exampleTerms")

--css file
css = $(embedFile "docs/css.css")

main :: IO()
main = mainWidgetWithCss css $ do
    elClass "h1" "mainTitle" $ text "System T Interpreter"
    el "div" $ do
        rec stuff <- foldDyn (\t _ -> fullUpdate t) (reservedTerms , []) update
            let myLib = fmap fst stuff
            let myMsgs = fmap (T.unlines . snd) stuff
            update <- el "div" $ do
                u2 <- el "left" $ do
                    code <- textArea $ def & textAreaConfig_initialValue .~ examples
                                          & attributes .~ constDyn ( "rows" =: "45" <> "style" =: "resize:none;width:80%")
                    el "br" $ return ()
                    load <- button "Load"
                    el "br" $ return ()
                    msgs <- textArea $ def & attributes .~ constDyn ( "readonly" =: "readonly" <> "rows" =: "10" <>  "style" =: "resize:none;width:80%" )
                                           & setValue   .~ (updated myMsgs)
                    return (fmap getLines $ tag (current (value code)) load)
                return u2
            el "right" $ do
                rec el "p" $ do
                        text "For an overview of the syntax look "
                        elAttr "a" ("target" =: "_blank" <> "href" =: "https://github.com/emarzion/SystemT/blob/master/README.md") $ text "here."
                        el "br" $ return ()
                        el "ul" $ do
                            el "li" $ text "To evaluate an expression, just type in that expression."
                            el "li" $ text "To get the type of an expression, type 'type' before the expression."
                            el "li" $ text "To print an expression (without definitions and without evaluating), type 'print' before the expression."
                        text "Keep in mind that neither System T nor this interpreter was written with efficient arithmetical computation in mind, so don't be surprised if it hangs up on even medium-sized numbers."
                        return ()
                    el "br" $ return ()
                    text "Input:"
                    el "br" $ return ()
                    textIn <- textInput $ def & attributes .~ constDyn ( "style" =: "width:80%"  )
                    el "br" $ return ()
                    command <- button "Evaluate command"
                    el "p" $ return ()
                    text "Output:"
                    el "br" $ return ()
                    textOut <- textArea $ def & attributes .~ constDyn ( "rows" =: "5" <> "style" =: "resize:none;width:80%" <> "readonly" =: "readonly" )
                                              & setValue   .~ attachPromptlyDynWith evalCommand myLib (tag (current (fmap T.strip (value textIn))) command)
                return ()
        return ()
