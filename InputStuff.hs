
module InputStuff where

import qualified Data.Text as T
import Data.FileEmbed
import Data.Text.Encoding

import Lang
import Pprinter
import Normalize
import Parser
import Typing

reservedTerms :: [(String,VTerm)]
reservedTerms = [("pair" , VPair) , ("p1" , VPr1) , ("p2" , VPr2) , ("case" , VCase) , ("i1" , VI1) , ("i2" , VI2) , 
                 ("false" , VLitB False) , ("true" , VLitB True) , ("if" , VIf) , ("S" , VSucc) , ("iter" , VIter) , ("tt",VAst)]

notComment :: String -> Bool
notComment "" = False
notComment (c:_) = c /= '#'

notCommentT :: T.Text -> Bool
notCommentT = notComment . T.unpack    

getLines :: T.Text -> [T.Text]
getLines txt = reverse $ filter notCommentT (map T.strip (T.lines txt))

updateWithMsg :: T.Text -> [(String,VTerm)] -> ([(String,VTerm)] , T.Text)
updateWithMsg txt lib = case parseD lib (T.unpack txt) of
                        Nothing -> (lib, T.pack $ "Syntax Error in line '" ++ (T.unpack txt) ++ "'.")
                        Just (n,t) -> case lookup n lib of
                                        Just _  -> (lib, T.pack $ "Identifier '" ++ n ++ "' is already used.")
                                        Nothing -> let x = dbTerm t in case closed x of
                                                                        False -> (lib, T.pack $ "Term " ++ n ++ " is not closed.")
                                                                        True  -> case maybeTy x of
                                                                                   Nothing -> (lib, T.pack $ "Term " ++ n ++ " is not typeable.")
                                                                                   Just s  -> ((n,t):lib , T.pack $ n ++ " : " ++ typprint s ++ " defined.")

fullUpdate_aux :: [T.Text] -> ([(String,VTerm)] , [T.Text])
fullUpdate_aux [] = (reservedTerms,[])
fullUpdate_aux (t:ts) = let (lib,msgs) = fullUpdate_aux ts in case updateWithMsg t lib of
                                                             (lib2,msg) -> (lib2,(msg:msgs))

fullUpdate :: [T.Text] -> ([(String,VTerm)] , [T.Text])
fullUpdate = (\(lib,msgs) -> (lib, reverse msgs)) . fullUpdate_aux

eitherTypeMsg :: Term -> Either Ty T.Text
eitherTypeMsg t = case closed t of
                    False -> Right $ T.pack "Expression is not closed."
                    True  -> case maybeTy t of
                                Nothing -> Right $ T.pack "Expression is not typeable."
                                Just s  -> Left s

evalCommand :: [(String,VTerm)] -> T.Text -> T.Text
evalCommand lib txt = case parseC lib (T.unpack txt) of
                        Nothing -> T.pack "Syntax error."
                        Just (Eval t) -> let x = dbTerm t in case eitherTypeMsg x of
                                                                Left s -> T.pack $ pprint (normalize x) ++ " : " ++ typprint s
                                                                Right msg -> msg
                        Just (GetType t) -> let x = dbTerm t in case eitherTypeMsg x of
                                                                    Left s -> T.pack $ typprint s
                                                                    Right msg -> msg
                        Just (PrintTerm t) -> let x = dbTerm t in case eitherTypeMsg x of
                                                                    Left s -> T.pack $ pprint x ++ " : " ++ typprint s
                                                                    Right msg -> msg