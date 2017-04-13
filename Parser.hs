module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List

import Lang 
import Pprinter
import Typing

--Lambda terms with variable names as opposed to De Bruijn indices
data VTerm = VLitN Int | VSucc | VIter | VLitB Bool | VIf | VAst | VPair | VPr1 | VPr2 | VI1 | VI2 | VCase | V String | VApp VTerm VTerm | VLam String VTerm deriving (Eq,Show)

--turning a variable term into a De Bruijn term
dbTerm_aux :: [String] -> VTerm -> Term
dbTerm_aux ctxt (V v) = case elemIndex v ctxt of
                            Nothing -> Ind 0
                            Just i  -> Ind (i+1)
dbTerm_aux ctxt (VLam v t) = Lam (dbTerm_aux (v:ctxt) t)
dbTerm_aux ctxt (VApp t1 t2) = App (dbTerm_aux ctxt t1) (dbTerm_aux ctxt t2)
dbTerm_aux _ (VLitN n) = LitN n
dbTerm_aux _ VSucc = Succ
dbTerm_aux _ VIter = Iter
dbTerm_aux _ (VLitB b) = LitB b
dbTerm_aux _ VIf = If
dbTerm_aux _ VAst = Ast
dbTerm_aux _ VPair = Pair
dbTerm_aux _ VPr1 = Pr1
dbTerm_aux _ VPr2 = Pr2
dbTerm_aux _ VI1 = I1
dbTerm_aux _ VI2 = I2
dbTerm_aux _ VCase = Case

dbTerm :: VTerm -> Term
dbTerm = dbTerm_aux []

multipleAbs :: [String] -> VTerm -> VTerm
multipleAbs [] t     = t
multipleAbs (v:vs) t = VLam v (multipleAbs vs t)

languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["fun","print","type","N","B","U"]
           , Token.reservedOpNames = ["=>" , "[" , "," , "]",":="]
           }

otherLang =
  emptyDef { Token.identStart  = upper
           , Token.identLetter = alphaNum
}

lexer = Token.makeTokenParser languageDef

lexer2 = Token.makeTokenParser otherLang

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                   --   parens p
                                   -- takes care of the parenthesis and
                                   -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

name = Token.identifier lexer2


parseVars :: Parser [String]
parseVars = many1 identifier


parseNum :: Parser VTerm
parseNum = do
            n <- natural
            return (VLitN (fromInteger n))

parsePair :: Parser VTerm -> Parser VTerm
parsePair p = do
                reservedOp "["
                t1 <- p
                reservedOp ","
                t2 <- p
                reservedOp "]"
                return (VApp (VApp VPair t1) t2)

parseRes :: [(String,VTerm)] -> Parser VTerm
parseRes ps = do
              name <- name
              case (lookup name ps) of
                Nothing -> fail "Not found."
                Just t  -> return t

parseAbs :: Parser VTerm -> Parser VTerm
parseAbs p = do
              reserved "fun"
              vs <- parseVars
              reservedOp "=>"
              t <- p
              return (multipleAbs vs t)

parseName :: [(String,VTerm)] -> Parser VTerm
parseName lib = do
                   v <- identifier
                   case lookup v lib of
                    Nothing -> return (V v)
                    Just t  -> return t

parseNonApp :: [(String,VTerm)] -> Parser VTerm -> Parser VTerm
parseNonApp lib p =  parens p
                 <|> parseName lib
                 <|> parseNum
                 <|> parsePair p

parseTerm :: [(String,VTerm)] -> Parser VTerm
parseTerm lib =  parseAbs (parseTerm lib)
             <|> chainl1 (parseNonApp lib (parseTerm lib)) (return VApp)

parseT :: [(String,VTerm)] -> String ->  Maybe VTerm
parseT lib str = case (parse (parseTerm lib) "" str) of
                  Left e -> Nothing
                  Right t -> Just t

parsedbT :: [(String,VTerm)] -> String -> Maybe Term
parsedbT lib str = do
                    t <- parseT lib str
                    return (dbTerm t)

pp :: [(String,VTerm)] -> String -> String
pp lib str = case parsedbT lib str of
              Nothing -> "ERROR"
              Just t -> pprint t

ty :: [(String,VTerm)] -> String -> String
ty lib str = case parsedbT lib str of
              Nothing -> "Syntax Error"
              Just t -> pty t

parseDef :: [(String,VTerm)] -> Parser (String,VTerm)
parseDef lib = do
                name <- identifier
                reservedOp ":="
                t <- parseTerm lib
                return (name,t)

parseD :: [(String,VTerm)] -> String -> Maybe (String,VTerm)
parseD lib str = case parse (parseDef lib) "" str of
                    Left e -> Nothing
                    Right p -> Just p

parseGetType :: [(String,VTerm)] -> Parser VTerm
parseGetType lib = do
                    reserved "type"
                    t <- parseTerm lib
                    return t

parsePrint :: [(String,VTerm)] -> Parser VTerm
parsePrint lib = do
                  reserved "print"
                  t <- parseTerm lib
                  return t

data Command = Eval VTerm | GetType VTerm | PrintTerm VTerm deriving (Eq,Show)

parseCommand :: [(String,VTerm)] -> Parser Command
parseCommand lib =   do
                        t <- parsePrint lib
                        return (PrintTerm t)
                 <|> do
                        t <- parseGetType lib
                        return (GetType t)
                 <|> do
                        t <- parseTerm lib
                        return (Eval t)

parseC :: [(String,VTerm)] -> String -> Maybe Command
parseC lib str = case parse (parseCommand lib) "" str of
                  Left e -> Nothing
                  Right c -> Just c