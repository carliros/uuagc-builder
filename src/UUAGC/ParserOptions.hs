{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction
                #-}

module UUAGC.ParserOptions (parseFileOptions) where

import Data.Char
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

type OptionList = [String]

parseFileOptions :: String -> [(FilePath, [String])]
parseFileOptions inp = runParser "x-uuagc-file-option" pOptions inp

pOptions :: Parser [(FilePath, OptionList)]
pOptions = pList1Sep pComma pOption

pOption :: Parser (FilePath, OptionList)
pOption = (,) <$> pQuotedString <* pSymbol ":" <*> pListOptions

pListOptions :: Parser OptionList
pListOptions = pListSep (pSpaces) pUuagcOption

pUuagcOption :: Parser String
pUuagcOption = pUuagcSimpleOption <|> pUuagcCompleteOption

pUuagcSimpleOption :: Parser String
pUuagcSimpleOption
  =   (\dash ch -> dash ++ (ch : [])) <$> pToken "-" <*> pLetter
  <|> (\a b c d -> a ++ (b:[]) ++ c ++ d) <$> pToken "-" <*> pLetter <*> pSymbol "=" <*> pNoSeparator

pUuagcCompleteOption :: Parser String
pUuagcCompleteOption
  = (\dash ch -> dash ++ (ch : [])) <$> pToken "--" <*> pLetter
 <|> (\a b c d -> a ++ (b:[]) ++ c ++ d) <$> pToken "--" <*> pLetter <*> pSymbol "=" <*> pNoSeparator

pNoSeparator :: Parser String
pNoSeparator = pMunch (not . isSeparator) <?> "uuagc extra option"
