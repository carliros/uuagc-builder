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
  = prepOptions <$> pToken "-" <*> pList1 pLetter <*> pMaybe ((++) <$> pSymbol "=" <*> pNoSeparator)
  where prepOptions ss ll mExtra = ss ++ ll ++ maybe "" (id) mExtra

pUuagcCompleteOption :: Parser String
pUuagcCompleteOption
  = prepOptions <$> pToken "--" <*> pList1 pLetter <*> pMaybe ((++) <$> pSymbol "=" <*> pNoSeparator)
  where prepOptions ss ll mExtra = ss ++ ll ++ maybe "" (id) mExtra

pNoSeparator :: Parser String
pNoSeparator = pMunch (not . isSeparator) <?> "uuagc extra option"
