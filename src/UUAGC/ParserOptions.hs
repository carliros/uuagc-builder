{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module UUAGC.ParserOptions (parseFileOptions) where

import           Data.Char
import           Data.Maybe
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils

type OptionList = [String]

parseFileOptions :: String -> [(FilePath, [String])]
parseFileOptions = runParser "x-uuagc-file-option" pOptions

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
  where prepOptions ss ll mExtra = ss ++ ll ++ fromMaybe "" mExtra

pUuagcCompleteOption :: Parser String
pUuagcCompleteOption
  = prepOptions <$> pToken "--" <*> pList1 pLetter <*> pMaybe ((++) <$> pSymbol "=" <*> pNoSeparator)
  where prepOptions ss ll mExtra = ss ++ ll ++ fromMaybe "" mExtra

pNoSeparator :: Parser String
pNoSeparator = pMunch (not . isSeparator) <?> "uuagc extra option"
