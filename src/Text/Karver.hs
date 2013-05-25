module Text.Karver where

import Text.Karver.Types
import Text.Karver.Parse

import Control.Applicative
import Data.Attoparsec.Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T

renderTemplate :: HashMap Text Value -> Text -> Text
renderTemplate varTable strTemplate = merge $
  case parseOnly render strTemplate of
    (Left err)  -> [Literal $ T.pack err]
    (Right res) -> res
  where render :: Parser [Tokens]
        render = many1 $ objectParser
                     <|> identityParser
                     <|> literalParser

        merge :: [Tokens] -> Text
        merge = T.concat . map mergeMap
        mergeMap (Literal x)  = x
        mergeMap (Identity x) =
          case H.lookup x varTable of
            (Just (String s)) -> s
            _                 -> T.empty
        mergeMap (Object i k) =
          case H.lookup i varTable of
            (Just (Map m)) ->
              case H.lookup k m of
                (Just x) -> x
                Nothing  -> T.empty
            _              -> T.empty
