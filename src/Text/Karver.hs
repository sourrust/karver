module Text.Karver where

import Text.Karver.Types
import Text.Karver.Parse

import Control.Applicative
import Data.Attoparsec.Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

renderTemplate :: HashMap Text Value -> Text -> Text
renderTemplate varTable = encode
  where encode :: Text -> Text
        encode tlp
          | T.null tlp = tlp
          | otherwise  = merge $
              case parseOnly render tlp of
                (Left err)  -> [LiteralTok $ T.pack err]
                (Right res) -> res

        render :: Parser [Tokens]
        render = many1 $ variableParser
                     <|> literalParser
                     <|> conditionParser

        hasVariable :: Text -> Bool
        hasVariable txt =
          case parseOnly variableParser' txt of
            (Right res) -> if T.null $ mergeMap res then False else True
            _           -> False

        merge :: [Tokens] -> Text
        merge = T.concat . map mergeMap
        mergeMap (LiteralTok x)  = x
        mergeMap (IdentityTok x) =
          case H.lookup x varTable of
            (Just (Literal s)) -> s
            _                 -> T.empty
        mergeMap (ObjectTok i k) =
          case H.lookup i varTable of
            (Just (Object m)) ->
              case H.lookup k m of
                (Just x) -> x
                Nothing  -> T.empty
            _              -> T.empty
        mergeMap (ListTok a i) =
          case H.lookup a varTable of
            (Just (List l)) -> l V.! i
            _               -> T.empty
        mergeMap (ConditionTok c t f) =
          if hasVariable c then merge $
            case parseOnly render t of
              (Left err)  -> [LiteralTok $ T.pack err]
              (Right res) -> res
            else f
