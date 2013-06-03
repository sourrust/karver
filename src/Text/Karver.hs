module Text.Karver where

import Text.Karver.Types
import Text.Karver.Parse

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
        render = many1 $ choice [ variableParser
                                , conditionParser
                                , loopParser
                                , literalParser
                                ]

        hasVariable :: Text -> Bool
        hasVariable txt =
          case parseOnly variableParser' txt of
            (Right res) -> if T.null $ mergeMap varTable res
                             then False
                             else True
            _           -> False

        merge :: [Tokens] -> Text
        merge = T.concat . map (mergeMap varTable)
        mergeMap _ (LiteralTok x)       = x
        mergeMap vTable (IdentityTok x) =
          case H.lookup x vTable of
            (Just (Literal s)) -> s
            _                 -> T.empty
        mergeMap vTable (ObjectTok i k) =
          case H.lookup i vTable of
            (Just (Object m)) ->
              case H.lookup k m of
                (Just x) -> x
                Nothing  -> T.empty
            _              -> T.empty
        mergeMap vTable (ListTok a i) =
          case H.lookup a vTable of
            (Just (List l)) -> l V.! i
            _               -> T.empty
        mergeMap _ (ConditionTok c t f) =
          if hasVariable c
            then encode t
            else encode f
        mergeMap vTable (LoopTok a v b) =
          case H.lookup a vTable of
            (Just (List l)) ->
              let toks = case parseOnly render b of
                           (Left _)  -> []
                           (Right res) -> res
                  mapVars x = let x' = Literal x
                              in map (mergeMap (H.singleton v x')) toks
              in if null toks
                   then T.empty
                   else T.concat . V.toList $ V.map (T.concat . mapVars) l
            _               -> T.empty
