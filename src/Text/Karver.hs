-- |
-- Module:      Data.Karver
-- Copyright:   Jeremy Hull 2013
-- License:     BSD3
--
-- Maintainer:  Jeremy Hull <sourdrums@gmail.com>
-- Stability:   experimental
-- Portability: unknown
--
-- The "Text.Karver" interface for translation 'Text' from it's template
-- syntax, to a generated value — based on the data that was given.

module Text.Karver where

import Text.Karver.Types
import Text.Karver.Parse

import Control.Applicative ((<$>))
import Data.Aeson (decode')
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy.Char8 as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)

-- | Renders a template
renderTemplate :: HashMap Text Value -- ^ Data map for variables inside
                                     --   a given template
               -> Text               -- ^ Template
               -> Text
renderTemplate varTable = encode
  where encode :: Text -> Text
        encode tlp
          | T.null tlp = tlp
          | otherwise  = merge $
              case parseOnly templateParser tlp of
                (Left err)  -> [LiteralTok $ T.pack err]
                (Right res) -> res

        merge :: [Token] -> Text
        merge = T.concat . map (decodeToken varTable)
        decodeToken _ (LiteralTok x)       = x
        decodeToken vTable (IdentityTok x) =
          case H.lookup x vTable of
            (Just (Literal s)) -> s
            _                 -> T.empty
        decodeToken vTable (ObjectTok i k) =
          case H.lookup i vTable of
            (Just (Object m)) ->
              case H.lookup k m of
                (Just x) -> x
                Nothing  -> T.empty
            _              -> T.empty
        decodeToken vTable (ListTok a i) =
          case H.lookup a vTable of
            (Just (List l)) -> case l V.! i of
                                 (Literal t) -> t
                                 _           -> T.empty
            _               -> T.empty
        decodeToken _ (ConditionTok c t f) =
          if hasVariable c
            then encode t
            else encode f
          where hasVariable txt =
                  case parseOnly variableParser' txt of
                    (Right res) ->
                      if T.null $ decodeToken varTable res
                        then False
                        else True
                    _           -> False
        decodeToken vTable (LoopTok a v b) =
          case H.lookup a vTable of
            (Just (List l)) ->
              let toks = case parseOnly templateParser b of
                           (Left _)  -> []
                           (Right res) -> res
                  mapVars x = let vTable' = H.insert v x vTable
                              in map (decodeToken vTable') toks
              in if null toks
                   then T.empty
                   else T.concat . V.toList $ V.map (T.concat . mapVars) l
            _               -> T.empty
        decodeToken _ (IncludeTok f) =
          unsafePerformIO $ encode . T.init <$> TI.readFile (T.unpack f)

-- | Similar to renderTemplate, only it takes JSON 'Text' instead of
-- a 'HashMap'
renderTemplate' :: Text -- ^ JSON data, for variables inside a given
                        --   template
                -> Text -- ^ Template
                -> Text
renderTemplate' json tpl =
  case decode' . L.pack $ T.unpack json of
    (Just hash) -> renderTemplate hash tpl
    Nothing     -> T.empty
