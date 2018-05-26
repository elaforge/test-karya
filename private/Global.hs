module Global (Map, mapMaybe, Text, txt, untxt, showt) where
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Data.Text (Text)


txt :: String -> Text
txt = Text.pack

untxt :: Text -> String
untxt = Text.unpack

showt :: Show a => a -> Text
showt = txt . show
