module Global (
    when, forever, unless, void, Map, mapMaybe, fromMaybe
    , (<>)
    , Text, txt, untxt, showt
    , whenJust, concatMapM
) where
import qualified Control.Monad as Monad
import Control.Monad (when, forever, unless, void)
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)


txt :: String -> Text
txt = Text.pack

untxt :: Text -> String
untxt = Text.unpack

showt :: Show a => a -> Text
showt = txt . show

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

concatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f = Monad.liftM mconcat . mapM f
