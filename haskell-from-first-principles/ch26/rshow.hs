import Control.Monad.Trans.Reader

rShow :: Show a => Reader a String
rShow = ReaderT $ pure . show
