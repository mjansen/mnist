import qualified Data.ByteString as B
import qualified Data.Vector as V

readLabels :: IO B.ByteString
readLabels = B.drop 8 <$> B.readFile "train-labels-idx1-ubyte"

getImage :: B.ByteString -> Int -> B.ByteString
getImage str n = B.take (28*28) . B.drop (4*4 + n*28*28) $ str
