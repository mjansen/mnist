import Data.Word

import Control.Concurrent

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V

import qualified Codec.Picture as P

import System.FilePath
import System.Directory

data MNIST = MNIST
  { nrOfImages :: Int
  , imgWidth   :: Int
  , imgHeight  :: Int
  , mnistLabel :: Int -> Word8
  , mnistImage :: Int -> B.ByteString
  }

load :: IO MNIST
load = do
  trldata <- B.readFile "train-labels-idx1-ubyte"
  tridata <- B.readFile "train-images-idx3-ubyte"
  let trldata' = B.drop  8 trldata
      tridata' = B.drop 16 tridata
  return $ MNIST 60000 28 28 (\ i -> trldata' `B.index` i) (\ i -> B.take (28*28) . B.drop (i*28*28) $ tridata' )
  
--------------------------------------------------------------------------------

toImage :: Int -> B.ByteString -> P.Image P.Pixel8
toImage zoom str = P.generateImage (\ x y -> str `B.index` (28*(y `quot` zoom) + (x `quot` zoom))) (28 * zoom) (28 * zoom)

dir :: FilePath
dir = "/dev/shm"

writeToFile :: B.ByteString -> IO ()
writeToFile str = do
  let fName = dir </> "digit.png"
  L.writeFile (fName <.> "part") . P.encodePng . toImage 20 $ str
  renameFile (fName <.> "part") fName
  
writeAll :: IO ()
writeAll = do
  mnist <- load
  cycleThroughImages . map (mnistImage mnist)
                     . filter ((== 2) . mnistLabel mnist)
                     $ [0..60000-1]

cycleThroughImages :: [B.ByteString] -> IO ()
cycleThroughImages xs = do
  mapM_ (\ img -> threadDelay 1000000 >> writeToFile img) $ xs
  cycleThroughImages xs
  
--------------------------------------------------------------------------------

toFloatImage :: B.ByteString -> V.Vector Double
toFloatImage str = V.fromList . map fromIntegral . B.unpack $ str

fromFloatImage :: V.Vector Double -> B.ByteString
fromFloatImage = B.pack . map round . V.toList

addImages :: V.Vector Double -> V.Vector Double -> V.Vector Double
addImages a b = V.zipWith (+) a b

average :: MNIST -> [Int] -> B.ByteString
average mnist xs =
  let xss = map (toFloatImage . mnistImage mnist) xs
      sum = foldr addImages (V.replicate (28*28) 0.0) xss
      nr  = length xs
  in fromFloatImage . V.map (/ fromIntegral nr) $ sum

onlyLabel :: Word8 -> MNIST -> [Int]
onlyLabel x mnist = filter ((== x) . mnistLabel mnist) [0..60000-1]

computeAverages :: MNIST -> [B.ByteString]
computeAverages mnist = map (\ x -> average mnist . onlyLabel x $ mnist) [0..9]

--------------------------------------------------------------------------------

main :: IO ()
main = cycleThroughImages . computeAverages =<< load
