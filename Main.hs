import Data.Word

import Control.Concurrent

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V

import qualified Codec.Picture as P

import System.Environment
import System.FilePath
import System.Directory

import Graphics.Gloss

--------------------------------------------------------------------------------

type Label     = Word8

type GrayImage = B.ByteString

data MNIST = MNIST
  { nrOfImages :: Int
  , imgWidth   :: Int
  , imgHeight  :: Int
  , mnistLabel :: Int -> Label
  , mnistImage :: Int -> GrayImage
  }

load :: IO MNIST
load = do
  trldata <- B.readFile "train-labels-idx1-ubyte"
  tridata <- B.readFile "train-images-idx3-ubyte"
  let trldata' = B.drop  8 trldata
      tridata' = B.drop 16 tridata
  return $ MNIST 60000 28 28 (\ i -> trldata' `B.index` i) (\ i -> B.take (28*28) . B.drop (i*28*28) $ tridata' )
  
--------------------------------------------------------------------------------

toImage :: Int -> GrayImage -> P.Image P.Pixel8
toImage zoom str = P.generateImage (\ x y -> str `B.index` (28*(y `quot` zoom) + (x `quot` zoom))) (28 * zoom) (28 * zoom)

dir :: FilePath
dir = "/dev/shm"

writeToFile :: GrayImage -> IO ()
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

cycleThroughImages :: [GrayImage] -> IO ()
cycleThroughImages xs = do
  mapM_ (\ img -> threadDelay 1000000 >> writeToFile img) $ xs
  cycleThroughImages xs
  
--------------------------------------------------------------------------------

type FloatImage = V.Vector Double

toFloatImage :: GrayImage -> FloatImage
toFloatImage str = V.fromList . map fromIntegral . B.unpack $ str

fromFloatImage :: FloatImage -> GrayImage
fromFloatImage = B.pack . map round . V.toList

addImages :: FloatImage -> FloatImage -> FloatImage
addImages a b = V.zipWith (+) a b

average :: [FloatImage] -> FloatImage
average [] = V.replicate (28*28) 0.0
average xs =
  let sum = foldr addImages (V.replicate (28*28) 0.0) xs
      nr  = length xs
  in V.map (/ fromIntegral nr) $ sum

average' :: MNIST -> [Int] -> GrayImage
average' mnist xs =
  fromFloatImage . average . map (toFloatImage . mnistImage mnist) $ xs

average'' :: MNIST -> [Int] -> FloatImage
average'' mnist xs = average . map (toFloatImage . mnistImage mnist) $ xs

onlyLabel :: Label -> MNIST -> [Int]
onlyLabel x mnist = filter ((== x) . mnistLabel mnist) [0..60000-1]

computeAverages' :: MNIST -> [GrayImage]
computeAverages' mnist = map (\ x -> average' mnist . onlyLabel x $ mnist) [0..9]

computeAverages :: MNIST -> [FloatImage]
computeAverages mnist = map (\ x -> average'' mnist . onlyLabel x $ mnist) [0..9]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "average" ] -> cycleThroughImages . computeAverages' =<< load
    [ "simple"  ] -> testInnerProduct
    [ "testGraphics1" ] -> testGraphics1 [0..60000-1]
    [ "testGraphics2" ] -> testGraphics2 [0..60000-1]
    [ "testGraphics3" ] -> testGraphics3

--------------------------------------------------------------------------------

innerProduct :: FloatImage -> FloatImage -> Double
innerProduct i1 i2 = V.foldr (+) 0 (V.zipWith (*) i1 i2)

testInnerProduct = do
  mnist <- load
  let as = computeAverages mnist
      rs d = map (\ i -> toFloatImage . mnistImage mnist $ i) . onlyLabel d $ mnist
      zs = [ (print (x, d, statistics . map (innerProduct (as!!x)) $ rs d)) | x <- [0..9], d <- [0..9] ]
  sequence_ zs

mean :: [Double] -> Double
mean [] = undefined
mean xs = sum xs / (fromIntegral . length $ xs)

deviation :: [Double] -> Double
deviation [] = undefined
deviation [_] = 0
deviation xs = sqrt . (/ (fromIntegral . length $ xs)) . sum . map ((\ x -> x*x) . (\ x -> x - m)) $ xs
  where
    m = mean xs

data Statistics = Statistics
  { st_populationMean      :: Double
  , st_populationDeviation :: Double
  } deriving Show
  
statistics :: [Double] -> Statistics
statistics xs = Statistics (mean xs) (deviation xs)

--------------------------------------------------------------------------------

testGraphics1 :: [Int] -> IO ()
testGraphics1 xs = do
  mnist <- load
  let scene :: Float -> Picture
      scene x = scale 25.0 25.0 . toPicture' . mnistImage mnist . round $ x
  animate (InWindow "Digit Window" (200, 200) (10, 10)) black scene

toPicture' :: GrayImage -> Picture
toPicture' img = bitmapOfByteString 28 28 (BitmapFormat TopToBottom PxRGBA)
                ( B.pack
                . concat
                . map (\ x -> [ x, x, x, 255 ])
                . B.unpack
                $ img ) False
                
toPicture :: FloatImage -> Picture
toPicture img = bitmapOfByteString 28 28 (BitmapFormat TopToBottom PxRGBA)
                ( B.pack
                . concat
                . map (\ x -> [ x, x, x, 255 ])
                . map round
                . V.toList
                $ img ) False
                
toArray :: Int -> Int -> [GrayImage] -> Picture
toArray nx ny xs =
  translate (fromIntegral (0 - nx*28) / 2.0) (fromIntegral (0 - ny*28) / 2.0) $
    Pictures [ Translate (fromIntegral $ x * 28) (fromIntegral $ y * 28) . toPicture' $ (xs !! (ny*y + x))
             | x <- [0 .. nx - 1]
             , y <- [0 .. ny - 1]
             ]

testGraphics2 :: [Int] -> IO ()
testGraphics2 xs = do
  mnist <- load
  let xs = map (mnistImage mnist) [0..60000-1]
  let scene :: Float -> Picture
      scene x = scale 25.0 25.0 . toPicture' . mnistImage mnist . round $ x
  -- animate (InWindow "Digit Window" (200, 200) (10, 10)) black scene
  display (InWindow "Digit Window" (200, 200) (10, 10)) black (toArray 40 20 xs)

testGraphics3 :: IO ()
testGraphics3 = do
  let nx = 40
      ny = 25
      n  = nx * ny
  mnist <- load
  let xs = V.fromList $ (map (mnistImage mnist) [0..60000-1] ++ replicate n (mnistImage mnist (60000-1)))
      offset = 0
  play (InWindow "Digit Window" (200, 200) (10, 10)) black 1 offset
       (\ offset -> toArray nx ny (V.toList (V.slice offset n xs)))
       (\ _ offset -> (offset + n) `rem` 60000)
       (\ _ offset -> offset)
