module Lib (
    writeAsciiImage
) where

import           Codec.Picture        (DynamicImage (ImageRGB8),
                                       Image (imageData, imageHeight, imageWidth),
                                       Pixel (PixelBaseComponent), Pixel8,
                                       PixelRGB8 (PixelRGB8), convertRGB8,
                                       pixelMap, readImage)

import           Codec.Picture.Extra  (scaleBilinear)
import           Codec.Picture.Types  (pixelFoldMap)
import           Data.List            (intercalate)
import qualified Data.Vector.Storable as Vector
import           Data.Word            (Word8)
import           System.Environment   (getArgs)

writeAsciiImage :: String -> IO ()
writeAsciiImage imagePath = do
    imageRead <- readImage imagePath
    putStrLn (case imageRead of
        Left e      -> e
        Right image -> convertToAsciiArt image)

convertToAsciiArt :: DynamicImage -> String
convertToAsciiArt dynImage = intercalate "\n" (chunksOf width (Vector.foldr (:) [] charImageData))
    where
        width = 300
        orgImage = dynToImg dynImage
        image = scaleBilinear width (width `div` 2) orgImage
        charImageData = Vector.map pixelChar (imageData . toGreyScale $ image)


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = [[]]
chunksOf n xs = take n xs:chunksOf n (drop n xs)

pixelChar :: Word8 -> Char
pixelChar x = chars !! index
    where
        index = (length chars * fromIntegral x) `div` 256
        chars = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'."

dynToImg :: DynamicImage -> Image PixelRGB8
dynToImg = convertRGB8

toGreyScale :: Image PixelRGB8 -> Image Pixel8
toGreyScale = pixelMap colorAvg
    where
        colorAvg (PixelRGB8 r g b) = sum (map fromIntegral [r,g,b]) `div` 3
