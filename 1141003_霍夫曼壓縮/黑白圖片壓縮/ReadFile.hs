module ReadFile
( paresFile
, Header(..)
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Word
import Data.Binary.Get

data Header = Header {
  _BM :: Word32,
  fileSize :: Word32,
  pixelData :: Word32,
  biSize :: Word32,
  biWidth :: Word32,
  biHeight :: Word32,
  biBitCount :: Word16,
  compressionMethod :: Word32,
  imageSize :: Word32,
  dpi_weight :: Word32,
  dpi_height :: Word32,
  colorAmount :: Word32,
  importantColorAmount :: Word32
} deriving (Show)

parseHeader :: Get Header
parseHeader = do
  bM <- getWord16le
  fS <- getWord32le -- 取3~6bytes 為檔案大小
  _ <- getWord32le
  pD <- getWord32le -- 取11~14bytes 為像素起始位置

  bS <- getWord32le -- 取15~18bytes DIB Header大小
  bW <- getWord32le -- 圖寬
  bH <- getWord32le -- 圖高
  _ <- getWord16le
  bBC <- getWord16le -- 色深
  cM <- getWord32le -- 壓縮方法
  iS <- getWord32le -- 圖片大小
  d_w <- getWord32le -- 橫向解析度
  d_h <- getWord32le -- 縱向解析度
  cA <- getWord32le -- 調色盤的顏色數
  iCA <- getWord32le -- 重要顏色數

  return Header {
    _BM = fromIntegral bM,
    fileSize = fS,
    pixelData = pD,
    biSize = bS,
    biWidth = bW,
    biHeight = bH,
    biBitCount = bBC,
    compressionMethod = cM,
    imageSize = iS,
    dpi_weight = d_w,
    dpi_height = d_h,
    colorAmount = cA,
    importantColorAmount = iCA
  }

parsePixel :: Get Word8
parsePixel = do
  grayScale <- getWord8
  return grayScale

parseRow :: Int -> Get [Word8] -- 先不考慮寬度不為4的倍數
parseRow width = sequence [parsePixel | _ <- [1..width]]

parseBody :: Int -> Int -> Get [[Word8]]
parseBody width height = do
  rows <- sequence [parseRow width | _ <- [1..height]]
  return $ reverse rows  -- 因為 BMP 從下往上存

getHeader :: BL.ByteString -> Header
getHeader bs = runGet parseHeader bs

getBody :: Int -> Int -> BL.ByteString -> [[Word8]]
getBody width height bs = runGet (parseBody width height) bs


paresFile :: BS.ByteString -> IO (Header, [[Word8]])
paresFile bmp = do
  let
    header = getHeader (BL.fromStrict bmp)
    offset = fromIntegral (pixelData header)
    width  = fromIntegral (biWidth header)
    height = fromIntegral (biHeight header)
    body   = getBody width height (BL.drop offset (BL.fromStrict bmp))
  return (header, body)
