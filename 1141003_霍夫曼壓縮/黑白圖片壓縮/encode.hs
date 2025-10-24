import ReadFile
import CreateTree
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Word
import Data.List (foldl')
import Text.Printf (printf)

combineToTuple :: Int -> String -> (Int, String)
combineToTuple vA vB = (vA, vB) -- 把次數和對應的編碼組成tuple

getDifferenceBody :: [[Word8]] -> [[Word8]]
getDifferenceBody body = differenceHead $ map getDifferenceRow body -- 最後更新第一直排
  where
    differenceHead (x:xs) = x : go (head x) xs
    go :: Word8 -> [[Word8]] -> [[Word8]] -- 改每一行的第一個 算與上方的差值
    go _ [] = []
    go last (z:zs) = newRow : go currentLast zs
      where
        currentLast = head z
        newZ = currentLast - last -- 拿本行第一格-上一行第一格
        newRow = newZ : tail z -- 把尾巴(不修改)直接接回
          
    -- xs = [x2, x3, ...]
    -- (x:xs) = [x1, x2, x3, ...]
    -- zipWith後 = [x2-x1, x3-x2, ...]
    getDifferenceRow :: [Word8] -> [Word8]
    getDifferenceRow (y:ys) = y : zipWith (-) ys (y:ys)

main :: IO ()
main = do
  bmp <- BS.readFile "Peppers.bmp" -- 讀入為ByteString
  (header, body) <- paresFile bmp -- (Header, [[Word8]])
  print header
  putStrLn ""
  putStrLn "原始霍夫曼："
  encode body
  putStrLn ""
  putStrLn "-----------------------------------------"
  putStrLn ""
  putStrLn "差值霍夫曼："
  encode $ getDifferenceBody body

encode :: [[Word8]] -> IO ()
encode body = do
  let
    timesTable = getTimesTable body -- Map Word8 Int
    totalPixels = sum (M.elems timesTable) -- 總像素數
  putStrLn "出現次數與機率："
  mapM_ (\(k,v) -> 
          let p = fromIntegral v / fromIntegral totalPixels :: Double
          in printf "鍵 %d: 出現 %d 次, 機率 %.4f\n" k v p
        ) (M.toList timesTable)
  putStrLn ""

  let
    encodeTree = getTree timesTable
    codingTable = getEncodePairs encodeTree -- Map Word8 String
  putStrLn "編碼表："
  mapM_ (\(key, value) -> putStrLn $ show key ++ ", " ++ value) (M.toList codingTable)
  putStrLn ""

  let
    finalMap = M.intersectionWith combineToTuple timesTable codingTable -- 把次數Map和編碼Map組起來
    encodedLength = foldl' (\acc x -> acc + fst (snd x) * length (snd (snd x))) 0 (M.toList finalMap) -- 次數*編碼長度 最後全部加起來
    -- ori_EncodedStringLength = 
  putStrLn $ "總長度：" ++ show encodedLength ++ " bits"