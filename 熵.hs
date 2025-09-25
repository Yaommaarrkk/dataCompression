import Text.Printf (printf)
import Data.List (isPrefixOf, delete)
import Graphics.Win32.GDI (c_ArcTo)
import Control.Monad.Trans.State.Strict (put)

log2 :: Floating a => a -> a
log2 = logBase 2 

entropy :: Floating a => [a] -> a
entropy p = foldl1 (+) $ map (\x -> -x * log2 x) p

averageCodingLength :: Floating a => [Int] -> [a] -> a
averageCodingLength c p = sum $ zipWith (*) (map fromIntegral c) p

cLength :: [String] -> [Int]
cLength c = map length c

compressionRatio :: Floating a => Int -> a -> a
compressionRatio len avg = fromIntegral ori / avg
  where
    ori = ceiling $ log2 $ fromIntegral len

isSuffix :: String -> String -> Maybe String
isSuffix x y -- 如果x是y的前綴
  | x `isPrefixOf` y = Just (drop (length x) y)
  | otherwise        = Nothing

newTestTable :: Eq a => a -> [a] -> [a]
newTestTable y list = filter (/= y) list -- 把接下來要測試的自己刪掉

-- 基礎表(測試後漸漸丟掉) -> 基礎表+suffix表 -> 基礎表+suffix表(測試後漸漸丟掉) -> 回傳含suffix表 或是 錯誤
sardinasPatterson :: [String] -> [String] -> String -> [String] -> [String] -> Maybe [String]
sardinasPatterson ori suffix _ [] [] = Just $ ori ++ suffix -- 測試完畢
sardinasPatterson ori suffix _ (x:xs) [] = sardinasPatterson ori suffix x xs (newTestTable x (ori ++ suffix))
sardinasPatterson ori suffix test testA (y:ys) = do
  -- putStrLn $ "Testing: " ++ show ori ++ " // " ++ show suffix ++ " // " ++ show test ++ " // " ++ show testA ++ " // " ++ show (y:ys)
  case isSuffix test y of
    Nothing -> sardinasPatterson ori suffix test testA ys -- y不是test的前綴 keep going
    Just str -> -- y是test的前綴 加入suffix表
      case str `elem` suffix of
        True -> sardinasPatterson ori suffix test testA ys -- 已經在suffix表 keep going
        False ->
          case str `elem` ori of -- 查原始表
            True -> Nothing -- 衝突 回傳錯誤
            False -> sardinasPatterson ori (str:suffix) test (str:testA) (str:ys) -- 新找到suffix 加入表
          
printAll_codingPerformance :: Int -> [[Double]] -> [[String]] -> IO ()
printAll_codingPerformance _ [] _ = return ()
printAll_codingPerformance _ _ [] = return ()
printAll_codingPerformance counter (p:ps) (c:cs) = do
  putStrLn $ "---table" ++ show counter ++ "---"
  printf "Entropy: %.4f\n" (entropy p)
  printf "Average Coding Length: %.4f\n" avg
  printf "Compression Ratio: %.2f : 1\n" (compressionRatio (length p) avg)
  putStrLn ""
  printAll_codingPerformance (succ counter) ps cs
  where
    avg = averageCodingLength (cLength c) p

printAll_uniquelyDecodable :: Int -> [Maybe [String]] -> IO ()
printAll_uniquelyDecodable _ [] = return ()
printAll_uniquelyDecodable counter (Nothing:xs) = do
  putStrLn $ "---uniquelyDecodable" ++ show counter ++ "---"
  putStrLn "False"
  printAll_uniquelyDecodable (succ counter) xs
printAll_uniquelyDecodable counter (x:xs) = do
  putStrLn $ "---uniquelyDecodable" ++ show counter ++ "---"
  putStrLn "True"
  putStrLn "table:"
  putStrLn $ show x
  printAll_uniquelyDecodable (succ counter) xs

main :: IO ()
main = do
  let
    p1 :: [Double]
    p1 = [0.2, 0.25, 0.35, 0.1, 0.1]
    c1 :: [String]
    c1 = ["100", "10", "1", "1000", "1001"]
    p2 :: [Double]
    p2 = [0.15, 0.25, 0.55, 0.05]
    c2 :: [String]
    c2 = ["10", "100", "1000", "1"]
    p = [p1, p2]
    c = [c1, c2]
  printAll_codingPerformance 1 p c -- 從1開始編號

  let
    uni1 :: [String]
    uni1 = ["0", "01", "11", "111"]
    uni2 :: [String]
    uni2 = ["1", "10", "110", "111"]
    uni3 :: [String]
    uni3 = ["0", "01", "110", "111"]
    uni4 :: [String]
    uni4 = ["0", "10", "100", "1000"]
    uni5 :: [String]
    uni5 = ["0", "10", "110", "1110"] -- true測資
    uni6 :: [String]
    uni6 = ["01", "011", "0111"] -- true測資
    uni7 :: [String]
    uni7 = ["011","11111","10","111010","0"]
    uni8 :: [String]
    uni8 = ["0", "01", "11"] -- true測資
    uni = [uni1, uni2, uni3, uni4, uni5, uni6, uni7, uni8] -- 1~4為題目測資
    -- uniTest = uni8
  -- x <- sardinasPatterson uniTest [] (head uniTest) (tail uniTest) (newTestTable (head uniTest) uniTest)
  -- putStrLn $ show x
  -- return ()
  printAll_uniquelyDecodable 1 (map (\x -> sardinasPatterson x [] (head x) (tail x) (newTestTable (head x) x)) uni)
