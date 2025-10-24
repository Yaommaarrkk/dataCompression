{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List ( isPrefixOf )

import System.Timeout -- 避免無線遞迴超時(debug用)
import Control.Exception (evaluate)
import Control.DeepSeq (deepseq)

data Node = Node Char Int (Maybe Node) (Maybe Node) deriving (Eq, Generic, NFData) -- c times left right
data Walk = L | R deriving (Show, Eq)

instance Show Node where
  show (Node c t l r) =
    "(" ++ show c ++ " " ++ show t ++ " " ++ showMaybe l ++ " " ++ showMaybe r ++ ")"

showMaybe :: Maybe Node -> String
showMaybe Nothing  = "."
showMaybe (Just n) = show n

getFixedCode :: Int -> Int -> [Char]
getFixedCode 1 0 = ['0']
getFixedCode 1 1 = ['1']
getFixedCode mul left 
  | left < mul = '0' : getFixedCode (mul `div` 2) left
  | otherwise  = '1' : getFixedCode (mul `div` 2) (left - mul)

updateWeight :: Maybe Node -> Maybe Node -- 以所有葉節點的權重來更新整棵樹的權重
updateWeight Nothing = Nothing
updateWeight (Just (Node c t Nothing Nothing)) = Just (Node c t Nothing Nothing) -- 葉節點就回傳權重
updateWeight (Just (Node c _ l r)) = Just (Node c (l_times + r_times) l_tree r_tree)
  where
    l_tree = updateWeight l
    r_tree = updateWeight r
    l_times = case l_tree of
      Just (Node _ t _ _) -> t
      Nothing -> 0
    r_times = case r_tree of
      Just (Node _ t _ _) -> t
      Nothing -> 0
    

newChar :: Maybe Node -> Set Char -> Char -> String -> (Maybe Node, Set Char, String)
newChar root set c str = 
  if isCharExist
    then (updateWeight $ succRoute newTree1 (fromJust $ getCharRoute newTree1 c) c, set, newStr1)
    else (updateWeight $ succRoute newTree3 (fromJust $ getCharRoute newTree3 c) c, Set.insert c set, newStr2)
    where
      isCharExist = Set.member c set
      newTree1 = Just (startSuccNode root c)
      newTree2 = fromJust $ nyt root nytRoute c
      newTree3 = upperSwitch newTree2 (nytRoute ++ [R])
      dynamicCode = fromJust $ getCharRoute root c
      newStr1 = str ++ toBinary dynamicCode
      newStr2 = str ++ toBinary nytRoute ++ fixedCode !! (ord c - ord 'a')
      toBinary :: [Walk] -> [Char]
      toBinary route = map (\w -> if w == L then '0' else '1') route
      nytRoute = fromJust $ getCharRoute root 'N'
      succRoute :: Maybe Node -> [Walk] -> Char -> Maybe Node
      succRoute = modifyRoute Succ
      nyt :: Maybe Node -> [Walk] -> Char -> Maybe Node
      nyt = modifyRoute Nyt

data ModifyType = Nyt | Succ -- NYT更新樹 | 已存在的Char葉子++
modifyRoute :: ModifyType -> Maybe Node -> [Walk] -> Char -> Maybe Node
modifyRoute _ Nothing _ _ = Nothing
modifyRoute Nyt (Just (Node c t l r)) [] char = Just $ Node ' ' 0 (Just (Node 'N' 0 Nothing Nothing)) (Just $ Node char 0 Nothing Nothing) -- 分裂
modifyRoute Succ (Just (Node c t l r)) [] char = Just $ Node c (t+1) l r
modifyRoute mType (Just (Node c t l r)) route@(L:xs) char = Just $ Node c t (modifyRoute mType l xs char) r 
modifyRoute mType (Just (Node c t l r)) route@(R:xs) char = Just $ Node c t l (modifyRoute mType r xs char)

startSuccNode :: Maybe Node -> Char -> Node
startSuccNode root target = fromJust $ upperSwitch (fromJust root) charRoute
  where
    charRoute = fromJust $ getCharRoute root target

getCharRoute :: Maybe Node -> Char -> Maybe [Walk] -- 找到目標字元的節點路徑
getCharRoute Nothing _ = Nothing
getCharRoute (Just (Node c times left right)) target
  | c == target = Just []
  | otherwise = 
      case getCharRoute right target of
        Just route -> Just (R : route)
        Nothing -> case getCharRoute left target of
          Just route -> Just (L : route)
          Nothing -> Nothing

mainSwitch :: Node -> [Walk] -> [Walk] -> Maybe Node -- 兩節點交換 交換後從交換後的位置繼續upperSwitch
mainSwitch _ [] _ = Nothing
mainSwitch _ _ [] = Nothing
mainSwitch tree route1 route2 = upperSwitch (fromJust newTree) (init route2) -- 把路徑末端刪掉 也就是換成檢查父節點
  where
    newTree = switchNode (Just tree) route1 route2

upperSwitch :: Node -> [Walk] -> Maybe Node -- 檢查目前的權重是否要交換 若無則往上一層父節點遞迴檢查
upperSwitch tree [] = Just tree
upperSwitch tree route =
  case getSwitchNode tree route of -- 是否需要交換
    Just found_route -> mainSwitch tree route found_route -- 呼叫兩節點交換 交換後從交換後的位置繼續upperSwitch
    Nothing -> upperSwitch tree (init route) -- 把路徑末端刪掉 也就是換成檢查父節點

switchNode :: Maybe Node -> [Walk] -> [Walk] -> Maybe Node -- 給兩個節點路徑做交換
switchNode root r1 r2 = replaceChild (replaceChild root r1 (fromJust node2)) r2 (fromJust node1)
  where
    node1 = toNode root r1
    node2 = toNode root r2

    replaceChild :: Maybe Node -> [Walk] -> Node -> Maybe Node -- 給一個路徑和一個新節點 用新節點覆蓋
    replaceChild Nothing _ _ = Nothing -- 路徑錯了 理論上不可能發生
    replaceChild _ [] newNode = Just newNode -- 路徑到了就掛上新node
    replaceChild (Just (Node c t l r)) (x:xs) newNode = 
      case x of
        L -> Just $ Node c t (replaceChild l xs newNode) r
        R -> Just $ Node c t l (replaceChild r xs newNode)

toNode :: Maybe Node -> [Walk] -> Maybe Node -- Route轉為Node
toNode Nothing _ = Nothing
toNode (Just node) [] = Just node
toNode (Just (Node c t l r)) (L : xs) = toNode l xs
toNode (Just (Node c t l r)) (R : xs) = toNode r xs

getSwitchNode :: Node -> [Walk] -> Maybe [Walk] -- 嘗試找到編號更大的節點
getSwitchNode root targetRoute = case notDirectDescendantList of
  [] -> Nothing
  (x:xs) -> Just x
  where
    sameWeightList = getBiggestNodeList root (getTimes (Just root) targetRoute) -- 取得所有權重相同的 按編號大小排列
    takeWhileTarget = takeWhile (/= targetRoute) sameWeightList -- 把編號小於等於自己的剃除
    notDirectDescendantList = filter (\route -> not $ isDirectDescendant targetRoute route) takeWhileTarget -- 過濾直系血親
    isDirectDescendant :: [Walk] -> [Walk] -> Bool
    isDirectDescendant xs ys = ys `isPrefixOf` xs
    

getBiggestNodeList :: Node -> Int -> [[Walk]] -- 得到權重相同、照編號排列的list
getBiggestNodeList root times = filter (\route -> getTimes (Just root) route == times) (bfsRightToLeft [(root, [])])

getTimes :: Maybe Node -> [Walk] -> Int
getTimes Nothing _ = error "Invalid path"
getTimes (Just (Node _ t _ _)) [] = t
getTimes (Just (Node _ _ l r)) (x:xs)
  | x == L = getTimes l xs
  | x == R = getTimes r xs
  | otherwise = error "getTimes Unexpected error"

bfsRightToLeft :: [(Node, [Walk])] -> [[Walk]]
bfsRightToLeft [] = []
bfsRightToLeft ((x, route) : xs) = route : bfsRightToLeft (xs ++ newSequence)
  where
    newSequence = 
      case x of
        Node _ _ Nothing Nothing -> []
        Node _ _ (Just left) Nothing -> [(left, route ++ [L])]
        Node _ _ Nothing (Just right) -> [(right, route ++ [R])]
        Node _ _ (Just left) (Just right) -> [(right, route ++ [R]), (left, route ++ [L])]

encode :: String -> (Node, Set Char, String)
encode input =
  foldl step (initTree, Set.empty, "") input
  where
    step :: (Node, Set Char, String) -> Char -> (Node, Set Char, String)
    step (tree, set, acc) c =
      let (newTree, newSet, out) = newChar (Just tree) set c []
      in (fromJust newTree, newSet, acc ++ out)

initTree :: Node
initTree = Node 'N' 0 Nothing Nothing -- 'N' -> NYT

fixedCode :: [[Char]]
-- fixedCode = map (getFixedCode 16) [0..19] ++ map (getFixedCode 8) [(15-5)..15]
fixedCode = ["000", "001", "010", "011", "100"]

main :: IO ()
main = do
  
  putStrLn "fixedCode: "
  print fixedCode
  putStrLn ""

  test "a"
  test "aa"
  test "ab"
  test "abc"
  test "abaccc"
  test "abacccc"
  test "ababba" -- 作業


test :: String -> IO ()
test input = do
  result <- timeout (2 * 10^6) $ do -- 限2秒
    let (tree, set, output) = encode input
    (tree, set, output) `deepseq` return (tree, set, output)

  putStrLn $ "input: " ++ input
  case result of
    Nothing -> putStrLn "⚠️  Time limit exceeded (3s)"
    Just (tree, set, output) -> do
      putStrLn "tree: "
      print tree
      putStrLn "output: "
      putStrLn output
      putStrLn "set: "
      print set
  putStrLn "" 