module CreateTree
( getTree,
  getTimesTable,
  getEncodePairs,
  Tree(..)
) where
import qualified Data.Map.Strict as M
import Data.Word
import Data.List (foldl', insertBy, sortOn)
import Data.Ord (comparing)

-- 霍夫曼是滿樹 所以用 Tree = Leaf | Node
-- 而不是 Node = Node (Maybe Node) (Maybe Node) 這種可以只有單邊子樹的寫法
data Tree = Leaf Word8 Int | Node Int Tree Tree deriving (Show) 

getNodeTimes :: Tree -> Int
getNodeTimes (Leaf _ i) = i
getNodeTimes (Node i _ _) = i


getTimesTable :: [[Word8]] -> M.Map Word8 Int
getTimesTable body = foldl' (\m x -> M.insertWith (+) x 1 m) M.empty (concat body) -- 這裡用map(n log n) 但其實array會更快


sortTreeList :: [Tree] -> [Tree]
sortTreeList = sortOn getNodeTimes

updateTreeList :: [Tree] -> [Tree]
updateTreeList [] = [Leaf 0 0] -- 理論上不會發生
updateTreeList [t] = [t] -- 合併到只剩一棵樹了
updateTreeList (x1:x2:xs) = updateTreeList newList -- 傳進來前就先排序好了 所以直接拿最小兩個合併
  where
    mergedTree = Node (getNodeTimes x1 + getNodeTimes x2) x1 x2
    newList = insertBy (comparing getNodeTimes) mergedTree xs -- insertBy :: 排序函式 -> 新元素 -> 目標List -> 新List

getTree :: M.Map Word8 Int -> Tree
getTree m = head $ updateTreeList $ sortTreeList oriTrees
  where
    oriTrees = map (\x -> Leaf (fst x) (snd x)) (M.toList m) -- Map轉成List再轉成Leafs

getEncodePairs :: Tree -> M.Map Word8 String
getEncodePairs tree = go tree ""
  where
    go (Leaf b _) code = M.singleton b code -- new一個Map的鍵值對
    go (Node _ left right) code =
      M.union (go left (code ++ "0")) (go right (code ++ "1")) -- 合併左右的Map