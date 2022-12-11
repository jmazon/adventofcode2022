import Data.Array
import Data.Char (ord)
import Data.List (find)
import Data.Sequence (ViewL((:<)))
import qualified Data.Sequence as Q
import qualified Data.Set as S

toAltitude :: Char -> Int
toAltitude 'E' = ord 'z'
toAltitude 'S' = ord 'a'
toAltitude  x  = ord  x

main :: IO ()
main = do
  rawGrid <- lines <$> getContents
  let h = length rawGrid
      w = length (head rawGrid)
      grid = listArray ((1,1),(h,w)) (concat rawGrid)

      neighbors (i,j) = filter (inRange (bounds grid))
                        [ (i+1,j), (i-1,j), (i,j+1), (i,j-1) ]

      Just target = find ((== 'E') . (grid !)) (indices grid)

      bfs _ _ q | Q.null q = []
      bfs goal cl (Q.viewl -> (p,d) :< q')
        | goal p = d : bfs goal cl' q'
        | p `S.member` cl = bfs goal cl q'
        | otherwise = d' `seq` bfs goal cl' (q' Q.>< q'')
        where
          d' = d + 1
          cl' = S.insert p cl
          q'' = Q.fromList $
                map (, d') $
                filter (`S.notMember` cl') $
                filter steppable $
                neighbors p
          steppable p' = toAltitude (grid ! p) - toAltitude (grid ! p') <= 1

      isGoal = (== 'S') . (grid !)

  print $ head $ bfs isGoal S.empty (Q.singleton (target,0))

  let isLow = (`elem` "aS") . (grid !)
  print $ minimum $ bfs isLow S.empty (Q.singleton (target,0))
