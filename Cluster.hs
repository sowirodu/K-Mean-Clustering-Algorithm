module Cluster where
import Data.List
import Data.Maybe
import Debug.Trace
import System.Directory.Internal.Prelude (on)

--Points are an x coordinate, a y coordinate, and an integer label.
type Point = (Double, Double, Int)
type Center = Point
--A cluster is a center and a list of points.
type Cluster = (Center, [Point])

-- All undefined values and functions should be completed. Your code will compile and test 
-- -- (with the -- test flag) even if some functions are left undefined.
--
-- --                                       Milestone
--

--Given a list of elements and an integer k, return k evenly spaced elements of the list.
--As a first attempt, simply take the first k elements. Fix this after you finish the rest of the
--project.
recNum :: [Int] -> Int -> [a] -> [a]
recNum car k [] = []
recNum car k (x:xs)
    | k `elem` car = x:next
    | otherwise = next
    where next = recNum car (k - 1) xs

getKElems :: Int -> [a] -> [a]  
getKElems k [] = []
getKElems 1 (x:xs) = [x]
getKElems k (x:xs) = 
    let lst = x:xs
        sublen = length lst - 1
        incraSize = (fromIntegral sublen :: Float)/((fromIntegral k :: Float) - 1)
        incraTook = [round(incraSize * fromIntegral x) | x <- [0..k-1]]
    in recNum incraTook sublen lst


--Return the Euclidean distance between two points. You may work in squared distances if you
--prefer.
eucDist :: Point -> Point -> Double
eucDist p1 p2 = (\(x1,y1,z1) (x2,y2,z2) -> sqrt ((x1 - x2)^2 + (y1 - y2)^2)) p1 p2
--Example: eucDist (0,0,10) (1,1,10) < eucDist (0,0,10) (0,2,10)

--Return the Manhattan distance between two points: the distance between the points based on
--strictly horizontal and vertical movement, as if on a city grid.
manhatDist :: Point -> Point -> Double
manhatDist p1 p2 = (\(x1,y1,z1) (x2,y2,z2) -> abs(x1 - x2) + abs(y1 - y2)) p1 p2
--Example: manhatDist (0,0,10) (1,1,10) == manhatDist (0,0,10) (0,2, 10)

--Return the Chebyshev distance between two points: the maximum between the x-distance and the
--y-distance, as if diagonal movement was free, like for a King in chess.
chebyDist :: Point -> Point -> Double
chebyDist p1 p2 = (\(x1,y1,z1) (x2,y2,z2) -> max (abs(x2 - x1)) (abs(y2 - y1))) p1 p2
--Example: chebyDist (0,0,10) [(0,5,10) == chebyDist (0,0,10) (5,5,10)

--Return the traffic-aware Manhattan distance: count horizontal distances twice as much as vertical.
trafficDist :: Point -> Point -> Double
trafficDist p1 p2 = (\(x1,y1,z1) (x2,y2,z2) -> 2*abs(x1 - x2) + abs(y1 - y2)) p1 p2
--Example: trafficDist (0,0,10) (0,10,10) == trafficDist (0,0,10) (5,0,10)

--Return the township-aware Manhattan distance. The label of the point is taken to be the township
--the point is in.  A penalty factor of 2 is applied to the distance between points in different
--townships.
townshipDist :: Point -> Point -> Double
townshipDist p1 p2 = (\(x1,y1,z1) (x2,y2,z2) -> if z1 == z2 then abs(x1 - x2) + abs(y1 - y2) else 2*abs(x1 - x2) + 2*abs(y1 - y2)) p1 p2
--Example: townshipDist (0,0,10) (1,1,20) == 2*townshipDist (0,0,10) (1,1,10) 

--Given a list of doubles, compute their average. You may need fromIntegral.
average :: [Double] -> Double
average lstDbl = sum lstDbl / genericLength lstDbl
--Example:  average [0,5,10] = 5.0

--Given a ranking function and a list of elements, return the element with the minimum rank.
minimize :: (a -> Double) -> [a] -> a
minimize func lst = head[x | x <- lst, func x  == minimum( map func lst)]
--Example: minimize (fromIntegral . length) ["aaaa", "b"] = "b"

--Given a bucketing function, a list of buckets, and a list of items to be placed in buckets, 
--and returns the list of bucket, items-in-buckets pairs.
--Go take your old buildCorpus function, copy it, and turn it into a HOF (higher-order function).
-- Bucket is a HOF, because it takes a function as input. You can write it using recursion, other
-- HOFs, or list comprehensions as you choose.
bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
bucket function [] items = []
bucket function (b:bs) items = (b, [x | x <- items, (function x) == b]):bucket function bs items
--Example:  bucket length [1..3] ["Hi","my","job","is","fun","!"]
--[(1,["!"]),(2,["Hi","my","is"]),(3,["job","fun"])]
--

--Full project!

--Given a metric, a list of centers, and a point, return the center closest to the point.
--Hint: you've already written a higher-order function to do this.
assignPoint :: (Point -> Center -> Double) -> [Center] -> Point -> Center
assignPoint metric lstCen pnt = minimize (metric pnt) lstCen 
--Examples: assignPoint eucDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (0.0,0.0,-1)
--          assignPoint trafficDist  [(0,0,-1),(5,7,-1)] (5,0,100) = (5.0,7.0,-1)

--Given a metric, a list of centers, and a list of point, return the clusters, where each point is
--assigned to the nearest center.
--Hint: you've already written a higher-order function to do this.
assignPoints :: (Point -> Center -> Double) -> [Center] -> [Point] -> [Cluster]
assignPoints metric lstCen lstPnt = bucket (assignPoint metric lstCen) lstCen lstPnt
--Examples:
--let testClusters = assignPoints trafficDist [(1,1,-1),(2,2,-1)] tenPoints 
--
--[(c, length ps) | (c,ps) <- testClusters]
--[((1.0,1.0,-1),1),((2.0,2.0,-1),9)]
--
--testClusters
--[((1.0,1.0,-1),[(1.0,7.0,700)]),
-- ((2.0,2.0,-1),[(7.0,1.0,100),(7.0,3.0,200),(8.0,1.0,300),(8.0,2.0,400),(7.5,3.5,500),
--                (2.0,7.0,600),(3.0,7.0,800),(2.0,8.0,900),(2.0,6.0,1000)])]


--Given a metric and a cluster, return the mean of the points in the cluster.
--The existing center is NOT a point, and should be ignored.
--The label should be the label of the closest point to the new center. 
--Since you can't take the mean of an empty list of points, return Nothing in that case.
findMean :: (Point -> Center -> Double) -> Cluster -> Maybe Center
findMean metric (_,[]) = Nothing
findMean metric cluster =  let f = sum [(\(x,y,z) -> x) elem |elem <- snd cluster]
                               s = sum [(\(x,y,z) -> y) elem |elem <- snd cluster]
                               l = length (snd cluster) 
                               fs = f / fromIntegral l
                               ss = s / fromIntegral l
                               c = assignPoint metric (snd cluster) (fs,ss,-1)
                               t = (\(x,y,z) -> z) c
                         in  Just (fs, ss, abs t)
--Example: findMean eucDist ((3,3,0), [(0,0,0), (10,10,0), (2,2,1)]) = Just (4.0,4.0,1)

--Given a metric and a list of clusters, relocate all the centers to the mean of their clusters. Be
--sure to return only the valid centers. If any cluster is empty, simply remove it.
moveCenters :: (Point -> Center -> Double) -> [Cluster] -> [Center]
moveCenters metric clus =
    let validCluster = [ x | x <- clus, snd x /= []]
    in [fromMaybe (0,0,0 ) (findMean metric y) | y <- validCluster]
--Example:  moveCenters trafficDist testClusters  = [(1.0,7.0,700),(5.166666666666667,4.277777777777778,200)]

--Given a metric, k, and a list of clusters, first move the centers, and then reassign the points
--to the new centers.
--Note that clusters can become empty and disappear. For full credit, replace missing clusters as
--described on the website.
splitClusters :: Int -> [Center] -> [Cluster] -> [Center]
splitClusters k cntrs clst = let clst_pt = [snd cl| cl <- clst]
                                 max_clst = maximumBy (compare `on` length) clst_pt 
                                 max_center = fst (head (filter (\(x,y) -> y == max_clst) clst))
                                 rndel = [el | el <- max_clst, el /= max_center] 
                             in if (length rndel) > 1 then cntrs ++ [head rndel] else cntrs

improveClusters :: (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
improveClusters metric k clusters = let pts = concat [snd cluster | cluster <- clusters] 
                                        cntrs = moveCenters metric clusters
                                        new_cntrs = if (length cntrs) == k then cntrs else splitClusters k cntrs clusters
                                    in assignPoints metric new_cntrs pts
--Example: let newClusters = improveClusters trafficDist 2 testClusters 
--[(c, length ps) | (c,ps) <- newClusters]
--[((1.0,7.0,700),5),((5.166666666666667,4.277777777777778,200),5)]

--iterationLimit should be used by kMeans to limit how many times improveClusters can be called.
--Remember variables are not mutable.
iterationLimit = 100
--Given a metric, k, and a list of points, create the initial clusters and repeatedly 
--improve them, until they stop changing.
updateClusters :: Int -> (Point -> Center -> Double) -> Int -> [Cluster] -> [Cluster]
updateClusters it metric k clst = if it >= iterationLimit then clst 
                                  else let new_clst = improveClusters metric k clst 
                                       in if clst == new_clst then clst 
                                                              else updateClusters (it+1) metric k new_clst
kMeans :: (Point -> Center -> Double) -> Int -> [Point] -> [Cluster]
kMeans metric k points = updateClusters 0 metric k (assignPoints metric (getKElems k points) points)
