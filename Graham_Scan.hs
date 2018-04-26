import Data.Function
import Data.List
{--
Left turn == Counter-Clockwise
Right turn == Clockwise
Straight == Straight--}
data Direction = Straight | LeftTurn | RightTurn deriving (Show, Eq)


points :: [(Double, Double)]
points = [(2.0,6.0), (4.0,6.0), (3.0,5.0), (0.1,4.0), (2.1,4.0), (4.1,4.0),
 (6.1,4.0), (1.3,3.0), (3.3,3.0), (5.3,3.0), (0.0,2.0), (2.1,2.0), (4.1,2.0),
 (6.1,2.0), (3.0,1.0), (2.0,0.0), (4.0,0.0)]
--}


{--function that finds the lowest point--}
lowest :: (Ord a, Ord b) => [(a, b)] -> (a, b)
lowest xs = head (sortBy (on compare fst) (take 2 (sortBy (on compare snd) xs)))


{--function that takes two 2D points and get returns the polar angle--}
polar :: (Ord a, Floating a) => (a , a) -> (a, a) -> a
polar (a1,a2) (b1,b2)
 | b2-a2>=0 && b1-a1>=0 = angle
 | b2-a2>=0 && b1-a1<=0 = angle+180
 | b2-a2<=0 && b1-a1<=0 = angle+180
 | b2-a2<=0 && b1-a1>=0 = angle+360
 where angle = ((atan ((b2-a2)/(b1-a1)))*180)/pi


--function that sorts the points according to polar angle
sorted :: (Ord a, Floating a) => (a, a) -> [(a, a)] -> [(a, a)]
sorted low xs = sortBy (on compare (polar low)) xs 


--function that finds the direction
findDir :: (Ord b, Num b) => (b, b) -> (b, b) -> (b, b) -> Direction
findDir (a1,a2) (b1,b2) (c1,c2)
 | res == 0 = Straight
 | res > 0 = LeftTurn
 | res < 0 = RightTurn
 where res = (b1 - a1)*(c2 - a2) - (b2 - a2)*(c1 - a1)


--Graham Scan
grahamS :: (Ord b, Num b) => [(b, b)] -> [(b, b)] -> [(b, b)]
grahamS ys [] = ys
grahamS ys xs = if (findDir (ys !! (length ys - 2)) (ys !! (length ys - 1)) (head xs)) /= RightTurn
 then grahamS (ys ++ [head xs]) (tail xs)
 else grahamS (take (length ys - 1) ys) xs


startScan xs = do 
 let ref = lowest points
 let apoints = sorted ref (delete ref points)
 let scanL = ref:(head apoints):[]
 let scaned = grahamS scanL (delete (head apoints) apoints)
 print scaned