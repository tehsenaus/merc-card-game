import Data.Array
import Data.List
import System.Random

data Tile = Tile {
	cards :: [Int]
} deriving Show

type Map = Array (Int,Int) (Maybe Tile)

data MercenaryState = Hidden | Revealed | Dead
data Mercenary = Merc {
	team :: Int,
	mercNum :: Int,
	mercState :: MercenaryState,
	inventory :: [Int],
	loc :: (Int,Int)
}
instance Eq Mercenary where
	(==) a b = team a == team b && mercNum a == mercNum b

type Deck = [Int]
data State = State {
	mapDim :: (Int,Int),
	gameMap :: Map,
	deck :: Deck,
	mercs :: [Mercenary],
	rng :: StdGen
}


updateMerc m m' s = s { mercs = upd (mercs s) }
	where
		upd (_m:ms) = if m == _m then m':ms else upd ms

data ActionType = NoCard | Attack | Card
data Action = Action {
	actionName :: String,
	actionType :: ActionType,
	stage :: Int,
	perform :: Mercenary -> State -> State
}

instance Show Action where
	show a = actionName a


tile :: Tile
tile = Tile []



mapdim mapsize = (mapsize * 2 - 1, mapsize * 2 - 1)
makeMap mapdim = array ((1,1),mapdim)
	[ ((i,j), makeTile i j)
		| i <- [1..(fst mapdim)], j <- [1..(snd mapdim)] ]
	where
		makeTile x y =
			if (x + y) `mod` 2 == 1 && xd <= yd
				then Just (Tile [])
				else Nothing
			where
				xd = abs (((fst mapdim + 1) `div` 2) - x)
				yd = y - (max 0 (y - (((snd mapdim) `div` 2) + 1)) * 2)
				--yd = (snd mapdim) - (abs (((snd mapdim) `div` 2) - y))
		


vicinity (x,y) m = filter (f . (!) m) [(i,j)
	| i <- [max 1 (x-2)..min xdim (x+2)], j <- [max 1 (y-1)..min ydim (y+1)]]
	where
		f Nothing = False
		f (Just x) = True
		xdim = (fst . snd . bounds) m
		ydim = (snd . snd . bounds) m



move c = Action ("move " ++ show c) NoCard 2 perform
	where perform m = updateMerc m (m { loc = c })



availableActions merc s =
	[ move c | c <- vicinity (loc merc) (gameMap s) ]




draw (State mapdim a d _) = unlines (
	("Deck: " ++ ((show . length) d)) : [
	(concat [ drawTile (a!(i,j)) | i <- [1..(fst mapdim)] ])
		| j <- [1..(snd mapdim)] ])
	where 
		drawTile Nothing = "   "
		drawTile (Just (Tile [])) = "   "
		drawTile (Just (Tile (x:xs))) = " " ++ drawCard x ++ " "
		drawCard 13 = "K"
		drawCard 12 = "Q"
		drawCard 11 = "J"
		drawCard 10 = "T"
		drawCard 1 = "A"
		drawCard x = show x



state = State md (makeMap md) ((take 52 . cycle) [1,2,3,4,5,6,7,8,9,10,11,12,13]) []
	where md = mapdim 4


dealMap s = (foldl f s . assocs . gameMap) s
	where
		f s (c,Nothing) = s
		f s (c,Just (Tile xs)) = s {
			gameMap = (gameMap s)//[(c,Just (Tile ((head (deck s)):xs)))],
			deck = tail (deck s)
		}

dealtState = dealMap state


main = do {
	putStr (draw dealtState);
	print (availableActions (Merc 1 1 Hidden [] (4,1)) dealtState);
	print (vicinity (4,1) (gameMap dealtState))
}



