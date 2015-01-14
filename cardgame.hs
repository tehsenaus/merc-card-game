import Data.Array
import Data.List
import Data.Maybe
import System.Random

data Tile = Tile {
	cards :: [Int]
} deriving Show

type Map = Array (Int,Int) (Maybe Tile)

data MercenaryState = Hidden | Revealed | Dead
	deriving Show

data Mercenary = Merc {
	team :: Int,
	mercNum :: Int,
	mercState :: MercenaryState,
	inventory :: [Int],
	loc :: (Int,Int)
} deriving Show

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



-- Game state Monad

newtype StateM s a = StateM {
	runState :: s -> (a, s)
}
instance Monad (StateM s) where
	return a = StateM $ \s -> (a,s)
	sp >>= spg = StateM $ \s ->
		let (a,s') = runState sp s
		in runState (spg a) s'
type GameStateM a = StateM State a


-- Dice 
-- ----

randomInt :: (Int,Int) -> GameStateM Int
randomInt r = StateM $ \s ->
	let (x,r') = randomR r (rng s)
	in (x, s { rng = r' })


roll :: GameStateM (Int,Int)
roll = do {
	a <- randomInt (1,6);
	b <- randomInt (1,6);
	return (a,b)
}


tileAt c = StateM (\s -> (
	(head . cards . fromJust . (\m -> m!c) . gameMap) s,
	s ))


takeCard = StateM (\s ->
	let c:cs = deck s
	in (c, s { deck = cs }) )

updateMerc :: Mercenary -> Mercenary -> GameStateM ()
updateMerc m m' = StateM (\s -> ((), s { mercs = upd (mercs s) }))
	where
		upd (_m:ms) = if m == _m then m':ms else upd ms




-- Actions
-- -------

data ActionType = NoCard | Attack | Card
data Action = Action {
	actionName :: String,
	actionType :: ActionType,
	stage :: Int,
	perform :: Mercenary -> GameStateM ()
}

instance Show Action where
	show a = actionName a




move c = Action ("move " ++ show c) NoCard 2 perform
	where perform m = do {
		t <- tileAt c;
		(a,b) <- roll;
		if a + b >= t then
			updateMerc m (m { loc = c })
		else return ()
	}

reinforce = Action ("reinforce") NoCard 5 perform
	where perform m = do {
		t <- tileAt (loc m);
		(a,b) <- roll;
		if a + b > t then do {
			c1 <- takeCard;
			c2 <- takeCard;
			updateMerc m (m { inventory = c1:c2:(inventory m) })
		} else return ()
	}


availableActions merc s =
	reinforce : 
	[ move c | c <- vicinity (loc merc) (gameMap s) ]





-- Game Map

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
		

vicinity (x,y) m = filter (f . (!) m) [(i,j)
	| i <- [max 1 (x-2)..min xdim (x+2)], j <- [max 1 (y-1)..min ydim (y+1)]]
	where
		f Nothing = False
		f (Just x) = True
		xdim = (fst . snd . bounds) m
		ydim = (snd . snd . bounds) m




packOfCards = take 52 $ cycle [1,2,3,4,5,6,7,8,9,10,11,12,13]
state = State md (makeMap md) packOfCards [
		Merc 1 1 Hidden [] (4,1)
	] (mkStdGen 1)
	where md = mapdim 4


dealMap s = (foldl f s . assocs . gameMap) s
	where
		f s (c,Nothing) = s
		f s (c,Just (Tile xs)) = s {
			gameMap = (gameMap s)//[(c,Just (Tile ((head (deck s)):xs)))],
			deck = tail (deck s)
		}

dealtState = dealMap state



-- User Input
-- ----------

selectAction :: Mercenary -> [Action] -> IO Action
selectAction m as = do {
	putStrLn ("Choose action for merc " ++ show (mercNum m) ++ ":");
	printActions 1 as;
	return (head as)
} where
	printActions i [] = do { return () }
	printActions i (a:as) = do {
		putStrLn ("(" ++ show i ++ ")\t" ++ actionName a);
		printActions (i+1) as
	}



draw (State mapdim a d ms _) = unlines (
	("Deck: " ++ ((show . length) d)) : [
	(concat [ drawTile (i,j) (a!(i,j)) | i <- [1..(fst mapdim)] ])
		| j <- [1..(snd mapdim)] ])
	where 
		drawTile c Nothing = "   "
		drawTile c (Just (Tile [])) = "   "
		drawTile c (Just (Tile (x:xs))) = drawMercs c a ms ++ drawCard x ++ " "

		drawMercs c a [] = " "
		drawMercs c a (m:ms) = 
			if loc m == c then (
				case mercState m of
					Hidden -> "o"
					Revealed -> "*"
					Dead -> drawMercs c a ms
			) else drawMercs c a ms

		drawCard 13 = "K"
		drawCard 12 = "Q"
		drawCard 11 = "J"
		drawCard 10 = "T"
		drawCard 1  = "A"
		drawCard x  = show x



main = do {
	_rng <- newStdGen;
	s <- return $ dealMap $ State md (makeMap md) packOfCards [m] _rng;
	putStr (draw s);
	a <- selectAction m as;
	(putStr . draw . snd . runState (perform a m)) s
} where	
	md = mapdim 4
	m = Merc 1 1 Hidden [] (4,1)
	as = availableActions m dealtState



