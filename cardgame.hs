import Data.Array
import Data.List
import Data.Maybe
import System.Random
import qualified System.Random.Shuffle as RS
import qualified Control.Monad.State as SM
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.State (lift)

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

type GameStateM a = SM.State State a

state = SM.get
state's = SM.gets
setState :: s -> SM.State s ()
setState = SM.put
updState = SM.modify


-- Dice 
-- ----

randomInt :: (Int,Int) -> GameStateM Int
randomInt r = do {
	_rng <- state's rng;
	let (x,r') = randomR r _rng
	in do {
		updState $ \s -> s { rng = r' };
		return x
	}
}

shuffle :: [a] -> GameStateM [a]
shuffle xs = do {
	rs <- randoms (length xs) 1 xs;
	return $ RS.shuffle xs rs
} where
	randoms n i [x] = do { return [] }
	randoms n i (x:xs) = do {
		r <- randomInt (0,n-i);
		rs <- randoms n (i+1) xs;
		return $ r:rs;
	}

roll :: GameStateM (Int,Int)
roll = do {
	a <- randomInt (1,6);
	b <- randomInt (1,6);
	return (a,b)
}


tileAt c = SM.gets (head . cards . fromJust . (\m -> m!c) . gameMap)

setDeck d = updState $ \s -> s { deck = d }


takeCard :: GameStateM Int
takeCard = do {
	s <- state;
	let d = deck s in do {
		setDeck $ tail d;
		return $ head d;
	}
}


updateMerc :: Mercenary -> Mercenary -> GameStateM ()
updateMerc m m' = updState $ \s -> s { mercs = upd (mercs s) }
	where upd (_m:ms) = if m == _m then m':ms else upd ms




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

dealMap s = (foldl f s . assocs . gameMap) s
	where
		f s (c,Nothing) = s
		f s (c,Just (Tile xs)) = s {
			gameMap = (gameMap s)//[(c,Just (Tile ((head (deck s)):xs)))],
			deck = tail (deck s)
		}



-- User Input
-- ----------

type GameIO a = SM.StateT State IO a
hoistState = hoist $ return . runIdentity

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

drawMap :: GameIO ()
drawMap = do {
	s <- hoistState state;
	lift $ putStr $ draw s
}

shuffleDeck = do {
	s <- state;
	d <- shuffle $ deck s;
	setDeck d;
}


um = Merc 1 1 Hidden [] (4,1)

runGameRound :: GameIO ()
runGameRound = do {
	s <- hoistState state;
	a <- lift $ selectAction um $ availableActions um s;
	hoistState $ perform a um
}

runGame :: GameIO ()
runGame = do {
	hoistState shuffleDeck;
	runGameRound;
	drawMap
}


main = do {
	_rng <- newStdGen;
	s <- return $ dealMap $ State md (makeMap md) packOfCards [m] _rng;
	SM.runStateT runGame $ s
	--a <- selectAction m as;
	--(putStr . draw . snd . runState (perform a m)) s
} where	
	md = mapdim 4
	m = Merc 1 1 Hidden [] (4,1)
	--as = availableActions m dealtState



