{-# OPTIONS -XTypeSynonymInstances #-}

module Game where

import BasicEval

import Control.Monad (when, forM_, forever)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Array.IO as A
import qualified Control.Exception as E

import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stdin, stdout, stderr)
import System.IO.Error (ioeGetErrorType, isUserErrorType)

type Fields = A.IOArray Int Field
data Player = Player { fields :: Fields } deriving (Show, Eq)
data Game = Game { proponent, opponent :: Player, applications :: Int, automode :: Bool, gameturn :: Integer } deriving (Show, Eq)

data Score = Draw | Won0 | Won1 deriving (Show, Eq)

instance Show Fields where
	show f = unsafePerformIO $ do
		assocs <- A.getAssocs f
		return $ show $ filter (\(_, Field val vit) -> vit /= 10000 || (show val /= "I")) assocs

type Context = St.StateT Game IO

instance Environment Context where
	readPropSlot n = St.get >>= \game -> lift $ A.readArray (fields $ proponent game) n
	writePropSlot n f = St.get >>= \game -> lift $ A.writeArray (fields $ proponent game) n f
	readOppSlot n = St.get >>= \game -> lift $ A.readArray (fields $ opponent game) n
	writeOppSlot n f = St.get >>= \game -> lift $ A.writeArray (fields $ opponent game) n f
	getAutoMode = St.gets automode
	countApplication = do
		game <- St.get
		let a = 1 + (applications game)
		if (a > 1000) then fail "Application limit exceeded" else St.put game { applications = a }

reset :: Context ()
reset = St.modify $ \game -> game { applications = 0}

switchPlayer :: Context ()
switchPlayer = St.modify $ \game -> game { proponent = opponent game, opponent = proponent game, gameturn = 1 + gameturn game }

-- catch "error x", PatternMatchFail and "fail x" exceptions
cCatch :: Context x -> (String -> Context x) -> Context x
cCatch run handler = do
		game <- St.get
		(res, game) <- lift $
			E.catch (
					E.catch (
							E.catch (wrapRun game) (wrapErrorCall game)
						) (wrapPatternMatchFail game)
				) (wrapIOException game)
		St.put game
		return res
	where
		wrapRun game = St.runStateT run game
		wrapErrorCall game e = St.runStateT (handler $ show (e:: E.ErrorCall)) game
		wrapPatternMatchFail game e = St.runStateT (handler $ show (e:: E.PatternMatchFail)) game
		wrapIOException game e = if (isUserErrorType $ ioeGetErrorType e) then St.runStateT (handler $ show e) game else E.throwIO e

runZombies :: Context ()
runZombies = do
	St.modify (\game -> game { automode = True })
	forM_ [0..255] $ \slot -> do
		Field val vit <- readPropSlot slot
		when (vit == -1) $ cCatch (do
				reset
				apply val valueIdentity
				return ()
			) (\s -> do
				lift $ hPutStrLn stderr $ "Zombie eval slot " ++ show slot ++ " stopped: " ++ s
			) >> writePropSlot slot (Field valueIdentity 0)
	St.modify (\game -> game { automode = False })

runMove :: Move -> Context ()
runMove m = do
	runZombies
	runMove' m

runMove' :: Move -> Context ()
runMove' m = do
	let slot = moveSlot m
	reset
	cCatch (do
			res <- applyMove m
			Field _ vit <- readPropSlot slot
			writePropSlot slot (Field res vit)
		) (\s -> do
			Field _ vit <- readPropSlot slot
			writePropSlot slot (Field valueIdentity vit)
			lift $ hPutStrLn stderr $ "Eval error: " ++ s
		)
-- 	St.get >>= \game -> lift $ hPutStrLn stderr (show game)
	switchPlayer

player0, player1 :: Context Player
player0 = St.get >>= \game -> return $ if gameturn game `mod` 2 == 0 then proponent game else opponent game
player1 = St.get >>= \game -> return $ if gameturn game `mod` 2 == 0 then opponent game else proponent game

countDeadFields :: Fields -> Context Int
countDeadFields fields = do
	elems <- lift $ A.getElems fields
	return $ length $ filter (\(Field _ vit) -> vit <= 0) elems

checkScore :: Context (Maybe Score)
checkScore = do
	game <- St.get
	deadSlots0 <- player0 >>= countDeadFields . fields
	deadSlots1 <- player1 >>= countDeadFields . fields
	if deadSlots0 == 256 then return $ Just (if deadSlots1 == 256 then Draw else Won1) else if deadSlots1 == 256 then return $ Just Won0 else
		if (gameturn game >= 2000000) then return $ Just (if deadSlots0 > deadSlots1 then Won1 else if deadSlots0 < deadSlots1 then Won0 else Draw) else return Nothing

runMoves = runGame . mapM_ runMove

initFields :: IO Fields
initFields = A.newArray (0, 255) (Field valueIdentity 10000)
initPlayer :: IO Player
initPlayer = initFields >>= return . Player
initGame :: IO Game
initGame = do
	p0 <- initPlayer
	p1 <- initPlayer
	return $ Game p0 p1 0 False 0

runGame :: Context x -> IO x
runGame f = do
	game <- initGame
	(res, _) <- St.runStateT f game
	return res

playGame :: Context Move -> Context Move -> IO Score
playGame p0 p1 = runGame $ l p0 p1 where
	l c n = do
		runZombies
		s <- checkScore
		case s of Just s -> return s; Nothing -> do c >>= runMove; s <- checkScore; case s of Just s -> return s; Nothing -> l n c
