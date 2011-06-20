
module BasicEval (
	Field (..),
	Card (..),
	Func1 (..), Func2 (..), Func3 (..),
	Value (..), valueIdentity,
	Move (..), moveSlot, printMove, readMove,
	callGet', callCopy', callInc', callDec', callAttack', callHelp', callRevive', callZombie',
	Environment (..),
	apply,
	applyMove,
	card,
) where

import Control.Monad (when)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Char as Char
import System.IO

data Field = Field { fieldValue :: !Value, fieldVitality :: !Int } deriving (Show, Eq)

data Card = Card_I | Card_zero | Card_succ | Card_dbl | Card_get | Card_put | Card_S | Card_K | Card_inc | Card_dec | Card_attack | Card_help | Card_copy | Card_revive | Card_zombie deriving (Enum, Eq)

data Func1 = Func_I | Func_succ | Func_dbl | Func_get | Func_put | Func_inc | Func_dec | Func_copy | Func_revive deriving (Eq)
data Func2 = Func_K | Func_zombie deriving ( Eq)
data Func3 = Func_S | Func_attack | Func_help deriving (Eq)
data Value = Const !Int | AppF1_B0 !Func1 | AppF2_B0 !Func2 | AppF2_B1 !Func2 !Value | AppF3_B0 !Func3 | AppF3_B1 !Func3 !Value | AppF3_B2 !Func3 !Value !Value deriving (Eq)

valueIdentity :: Value
valueIdentity = AppF1_B0 Func_I

data Move = MoveLeft !Card !Int | MoveRight !Int !Card deriving (Eq)

moveSlot :: Move -> Int
moveSlot (MoveLeft _ slot) = slot
moveSlot (MoveRight slot _) = slot

cardNames = [(Card_I, "I"),(Card_zero, "zero"),( Card_succ, "succ"),( Card_dbl, "dbl"),( Card_get, "get"),( Card_put, "put"),( Card_S, "S"),( Card_K, "K"),( Card_inc, "inc"),( Card_dec, "dec"),( Card_attack, "attack"),( Card_help, "help"),( Card_copy, "copy"),( Card_revive, "revive"),( Card_zombie, "zombie")]
cardNamesX = map (\(a,b) -> (b,a)) cardNames
func1Names = [(Func_I, "I"),(Func_succ, "succ"),(Func_dbl, "dbl"),(Func_get, "get"),(Func_put, "put"),(Func_inc, "inc"),(Func_dec, "dec"),(Func_copy, "copy"),(Func_revive, "revive")]
func2Names = [(Func_K, "K"),(Func_zombie, "zombie")]
func3Names = [(Func_S, "S"),(Func_attack, "attack"),(Func_help, "help")]

instance Show Card where
	show f = let Just n = lookup f cardNames in n
instance Show Func1 where
	show f = let Just n = lookup f func1Names in n
instance Show Func2 where
	show f = let Just n = lookup f func2Names in n
instance Show Func3 where
	show f = let Just n = lookup f func3Names in n
instance Show Value where
	show (Const n) = show n
	show (AppF1_B0 f) = show f
	show (AppF2_B0 f) = show f
	show (AppF2_B1 f x) = show f ++ "(" ++ show x ++ ")"
	show (AppF3_B0 f) = show f
	show (AppF3_B1 f x) = show f ++ "(" ++ show x ++ ")"
	show (AppF3_B2 f x y) = show f ++ "(" ++ show x ++ ")" ++ "(" ++ show y ++ ")"
instance Show Move where
	show (MoveLeft c pos) = (show c) ++ " " ++ (show pos)
	show (MoveRight pos c) = (show pos) ++ " " ++ (show c)

pCard :: ReadP.ReadP Card
pCard = do
	s <- ReadP.many1 (ReadP.satisfy Char.isAlpha)
	case lookup s cardNamesX of
		Just c -> return c
		Nothing -> ReadP.pfail
pmPos = ReadP.readS_to_P reads >>= \i -> if (i >= 0 && i <= 255) then return i else ReadP.pfail
pMove = ReadP.skipSpaces >> (pMoveLeft ReadP.<++ pMoveRight) where
	pMoveLeft = do c <- pCard; ReadP.skipSpaces; pos <- pmPos; ReadP.skipSpaces; return $ MoveLeft c pos
	pMoveRight = do pos <- pmPos; ReadP.skipSpaces; c <- pCard; ReadP.skipSpaces; return $ MoveRight pos c

instance Read Card where
	readsPrec _ = ReadP.readP_to_S pCard
instance Read Move where
	readsPrec _ = ReadP.readP_to_S pMove

printMove :: Handle -> Move -> IO ()
printMove h (MoveLeft c pos) = hPutStr h ("1\n" ++ show c ++ "\n" ++ show pos ++ "\n") >> hFlush h
printMove h (MoveRight pos c) = hPutStr h ("2\n" ++ show pos ++ "\n" ++ show c ++ "\n") >> hFlush h

readMove :: Handle -> IO Move
readMove h = do
	lr <- hGetLine h
	sf <- hGetLine h
	st <- hGetLine h
	if (lr == "1") then (do
			let f = read sf :: Card
			let t = read st :: Int
			return $ MoveLeft f t
		) else (do
			let f = read sf :: Int
			let t = read st :: Card
			return $ MoveRight f t
		)

callGet' :: (Environment m) => Int -> m Value
callGet' i = readPropSlot i >>= \(Field val vit) -> if (vit > 0) then return val else fail "get: Slot not alive"
callCopy' :: (Environment m) => Int -> m Value
callCopy' i = readOppSlot i >>= \(Field val _) -> return val
callInc' :: (Environment m) => Int -> m ()
callInc' i = do
	autoMode <- getAutoMode
	if autoMode then
			readPropSlot i >>= \(Field val vit) -> when (vit > 0 && vit < 65535) $ writePropSlot i (Field val (vit+1))
		else
			readPropSlot i >>= \(Field val vit) -> when (vit > 0) $ writePropSlot i (Field val (vit-1))
callDec' :: (Environment m) => Int -> m ()
callDec' i = do
	autoMode <- getAutoMode
	if autoMode then
			readOppSlot (255-i) >>= \(Field val vit) -> when (vit > 0) $ writeOppSlot (255-i) (Field val (vit-1))
		else
			readOppSlot (255-i) >>= \(Field val vit) -> when (vit > 0 && vit < 65535) $ writeOppSlot (255-i) (Field val (vit+1))
callAttack' :: (Environment m) => Int -> Int -> Int -> m ()
callAttack' i j n = do
	autoMode <- getAutoMode
	let op = if autoMode then (+) else (-)
	Field ival ivit <- readPropSlot i
	when (ivit < n) $ fail "attack: not enough vitality"
	writePropSlot i (Field ival (ivit - n))
	Field jval jvit <- readOppSlot (255-j)
	when (jvit > 0) $ writeOppSlot (255-j) (Field jval (clip 0 65535 $ jvit `op` (n*9) `div` 10))
callHelp' :: (Environment m) => Int -> Int -> Int -> m ()
callHelp' i j n = do
	autoMode <- getAutoMode
	let op = if autoMode then (-) else (+)
	Field ival ivit <- readPropSlot i
	when (ivit < n) $ fail "attack: not enough vitality"
	writePropSlot i (Field ival (ivit - n))
	Field jval jvit <- readPropSlot (255-j)
	when (jvit > 0) $ writePropSlot (255-j) (Field jval (clip 0 65535 $ jvit `op` (n*11) `div` 10))
callRevive' :: (Environment m) => Int -> m ()
callRevive' i = readPropSlot i >>= \(Field val vit) -> when (vit <= 0) $ writePropSlot i (Field val 1)
callZombie' :: (Environment m) => Int -> Value -> m ()
callZombie' i v = readOppSlot (255-i) >>= \(Field val vit) -> if (vit <= 0) then writeOppSlot (255-i) (Field v (-1)) else fail "zombie: target not dead"

class (Monad m) => Environment m where
	readPropSlot :: Int -> m Field
	writePropSlot :: Int -> Field -> m ()
	readOppSlot :: Int -> m Field
	writeOppSlot :: Int -> Field -> m ()
	getAutoMode :: m Bool

	callGet :: Int -> m Value
	callGet = callGet'
	callCopy :: Int -> m Value
	callCopy = callCopy'
	callInc :: Int -> m ()
	callInc = callInc'
	callDec :: Int -> m ()
	callDec = callDec'
	callAttack :: Int -> Int -> Int -> m ()
	callAttack = callAttack'
	callHelp :: Int -> Int -> Int -> m ()
	callHelp = callHelp'
	callRevive :: Int -> m ()
	callRevive = callRevive'
	callZombie :: Int -> Value -> m ()
	callZombie = callZombie'
	countApplication :: m ()

clip l u n = if (n < l) then l else if (n > u) then u else n
constValue n = Const $ clip 0 65535 n

getInt :: (Environment m) => Value -> m Int
getInt (Const n) = return n
getInt _ = fail "Expected Integer, not Function"

getSlot :: (Environment m) => Value -> m Int
getSlot (Const n) = if (n >= 0 && n <= 255) then return n else fail "Slot index out of range"
getSlot _ = fail "Expected Integer, not Function"

retI :: (Environment m) => m Value
retI = return $ valueIdentity

apply :: (Environment m) => Value -> Value -> m Value
_apply :: (Environment m) => Value -> Value -> m Value
-- count applications
apply f x = countApplication >> _apply f x

_apply (Const _) _ = fail "Expected Function, got Integer"
-- partical applications
_apply (AppF2_B0 f) val = return $ AppF2_B1 f val
_apply (AppF3_B0 f) val = return $ AppF3_B1 f val
_apply (AppF3_B1 f x) val = return $ AppF3_B2 f x val
-- evaluate functions
_apply (AppF1_B0 Func_I) val = return val
_apply (AppF1_B0 Func_succ) val = getInt val >>= return . constValue . (1+)
_apply (AppF1_B0 Func_dbl) val = getInt val >>= return . constValue . (2*)
_apply (AppF1_B0 Func_get) val = getSlot val >>= callGet
_apply (AppF1_B0 Func_put) val = retI
_apply (AppF3_B2 Func_S f g) val = do h <- apply f val; y <- apply g val; apply h y
_apply (AppF2_B1 Func_K x) val = return x
_apply (AppF1_B0 Func_inc) val = getSlot val >>= callInc >> retI
_apply (AppF1_B0 Func_dec) val = getSlot val >>= callDec >> retI
_apply (AppF3_B2 Func_attack i j) val = do i <- getSlot i; j <- getSlot j; n <- getInt val; callAttack i j n; retI
_apply (AppF3_B2 Func_help i j) val = do i <- getSlot i; j <- getSlot j; n <- getInt val; callHelp i j n; retI
_apply (AppF1_B0 Func_copy) val = getSlot val >>= callCopy
_apply (AppF1_B0 Func_revive) val = getSlot val >>= callRevive >> retI
_apply (AppF2_B1 Func_zombie i) val = do i <- getSlot i; callZombie i val; retI


applyMove :: (Environment m) => Move -> m Value
applyMove m = do
	Field val vit <- readPropSlot $ moveSlot m
	when (vit <= 0) $ fail "Applied card to dead slot"
	case m of
		MoveLeft c _ -> apply (card c) val
		MoveRight _ c -> apply val (card c)

card :: Card -> Value
card Card_I = AppF1_B0 Func_I
card Card_zero = Const 0
card Card_succ = AppF1_B0 Func_succ
card Card_dbl = AppF1_B0 Func_dbl
card Card_get = AppF1_B0 Func_get
card Card_put = AppF1_B0 Func_put
card Card_S = AppF3_B0 Func_S
card Card_K = AppF2_B0 Func_K
card Card_inc = AppF1_B0 Func_inc
card Card_dec = AppF1_B0 Func_dec
card Card_attack = AppF3_B0 Func_attack
card Card_help = AppF3_B0 Func_help
card Card_copy = AppF1_B0 Func_copy
card Card_revive = AppF1_B0 Func_revive
card Card_zombie = AppF2_B0 Func_zombie
