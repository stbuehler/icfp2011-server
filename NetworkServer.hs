
module Main where

import BasicEval
import Game

import qualified Network.Socket as N
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified System.Random as Random
import Control.Monad.Trans.Class (lift)

import System.IO
import System.Environment (getArgs)

genRandomChar = Random.getStdRandom (Random.randomR ('a', 'z'))
genRandomPw n = mapM (const genRandomChar) [1..n]
genRandomPws n = do
		pw0 <- genRandomPw n
		_snd pw0
	where
		_snd pw0 = do
			pw1 <- genRandomPw n
			if (pw0 == pw1) then _snd pw0 else return (pw0, pw1)

handleConnect :: (MVar Handle, MVar Handle) -> (String, String) -> N.Socket -> IO ()
handleConnect (sP0, sP1) (pw0, pw1) sock = go where
	go = do
		(client, addr) <- N.accept sock
		hPutStrLn stderr $ show addr ++ ": connected"
		h <- N.socketToHandle client ReadWriteMode
		hSetBuffering h $ BlockBuffering $ Just 1024
		s <- hGetLine h
		if (s == pw0) then try0 addr h else if (s == pw1) then try1 addr h else retry addr h "wrong password"
	try0 addr h = do
		res <- tryPutMVar sP0 h
		if (res) then hPutStrLn h "welcome player 0" >> hFlush h >> hPutStrLn stdout (show addr ++ ": player 0 connected") else retry addr h "player 0 already connected"
	try1 addr h = do
		res <- tryPutMVar sP1 h
		if (res) then hPutStrLn h "welcome player 1" >> hFlush h >> hPutStrLn stdout (show addr ++ ": player 1 connected") else retry addr h "player 1 already connected"
	retry addr h s = do
		hPutStrLn stderr $ show addr ++ ": " ++ s
		hPutStrLn h s
		hClose h
		go

connectLine host port password player = "(P=\"/tmp/icfp_pipe_$$\"; rm -f $P; mkfifo $P; nc -v " ++ host ++ " " ++ show port ++ " < $P | (echo " ++ password ++ "; head -n1 >&2; exec ./run " ++ show player ++ ") > $P; rm $P)"

setup :: String -> IO (Handle, Handle)
setup host = do
	(pw0, pw1) <- genRandomPws 10
	sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
	N.bindSocket sock $ N.SockAddrInet 0 0
	N.SockAddrInet port _ <- N.getSocketName sock
	hPutStrLn stdout $ show port
	hPutStrLn stdout $ connectLine host port pw0 0
	hPutStrLn stdout $ connectLine host port pw1 1
	N.listen sock 2
	sP0 <- newEmptyMVar
	sP1 <- newEmptyMVar
	forkIO $ handleConnect (sP0, sP1) (pw0, pw1) sock
	forkIO $ handleConnect (sP0, sP1) (pw0, pw1) sock
	h0 <- takeMVar sP0
	h1 <- takeMVar sP1
	return (h0, h1)

main = do
	hSetBuffering stdout $ LineBuffering
	hSetBuffering stderr $ LineBuffering
	args <- getArgs
	let host = case args of s:_ -> s; [] -> "0.0.0.0"
	(h0, h1) <- setup host
	let read0 = lift $ do m <- readMove h0; printMove h1 m; return m
	let read1 = lift $ do m <- readMove h1; printMove h0 m; return m
	playGame read0 read1
