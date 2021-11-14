import "transformers" Data.Functor.Reverse (Reverse (Reverse))

import "ansi-terminal" System.Console.ANSI (cursorUp, clearScreen)
import "base" Control.Monad (forever, void)
import "base" System.IO (Handle, BufferMode (NoBuffering), hReady, stdin, hSetBuffering)
import "text" Data.Text (Text, pack)
import "sqlite-simple" Database.SQLite.Simple (Only (Only), open, query, query_, execute)
import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)
import "vty" Graphics.Vty (Vty, Event (EvKey), Key (KChar), standardIOConfig, mkVty, update, picForImage, (<->), blue, defAttr, string, withBackColor, withForeColor, green, nextEvent, shutdown)

import qualified "text" Data.Text.IO as T (putStrLn)

data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
	fmap f (Zipper bs x fs) = Zipper (f <$> bs) (f x) (f <$> fs)

focus :: Zipper a -> a
focus (Zipper _ x _) = x

up :: Zipper a -> Zipper a
up (Zipper [] x fs) = Zipper [] x fs
up (Zipper (b : bs) x fs) = Zipper bs b (x : fs)

down :: Zipper a -> Zipper a
down (Zipper bs x []) = Zipper bs x []
down (Zipper bs x (f : fs)) = Zipper (x : bs) f fs

print_zipper_objectives :: Zipper Objective -> IO ()
print_zipper_objectives (Zipper bs x fs) = void
	$ traverse (putStrLn . ("   " <>) . show) (Reverse $ objective_title <$> bs)
		*> putStrLn (" * " <> show (objective_title x)) *> traverse (putStrLn . ("   " <>) . show) (objective_title <$> fs)

-- main = print_zipper_objectives $ pack . show <$> Zipper [3, 2, 1] 4 [5, 6]

event_loop :: Vty -> IO ()
event_loop vty = nextEvent vty >>= \case
	EvKey (KChar 'q') _ -> pure ()
	-- EvKey (KChar x) _ -> print x *> event_loop vty
	_ -> event_loop vty

data Objective = Objective Int Text deriving (Eq, Show)

instance FromRow Objective where
	fromRow = Objective <$> field <*> field

objective_title :: Objective -> Text
objective_title (Objective _ title) = title

handler :: Vty -> StateT (Zipper Objective) IO ()
handler vty = do
	lift clearScreen
	lift $ cursorUp 1
	get >>= lift . print_zipper_objectives
	lift (nextEvent vty) >>= \case
		EvKey (KChar 'q') _ -> pure ()
		EvKey (KChar 'j') _ -> modify down *> handler vty
		EvKey (KChar 'k') _ -> modify up *> handler vty
		_ -> handler vty

whenKeyIsPressed :: Handle -> IO a -> IO (Maybe a)
whenKeyIsPressed handle getCh = hReady handle >>= go where

	go True = getCh >>= return. Just
	go _ = return Nothing

--main :: IO ()
--main = do
	--hSetBuffering stdin NoBuffering
	--ch <- whenKeyIsPressed stdin getChar
	--case ch of
		--Just c -> putStrLn $ "Key pressed: " ++ [c]
		--Nothing -> pure ()
	--main

main = do
	vty <- mkVty =<< standardIOConfig
	connection <- open "facts.db"
	query_ connection "SELECT id, title FROM objectives" >>= \case
		o : os -> evalStateT (handler vty) $ Zipper [] o os
