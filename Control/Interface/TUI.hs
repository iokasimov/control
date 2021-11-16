module Control.Interface.TUI where

import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "ansi-terminal" System.Console.ANSI (cursorUp, clearScreen)
import "base" Control.Monad (forever, void)
import "base" Data.Char (toLower)
import "base" Data.List (isInfixOf)
import "base" System.IO (Handle, BufferMode (NoBuffering), hReady, stdin, hSetBuffering)
import "sqlite-simple" Database.SQLite.Simple (Only (Only), open, query, query_, execute)
import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)
import "vty" Graphics.Vty (Vty, Event (EvKey), Key (KEsc, KBS, KUp, KDown, KChar), standardIOConfig, mkVty, update, picForImage, (<->), blue, defAttr, string, withBackColor, withForeColor, green, nextEvent, shutdown)

import Control.Objective (Objective (Objective), objective_title)

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

filter_zipper :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
filter_zipper c (Zipper bs_ x fs_) = case (filter c bs_, c x, filter c fs_) of
	([], False, []) -> Nothing
	(b : bs, False, fs) -> Just $ Zipper bs b fs
	(bs, False, f : fs) -> Just $ Zipper bs f fs
	(bs, True, fs) -> Just $ Zipper bs x fs

print_zipper_objectives :: Zipper Objective -> IO ()
print_zipper_objectives (Zipper bs x fs) = void
	$ traverse (putStrLn . ("   " <>)) (Reverse $ objective_title <$> bs)
		*> putStrLn (" * " <> objective_title x) *> traverse (putStrLn . ("   " <>)) (objective_title <$> fs)

handler :: Vty -> StateT (String, Zipper Objective) IO ()
handler vty = do
	lift clearScreen
	get >>= \(p, z) -> lift $ do
		putStrLn $ "Search: " <> reverse p <> "\n"
		maybe (putStrLn "No such an objective...") print_zipper_objectives
			$ filter_zipper (\o -> isInfixOf (toLower <$> reverse p) $ toLower <$> objective_title o) z
	lift $ cursorUp 11111
	lift (nextEvent vty) >>= \case
		EvKey KEsc _ -> pure ()
		EvKey KDown _ -> cursor_down *> handler vty
		EvKey KUp _ -> cursor_up *> handler vty
		EvKey (KChar x) _ -> type_pattern x *> handler vty
		EvKey KBS _ -> remove_last_char *> handler vty
		_ -> handler vty

cursor_up, cursor_down :: StateT (String, Zipper Objective) IO ()
cursor_up = modify $ \(p, z) -> (p, up z)
cursor_down = modify $ \(p, z) -> (p, down z)

type_pattern :: Char -> StateT (String, Zipper Objective) IO ()
type_pattern c = modify $ \(p, z) -> (c : p, z)

remove_last_char :: StateT (String, Zipper Objective) IO ()
remove_last_char = modify $ \(p, z) -> (if null p then p else tail p, z)

main = do
	vty <- mkVty =<< standardIOConfig
	connection <- open "facts.db"
	query_ connection "SELECT id, title FROM objectives" >>= \case
		o : os -> evalStateT (handler vty) $ ("", Zipper [] o os)
