import "transformers" Data.Functor.Reverse (Reverse (Reverse))

import "base" Control.Monad (void)
import "text" Data.Text (Text, pack)
import qualified "text" Data.Text.IO as T (putStrLn)

data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
	fmap f (Zipper bs x fs) = Zipper (f <$> bs) (f x) (f <$> fs)

print_zipper_items :: Zipper Text -> IO ()
print_zipper_items (Zipper bs x fs) = void
	$ traverse (putStrLn . ("   " <>) . show) (Reverse bs)
		*> putStrLn (" * " <> show x) *> traverse (putStrLn . ("   " <>) . show) fs

main = print_zipper_items $ pack . show <$> Zipper [3, 2, 1] 4 [5, 6]
