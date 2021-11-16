module Control.Objective where

import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

data Objective = Objective Int String deriving Eq

instance Show Objective where
	show (Objective _ title) = title

instance FromRow Objective where
	fromRow = Objective <$> field <*> field
