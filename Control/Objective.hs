module Control.Objective where

import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

data Objective = Objective Int String deriving (Eq, Show)

instance FromRow Objective where
	fromRow = Objective <$> field <*> field

objective_title :: Objective -> String
objective_title (Objective _ title) = title
