module Control.Pandora.SQLite where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Data.Semigroup ((<>))
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, FromRow (fromRow), field, open, query_, execute)

import Control.SQL.Query (start_of_today, end_of_today, start_of_tomorrow, end_of_tomorrow)

instance FromRow (Int :*: Int :*: Int :*: String :*: String :*: String) where
	fromRow = (\id status mode title start stop -> id :*: status :*: mode :*: title :*: start :*: stop)
		<$> field <*> field <*> field <*> field <*> field <*> field

update_task_status :: Query
update_task_status = "UPDATE tasks SET status = ? WHERE id = ?"

today_tasks :: Query
today_tasks =
	"SELECT tasks.id, status, mode, title, \
	\strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND IFNULL(stop <= " <> end_of_today <> ", 1) \
	\ORDER BY status, mode, start;"

tomorrow_tasks :: Query
tomorrow_tasks =
	"SELECT tasks.id, status, mode, title, \
	\strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_tomorrow <> " AND IFNULL(stop <= " <> end_of_tomorrow <> ", 1) \
	\ORDER BY status, mode, start;"

someday_todo :: Query
someday_todo =
	"SELECT tasks.id, 1, mode, title, strftime('%d.%m', start, 'unixepoch', 'localtime'), '' \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE stop IS NULL AND status = 1 AND mode = 0;"

overdue :: Query
overdue =
	"SELECT tasks.id, -2, mode, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 and IFNULL(stop < strftime('%s', 'now'), 0) \
	\ORDER BY start;"
