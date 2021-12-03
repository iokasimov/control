module Control.Pandora.SQLite where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Semigroup ((<>))
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)

import Control.SQL.Query (start_of_today, end_of_today, start_of_tomorrow, end_of_tomorrow)

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
