module Control.SQL.Query where

import "sqlite-simple" Database.SQLite.Simple (Query)

start_of_today, start_of_tomorrow, end_of_today, end_of_tomorrow :: Query
start_of_today = "(strftime ('%s', 'now') - (strftime('%s', 'now', 'localtime') % 86400))"
start_of_tomorrow = "(strftime ('%s', 'now') - (strftime('%s', 'now', 'localtime') % 86400)) + 86400"
end_of_today = "(strftime ('%s', 'now') - (strftime('%s', 'now', 'localtime') % 86400)) + 86399"
end_of_tomorrow = "(strftime ('%s', 'now') - (strftime('%s', 'now', 'localtime') % 86400)) + 86400 + 86399"

start_objective_event :: Query
start_objective_event = 
	"INSERT INTO events (objective_id, start) VALUES (?, strftime('%s', 'now'))"

get_just_created_event :: Query
get_just_created_event = 
	"SELECT id, strftime('%H:%M', start, 'unixepoch', 'localtime') \
	\FROM events WHERE objective_id = ? AND stop IS NULL ORDER BY id DESC"

stop_objective_event :: Query
stop_objective_event =
	"UPDATE events SET stop = strftime('%s', datetime('now')) \
	\WHERE objective_id = ? AND stop IS NULL"

today_time_query :: Query
today_time_query = 
	"SELECT title, strftime('%H:%M', datetime(SUM (stop - start), 'unixepoch')) \
	\FROM events JOIN objectives on events.objective_id = objectives.id \
	\WHERE start > " <> start_of_today <> " AND stop IS NOT NULL \
	\GROUP BY objective_id;"

all_unfinished_events :: Query
all_unfinished_events =
	"SELECT objectives.id, title, events.id, strftime('%H:%M', start, 'unixepoch', 'localtime') \
	\FROM events join objectives ON objectives.id = events.objective_id \
	\WHERE stop IS NULL;"

timeline_today_events_query :: Query
timeline_today_events_query =
	"SELECT title, strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '..:..'), \
	\strftime('%H:%M', IFNULL(stop, strftime('%s', datetime('now'))) - start, 'unixepoch') \
	\FROM events JOIN objectives on events.objective_id = objectives.id \
	\WHERE start > " <> start_of_today <> " AND IFNULL(stop < " <> start_of_tomorrow <> ", 1);"

today_tasks_query :: Query
today_tasks_query =
	"SELECT status, title, strftime('%H:%M', start, 'unixepoch', 'localtime'), strftime('%H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start < " <> end_of_today <> ";"

tomorrow_tasks_query :: Query
tomorrow_tasks_query =
	"SELECT status, title, strftime('%H:%M', start, 'unixepoch', 'localtime'), strftime('%H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 AND start >= " <> start_of_tomorrow <> " AND stop <= " <> end_of_tomorrow <> " \
	\ORDER BY start;"
