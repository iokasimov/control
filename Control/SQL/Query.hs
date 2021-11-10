module Control.SQL.Query where

import "sqlite-simple" Database.SQLite.Simple (Query)

start_objective_event :: Query
start_objective_event = 
	"INSERT INTO events (objective_id, start) VALUES (?, strftime('%s', 'now'))"

get_just_created_event :: Query
get_just_created_event = 
	"SELECT event_id, strftime('%H:%M', start, 'unixepoch', 'localtime') \
	\FROM events WHERE objective_id = ? AND end IS NULL ORDER BY event_id DESC"

end_objective_event :: Query
end_objective_event =
	"UPDATE events SET end = strftime('%s', datetime('now')) \
	\WHERE objective_id = ? AND end IS NULL"

today_time_query :: Query
today_time_query = 
	"SELECT title, strftime('%H:%M', datetime(SUM (end - start), 'unixepoch')) \
	\FROM events JOIN objectives on events.objective_id = objectives.id \
	\WHERE start > strftime('%s', date('now')) AND end IS NOT NULL \
	\GROUP BY objective_id;"

all_unfinished_events :: Query
all_unfinished_events =
	"SELECT id, interest, behaviour, repeat, title, event_id, strftime('%H:%M', start, 'unixepoch', 'localtime') \
	\FROM events join objectives ON objectives.id = events.objective_id \
	\WHERE end IS NULL;"

timeline_today_events_query :: Query
timeline_today_events_query =
	"SELECT title, strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', end, 'unixepoch', 'localtime'), '..:..'), \
	\strftime('%H:%M', IFNULL(end, strftime('%s', datetime('now'))) - start,'unixepoch') \
	\FROM events JOIN objectives on events.objective_id = objectives.id \
	\WHERE start > strftime('%s', date('now')) AND start < strftime('%s', date('now', '+1 day'));"
