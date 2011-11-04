-define(COUNTER_SERVER_NAME(Namespace, Counter), list_to_atom("bstats_counter_" ++ atom_to_list(Namespace) ++ "_" ++ atom_to_list(Counter))).
-define(GAUGE_SERVER_NAME(Name), list_to_atom("bstats_gauge" ++ atom_to_list(Name))).

%TODO: this is clunky/move to env variables
-define(PER_SECOND_LIMIT, 5). % in minutes
-define(PER_MINUTE_LIMIT, 1). % in hours
-define(PER_HOUR_LIMIT, 24). % in hours
